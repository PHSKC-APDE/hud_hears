# Header ----
# Author: Danny Colombara
# Date: May 23, 2022
# R version 4.1.2
# Purpose: Pursue difference in differences (DiD) models to assess whether 
#          those with positive exits have greater changes in wages 1 year post 
#          exit when compared to those with negative exits
#           
# Notes: need to account for repeated measures on individuals and clustering by 
#        by households. Will address both issues with inclusion of random effects
#
#        Will assess whether parallel trends assumption holds by looking at the 
#        the change in wages between the fourth quarter prior to exit and the 
#        the exit quarter
#
#        Prefer to use percent area median income (AMI) rather than the exact
#        wage when possible because % AMI accounts for household size and is 
#        also the criteria for entrance into public housing. 
#
#        The models below only have potential confounders that were found to 
#        be associated with both the exposure (exit type) and outcome (wages). 
#        

# Set up ----
    rm(list=ls())
    options(scipen = 999)
    pacman::p_load(lubridate, rads, data.table, DBI, odbc, ggplot2, lme4, marginaleffects)

    # output folder
    outputdir <- "C:/Users/dcolombara/King County/DPH Health And Housing - Documents/HUD HEARS Study/wage_analysis/output/"
    
    # easy SQL connections
    devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/apde/main/R/create_db_connection.R") 
 
# Custom functions ----
    # standard ggplot formatting ----
    formatplots <- function(myplot){
      myplot <- myplot +
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Negative' = '#2ca25f')) +
        scale_y_continuous(labels=scales::dollar_format())+
        theme(panel.grid.major = element_line(color = "white"), 
              panel.background = element_rect(fill = "white"), 
              panel.border = element_rect(colour = "black", fill=NA, size=.5),  
              plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(size=6),
              legend.position = "right",
              legend.background = element_rect(fill="white", size=0.5, linetype="solid", color ="white"), 
              legend.title = element_text(size = 12), 
              legend.key = element_rect(fill = "white", color = "white"),
              legend.text = element_text(size = 10))
        
      return(myplot)
    }
    
    # save plots with proper dimensions ----
    saveplots <- function(plot.object = NULL, plot.name = NULL){
      ggsave(paste0(outputdir, plot.name, ".pdf"),
             plot = plot.object, 
             dpi=600, 
             width = 6, 
             height = 4, 
             units = "in") 
      ggsave(paste0(outputdir, plot.name, ".png"),
             plot = plot.object, 
             dpi=600, 
             width = 6, 
             height = 4, 
             units = "in") 
    }
       
# Load data ----
    # open connection 
    hhsaw16 = create_db_connection( # this is prod
      server = "hhsaw", 
      prod = T, 
      interactive = F
    )
    
    # pull from SQL
    raw <- setDT(DBI::dbGetQuery(conn = hhsaw16, 
                                 "SELECT * FROM [hudhears].[wage_analytic_table]"))
    
    # get confounder vars
    load(paste0(outputdir, "confounders.Rdata"))
    confounders <- confounders$confounders
    if(length(confounders) > 1){ 
      confounders <- paste(confounders, collapse = " + ")
      }
    
# Preparatory data manipulation ----
    raw[, exit_year := as.factor(exit_year)]
    raw[, exit := as.integer(exit)]

# Create dt1 for assessing parallel trend assumption from quarter -4 to quarter 0 (exit) ----
    dt1 <- copy(raw)
    
    dt1[qtr == -4, time := -1]
    dt1[qtr == 0, time := 0]
    dt1[exit_category == "Negative", exit := 0]
    dt1[exit_category == "Positive", exit := 1]
    
    dt1 <- dt1[!is.na(time)]
    
# Model 1: assess parallel trends prior to exit ----
    # model ----
      mod1.formula <- paste0("wage ~ ", 
                             "exit*time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod1 <- lme4::lmer(mod1.formula, data = dt1)
      mod1.tidy <- as.data.table(broom.mixed::tidy(mod1, conf.int = T))

    # test if p-value for interaction term is < 0.05 ----
      mod1.test <- suppressWarnings(lmerTest::lmer(mod1.formula, data = dt1, REML = FALSE))
      mod1.test <- as.data.table(coef(summary(mod1.test)), keep.rownames = T)    
      if( mod1.test[rn == "exit:time"][["Pr(>|t|)"]] < 0.05) {
        print(mod1.test)
        message("\nThe exit:time interaction term is significant, so fails test of parallel trend prior to exit")
      }

    # average marginal effects (just for curiosity) ----
      mod1.margin.summary <- as.data.table(summary(margins::margins(mod1))) 
      mod1.margin.summary[]
    
    # create table of predictions for slopes in ggplot ... use setDF b/c can't use data.table ----
      # Try three methods to get predicted values ---
        # Using prediction::prediction ----
          # good, but doesn't provide CI
            mod1.preds <- as.data.table(
              summary(margins::prediction(mod1, 
                                          data = setDF(copy(dt1)), 
                                          at = list(time=c(-1, 0), exit = c(0, 1)))
                      )
              )
            mod1.preds[`at(exit)` == 0, exit_category := "Negative"][`at(exit)` == 1, exit_category := "Positive"]
            mod1.preds <- mod1.preds[, .(time = `at(time)`, exit_category, wage = Prediction)]
            mod1.preds[]
  
        # Using stats::predict (actually predict.merMod) ----
          # good, but doesn't provide C
            mod1.preds <- predict(mod1, 
                              newdata = data.table(expand.grid(time = c(-1, 0), exit = c(0, 1)), 
                                                   housing_time_at_exit = mean(dt1$housing_time_at_exit)), 
                              re.form=~0)
            mod1.preds[]      
        
        # Using marginaleffects::predictions ----
          # good estimates + se & CI
            mod1.preds <- as.data.table(marginaleffects::predictions(mod1, 
                                                     newdata = datagrid(time=c(-1, 0), exit = c(0, 1)), 
                                                     re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
            mod1.preds[exit == 0, exit_category := "Negative"][exit == 1, exit_category := "Positive"]
            mod1.preds <- mod1.preds[, .(time, exit, exit_category, wage = predicted, se = std.error, lower = `conf.low`, upper = `conf.high`)]
            mod1.preds[]

          # calculate counterfactual for positive exits using 10,000 draws----
            set.seed(98104)
            counterfactual <- data.table(neg.minus1 = rnorm(10000, mean = mod1.preds[exit == 0 & time == -1]$wage, sd = mod1.preds[exit == 0 & time == -1]$se), 
                                         neg.zero = rnorm(10000, mean = mod1.preds[exit == 0 & time == 0]$wage, sd = mod1.preds[exit == 0 & time == 0]$se), 
                                         pos.minus1 = rnorm(10000, mean = mod1.preds[exit == 1 & time == -1]$wage, sd = mod1.preds[exit == 1 & time == -1]$se))
            counterfactual[, diff := neg.zero - neg.minus1]
            counterfactual[, counterfactual := pos.minus1 + diff]
            counterfactual[, mean := mean(counterfactual)]
            counterfactual[, sd := sd(counterfactual)]
            counterfactual <- unique(counterfactual[, .(time = 0, exit = 1, exit_category = "Counterfactual", 
                                                        wage = mean, se = sd, 
                                                        lower = mean - (qnorm(0.975) * sd), 
                                                        upper = mean + (qnorm(0.975) * sd))])
            
          # add rows for counterfactual to mod1.preds ----
            mod1.preds <- rbind(
              mod1.preds,
              copy(mod1.preds)[exit == 1 & time == -1][, exit_category := "Counterfactual"], 
              counterfactual)
            
            mod1.preds[]
            
            
    # Plot data prior to exit ----
      dt1.plot = copy(dt1)
      dt1.plot[exit == 0, time := time - .015] # add tiny shift for easier visualization
      dt1.plot[exit == 1, time := time + .015] # add tiny shift for easier visualization
      
      plot1 <- ggplot() +
        # geom_point(data = dt1.plot, aes(x = time, y = wage, color = exit_category)) + 
        geom_line(data = mod1.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
        geom_line(data = mod1.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
        labs(title = paste0("Population level trends 1 year prior to exit"), 
             subtitle = "Model 1: Assess parallel trends prior to exit",
             caption = paste0("", mod1.formula), 
             x = "", 
             y = "Quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit"), breaks=c(-1, 0)) 
        
      plot1 <- formatplots(plot1) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f'))

      plot(plot1)
      
    # Save plot ----
      saveplots(plot.object = plot1, plot.name = 'figure_1_prior_trends')

# Model 2: match based on initial wages to create more homogeneous groups ----
    # also hope it will reduce the problem with non-parallel slopes prior to exit
    # create sets based on wage brackets ----
      dt1[, wagecat := cut(wage, breaks = c( seq(0, 20000, 5000), 40000))]
      dt1[, wagecatlow :=  as.character(as.numeric(gsub("\\((.+),.*", "\\1", wagecat ) ) + 1) ]
      dt1[, wagecathigh :=  as.character(as.numeric(gsub("[^,]*,([^]]*)\\]", "\\1", wagecat ) )) ]
      dt1[, wagecat := paste0("[", wagecatlow, ",", wagecathigh, "]")]
      dt1[wage == 0 | wagecat == "[1,5000]", wagecat := "[0,5000]"]
      dt1.wagecat.time0 = dt1[time == 0, .(hh_id_kc_pha, id_kc_pha, wagecat)]
      dt1[, c("wagecat", "wagecatlow", "wagecathigh") := NULL]
      dt1 <- merge(dt1, dt1.wagecat.time0, by = c("hh_id_kc_pha", "id_kc_pha"), all = T)
      dt1[, wagecat := as.factor(wagecat)]
      
    # model ----
      mod2.formula <- paste0("wage ~ ", 
                             "exit*time + ", 
                             confounders, " + ",
                             "(1 | wagecat) + ", # random intercept initial wage category
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod2 <- lme4::lmer(mod2.formula, data = dt1)
      mod2.tidy <- as.data.table(broom.mixed::tidy(mod2, conf.int = T))
      
    # test if p-value for interaction term is < 0.05 ----
      mod2.test <- suppressWarnings(lmerTest::lmer(mod2.formula, data = dt1, REML = FALSE))
      mod2.test <- as.data.table(coef(summary(mod2.test)), keep.rownames = T)    
      if( mod2.test[rn == "exit:time"][["Pr(>|t|)"]] < 0.05) {
        print(mod2.test)
        message("\nThe exit:time interaction term is significant, so fails test of parallel trend prior to exit")
      }

    # average marginal effects (just for curiosity) ----
      mod2.margin.summary <- as.data.table(summary(margins::margins(mod2))) 
      mod2.margin.summary[]
      
    # create table of predictions for slopes in ggplot ----
      mod2.preds <- as.data.table(summary(margins::prediction(mod2, data = setDF(copy(dt1)), at = list(time=c(-1, 0), exit = c(0, 1)))))
      mod2.preds[`at(exit)` == 0, exit_category := "Negative"][`at(exit)` == 1, exit_category := "Positive"]
      mod2.preds <- mod2.preds[, .(time = `at(time)`, exit_category, wage = Prediction)]
      mod2.preds[]
      
      mod2.preds <- as.data.table(marginaleffects::predictions(mod2, 
                                              newdata = datagrid(time=c(-1, 0), exit = c(0, 1)), 
                                              re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      mod2.preds[exit == 0, exit_category := "Negative"][exit == 1, exit_category := "Positive"]
      mod2.preds <- mod2.preds[, .(time, exit, exit_category, wage = predicted, se = std.error, lower = `conf.low`, upper = `conf.high`)]
      mod2.preds[]
      
    # Plot data prior to exit ----
      dt1.plot = copy(dt1)
      dt1.plot[exit == 0, time := time - .015] # add tiny shift for easier visualization
      dt1.plot[exit == 1, time := time + .015] # add tiny shift for easier visualization
      
      plot2 <- ggplot() +
        # geom_point(data = dt1.plot, aes(x = time, y = wage, color = exit_category)) + 
        geom_line(data = mod2.preds, aes(x = time, y = wage, color = exit_category), size = 1) +
        labs(title = paste0("Population level trends 1 year prior to exit"), 
             subtitle = paste0("Model 2: matching on initial wages"), 
             caption = paste0("", mod2.formula), 
             x = "", 
             y = "Quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit"), breaks=c(-1, 0)) 
      
      plot2 <- formatplots(plot2)
      
      plot(plot2)
      
    # Save plot ----
      saveplots(plot.object = plot2, plot.name = 'figure_2_prior_trends')
      
# Compare fit of model 1 and model 2 using ANOVA likelihood ratio test ----
      comp_mod1_mod2 = anova(mod1, mod2, test = 'LRT')
      if(comp_mod1_mod2$`Pr(>Chisq)`[2] < 0.05 & 
         comp_mod1_mod2$AIC[2] < comp_mod1_mod2$AIC[1]) {
        final.prior.mod <- copy(mod2)
        final.prior.plot <- copy(plot2)
      } else {
        final.prior.mod <- copy(mod1)
        final.prior.plot <- copy(plot1)
        }
      
# Create dt3 for DiD analysis from exit (quarter 0) to 1 year post exit (quarter 4) ----
      # because assessment of parallel trends failed know this is is not 'correct'
      dt3 <- copy(raw)
      dt3[qtr == 4, time := 1]
      dt3[qtr == 0, time := 0]
      dt3[exit_category == "Negative", exit := 0]
      dt3[exit_category == "Positive", exit := 1]
      dt3 <- dt3[!is.na(time)]      

# Model 3: DiD base model ----
    # model ----
      mod3.formula <- paste0("wage ~ ",
                             "exit*time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod3 <- lme4::lmer(mod3.formula, data = dt3)
      mod3.tidy <- as.data.table(broom.mixed::tidy(mod3, conf.int = T))
      
    # test if p-value for interaction term is < 0.05 (could also perform likelihood ratio test using anova function) ----
      mod3.test <- suppressWarnings(lmerTest::lmer(mod3.formula, data = dt3, REML = FALSE))
      mod3.test <- as.data.table(coef(summary(mod3.test)), keep.rownames = T)    
      print(mod3.test)
      if( mod3.test[rn == "exit:time"][["Pr(>|t|)"]] < 0.05) {
        message("\nThe exit:time interaction term is significant, so there is a significant difference in differences")
        caption.text <- paste0("The DiD for positive vs. negative exits is: ",
                               scales::dollar(rads::round2(mod3.tidy[term=='exit:time']$estimate, 0)) )
      } else {
        message("\nThe exit:time interaction term is NOT significant, so there is no significant difference in increase in wages post exit.")
        caption.text <- "There is no significant DiD."
        # mod3.formula <- paste0("wage ~ ",
        #                            "exit + time + ", 
        #                            confounders, " + ",
        #                            "(1 | id_kc_pha) + ", # random intercept for persons
        #                            "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
        # mod3 <- lme4::lmer(mod3.alt.formula, data = dt3)  
        # mod3.tidy <- as.data.table(broom.mixed::tidy(mod3, conf.int = T))
        }

    # predictions ----
      # standard predictions ----
      mod3.preds <- as.data.table(predictions(mod3, 
                                              newdata = datagrid(time=c(0, 1), exit = c(0, 1)), 
                                              re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      mod3.preds[exit == 0, exit_category := "Negative"][exit == 1, exit_category := "Positive"]
      mod3.preds <- mod3.preds[, .(time, exit, exit_category, wage = predicted, se = std.error, lower = `conf.low`, upper = `conf.high`)]
      
      # calculate counterfactual for positive exits using 10,000 draws----
      set.seed(98104)
      counterfactual <- data.table(neg.zero = rnorm(10000, mean = mod3.preds[exit == 0 & time == 0]$wage, sd = mod3.preds[exit == 0 & time == 0]$se), 
                                   neg.one = rnorm(10000, mean = mod3.preds[exit == 0 & time == 1]$wage, sd = mod3.preds[exit == 0 & time == 1]$se), 
                                   pos.zero = rnorm(10000, mean = mod3.preds[exit == 1 & time == 0]$wage, sd = mod3.preds[exit == 1 & time == 0]$se))
      counterfactual[, diff := neg.one - neg.zero]
      counterfactual[, counterfactual := pos.zero + diff]
      counterfactual[, mean := mean(counterfactual)]
      counterfactual[, sd := sd(counterfactual)]
      counterfactual <- unique(counterfactual[, .(time = 1, exit = 1, exit_category = "Counterfactual", 
                                                  wage = mean, se = sd, 
                                                  lower = mean - (qnorm(0.975) * sd), 
                                                  upper = mean + (qnorm(0.975) * sd))])

      # add rows for counterfactual to mod3.preds ----
      mod3.preds <- rbind(
        mod3.preds,
        copy(mod3.preds)[exit == 1 & time == 0][, exit_category := "Counterfactual"], 
        counterfactual)

      mod3.preds[]
      
    # Plot data prior to exit ----
      dt3.plot = copy(dt3)
      dt3.plot[exit == 0, time := time - .015] # add tiny shift for easier visualization
      dt3.plot[exit == 1, time := time + .015] # add tiny shift for easier visualization
      
      plot3 <- ggplot() +
        # geom_point(data = dt3.plot, aes(x = time, y = wage, color = exit_category)) + 
        geom_line(data = mod3.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
        geom_line(data = mod3.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
        labs(title = paste0("Population level trends 1 year post exit"), 
             subtitle = paste0("Model 3: ", caption.text), 
             caption = mod3.formula, 
             x = "", 
             y = "Quarterly wages") +
        scale_x_continuous(labels=c("Exit", "1 year post"), breaks=c(0, 1)) 
      
      plot3 <- formatplots(plot3) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f'))
      
      plot(plot3)
      
    # Save plot ----
      saveplots(plot.object = plot3, plot.name = 'figure_3_post_trends')


# Model 4: Model for 1 prior to exit to one year post exit ----
    # Create dt4 for complete analysis: 1 year prior, at exit, and 1 year post ----
      dt4 <- copy(raw)
      dt4[exit_category == "Negative", exit := 0]
      dt4[exit_category == "Positive", exit := 1]
      dt4[qtr == -4, time := -1]
      dt4[qtr == 0, time := 0]
      dt4[qtr == 4, time := 1]
      dt4 <- dt4[!is.na(time)]      
      dt4[, time := factor(time)]

    # model ----
      mod4.formula <- paste0("wage ~ ",
                             "exit*time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod4 <- lme4::lmer(mod4.formula, data = dt4)
      mod4.tidy <- as.data.table(broom.mixed::tidy(mod4, conf.int = T))
      
    # test if p-value for interaction term is < 0.05 ----
      mod4.formula.alt <- paste0("wage ~ ",
                             "exit + time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod4.alt <- lme4::lmer(mod4.formula.alt, data = dt4)
      
      mod4.test = anova(mod4, mod4.alt, test = 'LRT')
      
      if( mod4.test[["Pr(>Chisq)"]][2] < 0.05 ) {
        print("\nThe exit:time interaction term is significant, so keep the full model with the interaction term")
        caption.text <- paste0("", mod4.formula)
      } else {
        print("\nThe exit:time interaction term is NOT significant, so use more parsimonuous model.")
        mod4.formula <- paste0("wage ~ ",
                               "exit + time + ", 
                               confounders, " + ",
                               "(1 | id_kc_pha) + ", # random intercept for persons
                               "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
        mod4 <- lme4::lmer(mod4.formula, data = dt4)
        mod4.tidy <- as.data.table(broom.mixed::tidy(mod4, conf.int = T))
        caption.text <- paste0("", mod4.formula)
      }

    # predictions ----
      # standard predictions ----
      mod4.preds <- as.data.table(predictions(mod4, 
                                              newdata = datagrid(time=c(-1, 0, 1), exit = c(0, 1)), 
                                              re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      mod4.preds[exit == 0, exit_category := "Negative"][exit == 1, exit_category := "Positive"]
      mod4.preds <- mod4.preds[, .(time, exit, exit_category, wage = predicted, se = std.error, lower = `conf.low`, upper = `conf.high`)]
      
      mod4.counterfactual <- mod4.preds[exit_category == "Positive"]
      
      # calculate counterfactual (if positive exits had same slope as negative exits) exits using 10,000 draws----
      set.seed(98104)
      counterfactual <- data.table(neg.minus1 = rnorm(100000, mean = mod4.preds[exit == 0 & time == -1]$wage, sd = mod4.preds[exit == 0 & time == -1]$se), 
                                   neg.zero = rnorm(100000, mean = mod4.preds[exit == 0 & time == 0]$wage, sd = mod4.preds[exit == 0 & time == 0]$se), 
                                   neg.one = rnorm(100000, mean = mod4.preds[exit == 0 & time == 1]$wage, sd = mod4.preds[exit == 0 & time == 1]$se), 
                                   pos.minus1 = rnorm(100000, mean = mod4.preds[exit == 1 & time == -1]$wage, sd = mod4.preds[exit == 1 & time == -1]$se), 
                                   pos.zero = rnorm(100000, mean = mod4.preds[exit == 1 & time == 0]$wage, sd = mod4.preds[exit == 1 & time == 0]$se),
                                   pos.zero = rnorm(100000, mean = mod4.preds[exit == 1 & time == 1]$wage, sd = mod4.preds[exit == 1 & time == 1]$se))
      counterfactual[, diff.zero := neg.zero - neg.minus1] # increase among negative exits from time == -1 to time == 0
      counterfactual[, diff.minus1.one := neg.one - neg.minus1] # entire increase among negative exits from time == -1 to time == 1
      counterfactual[, counterfactual.zero := pos.minus1 + diff.zero] # add the differences from neg exits time -1 to time 0 to the positive exit value time -1
      counterfactual[, counterfactual.one := pos.minus1+ diff.minus1.one] # add the differences from neg exits  time 0 to time 1 to the positive exit values at time 0
      counterfactual[, mean.zero := mean(counterfactual.zero)]
      counterfactual[, sd.zero := sd(counterfactual.zero)]
      counterfactual[, mean.one := mean(counterfactual.one)]
      counterfactual[, sd.one := sd(counterfactual.one)]
      counterfactual <- rbind(
        unique(counterfactual[, .(time = 0, exit = 1, exit_category = "Counterfactual", 
                                  wage = mean.zero, se = sd.zero, 
                                  lower = mean.zero - (qnorm(0.975) * sd.zero), 
                                  upper = mean.zero + (qnorm(0.975) * sd.zero))]), 
        unique(counterfactual[, .(time = 1, exit = 1, exit_category = "Counterfactual", 
                                  wage = mean.one, se = sd.one, 
                                  lower = mean.one - (qnorm(0.975) * sd.one), 
                                  upper = mean.one + (qnorm(0.975) * sd.one))])
      )

      # add rows for counterfactual to mod4.preds ----
      setorder(mod4.preds, exit)[]
      mod4.preds <- rbind(
        mod4.preds,
        copy(mod4.preds)[exit == 1 & time == -1][, exit_category := "Counterfactual"], 
        counterfactual)
      setorder(mod4.preds, exit, exit_category, time)
      
      mod4.preds[, time := as.integer(as.character(time))] # convert time back to numeric for graphing

      mod4.preds[]
      
    # Plot data prior to exit ----
      dt4.plot = copy(dt4)
      dt4.plot[exit == 0, time := time - .015] # add tiny shift for easier visualization
      dt4.plot[exit == 1, time := time + .015] # add tiny shift for easier visualization
      
      plot4 <- ggplot() +
        # geom_point(data = dt4.plot, aes(x = time, y = wage, color = exit_category)) + 
        geom_line(data = mod4.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
        geom_line(data = mod4.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
        geom_point(data = mod4.preds, 
                   aes(x = time, y = wage, color = exit_category), 
                   size = 2.5) +
        geom_errorbar(data = mod4.preds, 
                      aes(x = time, ymax = upper, ymin = lower, color = exit_category), 
                      size = 1, 
                      width = .05) + 
        labs(title = paste0("Quarterly wage history by exit type and time"), 
             subtitle = "Model 4: three time points as a factor", 
             caption = caption.text, 
             x = "", 
             y = "Quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-1, 0, 1))
      
      plot4 <- formatplots(plot4) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) 
      
      # dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(plot4)
      
    # Save plot ----
      saveplots(plot.object = plot4, plot.name = 'figure_4_pre_post_trends')      
      
# Model 5: Model for all available quarterly wage data ----
    # Create dt5 for complete quarterly analysis ----
      dt5 <- copy(raw)
      dt5[exit_category == "Negative", exit := 0]
      dt5[exit_category == "Positive", exit := 1]
      dt5[, time := factor(qtr)]
      
    # model ----
      mod5.formula <- paste0("wage ~ ",
                             "exit*time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod5 <- lme4::lmer(mod5.formula, data = dt5)
      mod5.tidy <- as.data.table(broom.mixed::tidy(mod5, conf.int = T))
      
    # test if p-value for interaction term is < 0.05 ----
      mod5.formula.alt <- paste0("wage ~ ",
                                 "exit + time + ", 
                                 confounders, " + ",
                                 "(1 | id_kc_pha) + ", # random intercept for persons
                                 "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod5.alt <- lme4::lmer(mod5.formula.alt, data = dt5)
      
      mod5.test = anova(mod5, mod5.alt, test = 'LRT')
      
      if( mod5.test[["Pr(>Chisq)"]][2] < 0.05 ) {
        print("\nThe exit:time interaction term is significant, so keep the full model with the interaction term")
        caption.text <- paste0("", mod5.formula)
      } else {
        print("\nThe exit:time interaction term is NOT significant, so use more parsimonuous model.")
        mod5.formula <- paste0("wage ~ ",
                               "exit + time + ", 
                               confounders, " + ",
                               "(1 | id_kc_pha) + ", # random intercept for persons
                               "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
        mod5 <- lme4::lmer(mod5.formula, data = dt5)
        mod5.tidy <- as.data.table(broom.mixed::tidy(mod5, conf.int = T))
        caption.text <- paste0("", mod5.formula)
      }
      
    # predictions ----
      # standard predictions ----
      mod5.preds <- as.data.table(predictions(mod5, 
                                              newdata = datagrid(time=c(-4:4), # set to -4, 0, 4 because the time scale is in quarters, so -/+ 1 year
                                                                 exit = c(0, 1)), 
                                              re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      mod5.preds[exit == 0, exit_category := "Negative"][exit == 1, exit_category := "Positive"]
      mod5.preds <- mod5.preds[, .(time, exit, exit_category, wage = predicted, se = std.error, lower = `conf.low`, upper = `conf.high`)]
      

      # calculate counterfactual (ascribe change observed in negative exits to positive exits) ----
      mod5.negdiff <-  mod5.preds[exit_category == "Negative"]
      mod5.negdiff[, wage.prev := shift(x = wage, n = 1L, type = 'lag')]
      mod5.negdiff[, se.prev := shift(x = se, n = 1L, type = 'lag')]
      mod5.negdiff[, wage.diff := wage - wage.prev]
      mod5.negdiff[, se.diff := sqrt((se^2) + (se.prev^2))]
      mod5.negdiff <- mod5.negdiff[, .(time, wage.diff, se.diff)]
      
      
      mod5.counterfactual <- mod5.preds[exit_category == "Positive"]
      mod5.counterfactual[, c("lower", "upper") := NULL]
      mod5.counterfactual[time != -4, wage := NA]
      mod5.counterfactual <- merge(mod5.counterfactual, mod5.negdiff, by = c("time"), all = T)
      mod5.counterfactual[is.na(wage.diff), wage.diff := wage]
      mod5.counterfactual[, wage := cumsum(wage.diff)]
      mod5.counterfactual[!is.na(se.diff), se := sqrt((se^2) + (se.diff^2))]
      mod5.counterfactual <- mod5.counterfactual[, .(time, exit, exit_category = 'Counterfactual', 
                                                     wage, se, lower = wage - (se * qnorm(0.975)), 
                                                     upper = wage + (se * qnorm(0.975)))]
      
      # add rows for counterfactual to mod5.preds ----
      mod5.preds <- rbind(
        mod5.preds,
        copy(mod5.preds)[exit == 1 & time == -4][, exit_category := "Counterfactual"], 
        mod5.counterfactual[time != -4])
      setorder(mod5.preds, exit, exit_category, time)
      
      mod5.preds[, time := as.integer(as.character(time))] # convert time back to numeric for graphing
      
      mod5.preds[]
      
    # Plot data prior to exit ----
      plot5 <- ggplot() +
        # geom_point(data = dt5.plot, aes(x = time, y = wage, color = exit_category)) + 
        geom_line(data = mod5.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
        geom_line(data = mod5.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
        geom_point(data = mod5.preds[exit_category != "Counterfactual"], 
                   aes(x = time, y = wage, color = exit_category), 
                   size = 2.5) +
        geom_errorbar(data = mod5.preds[exit_category != "Counterfactual"], 
                      aes(x = time, ymax = upper, ymin = lower, color = exit_category), 
                      size = 1, 
                      width = .05) + 
        labs(title = paste0("Quarterly wage history by exit type and time"), 
             subtitle = "Model 5: Four quarters pre/post exit", 
             caption = caption.text, 
             x = "", 
             y = "Quarterly wages") +
        scale_x_continuous(breaks=c(-4:4))
      
      plot5 <- formatplots(plot5) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) 
      
      dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(plot5)
      
      # alternative plot with CI for counterfactual
      plot5.alt <- ggplot() +
        # geom_point(data = dt5.plot, aes(x = time, y = wage, color = exit_category)) + 
        geom_line(data = mod5.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
        geom_line(data = mod5.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
        geom_point(data = mod5.preds[], 
                   aes(x = time, y = wage, color = exit_category), 
                   size = 2.5) +
        geom_errorbar(data = mod5.preds[], 
                      aes(x = time, ymax = upper, ymin = lower, color = exit_category), 
                      size = 1, 
                      width = .05) + 
        labs(title = paste0("Quarterly wage history by exit type and time"), 
             subtitle = "Model 5: Four quarters pre/post exit", 
             caption = caption.text, 
             x = "", 
             y = "Quarterly wages") +
        scale_x_continuous(breaks=c(-4:4))
      
      plot5.alt <- formatplots(plot5.alt) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) 
      
      dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(plot5.alt)
      
    # Save plots ----
      saveplots(plot.object = plot5, plot.name = 'figure_5_pre_post_by_qtr')      
      saveplots(plot.object = plot5.alt, plot.name = 'figure_5_pre_post_by_qtr_alt')      
      
      
      
# The end ----