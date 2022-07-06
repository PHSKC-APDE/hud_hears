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
# Notes on predictions:
#        * Used marginaleffects::predictions because provides estimates + SE & CI
#        * margins::prediction & prediction::prediction do not provide SE & CI
#        * stats::predict (predict.merMod) doesn't provide SE & CI & need to specify 
#          all fixed effects

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
       
    # calculate counterfactuals ----
      calc.counterfactual <- function(mymod.preds = NULL){
      mymod.negdiff <-  copy(mymod.preds)[exit_category == "Negative"]
      mymod.negdiff[, wage.prev := shift(x = wage, n = 1L, type = 'lag')]
      mymod.negdiff[, se.prev := shift(x = se, n = 1L, type = 'lag')]
      mymod.negdiff[, wage.diff := wage - wage.prev]
      mymod.negdiff[, se.diff := sqrt((se^2) + (se.prev^2))]
      mymod.negdiff <- mymod.negdiff[, .(time, wage.diff, se.diff)]
      
      mymod.counterfactual <- copy(mymod.preds)[exit_category == "Positive"]
      mymod.counterfactual[, c("lower", "upper") := NULL]
      min.time <- min(mymod.preds$time)
      mymod.counterfactual[time != min(mymod.preds$time), wage := NA]
      mymod.counterfactual <- merge(mymod.counterfactual, mymod.negdiff, by = c("time"), all = T)
      mymod.counterfactual[is.na(wage.diff), wage.diff := wage]
      mymod.counterfactual[, wage := cumsum(wage.diff)]
      mymod.counterfactual[!is.na(se.diff), se := sqrt((se^2) + (se.diff^2))]
      mymod.counterfactual <- mymod.counterfactual[, .(time = as.integer(as.character(time)), exit, exit_category = 'Counterfactual', 
                                                         wage, se, lower = wage - (se * qnorm(0.975)), 
                                                         upper = wage + (se * qnorm(0.975)))]
      mymod.preds <- rbind(
        mymod.preds,
        mymod.counterfactual)
      setorder(mymod.preds, exit, exit_category, time)
      
      mymod.preds[, time := as.numeric(as.character(time))] # convert time back to numeric for graphing
    }    

    # clean model estimates ----
      model.clean <- function(mymod){
        mymod.tidy <- as.data.table(broom.mixed::tidy(mymod, conf.int = T))
        roundvars <- c("estimate", "conf.low", "conf.high", "std.error")
        mymod.tidy[, (roundvars) := rads::round2(.SD, 0), .SDcols = roundvars]
        mymod.tidy <- mymod.tidy[, .(Effect = effect, 
                                     Group = group, 
                                     Term = term, 
                                     Estimate = paste0(
                                       prettyNum(estimate, big.mark = ','),
                                       " (", 
                                       prettyNum(conf.low, big.mark = ','), 
                                       ", ", 
                                       prettyNum(conf.high, big.mark = ','), 
                                       ")") 
                                     # , SE = std.error
                                     )]
        mymod.tidy[, Estimate := gsub(" \\(NA\\, NA\\)", "", Estimate)]
        setnames(mymod.tidy, "Estimate", "Estimate (95% CI)")
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
    
    # get pscovariates
    load(paste0(outputdir, "pscovariates.Rdata"))
    pscovariates <- pscovariates$pscovariates
    if(length(pscovariates) > 1){ 
      pscovariates <- paste(pscovariates, collapse = " + ")
    }
    
# Preparatory data manipulation ----
    raw[, exit_year := as.factor(exit_year)]
    raw[, exit := as.integer(exit)]

# Model 1: assess parallel trends prior to exit ----
    # Create dt1 for assessing parallel trend assumption from quarter -4 to quarter 0 (exit) ----
        dt1 <- copy(raw)
        
        dt1[qtr == -4, time := -1]
        dt1[qtr == 0, time := 0]
        dt1[exit_category == "Negative", exit := 0]
        dt1[exit_category == "Positive", exit := 1]
        
        dt1 <- dt1[!is.na(time)]
        
    # model ----
      dt1[, .N, .(exit, time)]
      mod1.formula <- paste0("wage ~ ", 
                             "exit*time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod1 <- lme4::lmer(mod1.formula, data = dt1)
      mod1.tidy <- model.clean(mod1)

    # test if p-value for interaction term is < 0.05 ----
      mod1.test <- suppressWarnings(lmerTest::lmer(mod1.formula, data = dt1, REML = FALSE))
      mod1.test <- as.data.table(coef(summary(mod1.test)), keep.rownames = T)    
      if( mod1.test[rn == "exit:time"][["Pr(>|t|)"]] < 0.05) {
        print(mod1.test[rn == "exit:time"])
        message("The exit:time interaction term is significant, so fails test of parallel trend prior to exit")
      }

    # average marginal effects (just for curiosity) ----
      mod1.margin.summary <- as.data.table(summary(margins::margins(mod1))) 
      mod1.margin.summary[factor == 'exit']
    
    # create table of predictions for slopes in ggplot  ----
      # Try three methods to get predicted values ---
        # Using marginaleffects::predictions ----
          # good estimates + se & CI
            mod1.preds <- as.data.table(marginaleffects::predictions(mod1, 
                                                     newdata = datagrid(time=c(-1, 0), exit = c(0, 1)), 
                                                     re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
            mod1.preds[exit == 0, exit_category := "Negative"][exit == 1, exit_category := "Positive"]
            mod1.preds <- mod1.preds[, .(time = as.integer(as.character(time)), exit, exit_category, wage = predicted, se = std.error, lower = `conf.low`, upper = `conf.high`)]
            mod1.preds[]

        # calculate counterfactual ----
          mod1.preds = calc.counterfactual(mod1.preds)

    # Plot data prior to exit ----
      dt1.plot = copy(dt1)
      dt1.plot[exit == 0, time := time - .015] # add tiny shift for easier visualization
      dt1.plot[exit == 1, time := time + .015] # add tiny shift for easier visualization
      
      plot1 <- ggplot() +
        # geom_point(data = dt1.plot, aes(x = time, y = wage, color = exit_category)) + 
        geom_line(data = mod1.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
        geom_line(data = mod1.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
        labs(
          #title = paste0("Population level trends 1 year prior to exit"), 
           #subtitle = "Model 1: Assess parallel trends prior to exit",
           # caption = paste0("", mod1.formula), 
           x = "", 
           y = "Quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit"), breaks=c(-1, 0)) 
        
      plot1 <- formatplots(plot1) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) +
        scale_y_continuous(limits = c(3000, 9000), breaks=c(seq(4000, 8000, 2000)), labels=scales::dollar_format()) 

      plot(plot1)
      
    # Save plot ----
      saveplots(plot.object = plot1, plot.name = 'figure_1_prior_trends')

# Model 1.5: assess parallel trends prior to exit ----
    # Create dt1.5.5 for assessing parallel trend assumption from quarter -4:0 ----
      dt1.5 <- copy(raw)
      dt1.5[qtr %in% -4:0, time := factor(qtr)]
      dt1.5[exit_category == "Negative", exit := 0]
      dt1.5[exit_category == "Positive", exit := 1]
      dt1.5 <- dt1.5[!is.na(time)]
      
    # model ----
      dt1.5[, .N, .(exit, time)]
      mod1.5.formula <- paste0("wage ~ ", 
                             "exit*time + ", 
                             # "exit*splines::bs(as.integer(time), df = 3) + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod1.5 <- lme4::lmer(mod1.5.formula, data = dt1.5)
      mod1.5.tidy <- model.clean(mod1.5)
      
    # test if interaction is significant ----
      mod1.5.formula.alt <- paste0("wage ~ ", 
                               "exit + time + ", 
                               # "exit + splines::bs(as.integer(time), df = 3) + ", 
                               confounders, " + ",
                               "(1 | id_kc_pha) + ", # random intercept for persons
                               "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod1.5.alt <- lme4::lmer(mod1.5.formula.alt, data = dt1.5)
      mod1.5.test = anova(mod1.5, mod1.5.alt, test = 'LRT')
      
      if( mod1.5.test[["Pr(>Chisq)"]][2] < 0.05 ) {
        print("The exit:time interaction term is significant, so keep the full model with the interaction term")
        caption.text <- paste0("", mod1.5.formula)
      } else {
        print("The exit:time interaction term is NOT significant, so use more parsimonuous model.")
        mod1.5.formula <- copy(mod1.5.formula.alt)
        mod1.5 <- copy(mod1.5.alt)
        mod1.5.tidy <- model.clean(mod1.5)
        caption.text <- paste0("", mod1.5.formula)
      } 
      
      
    # predictions ----  
      # Using marginaleffects::predictions ----
        mod1.5.preds <- as.data.table(marginaleffects::predictions(mod1.5, 
                                                                 newdata = datagrid(time=c(-4:0), exit = c(0, 1)), 
                                                                 re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
        mod1.5.preds[exit == 0, exit_category := "Negative"][exit == 1, exit_category := "Positive"]
        mod1.5.preds <- mod1.5.preds[, .(time = as.integer(as.character(time)), exit, exit_category, wage = predicted, se = std.error, lower = `conf.low`, upper = `conf.high`)]
        
      # Calculate counterfactual ----
        mod1.5.preds <- calc.counterfactual(mod1.5.preds)
        mod1.5.preds[exit_category == "Positive", time := time - .0]
        mod1.5.preds[exit_category == "Negative", time := time + .0]
        mod1.5.preds[]

    # plot ----
        plot1.5 <- ggplot() +
          geom_line(data = mod1.5.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
          geom_line(data = mod1.5.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
          geom_point(data = mod1.5.preds[exit_category != "Counterfactual"], 
                     aes(x = time, y = wage, color = exit_category), 
                     size = 2.5) +
          geom_errorbar(data = mod1.5.preds[exit_category != "Counterfactual"], 
                        aes(x = time, ymax = upper, ymin = lower, color = exit_category), 
                        size = 1, 
                        width = .05) + 
          labs(
            #title = paste0("Population level trends 1 year prior to exit"), 
            #subtitle = "Model 1: Assess parallel trends prior to exit",
            # caption = paste0("", mod1.formula), 
            x = "", 
            y = "Quarterly wages") +
          scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
        
        plot1.5 <- formatplots(plot1.5) + 
          scale_color_manual("Exit type", 
                             values=c('Positive' = '#2c7fb8', 
                                      'Counterfactual' = '#e41a1c', 
                                      'Negative' = '#2ca25f')) 
        
        # dev.new(width = 7,  height = 4, unit = "in", noRStudioGD = TRUE)
        plot(plot1.5)
        
    # Save plot ----
      saveplots(plot.object = plot1.5, plot.name = 'appendix_figure_1_assess_parallel_trends')
      
      
# Model 2: IPW to see if improves parallel trends of pre-exit curves ----
    # also hope it will reduce the problem with non-parallel slopes prior to exit
    # create dt2 ----
      dt2 <- copy(dt1)
      
    # model ----
      # propensity score ----
      mod2.psformula <- paste0("exit ~ ", pscovariates) # no random effects bc only at time zero
      mod2.ps <- glm(mod2.psformula, 
                     family = binomial(link=logit),
                     data = dt2[qtr==0])
      mod2.ps.prob <- data.table(hh_id_kc_pha = dt2[qtr==0]$hh_id_kc_pha, 
                                 id_kc_pha = dt2[qtr==0]$id_kc_pha, 
                                 exit = dt2[qtr==0]$exit, 
                                 prob = predict(mod2.ps, type = 'response')) # predicted probability
      mod2.ps.prob[exit == 1, ipw := 1 / prob] # weight for IPW
      mod2.ps.prob[exit == 0, ipw := 1 / (1-prob)] # weight for IPW
      dt2 <- merge(dt2, mod2.ps.prob)
      
      # main model ----
      dt2[, .N, .(exit, time)]
      mod2.formula <- paste0("wage ~ ", 
                             "exit*time + ", 
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)")  # random intercept and slope for households
      mod2 <- lme4::lmer(mod2.formula, data = dt2, weights = ipw)
      mod2.tidy <- model.clean(mod2)
      
    # test if p-value for interaction term is < 0.05 using likelihood ratio test ----
      mod2.formula.alt <- paste0("wage ~ ", 
                                 "exit + time + ", 
                                 "(1 | id_kc_pha) + ", # random intercept for persons
                                 "(1 + exit | hh_id_kc_pha)")  # random intercept and slope for households
      mod2.alt <- lme4::lmer(mod2.formula.alt, data = dt2, weights = ipw)
      mod2.test = anova(mod2, mod2.alt, test = 'LRT')

      if( mod2.test[["Pr(>Chisq)"]][2] < 0.05 ) {
        print("The exit:time interaction term is significant, so keep the full model with the interaction term")
        caption.text <- paste0("", mod2.formula)
      } else {
        print("The exit:time interaction term is NOT significant, so use more parsimonuous model.")
        mod2.formula <- copy(mod2.formula.alt)
        mod2 <- copy(mod2.alt)
        mod2.tidy <- model.clean(mod2)
        caption.text <- paste0("", mod2.formula)
      }      
      
    # average marginal effects  ----
      mod2.margin.summary <- as.data.table(summary(margins::margins(mod2))) 
      mod2.margin.summary[factor == 'exit']
      
    # create table of predictions for slopes in ggplot ----
      mod2.preds <- as.data.table(marginaleffects::predictions(mod2, 
                                              newdata = datagrid(time=c(-1, 0), exit = c(0, 1)), 
                                              re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      mod2.preds[exit == 0, exit_category := "Negative"][exit == 1, exit_category := "Positive"]
      mod2.preds <- mod2.preds[, .(time = as.integer(as.character(time)), exit, exit_category, wage = predicted, se = std.error, lower = `conf.low`, upper = `conf.high`)]
      mod2.preds[]
      
      # calculate counterfactual for positive exits ----
      counterfactual <- calc.counterfactual(mod2.preds)

      # add rows for counterfactual to mod2.preds ----
      mod2.preds <- rbind(
        mod2.preds,
        counterfactual)
      
      mod2.preds[]

    # Plot data prior to exit ----
      dt2.plot = copy(dt2)
      dt2.plot[exit == 0, time := time - .015] # add tiny shift for easier visualization
      dt2.plot[exit == 1, time := time + .015] # add tiny shift for easier visualization
      
      plot2 <- ggplot() +
        # geom_point(data = dt2.plot, aes(x = time, y = wage, color = exit_category)) + 
        geom_line(data = mod2.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
        geom_line(data = mod2.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
        labs(title = paste0("Population level trends 1 year prior to exit"), 
             subtitle = paste0("Model 2: IPW for trends prior to exit"), 
             caption = paste0("", mod2.formula), 
             x = "", 
             y = "Quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit"), breaks=c(-1, 0)) 
      
      plot2 <- formatplots(plot2) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) +
        scale_y_continuous(limits = c(3000, 9000), breaks=c(seq(4000, 8000, 2000)), labels=scales::dollar_format()) 

      plot(plot2)
      
    # Save plot ----
      saveplots(plot.object = plot2, plot.name = 'figure_2_prior_trends')
      
# Model 2.5: IPW to see if improves parallel trends of pre-exit curves ----
    # also hope it will reduce the problem with non-parallel slopes prior to exit
    # create dt2.5 ----
      dt2.5 <- copy(raw)
      dt2.5[qtr %in% -4:0, time := factor(qtr)]
      dt2.5[exit_category == "Negative", exit := 0]
      dt2.5[exit_category == "Positive", exit := 1]
      dt2.5 <- dt2.5[!is.na(time)]
      
    # model ----
      # propensity score ----
      mod2.5.psformula <- paste0("exit ~ ", pscovariates) # no random effects bc only at time zero
      mod2.5.ps <- glm(mod2.5.psformula, 
                     family = binomial(link=logit),
                     data = dt2.5[qtr==0])
      mod2.5.ps.prob <- data.table(hh_id_kc_pha = dt2.5[qtr==0]$hh_id_kc_pha, 
                                 id_kc_pha = dt2.5[qtr==0]$id_kc_pha, 
                                 exit = dt2.5[qtr==0]$exit, 
                                 prob = predict(mod2.5.ps, type = 'response')) # predicted probability
      mod2.5.ps.prob[exit == 1, ipw := 1 / prob] # weight for IPW
      mod2.5.ps.prob[exit == 0, ipw := 1 / (1-prob)] # weight for IPW
      dt2.5 <- merge(dt2.5, mod2.5.ps.prob)
      
      # main model ----
      dt2.5[, .N, .(exit, time)]
      mod2.5.formula <- paste0("wage ~ ", 
                             "exit*time + ", 
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)")  # random intercept and slope for households
      mod2.5 <- lme4::lmer(mod2.5.formula, data = dt2.5, weights = ipw)
      mod2.5.tidy <- model.clean(mod2.5)
      
    # test if p-value for interaction term is < 0.05 using likelihood ratio test ----
      mod2.5.formula.alt <- paste0("wage ~ ", 
                                 "exit + time + ", 
                                 "(1 | id_kc_pha) + ", # random intercept for persons
                                 "(1 + exit | hh_id_kc_pha)")  # random intercept and slope for households
      mod2.5.alt <- lme4::lmer(mod2.5.formula.alt, data = dt2.5, weights = ipw)
      mod2.5.test = anova(mod2.5, mod2.5.alt, test = 'LRT')

      if( mod2.5.test[["Pr(>Chisq)"]][2] < 0.05 ) {
        print("The exit:time interaction term is significant, so keep the full model with the interaction term")
        caption.text <- paste0("", mod2.5.formula)
      } else {
        print("The exit:time interaction term is NOT significant, so use more parsimonuous model.")
        mod2.5.formula <- copy(mod2.5.formula.alt)
        mod2.5 <- copy(mod2.5.alt)
        mod2.5.tidy <- model.clean(mod2.5)
        caption.text <- paste0("", mod2.5.formula)
      }      
      
    # average marginal effects  ----
      mod2.5.margin.summary <- as.data.table(summary(margins::margins(mod2.5))) 
      mod2.5.margin.summary[factor == 'exit']
      
    # create table of predictions for slopes in ggplot ----
      mod2.5.preds <- as.data.table(marginaleffects::predictions(mod2.5, 
                                              newdata = datagrid(time=c(-4:0), exit = c(0, 1)), 
                                              re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      mod2.5.preds[exit == 0, exit_category := "Negative"][exit == 1, exit_category := "Positive"]
      mod2.5.preds <- mod2.5.preds[, .(time = as.integer(as.character(time)), exit, exit_category, wage = predicted, se = std.error, lower = `conf.low`, upper = `conf.high`)]

    # calculate counterfactual for positive exits ----
      mod2.5.preds <- calc.counterfactual(mod2.5.preds)
      mod2.5.preds[]

    # Plot data prior to exit ----
      dt2.5.plot = copy(dt2.5)
      dt2.5.plot[exit == 0, time := time - .0] # add tiny shift for easier visualization
      dt2.5.plot[exit == 1, time := time + .0] # add tiny shift for easier visualization
      
      plot2.5 <- ggplot() +
        # geom_point(data = dt2.5.plot, aes(x = time, y = wage, color = exit_category)) + 
        geom_line(data = mod2.5.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
        geom_line(data = mod2.5.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
        geom_point(data = mod1.5.preds[exit_category != "Counterfactual"], 
                   aes(x = time, y = wage, color = exit_category), 
                   size = 2.5) +
        geom_errorbar(data = mod1.5.preds[exit_category != "Counterfactual"], 
                      aes(x = time, ymax = upper, ymin = lower, color = exit_category), 
                      size = 1, 
                      width = .05) + 
        labs(
           # title = paste0("Population level trends 1 year prior to exit"), 
           # subtitle = paste0("Model 2: IPW for trends prior to exit"), 
           # caption = paste0("", mod2.5.formula), 
           x = "", 
           y = "Quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit"), breaks=c(-4, 0)) 
      
      plot2.5 <- formatplots(plot2.5) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) +
        scale_y_continuous(limits = c(3000, 9000), breaks=c(seq(4000, 8000, 2000)), labels=scales::dollar_format()) 

      plot(plot2.5)
      
    # Save plot ----
      saveplots(plot.object = plot2.5, plot.name = 'appendix_figure_2_IPTW_assess_parallel_trends')

      
# Model 3: DiD base model ----
    # create dt3 for DiD analysis from exit (quarter 0) to 1 year post exit (quarter 4) ----
      # because assessment of parallel trends failed know this is is not 'correct'
      dt3 <- copy(raw)
      dt3[qtr == 4, time := 1]
      dt3[qtr == 0, time := 0]
      dt3[exit_category == "Negative", exit := 0]
      dt3[exit_category == "Positive", exit := 1]
      dt3 <- dt3[!is.na(time)]      
  
    # model ----
      dt3[, .N, .(time, exit)]
      mod3.formula <- paste0("wage ~ ",
                             "exit*time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod3 <- lme4::lmer(mod3.formula, data = dt3)
      mod3.tidy <- model.clean(mod3)
      
    # test if p-value for interaction term is < 0.05 (could also perform likelihood ratio test using anova function) ----
      mod3.test <- suppressWarnings(lmerTest::lmer(mod3.formula, data = dt3, REML = FALSE))
      mod3.test <- as.data.table(coef(summary(mod3.test)), keep.rownames = T)    
      print(mod3.test)
      if( mod3.test[rn == "exit:time"][["Pr(>|t|)"]] < 0.05) {
        message("The exit:time interaction term is significant, so there is a significant difference in differences")
        caption.text <- paste0("The DiD for positive vs. negative exits is: ",
                               scales::dollar(rads::round2(mod3.tidy[term=='exit:time']$estimate, 0)) )
      } else {
        message("The exit:time interaction term is NOT significant, so there is no significant difference in increase in wages post exit.")
        caption.text <- "There is no significant DiD."
        # mod3.formula <- paste0("wage ~ ",
        #                            "exit + time + ", 
        #                            confounders, " + ",
        #                            "(1 | id_kc_pha) + ", # random intercept for persons
        #                            "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
        # mod3 <- lme4::lmer(mod3.alt.formula, data = dt3)  
        # mod3.tidy <- model.clean(mod3)
        }

    # predictions ----
      # standard predictions ----
      mod3.preds <- as.data.table(predictions(mod3, 
                                              newdata = datagrid(time=c(0, 1), exit = c(0, 1)), 
                                              re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      mod3.preds[exit == 0, exit_category := "Negative"][exit == 1, exit_category := "Positive"]
      mod3.preds <- mod3.preds[, .(time = as.integer(as.character(time)), exit, exit_category, wage = predicted, se = std.error, lower = `conf.low`, upper = `conf.high`)]
      
      # calculate counterfactual for positive exits----
      mod3.preds <- calc.counterfactual(mod3.preds)
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
                                    'Negative' = '#2ca25f'))+
        scale_y_continuous(limits = c(3000, 9000), breaks=c(seq(4000, 8000, 2000)), labels=scales::dollar_format()) 
      
      plot(plot3)
      
    # Save plot ----
      saveplots(plot.object = plot3, plot.name = 'figure_3_post_trends')


# Model 4: Quarterly wage model for 1 prior to exit to one year post exit ----
    # Create dt4 for complete analysis: 1 year prior, at exit, and 1 year post ----
      dt4 <- copy(raw)
      dt4[qtr == -4, time := -1]
      dt4[qtr == 0, time := 0]
      dt4[qtr == 4, time := 1]
      dt4 <- dt4[!is.na(time)]      
      dt4[, time := factor(time)]
      dt4[exit_category == "Negative", exit := 0]
      dt4[exit_category == "Positive", exit := 1]
      # dt4[time == -1, exit := 0] # prior to exit, everyone is exit == 0

    # model ----
      dt4[, .N, .(exit, time)]
      mod4.formula <- paste0("wage ~ ",
                             "exit*time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod4 <- lme4::lmer(mod4.formula, data = dt4)
      mod4.tidy <- model.clean(mod4)
      
    # test if p-value for interaction term is < 0.05 with LRT ----
      mod4.formula.alt <- paste0("wage ~ ",
                             "exit + time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod4.alt <- lme4::lmer(mod4.formula.alt, data = dt4)
      
      mod4.test = anova(mod4, mod4.alt, test = 'LRT')
      
      if( mod4.test[["Pr(>Chisq)"]][2] < 0.05 ) {
        print("The exit:time interaction term is significant")
      } else {
        print("The exit:time interaction term is NOT significant, but keep in model because it is the DiD.")
      }
        caption.text <- paste0("", mod4.formula)

    # predictions ----
      # standard predictions ----
      mod4.preds <- as.data.table(predictions(mod4, 
                                              newdata = datagrid(time=c(-1, 0, 1), exit = c(0, 1)), 
                                              re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      mod4.preds[exit == 0, exit_category := "Negative"][exit == 1, exit_category := "Positive"]
      mod4.preds <- mod4.preds[, .(time = as.integer(as.character(time)), exit, exit_category, wage = predicted, se = std.error, lower = `conf.low`, upper = `conf.high`)]
      
      mod4.counterfactual <- mod4.preds[exit_category == "Positive"]
      
      # calculate counterfactual (ascribe change observed in negative exits to positive exits) ----
      mod4.preds <- calc.counterfactual(mod4.preds)
      mod4.preds[exit_category == "Positive", time := time - 0]
      mod4.preds[exit_category == "Negative", time := time + 0]
      mod4.preds[]
      
    # Plot data before and after exit ----
      dt4.plot = copy(dt4)
      dt4.plot[exit == 0, time := time - .015] # add tiny shift for easier visualization
      dt4.plot[exit == 1, time := time + .015] # add tiny shift for easier visualization
      
      plot4 <- ggplot() +
        # geom_point(data = dt4.plot, aes(x = time, y = wage, color = exit_category)) + 
        geom_line(data = mod4.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
        geom_line(data = mod4.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
        geom_point(data = mod4.preds[exit_category != "Counterfactual"], 
                   aes(x = time, y = wage, color = exit_category), 
                   size = 2.5) +
        geom_errorbar(data = mod4.preds[exit_category != "Counterfactual"], 
                      aes(x = time, ymax = upper, ymin = lower, color = exit_category), 
                      size = 1, 
                      width = .05) + 
        labs(
          # title = paste0("Quarterly wage history by exit type and time"), 
          # subtitle = "Model 4: three time points", 
          # caption = caption.text, 
          x = "", 
          y = "Quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-1, 0, 1))
      
      plot4 <- formatplots(plot4) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) +
        scale_y_continuous(limits = c(3000, 9000), breaks=c(seq(4000, 8000, 2000)), labels=scales::dollar_format()) 
      
      # dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(plot4)
      
    # Plot of residuals vs time to assess autocorrelation ----
      mod4.resid <- copy(dt4)[, fitted := fitted(mod4)]
      mod4.resid[, residual := wage - fitted]
      mod4.resid[, time := as.numeric(as.character(time))]
      mod4.resid[exit_category == "Negative", time := time - .05]
      mod4.resid[exit_category == "Positive", time := time + .05]
      set.seed(98104) # because jitter is 'random'
      
      plot.resid.4 <- ggplot() +
        geom_point(data = mod4.resid[exit_category != "Counterfactual"], 
                   aes(x = time, y = residual, color = exit_category), 
                   size = 2.5, 
                   position=position_jitterdodge(dodge.width=0.65, jitter.height=0, jitter.width=0.15), alpha=0.7) +
        labs(title = paste0("Hourly wage history by exit type and time"), 
             subtitle = "Model 4: residuals", 
             caption = caption.text, 
             x = "", 
             y = "Quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-1, 0, 1))
      
      plot.resid.4 <- formatplots(plot.resid.4) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Negative' = '#2ca25f')) 
      message("No pattern with residuals, so evidence of autocorrelation, and no need for including lag dependent variables")
      # plot.resid.4
      
    # Save plot ----
      saveplots(plot.object = plot4, plot.name = 'figure_4_pre_post_trends')  
      saveplots(plot.object = plot4, plot.name = 'appendix_figure_4_pre_post_trends_yearly')  
      saveplots(plot.object = plot.resid.4, plot.name = 'figure_4_pre_post_trends_residuals')  
      openxlsx::write.xlsx(mod4.preds, file = paste0(outputdir, "model_4_predictions.xlsx"), asTable = T, overwrite = T)
      openxlsx::write.xlsx(mod4.tidy, file = paste0(outputdir, "model_4_estimates.xlsx"), asTable = T, overwrite = T)
      
# Model 4.5: Hourly wage model for 1 prior to exit to one year post exit ----
    # Create dt4.5 for complete analysis: 1 year prior, at exit, and 1 year post ----
      dt4.5 <- copy(raw)[!is.na(wage_hourly)]
      dt4.5[qtr == -4, time := -1]
      dt4.5[qtr == 0, time := 0]
      dt4.5[qtr == 4, time := 1]
      dt4.5 <- dt4.5[!is.na(time)]      
      dt4.5[, time := factor(time)]
      dt4.5[exit_category == "Negative", exit := 0]
      dt4.5[exit_category == "Positive", exit := 1]
      # dt4.5[time == -1, exit := 0] # prior to exit, everyone is exit == 0

    # model ----
      dt4.5[, .N, .(exit, time)]
      mod4.5.formula <- paste0("wage_hourly ~ ",
                             "exit*time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod4.5 <- lme4::lmer(mod4.5.formula, data = dt4.5)
      mod4.5.tidy <- model.clean(mod4.5)

    # test if p-value for interaction term is < 0.05 with LRT ----
      mod4.5.formula.alt <- paste0("wage_hourly ~ ",
                             "exit + time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod4.5.alt <- lme4::lmer(mod4.5.formula.alt, data = dt4.5)
      
      mod4.5.test = anova(mod4.5, mod4.5.alt, test = 'LRT')
      
      if( mod4.5.test[["Pr(>Chisq)"]][2] < 0.05 ) {
        print("The exit:time interaction term is significant")
      } else {
        print("The exit:time interaction term is NOT significant, but keep in model because it is the DiD.")
      }
        caption.text <- paste0("", mod4.5.formula)

    # predictions ----
      # standard predictions ----
      mod4.5.preds <- as.data.table(predictions(mod4.5, 
                                              newdata = datagrid(time=c(-1, 0, 1), exit = c(0, 1)), 
                                              re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      mod4.5.preds[exit == 0, exit_category := "Negative"][exit == 1, exit_category := "Positive"]
      mod4.5.preds <- mod4.5.preds[, .(time = as.integer(as.character(time)), exit, exit_category, wage = predicted, se = std.error, lower = `conf.low`, upper = `conf.high`)]
      
      mod4.5.counterfactual <- mod4.5.preds[exit_category == "Positive"]
      
      # calculate counterfactual (ascribe change observed in negative exits to positive exits) ----
      mod4.5.preds <- calc.counterfactual(mod4.5.preds)
      mod4.5.preds[exit_category == "Positive", time := time - 0]
      mod4.5.preds[exit_category == "Negative", time := time + 0]
      mod4.5.preds[]
      
    # Plot data before and after exit ----
      dt4.5.plot = copy(dt4.5)
      dt4.5.plot[exit == 0, time := time - .015] # add tiny shift for easier visualization
      dt4.5.plot[exit == 1, time := time + .015] # add tiny shift for easier visualization
      
      plot4.5 <- ggplot() +
        # geom_point(data = dt4.5.plot, aes(x = time, y = wage, color = exit_category)) + 
        geom_line(data = mod4.5.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
        geom_line(data = mod4.5.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
        geom_point(data = mod4.5.preds[exit_category != "Counterfactual"], 
                   aes(x = time, y = wage, color = exit_category), 
                   size = 2.5) +
        geom_errorbar(data = mod4.5.preds[exit_category != "Counterfactual"], 
                      aes(x = time, ymax = upper, ymin = lower, color = exit_category), 
                      size = 1, 
                      width = .05) + 
        labs(title = paste0("Hourly wage history by exit type and time"), 
             subtitle = "Model 4.5: three time points", 
             caption = caption.text, 
             x = "", 
             y = "Quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-1, 0, 1))
      
      plot4.5 <- formatplots(plot4.5) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) 
      
      # dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(plot4.5)
      
    # Plot of residuals vs time to assess autocorrelation ----
      mod4.5.resid <- copy(dt4.5)[, fitted := fitted(mod4.5)]
      mod4.5.resid[, residual := wage - fitted]
      mod4.5.resid[, time := as.numeric(as.character(time))]
      mod4.5.resid[exit_category == "Negative", time := time - .05]
      mod4.5.resid[exit_category == "Positive", time := time + .05]
      set.seed(98104) # because jitter is 'random'
      
      plot.resid.4.5 <- ggplot() +
        geom_point(data = mod4.5.resid[exit_category != "Counterfactual"], 
                   aes(x = time, y = residual, color = exit_category), 
                   size = 2.5, 
                   position=position_jitterdodge(dodge.width=0.65, jitter.height=0, jitter.width=0.15), alpha=0.7) +
        labs(title = paste0("Hourly wage history by exit type and time"), 
             subtitle = "Model 4.5: residuals", 
             caption = caption.text, 
             x = "", 
             y = "Quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-1, 0, 1))
      
      plot.resid.4.5 <- formatplots(plot.resid.4.5) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Negative' = '#2ca25f')) 
      message("No pattern with residuals, so evidence of autocorrelation, and no need for including lag dependent variables")
      # plot.resid.4.5
      
    # Save plot ----
      saveplots(plot.object = plot4.5, plot.name = 'figure_4.5_pre_post_trends')      
      saveplots(plot.object = plot.resid.4.5, plot.name = 'figure_4.5_pre_post_trends_residuals')  
      openxlsx::write.xlsx(mod4.5.preds, file = paste0(outputdir, "model_4.5_predictions.xlsx"), asTable = T, overwrite = T)
      openxlsx::write.xlsx(mod4.5.tidy, file = paste0(outputdir, "model_4.5_estimates.xlsx"), asTable = T, overwrite = T)
      
# Model 5: Model for all available quarterly wage data ----
    # Create dt5 for complete quarterly analysis ----
      dt5 <- copy(raw)
      dt5[exit_category == "Negative", exit := 0]
      dt5[exit_category == "Positive", exit := 1]
      dt5 <- dt5[qtr %in% -4:4]
      dt5[, time := factor(qtr)]
      
    # model ----
      mod5.formula <- paste0("wage ~ ",
                             "exit*time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod5 <- lme4::lmer(mod5.formula, data = dt5)
      mod5.tidy <- model.clean(mod5)
      
    # test if p-value for interaction term is < 0.05 ----
      mod5.formula.alt <- paste0("wage ~ ",
                                 "exit + time + ", 
                                 confounders, " + ",
                                 "(1 | id_kc_pha) + ", # random intercept for persons
                                 "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod5.alt <- lme4::lmer(mod5.formula.alt, data = dt5)
      
      mod5.test = anova(mod5, mod5.alt, test = 'LRT')
      
      if( mod5.test[["Pr(>Chisq)"]][2] < 0.05 ) {
        print("The exit:time interaction term is significant, so keep the full model with the interaction term")
        caption.text <- paste0("", mod5.formula)
      } else {
        print("The exit:time interaction term is NOT significant, so use more parsimonuous model.")
        mod5.formula <- paste0("wage ~ ",
                               "exit + time + ", 
                               confounders, " + ",
                               "(1 | id_kc_pha) + ", # random intercept for persons
                               "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
        mod5 <- lme4::lmer(mod5.formula, data = dt5)
        mod5.tidy <- model.clean(mod5)
        caption.text <- paste0("", mod5.formula)
      }
      
    # predictions ----
      # standard predictions ----
      mod5.preds <- as.data.table(predictions(mod5, 
                                              newdata = datagrid(time=c(-4:4), # set to -4, 0, 4 because the time scale is in quarters, so -/+ 1 year
                                                                 exit = c(0, 1)), 
                                              re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      mod5.preds[exit == 0, exit_category := "Negative"][exit == 1, exit_category := "Positive"]
      mod5.preds <- mod5.preds[, .(time = as.integer(as.character(time)), exit, exit_category, wage = predicted, se = std.error, lower = `conf.low`, upper = `conf.high`)]
      

      # calculate counterfactual (ascribe change observed in negative exits to positive exits) ----
      mod5.preds <- calc.counterfactual(mod5.preds)
      mod5.preds[exit_category == "Positive", time := time - .05]
      mod5.preds[exit_category == "Negative", time := time + .05]
      mod5.preds[]
      
    # Plot data ----
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
        labs(
          # title = paste0("Quarterly wage history by exit type and time"), 
          # subtitle = "Model 5: Four quarters pre/post exit", 
          # caption = caption.text, 
          x = "", 
          y = "Quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      plot5 <- formatplots(plot5) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) 
      
      # dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
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
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      plot5.alt <- formatplots(plot5.alt) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) 
      
      # dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(plot5.alt)
      
    # Plot of residuals vs time to assess autocorrelation ----
      mod5.resid <- copy(dt5)[, fitted := fitted(mod5)][time %in% -4:4]
      mod5.resid[, residual := wage - fitted]
      mod5.resid[, time := as.numeric(as.character(time))]
      mod5.resid[exit_category == "Negative", time := time - .05]
      mod5.resid[exit_category == "Positive", time := time + .05]
      set.seed(98104) # because jitter is 'random'
      
      plot.resid.5 <- ggplot() +
        geom_point(data = mod5.resid[exit_category != "Counterfactual"], 
                   aes(x = time, y = residual, color = exit_category), 
                   size = 2.5, 
                   position=position_jitterdodge(dodge.width=0.65, jitter.height=0, jitter.width=0.15), alpha=0.7) +
        labs(
          # title = paste0("Quarterly wage history by exit type and time"), 
          # subtitle = "Model 5: residuals", 
          # caption = caption.text, 
          x = "", 
          y = "Quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      plot.resid.5 <- formatplots(plot.resid.5) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Negative' = '#2ca25f')) 
      message("No pattern with residuals, so evidence of autocorrelation, and no need for including lag dependent variables")
      # plot.resid.5
      
    # Tidy predictions for saving ----
      roundvars <- c("wage", "lower", "upper", "se")
      mod5.preds[, (roundvars) := rads::round2(.SD, 0), .SDcols = roundvars]
      mod5.preds <- mod5.preds[, .(Quarter = rads::round2(time, 0), 
                                   `Exit Type` = exit_category, 
                                   `Predicted wages (95% CI)` = paste0(
                                     "$",
                                     prettyNum(wage, big.mark = ','),
                                     " (", 
                                     prettyNum(lower, big.mark = ','), 
                                     ", ", 
                                     prettyNum(upper, big.mark = ','), 
                                     ")")
                                   # ,SE = se
                                   )]
      
    # Save plots ----
      saveplots(plot.object = plot5, plot.name = 'figure_5_pre_post_by_qtr')      
      saveplots(plot.object = plot5, plot.name = 'Body_Figure_2_predicted_wages_by_quarter')      
      saveplots(plot.object = plot.resid.5, plot.name = 'appendix_figure_3_residuals')      
      saveplots(plot.object = plot5.alt, plot.name = 'figure_5_pre_post_by_qtr_alt')      
      
      openxlsx::write.xlsx(mod5.tidy, file = paste0(outputdir, "appendix_table_3_coefficients.xlsx"), asTable = T, overwrite = T)
      openxlsx::write.xlsx(mod5.preds, file = paste0(outputdir, "appendix_table_4_predictions.xlsx"), asTable = T, overwrite = T)
      
      
# Model 6: Model for all available hourly wage data ----
    # Create dt6 for complete quarterly analysis ----
      dt6 <- copy(raw)
      dt6 <- dt6[!is.na(wage_hourly)]
      dt6[exit_category == "Negative", exit := 0]
      dt6[exit_category == "Positive", exit := 1]
      dt6[, time := factor(qtr)]
      
    # model ----
      mod6.formula <- paste0("wage_hourly ~ ",
                             "exit*time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod6 <- lme4::lmer(mod6.formula, data = dt6)
      mod6.tidy <- model.clean(mod6)
      
    # test if p-value for interaction term is < 0.05 ----
      mod6.formula.alt <- paste0("wage_hourly ~ ",
                                 "exit + time + ", 
                                 confounders, " + ",
                                 "(1 | id_kc_pha) + ", # random intercept for persons
                                 "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod6.alt <- lme4::lmer(mod6.formula.alt, data = dt6)
      
      mod6.test = anova(mod6, mod6.alt, test = 'LRT')
      
      if( mod6.test[["Pr(>Chisq)"]][2] < 0.05 ) {
        print("The exit:time interaction term is significant, so keep the full model with the interaction term")
        caption.text <- paste0("", mod6.formula)
      } else {
        print("The exit:time interaction term is NOT significant, so use more parsimonuous model.")
        mod6.formula <- paste0("wage_hourly ~ ",
                               "exit + time + ", 
                               confounders, " + ",
                               "(1 | id_kc_pha) + ", # random intercept for persons
                               "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
        mod6 <- lme4::lmer(mod6.formula, data = dt6)
        mod6.tidy <- model.clean(mod6)
        caption.text <- paste0("", mod6.formula)
      }
      
    # predictions ----
      # standard predictions ----
      mod6.preds <- as.data.table(predictions(mod6, 
                                              newdata = datagrid(time=c(-4:4), # set to -4, 0, 4 because the time scale is in quarters, so -/+ 1 year
                                                                 exit = c(0, 1)), 
                                              re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      mod6.preds[exit == 0, exit_category := "Negative"][exit == 1, exit_category := "Positive"]
      print('wages are actually hourly, but will label `wage` so that can reuse code from above.')
      mod6.preds <- mod6.preds[, .(time = as.integer(as.character(time)), exit, exit_category, wage = predicted, se = std.error, lower = `conf.low`, upper = `conf.high`)]
      

      # calculate counterfactual (ascribe change observed in negative exits to positive exits) ----
      mod6.preds <- calc.counterfactual(mod6.preds)
      mod6.preds[exit_category == "Positive", time := time - .05]
      mod6.preds[exit_category == "Negative", time := time + .05]
      mod6.preds[]
      
    # Plot data prior to exit ----
      plot6 <- ggplot() +
        # geom_point(data = dt6.plot, aes(x = time, y = wage, color = exit_category)) + 
        geom_line(data = mod6.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
        geom_line(data = mod6.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
        geom_point(data = mod6.preds[exit_category != "Counterfactual"], 
                   aes(x = time, y = wage, color = exit_category), 
                   size = 2.5) +
        geom_errorbar(data = mod6.preds[exit_category != "Counterfactual"], 
                      aes(x = time, ymax = upper, ymin = lower, color = exit_category), 
                      size = 1, 
                      width = .05) + 
        labs(title = paste0("Hourly wage history by exit type and time"), 
             subtitle = "Model 6: Four quarters pre/post exit", 
             caption = caption.text, 
             x = "", 
             y = "Hourly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      plot6 <- formatplots(plot6) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) 
      
      # dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(plot6)
      
      # alternative plot with CI for counterfactual
      plot6.alt <- ggplot() +
        # geom_point(data = dt6.plot, aes(x = time, y = wage, color = exit_category)) + 
        geom_line(data = mod6.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
        geom_line(data = mod6.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
        geom_point(data = mod6.preds[], 
                   aes(x = time, y = wage, color = exit_category), 
                   size = 2.5) +
        geom_errorbar(data = mod6.preds[], 
                      aes(x = time, ymax = upper, ymin = lower, color = exit_category), 
                      size = 1, 
                      width = .05) + 
        labs(title = paste0("Hourly wage history by exit type and time"), 
             subtitle = "Model 6: Four quarters pre/post exit", 
             caption = caption.text, 
             x = "", 
             y = "Hourly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      plot6.alt <- formatplots(plot6.alt) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) 
      
      # dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(plot6.alt)
      
    # Save plots ----
      saveplots(plot.object = plot6, plot.name = 'figure_6_pre_post_by_qtr')      
      saveplots(plot.object = plot6.alt, plot.name = 'figure_6_pre_post_by_qtr_alt')      
      
# Model 7: Model with IPTW for propensity scores rather than covariate adjustment ----
    # Create dt7 for complete quarterly analysis ----
      dt7 <- copy(raw)
      dt7[exit_category == "Negative", exit := 0]
      dt7[exit_category == "Positive", exit := 1]
      dt7[, time := factor(qtr)]
      
    # Model ----
      # get propensity score ----
        mod7.psformula <- paste0("exit ~ ", pscovariates) # no random effects bc only at time zero
        mod7.ps <- glm(mod7.psformula, 
                       family = binomial(link=logit),
                       data = dt7[qtr==0])
        mod7.ps.prob <- data.table(hh_id_kc_pha = dt7[qtr==0]$hh_id_kc_pha, 
                                       id_kc_pha = dt7[qtr==0]$id_kc_pha, 
                                       exit = dt7[qtr==0]$exit, 
                                       prob = predict(mod7.ps, type = 'response')) # predicted probability
        mod7.ps.prob[exit == 1, ipw := 1 / prob] # weight for IPW
        mod7.ps.prob[exit == 0, ipw := 1 / (1-prob)] # weight for IPW
        
      # merge propensity score weight onto the underlying data ----
        dt7 <- merge(dt7, mod7.ps.prob)
        
      # run mixed model with IPW ----
        mod7.formula <- paste0("wage ~ ",
                               "exit*time + ", 
                               "(1 | id_kc_pha) + ", # random intercept for persons
                               "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
        mod7 <- lme4::lmer(mod7.formula, data = dt7, weights = ipw)
        mod7.tidy <- model.clean(mod7)
    # Test if p-value for interaction term is < 0.05 ----
        mod7.formula.alt <- paste0("wage ~ ",
                                   "exit + time + ", 
                                   "(1 | id_kc_pha) + ", # random intercept for persons
                                   "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
        mod7.alt <- lme4::lmer(mod7.formula.alt, data = dt7, weights = ipw)
        
        mod7.test = anova(mod7, mod7.alt, test = 'LRT')
        
        if( mod7.test[["Pr(>Chisq)"]][2] < 0.05 ) {
          print("The exit:time interaction term is significant, so keep the full model with the interaction term")
          caption.text <- paste0("", mod7.formula)
        } else {
          print("The exit:time interaction term is NOT significant, so use more parsimonuous model.")
          mod7.formula <- copy(mod7.formula.alt)
          mod7 <- copy(mod7.alt)
          mod7.tidy <- model.clean(mod7.alt)
          caption.text <- paste0("", mod7.formula)
        }
    # Predictions ----
        # standard predictions ----
          mod7.preds <- as.data.table(predictions(mod7, 
                                                  newdata = datagrid(time=c(-4:4), # set to -4, 0, 4 because the time scale is in quarters, so -/+ 1 year
                                                                     exit = c(0, 1)), 
                                                  re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
          mod7.preds[exit == 0, exit_category := "Negative"][exit == 1, exit_category := "Positive"]
          mod7.preds <- mod7.preds[, .(time = as.integer(as.character(time)), exit, exit_category, wage = predicted, se = std.error, lower = `conf.low`, upper = `conf.high`)]
        
        
        # calculate counterfactual (ascribe change observed in negative exits to positive exits) ----
          mod7.preds <- calc.counterfactual(mod7.preds)
          mod7.preds[exit_category == "Positive", time := time - .05]
          mod7.preds[exit_category == "Negative", time := time + .05]
          mod7.preds[]
        
    # Plot data prior to exit ----
        plot7 <- ggplot() +
          # geom_point(data = dt7.plot, aes(x = time, y = wage, color = exit_category)) + 
          geom_line(data = mod7.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
          geom_line(data = mod7.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
          geom_point(data = mod7.preds[exit_category != "Counterfactual"], 
                     aes(x = time, y = wage, color = exit_category), 
                     size = 2.5) +
          geom_errorbar(data = mod7.preds[exit_category != "Counterfactual"], 
                        aes(x = time, ymax = upper, ymin = lower, color = exit_category), 
                        size = 1, 
                        width = .05) + 
          labs(title = paste0("Quarterly wage history by exit type and time"), 
               subtitle = "Model 7: IPTW four quarters pre/post exit", 
               caption = caption.text, 
               x = "", 
               y = "Quarterly wages") +
          scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
        
        plot7 <- formatplots(plot7) + 
          scale_color_manual("Exit type", 
                             values=c('Positive' = '#2c7fb8', 
                                      'Counterfactual' = '#e41a1c', 
                                      'Negative' = '#2ca25f')) 
        
        # dev.new(width = 7,  height = 4, unit = "in", noRStudioGD = TRUE)
        plot(plot7)
        
        # alternative plot with CI for counterfactual
        plot7.alt <- ggplot() +
          # geom_point(data = dt7.plot, aes(x = time, y = wage, color = exit_category)) + 
          geom_line(data = mod7.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
          geom_line(data = mod7.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
          geom_point(data = mod7.preds[], 
                     aes(x = time, y = wage, color = exit_category), 
                     size = 2.5) +
          geom_errorbar(data = mod7.preds[], 
                        aes(x = time, ymax = upper, ymin = lower, color = exit_category), 
                        size = 1, 
                        width = .05) + 
          labs(title = paste0("Quarterly wage history by exit type and time"), 
               subtitle = "Model 7: IPTW four quarters pre/post exit", 
               caption = caption.text, 
               x = "", 
               y = "Quarterly wages") +
          scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
        
        plot7.alt <- formatplots(plot7.alt) + 
          scale_color_manual("Exit type", 
                             values=c('Positive' = '#2c7fb8', 
                                      'Counterfactual' = '#e41a1c', 
                                      'Negative' = '#2ca25f')) 
        
        # dev.new(width = 7,  height = 4, unit = "in", noRStudioGD = TRUE)
        plot(plot7.alt)
        
    #  Save plots ----
        saveplots(plot.object = plot7, plot.name = 'figure_7_pre_post_by_qtr')      
        saveplots(plot.object = plot7.alt, plot.name = 'figure_7_pre_post_by_qtr_alt')      
        
        
# The end ----