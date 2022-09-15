# Header ----
# Author: Danny Colombara
# Date: May 23, 2022
# R version 4.1.2
# Purpose: Model the relationship between exit type and wages. 
#          Originally intended to to pursue a difference in differences (DiD)
#          model but exits are influenced by wages, so cannot use DiD to 
#          ascribe changes in wages post exit to the exit type. 
#          Will pursue a descriptive model of quarterly wages for the time period
#          on year pre and one year post exit.
#           
# Notes: Need to account for repeated measures on individuals and clustering by 
#        by households. Will address both issues with inclusion of random effects
#
#        Will assess whether parallel trends assumption (for DiD) holds by looking 
#        at the change in wages between the fourth quarter prior to exit and the 
#        the exit quarter
#
#        Prefer to use percent area median income (AMI) rather than the exact
#        wage when possible because % AMI accounts for household size and is 
#        also the criteria for entrance into public housing. 
#
#        The models below only have potential confounders that were found to 
#        be associated with both the exposure (exit type) and outcome (wages). 
# 
#        Any propensity scores will be comprised of variables that are associated
#        with the predictor (exit type), regardless of the association with the 
#        outcome (change in wages).
#
# Notes on predictions:
#        * Used marginaleffects::predictions because provides estimates + SE & CI
#        * margins::prediction & prediction::prediction do not provide SE & CI
#        * stats::predict (predict.merMod) doesn't provide SE & CI & need to specify 
#          all fixed effects

# Set up ----
    rm(list=ls())
    options(scipen = 999)
    pacman::p_load(lubridate, rads, data.table, DBI, odbc, ggplot2, lme4, marginaleffects, openxlsx)

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
        ggsave(paste0(outputdir, '/pdf/', plot.name, ".pdf"),
               plot = plot.object, 
               dpi=600, 
               width = 6, 
               height = 4, 
               units = "in") 
        ggsave(paste0(outputdir, '/png/', plot.name, ".png"),
               plot = plot.object, 
               dpi=600, 
               width = 6, 
               height = 4, 
               units = "in") 
      }
       
    # calculate mean values of predictions for my dataset using random draws ----
      prediction_summary <- function(predDT, ndraw = 1000){
        myest <- predDT[, .(time, exit_category, predicted, std.error)]
        
        # create summary table of each combination of time and exit_category
        mysets <- unique(myest[, .(time, exit_category, wage = NA_real_, se = NA_real_)])
        
        # loop over each unique combination of time and exit_category to calculate the mean and se of predictions
        for(i in 1:nrow(mysets)){
          # create subset of the data for the unique time * exit_category
          mysubset <- myest[time == mysets[i, ][['time']] & exit_category == mysets[i, ][['exit_category']] ]
          
          # take draws from prediction and standard error for each row of the subset
          set.seed(98104)
          mydraws <- c() # empty vector to store draws
          for(ii in 1:nrow(mysubset)){
            mydraws <- c(mydraws, rnorm(ndraw, mysubset[ii]$predicted, mysubset[ii]$std.error))
          }
          
          # calculate summary (mean and se) from the draws
          mysets[i, wage := mean(mydraws)]
          mysets[i, se := sd(mydraws)/sqrt(length(mydraws)) ] # This standard error seems way too small!
          mysets[i, se := sd(mydraws)] # This standard error seems a bit large, but reasonable considering that it is summarizing all of the predictions, for different populations
          mysets[i, lower := wage - qnorm(0.975) * se]
          mysets[i, upper := wage + qnorm(0.975) * se]
        }
        
        # return object
        return(mysets)
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
        mymod.counterfactual <- mymod.counterfactual[, .(time = as.integer(as.character(time)), exit_category = 'Counterfactual', 
                                                           wage, se, lower = wage - (se * qnorm(0.975)), 
                                                           upper = wage + (se * qnorm(0.975)))]
        mymod.preds <- rbind(
          mymod.preds,
          mymod.counterfactual)
        setorder(mymod.preds, exit_category, time)
        
        mymod.preds[, time := as.numeric(as.character(time))] # convert time back to numeric for graphing
      }    

    # clean model estimates ----
      model.clean <- function(mymod, myformat = NA){
        mymod.tidy <- as.data.table(broom.mixed::tidy(mymod, conf.int = T))
        roundvars <- c("estimate", "conf.low", "conf.high", "std.error")
        mymod.tidy[, (roundvars) := rads::round2(.SD, 0), .SDcols = roundvars]
        mymod.tidy[, p.value := as.character(rads::round2(p.value, 3))]
        mymod.tidy[as.numeric(p.value) < 0.001, p.value := "<0.001"]
        if(myformat == 'dollar'){
          mymod.tidy <- mymod.tidy[, .(Effect = effect, 
                                       Group = group, 
                                       Term = term, 
                                       Estimate = paste0("$",
                                         prettyNum(estimate, big.mark = ','),
                                         " ($", 
                                         prettyNum(conf.low, big.mark = ','), 
                                         ", $", 
                                         prettyNum(conf.high, big.mark = ','), 
                                         ")") , 
                                       'P-value' = p.value
                                       # , SE = std.error
                                       )]
        }
        if(myformat == 'percent'){
          mymod.tidy <- mymod.tidy[, .(Effect = effect, 
                                       Group = group, 
                                       Term = term, 
                                       Estimate = paste0(prettyNum(estimate, big.mark = ','),
                                                         "% (", 
                                                         prettyNum(conf.low, big.mark = ','), 
                                                         "%, ", 
                                                         prettyNum(conf.high, big.mark = ','), 
                                                         "%)") , 
                                       'P-value' = p.value)]
        }        
        mymod.tidy[Term == 'exit', Term := "Positive exit"]
        mymod.tidy[Term %like% 'AI/AN', Term := "American Indian / Alaska Native"]
        mymod.tidy[Term %like% 'Asian', Term := "Asian"]
        mymod.tidy[Term %like% 'Latino', Term := "Hispanic"]
        mymod.tidy[Term %like% 'Multiple', Term := "Multiple"]
        mymod.tidy[Term %like% 'NH/PI', Term := "Native Hawaiian / Pacific Islander"]
        mymod.tidy[Term %like% 'White', Term := "White"]
        mymod.tidy[, Term := gsub("as.integer\\(time\\)", "time", Term)]
        mymod.tidy[, Term := gsub("exit_year", "Exit year: ", Term)]
        mymod.tidy[Term == 'single_caregiverTRUE', Term := "Single caregiver household"]
        mymod.tidy[Term == 'housing_time_at_exit', Term := "Years in public housing"]
        mymod.tidy[, Term := gsub("splines::bs", "spline", Term)]
        mymod.tidy[, Estimate := gsub("\\$-", "-\\$", Estimate)]
        mymod.tidy[, Estimate := gsub(" \\(\\$NA\\, \\$NA\\)", "", Estimate)]
        mymod.tidy[, Estimate := gsub("\\$NaN", NA, Estimate)]
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
    raw[, exit_year := factor(exit_year)]
    raw[, exit := as.integer(exit)]
    raw[, exit_category := factor(exit_category, levels = c("Positive", "Negative"))]
    raw[, quarter := as.factor(quarter(exit_date))]
    raw[, season := factor(quarter(exit_date), levels = 1:4, labels = c("Winter", "Spring", "Summer", "Fall"))]
    raw[, opportunity_index1k := 1000*opportunity_index]
    
    raw <- raw[qtr %in% -4:4] # limit to quarters of interest
    
    # set reference for factors
    raw[, race_eth_me := factor(race_eth_me)]
    raw$race_eth_me <- relevel(raw$race_eth_me, ref = 'Black')

# Model Appendix 1: assess parallel trends prior to exit ----
    # Create dtappdx1 for assessing parallel trend assumption from quarter -4:0 ----
      dtappdx1 <- copy(raw)[qtr %in% -4:0]
      dtappdx1[qtr %in% -4:0, time := qtr]
      dtappdx1[, .N, .(exit, time)] # check to see how much data there is

    # model ----
      modappdx1.formula <- paste0("wage ~ ", 
                             #"exit*time + ", 
                             "exit*splines::bs(as.integer(time), df = 3) + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      modappdx1 <-lmerTest::lmer(modappdx1.formula, data = dtappdx1)
      modappdx1.tidy <- model.clean(modappdx1)
      
    # test if interaction is significant ----
      modappdx1.formula.alt <- paste0("wage ~ ", 
                               "exit + time + ", 
                               # "exit + splines::bs(as.integer(time), df = 3) + ", 
                               confounders, " + ",
                               "(1 | id_kc_pha) + ", # random intercept for persons
                               "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      modappdx1.alt <- lmerTest::lmer(modappdx1.formula.alt, data = dtappdx1)
      modappdx1.test = anova(modappdx1, modappdx1.alt, test = 'LRT')
      
      if( modappdx1.test[["Pr(>Chisq)"]][2] < 0.05 ) {
        print("The exit:time interaction term is significant, so keep the full model with the interaction term")
        caption.text <- paste0("", modappdx1.formula)
      } else {
        print("The exit:time interaction term is NOT significant, so use more parsimonuous model.")
        modappdx1.formula <- copy(modappdx1.formula.alt)
        modappdx1 <- copy(modappdx1.alt)
        modappdx1.tidy <- model.clean(modappdx1)
        caption.text <- paste0("", modappdx1.formula)
      } 
      
    # Plot observed vs expected to assess model quality and problems ----
      OvE.appx1 <- copy(dtappdx1)[, fitted := fitted(modappdx1)]
      OvE.appx1 <- ggplot(data = OvE.appx1, aes(x = wage, y = fitted, color = exit_category)) + geom_point()
      OvE.appx1 <- formatplots(OvE.appx1)
      
    # predictions ----  
      # Using marginaleffects::predictions ----
        modappdx1.preds <- as.data.table(marginaleffects::predictions(modappdx1, 
                                                                 newdata = dtappdx1, 
                                                                 re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
        modappdx1.preds <- prediction_summary(modappdx1.preds, ndraw = 10000)
        
      # Calculate counterfactual ----
        modappdx1.preds <- calc.counterfactual(modappdx1.preds)
        modappdx1.preds[exit_category == "Positive", time := time - .0]
        modappdx1.preds[exit_category == "Negative", time := time + .0]
        # modappdx1.preds[]

    # plot ----
        # commented out errorbars because don't trust standard error calculation from prediction_summary()
        plotappdx1 <- ggplot() +
          geom_line(data = modappdx1.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
          geom_line(data = modappdx1.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
          geom_point(data = modappdx1.preds[exit_category != "Counterfactual"], 
                     aes(x = time, y = wage, color = exit_category), 
                     size = 2.5) +
          # geom_errorbar(data = modappdx1.preds[exit_category != "Counterfactual"], 
          #               aes(x = time, ymax = upper, ymin = lower, color = exit_category), 
          #               size = 1, 
          #               width = .05) + 
          labs(
            x = "", 
            y = "Predicted quarterly wages") +
          scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
        
        plotappdx1 <- formatplots(plotappdx1) + 
          scale_color_manual("Exit type", 
                             values=c('Positive' = '#2c7fb8', 
                                      'Counterfactual' = '#e41a1c', 
                                      'Negative' = '#2ca25f')) + 
          scale_y_continuous(limits = c(3000, 9000), breaks=c(seq(4000, 8000, 2000)), labels=scales::dollar_format()) 
        
        # dev.new(width = 7,  height = 4, unit = "in", noRStudioGD = TRUE)
        plot(plotappdx1)
        
    # Save plots ----
      saveplots(plot.object = plotappdx1, plot.name = 'appendix_figure_1_assess_parallel_trends')
      # saveplots(plot.object = OvE.appx1, plot.name = 'appendix_figure_1_assess_parallel_trends_Obs_v_Exp')
      
# Model Appendix 2: IPW to see if improves parallel trends of pre-exit curves ----
    # also hope it will reduce the problem with non-parallel slopes prior to exit
    # create dtappdx2 ----
      dtappdx2 <- copy(raw)[qtr %in% -4:0]
      dtappdx2[qtr %in% -4:0, time := qtr]
      dtappdx2[, .N, .(exit, time)] # check to see how much data there is

    # model ----
      # propensity score ----
      modappdx2.psformula <- paste0("exit ~ ", pscovariates) # no random effects bc only at time zero
      modappdx2.ps <- glm(modappdx2.psformula, 
                     family = binomial(link=logit),
                     data = dtappdx2[qtr==0])
      
      modappdx2.ps.preds = as.data.table(predictions(modappdx2.ps, newdata = copy(dtappdx2)))
      modappdx2.ps.preds <- modappdx2.ps.preds[qtr == 0 & !is.na(predicted)]
      
      modappdx2.ps.prob <- data.table(hh_id_kc_pha = modappdx2.ps.preds$hh_id_kc_pha, 
                                 id_kc_pha = modappdx2.ps.preds$id_kc_pha, 
                                 exit = modappdx2.ps.preds$exit, 
                                 prob = modappdx2.ps.preds$predicted) # predicted probability
      
      modappdx2.ps.prob[exit == 1, ipw := 1 / prob] # weight for IPW
      modappdx2.ps.prob[exit == 0, ipw := 1 / (1-prob)] # weight for IPW
      
      dtappdx2 <- merge(dtappdx2, modappdx2.ps.prob, by = c("hh_id_kc_pha", "id_kc_pha", "exit"), all.x = F, all.y = T)
      
      # main model ----
      dtappdx2[, .N, .(exit, time)]
      modappdx2.formula <- paste0("wage ~ ", 
                             "exit*splines::bs(as.integer(time), df = 3) + ", 
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)")  # random intercept and slope for households
      modappdx2 <- lmerTest::lmer(modappdx2.formula, data = dtappdx2, weights = ipw)
      modappdx2.tidy <- model.clean(modappdx2)
      
    # test if p-value for interaction term is < 0.05 using likelihood ratio test ----
      modappdx2.formula.alt <- paste0("wage ~ ", 
                                 "exit + time + ", 
                                 "(1 | id_kc_pha) + ", # random intercept for persons
                                 "(1 + exit | hh_id_kc_pha)")  # random intercept and slope for households
      modappdx2.alt <- lmerTest::lmer(modappdx2.formula.alt, data = dtappdx2, weights = ipw)
      modappdx2.test = anova(modappdx2, modappdx2.alt, test = 'LRT')

      if( modappdx2.test[["Pr(>Chisq)"]][2] < 0.05 ) {
        print("The exit:time interaction term is significant, so keep the full model with the interaction term")
        caption.text <- paste0("", modappdx2.formula)
      } else {
        print("The exit:time interaction term is NOT significant, so use more parsimonuous model.")
        modappdx2.formula <- copy(modappdx2.formula.alt)
        modappdx2 <- copy(modappdx2.alt)
        modappdx2.tidy <- model.clean(modappdx2)
        caption.text <- paste0("", modappdx2.formula)
      }      
      
    # create table of predictions for slopes in ggplot ----
      modappdx2.preds <- as.data.table(marginaleffects::predictions(modappdx2, 
                                              newdata = copy(dtappdx2), # copy so not changed to data.frame
                                              re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      modappdx2.preds <- prediction_summary(modappdx2.preds, ndraw = 10000)
      
    # calculate counterfactual for positive exits ----
      modappdx2.preds <- calc.counterfactual(modappdx2.preds)
      # modappdx2.preds[]

    # Plot data prior to exit ----
      # commented out errorbars because don't trust standard error calculation from prediction_summary()
      plotappdx2 <- ggplot() +
        geom_line(data = modappdx2.preds[exit_category != "Counterfactual"], 
                  aes(x = time, y = wage, color = exit_category), 
                  size = 1) +
        geom_line(data = modappdx2.preds[exit_category == "Counterfactual"], 
                  aes(x = time, y = wage, color = exit_category), linetype="dashed", 
                  size = 1) +
        geom_point(data = modappdx2.preds[exit_category != "Counterfactual"], 
                   aes(x = time, y = wage, color = exit_category), 
                   size = 2.5) +
        # geom_errorbar(data = modappdx2.preds[exit_category != "Counterfactual"], 
        #               aes(x = time, ymax = upper, ymin = lower, color = exit_category), 
        #               size = 1, 
        #               width = .05) + 
        labs(
           x = "", 
           y = "Predicted quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit"), breaks=c(-4, 0)) 
      
      plotappdx2 <- formatplots(plotappdx2) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) +
       scale_y_continuous(limits = c(3000, 9000), breaks=c(seq(4000, 8000, 2000)), labels=scales::dollar_format()) 

      plot(plotappdx2)
      
    # Save plot ----
      saveplots(plot.object = plotappdx2, plot.name = 'appendix_figure_2_IPTW_assess_parallel_trends')

# Model Appendix 4: Simplified quarterly wage model for 1 prior to exit to one year post exit ----
    # Create dtappdx4 for complete analysis: 1 year prior, at exit, and 1 year post ----
      dtappdx4 <- copy(raw)
      dtappdx4[qtr == -4, time := -1]
      dtappdx4[qtr == 0, time := 0]
      dtappdx4[qtr == 4, time := 1]
      dtappdx4 <- dtappdx4[!is.na(time)]      
      dtappdx4[, time := ordered(time)]
      dtappdx4[exit_category == "Negative", exit := 0]
      dtappdx4[exit_category == "Positive", exit := 1]
      # dtappdx4[time == -1, exit := 0] # prior to exit, everyone is exit == 0

    # model ----
      dtappdx4[, .N, .(exit, time)]
      modappdx4.formula <- paste0("wage ~ ",
                             "exit*time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      modappdx4 <- lmerTest::lmer(modappdx4.formula, data = dtappdx4)
      modappdx4.tidy <- model.clean(modappdx4)
      
    # test if p-value for interaction term is < 0.05 with LRT ----
      modappdx4.formula.alt <- paste0("wage ~ ",
                             "exit + time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      modappdx4.alt <- lmerTest::lmer(modappdx4.formula.alt, data = dtappdx4)
      
      modappdx4.test = anova(modappdx4, modappdx4.alt, test = 'LRT')
      
      if( modappdx4.test[["Pr(>Chisq)"]][2] < 0.05 ) {
        print("The exit:time interaction term is significant")
      } else {
        print("The exit:time interaction term is NOT significant.")
        modappdx4.formula <- copy(modappdx4.formula.alt)
      }
        caption.text <- paste0("", modappdx4.formula)

    # predictions ----
      # standard predictions ----
      modappdx4.preds <- as.data.table(predictions(modappdx4, 
                                              newdata = copy(dtappdx4), 
                                              re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      modappdx4.preds <- prediction_summary(modappdx4.preds, ndraw = 10000)
        
      # calculate counterfactual (ascribe change observed in negative exits to positive exits) ----
      modappdx4.preds <- calc.counterfactual(modappdx4.preds)
      modappdx4.preds[]
      
    # Plot data before and after exit ----
      # commented out errorbars because don't trust standard error calculation from prediction_summary()
      plotappdx4 <- ggplot() +
        geom_line(data = modappdx4.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
        geom_line(data = modappdx4.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
        geom_point(data = modappdx4.preds[exit_category != "Counterfactual"], 
                   aes(x = time, y = wage, color = exit_category), 
                   size = 2.5) +
        # geom_errorbar(data = modappdx4.preds[exit_category != "Counterfactual"], 
        #               aes(x = time, ymax = upper, ymin = lower, color = exit_category), 
        #               size = 1, 
        #               width = .05) + 
        labs(
          # title = paste0("Quarterly wage history by exit type and time"), 
          # subtitle = "Model 4: three time points", 
          # caption = caption.text, 
          x = "", 
          y = "Predicted quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-1, 0, 1))
      
      plotappdx4 <- formatplots(plotappdx4) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) +
        scale_y_continuous(limits = c(3000, 9000), breaks=c(seq(4000, 8000, 2000)), labels=scales::dollar_format()) 
      
      # dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(plotappdx4)
      
    # Plot observed vs expected to assess model quality and problems ----
      OvE.appx4 <- copy(dtappdx4)[, fitted := fitted(modappdx4)]
      OvE.appx4 <- ggplot(data = OvE.appx4, aes(x = wage, y = fitted, color = exit_category)) + geom_point()
      OvE.appx4 <- formatplots(OvE.appx4)
      plot(OvE.appx4)
      
    # Plot of residuals vs time to assess autocorrelation ----
      modappdx4.resid <- copy(dtappdx4)[, fitted := fitted(modappdx4)]
      modappdx4.resid[, residual := wage - fitted]
      modappdx4.resid[, time := as.numeric(as.character(time))]
      modappdx4.resid[exit_category == "Negative", time := time - .05]
      modappdx4.resid[exit_category == "Positive", time := time + .05]
      set.seed(98104) # because jitter is 'random'
      
      plot.resid.4 <- ggplot() +
        geom_point(data = modappdx4.resid[exit_category != "Counterfactual"], 
                   aes(x = time, y = residual, color = exit_category), 
                   size = 2.5, 
                   position=position_jitterdodge(dodge.width=0.65, jitter.height=0, jitter.width=0.15), alpha=0.7) +
        labs(title = paste0("Hourly wage history by exit type and time"), 
             subtitle = "Model 4: residuals", 
             caption = caption.text, 
             x = "", 
             y = "Predicted quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-1, 0, 1))
      
      plot.resid.4 <- formatplots(plot.resid.4) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Negative' = '#2ca25f')) 
      message("No pattern with residuals, so evidence of autocorrelation, and no need for including lag dependent variables")
      plot.resid.4
      
    # Save plots ----
      saveplots(plot.object = plotappdx4, plot.name = 'appendix_figure_4_pre_post_trends_yearly')  
      # saveplots(plot.object = plot.resid.4, plot.name = 'appendix_figure_4_pre_post_trends_residuals')  
      # saveplots(plot.object = OvE.appx4, plot.name = 'appendix_figure_4_trends_Obs_v_Exp')

# Model 1: Model for all available quarterly wage data ----
    # Create dt1 for complete quarterly analysis ----
      dt1 <- copy(raw)[qtr %in% -4:4]
      dt1[qtr %in% -4:4, time := qtr]
      dt1[, .N, .(exit, time)] # check to see how much data there is
      # dt1$time <- relevel(dt1$time, ref = '0')
      
    # model ----
      mod1.formula <- paste0("wage ~ ",
                             "exit*splines::bs(as.integer(time), knots = c(0)) + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod1 <- lmerTest::lmer(mod1.formula, data = setDF(copy(dt1))) # use lmerTest rather than lme4 because gives p.values but estimates and SE are identical
      mod1.tidy <- model.clean(mod1, myformat = 'dollar')
      mod1.rsquared <- MuMIn::r.squaredGLMM(mod1) # psuedo-R squared

    # test if p-value for interaction term is < 0.05 ----
      mod1.formula.alt <- paste0("wage ~ ",
                                 "exit + time + ", 
                                 confounders, " + ",
                                 "(1 | id_kc_pha) + ", # random intercept for persons
                                 "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod1.alt <- lmerTest::lmer(mod1.formula.alt, data = setDF(copy(dt1)))
      
      mod1.test = anova(mod1, mod1.alt, test = 'LRT')
      
      if( mod1.test[["Pr(>Chisq)"]][2] < 0.05 ) {
        print("The exit:time interaction term is significant, so keep the full model with the interaction term")
        caption.text <- paste0("", mod1.formula)
      } else {
        print("The exit:time interaction term is NOT significant, so use more parsimonuous model.")
        mod1.formula <- paste0("wage ~ ",
                               "exit + time + ", 
                               confounders, " + ",
                               "(1 | id_kc_pha) + ", # random intercept for persons
                               "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
        mod1 <- lmerTest::lmer(mod1.formula, data = dt1)
        mod1.tidy <- model.clean(mod1)
        caption.text <- paste0("", mod1.formula)
      }
      
    # predictions ----
      # standard predictions ----
      mod1.preds <- as.data.table(predictions(mod1, 
                                              newdata = copy(dt1),
                                              re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      mod1.preds <- prediction_summary(mod1.preds, ndraw = 10000)
      
      # calculate counterfactual (ascribe change observed in negative exits to positive exits) ----
      mod1.preds <- calc.counterfactual(mod1.preds)
      # mod1.preds[]
      
      # calculate quarterly change ----
      mod1.preds[, prev.wage := shift(wage), by = "exit_category"]
      mod1.preds[, change.dollars := wage - prev.wage]
      mod1.preds[, change.percent := rads::round2(change.dollars / prev.wage, 3)]
      mod1.preds[, prev.wage := NULL]
      
    # Plot data four quarters before and after exit ----
      # Main (predicted wages) ----
      plot1 <- ggplot() +
        # commented out errorbars because don't trust standard error calculation from prediction_summary()
        geom_line(data = mod1.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
        geom_line(data = mod1.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
        geom_point(data = mod1.preds[exit_category != "Counterfactual"], 
                   aes(x = time, y = wage, color = exit_category), 
                   size = 2.5) +
        # geom_errorbar(data = mod1.preds[exit_category != "Counterfactual"], 
        #               aes(x = time, ymax = upper, ymin = lower, color = exit_category), 
        #               size = 1, 
        #               width = .05) + 
        labs(
          x = "", 
          y = "Predicted quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      plot1 <- formatplots(plot1) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) 
      
      # dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(plot1)
      
      # Change in dollars ----
      plot1.change.dollars <- ggplot() +
        geom_line(data = mod1.preds[exit_category != "Counterfactual"], aes(x = time, y = change.dollars, color = exit_category), size = 1) +
        geom_point(data = mod1.preds[exit_category != "Counterfactual"], 
                   aes(x = time, y = change.dollars, color = exit_category), 
                   size = 2.5) +
        labs(
          x = "", 
          y = "Predicted increase in quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      plot1.change.dollars <- formatplots(plot1.change.dollars) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Negative' = '#2ca25f')) +
        coord_cartesian(ylim = c(0, 500)) 
      
      
      # dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(plot1.change.dollars)
      
      # Change in percent ----
      plot1.change.percent <- ggplot() +
        geom_line(data = mod1.preds[exit_category != "Counterfactual"], aes(x = time, y = change.percent, color = exit_category), size = 1) +
        geom_point(data = mod1.preds[exit_category != "Counterfactual"], 
                   aes(x = time, y = change.percent, color = exit_category), 
                   size = 2.5) +
        labs(
          x = "", 
          y = "Predicted increase in quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      plot1.change.percent <- formatplots(plot1.change.percent) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Negative' = '#2ca25f')) +
        scale_y_continuous(labels=scales::percent_format()) + 
        coord_cartesian(ylim = c(0, .07)) 
        
      
      # dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(plot1.change.percent)
      
    # Plot observed vs expected to assess model quality and problems ----
      OvE.mod1 <- copy(dt1)[, fitted := fitted(mod1)]
      OvE.mod1 <- ggplot(data = OvE.mod1, aes(x = wage, y = fitted, color = exit_category)) + 
        geom_point() +
        labs(
          x = "Observed wages", 
          y = "Predicted wages") 
      OvE.mod1 <- formatplots(OvE.mod1) +
        scale_x_continuous(labels=scales::dollar_format()) +
        geom_abline(intercept = 0 , slope = 1) 

      plot(OvE.mod1)
      
      
      
    # Plot of residuals vs time to assess autocorrelation ----
      mod1.resid <- copy(dt1)[, fitted := fitted(mod1)][time %in% -4:4]
      mod1.resid[, residual := wage - fitted]
      mod1.resid[, time := as.numeric(as.character(time))]
      mod1.resid[exit_category == "Negative", time := time - .05]
      mod1.resid[exit_category == "Positive", time := time + .05]
      set.seed(98104) # because jitter is 'random'
      
      plot.resid.mod1 <- ggplot() +
        geom_point(data = mod1.resid[exit_category != "Counterfactual"], 
                   aes(x = time, y = residual, color = exit_category), 
                   size = 2.5, 
                   position=position_jitterdodge(dodge.width=0.65, jitter.height=0, jitter.width=0.15), alpha=0.7) +
        labs(
          # title = paste0("Quarterly wage history by exit type and time"), 
          # subtitle = "Model 5: residuals", 
          # caption = caption.text, 
          x = "", 
          y = "Wage residuals") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      plot.resid.mod1 <- formatplots(plot.resid.mod1) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Negative' = '#2ca25f')) 
      message("No pattern with residuals, so evidence of autocorrelation, and no need for including lag dependent variables")
      plot.resid.mod1
      
    # Tidy predictions for saving ----
      roundvars <- c("wage", "lower", "upper", "se", "change.dollars")
      mod1.preds[, (roundvars) := rads::round2(.SD, 0), .SDcols = roundvars]
      mod1.obs = dt1[, .(Observed = rads::round2(mean(wage))), .(time, exit_category)]
      mod1.preds <- merge(mod1.preds, mod1.obs, by = c("time", "exit_category"), all.x = TRUE, all.y = TRUE)
      mod1.preds <- mod1.preds[, .(Quarter = rads::round2(time, 0), 
                                   `Exit Type` = exit_category, 
                                   `Predicted wages (95% CI)` = paste0(
                                     "$",
                                     prettyNum(wage, big.mark = ','),
                                     " ($", 
                                     prettyNum(lower, big.mark = ','), 
                                     ", $", 
                                     prettyNum(upper, big.mark = ','), 
                                     ")"), 
                                   Predicted = paste0("$", prettyNum(wage, big.mark = ',')),
                                   Observed = paste0("$", prettyNum(Observed, big.mark = ',')),
                                   `Increase (dollars)` = paste0("$", prettyNum(change.dollars, big.mark = ',')), 
                                   `Increase (percent)` = paste0(100*change.percent, "%")
                                   # ,SE = se
                                   )]
      mod1.preds[, "Predicted wages (95% CI)" := gsub("\\$-", "-\\$", `Predicted wages (95% CI)`)]
      mod1.preds[Observed == "$NA", Observed := NA]
      mod1.preds[`Increase (dollars)` == "$NA", `Increase (dollars)` := NA]
      mod1.preds[`Increase (percent)` == "NA%", `Increase (percent)` := NA]
      setorder(mod1.preds, `Exit Type`, Quarter)
      
    # Save plots ----
      saveplots(plot.object = plot1, plot.name = 'figure_2_pre_post_by_qtr')      
      saveplots(plot.object = plot1.change.percent, plot.name = 'figure_2_pre_post_by_qtr_change_percent')      
      saveplots(plot.object = plot1.change.dollars, plot.name = 'figure_2_pre_post_by_qtr_change_dollar')      
      saveplots(plot.object = plot.resid.mod1, plot.name = 'appendix_figure_3_residuals')      
      saveplots(plot.object = OvE.mod1, plot.name = 'appendix_figure_3_trends_Obs_v_Exp')      

# Model 2: Secondary analysis with %AMI as outcome ----
    # Create dt2 for complete quarterly analysis ----
      dt2 <- copy(raw)[qtr %in% -4:4]
      dt2[qtr %in% -4:4, time := qtr]
      
      # identify and drop any clients who are missing percent_ami for any of the quarters
      dt2 <- dt2[!id_kc_pha %in% unique(dt2[is.na(percent_ami)]$id_kc_pha)]
      
      dt2[, .N, .(exit, time)] # check to see how much data there is
      message("Unique persons with percent AMI:", length(unique(dt2$id_kc_pha)))
      message("Unique persons with percent AMI & NEG exit:", length(unique(dt2[exit_category == 'Negative']$id_kc_pha)))
      message("Unique persons with percent AMI & POS exit:", length(unique(dt2[exit_category == 'Positive']$id_kc_pha)))
      message("Unique households with percent AMI:", length(unique(dt2$hh_id_kc_pha)))
      message("Unique households with percent AMI & NEG exit:", length(unique(dt2[exit_category == 'Negative']$hh_id_kc_pha)))
      message("Unique households with percent AMI & POS exit:", length(unique(dt2[exit_category == 'Positive']$hh_id_kc_pha)))
      # dt2$time <- relevel(dt21$time, ref = '0')
      
    # Model ----
      mod2.formula <- paste0("percent_ami ~ ",
                             "exit*splines::bs(as.integer(time), knots = c(0)) + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod2 <- lmerTest::lmer(mod2.formula, data = setDF(copy(dt2))) # use lmerTest rather than lme4 because gives p.values but estimates and SE are identical
      mod2.tidy <- model.clean(mod2, myformat = 'percent')
      mod2.rsquared <- MuMIn::r.squaredGLMM(mod2) # psuedo-R squared
      
    # test if p-value for interaction term is < 0.05 ----
      mod2.formula.alt <- paste0("wage ~ ",
                                 "exit + time + ", 
                                 confounders, " + ",
                                 "(1 | id_kc_pha) + ", # random intercept for persons
                                 "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod2.alt <- lmerTest::lmer(mod2.formula.alt, data = setDF(copy(dt2)))
      
      mod2.test = anova(mod2, mod2.alt, test = 'LRT')
      
      if( mod2.test[["Pr(>Chisq)"]][2] < 0.05 ) {
        print("The exit:time interaction term is significant, so keep the full model with the interaction term")
        caption.text <- paste0("", mod2.formula)
      } else {
        print("The exit:time interaction term is NOT significant, so use more parsimonuous model.")
        mod2.formula <- paste0("wage ~ ",
                               "exit + time + ", 
                               confounders, " + ",
                               "(1 | id_kc_pha) + ", # random intercept for persons
                               "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
        mod2 <- lmerTest::lmer(mod2.formula, data = dt2)
        mod2.tidy <- model.clean(mod2)
        caption.text <- paste0("", mod2.formula)
      }
      
    # predictions ----
      # standard predictions ----
        mod2.preds <- as.data.table(predictions(mod2, 
                                                newdata = copy(dt2),
                                                re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
        mod2.preds <- prediction_summary(mod2.preds, ndraw = 10000)
      
      # calculate counterfactual (ascribe change observed in negative exits to positive exits) ----
        mod2.preds <- calc.counterfactual(mod2.preds)
        setnames(mod2.preds, "wage", "percent_ami") # formally change column name to prevent confusion below
        # mod2.preds[]
        
    # Plot data four quarters before and after exit ----
      plot2 <- ggplot() +
        # commented out errorbars because don't trust standard error calculation from prediction_summary()
        geom_line(data = mod2.preds[exit_category != "Counterfactual"], aes(x = time, y = percent_ami, color = exit_category), size = 1) +
        geom_line(data = mod2.preds[exit_category == "Counterfactual"], aes(x = time, y = percent_ami, color = exit_category), linetype="dashed", size = 1) +
        geom_point(data = mod2.preds[exit_category != "Counterfactual"], 
                   aes(x = time, y = percent_ami, color = exit_category), 
                   size = 2.5) +
        labs(
          x = "", 
          y = "Predicted percent AMI") 
      
      plot2 <- formatplots(plot2) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) +
        scale_y_continuous(labels=scales::label_percent(scale = 1)) +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      # dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(plot2)
      
    # Plot observed vs expected to assess model quality and problems ----
      OvE.mod2 <- copy(dt2)[, fitted := fitted(mod2)]
      OvE.mod2 <- ggplot(data = OvE.mod2, aes(x = percent_ami, y = fitted, color = exit_category)) + 
        geom_point() +
        labs(
          x = "Observed percent AMI",
          y = "Predicted percent AMI") 
      OvE.mod2 <- formatplots(OvE.mod2) +
        scale_x_continuous(labels=scales::label_percent(scale = 1)) + 
        scale_y_continuous(labels=scales::label_percent(scale = 1)) +
        geom_abline(intercept = 0 , slope = 1) + 
        xlim(0,200)+ylim(0,200)
      
      plot(OvE.mod2)
      
    # Plot of residuals vs time to assess autocorrelation ----
      mod2.resid <- copy(dt2)[, fitted := fitted(mod2)][time %in% -4:4]
      mod2.resid[, residual := percent_ami - fitted]
      mod2.resid[, time := as.numeric(as.character(time))]
      mod2.resid[exit_category == "Negative", time := time - .05]
      mod2.resid[exit_category == "Positive", time := time + .05]
      set.seed(98104) # because jitter is 'random'
      
      plot.resid.mod2 <- ggplot() +
        geom_point(data = mod2.resid[exit_category != "Counterfactual"], 
                   aes(x = time, y = residual, color = exit_category), 
                   size = 2.5, 
                   position=position_jitterdodge(dodge.width=0.65, jitter.height=0, jitter.width=0.15), alpha=0.7) +
        labs(
          # title = paste0("Quarterly wage history by exit type and time"), 
          # subtitle = "Model 5: residuals", 
          # caption = caption.text, 
          x = "", 
          y = "Percent AMI residuals") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      plot.resid.mod2 <- formatplots(plot.resid.mod2) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Negative' = '#2ca25f')) +
        scale_y_continuous(labels=scales::label_percent(scale = 1))
      
      message("No pattern with residuals, so evidence of autocorrelation, and no need for including lag dependent variables")
      plot.resid.mod2
      
    # Tidy predictions for saving ----
      roundvars <- c("percent_ami", "lower", "upper", "se")
      mod2.preds[, (roundvars) := rads::round2(.SD, 0), .SDcols = roundvars]
      mod2.obs = dt2[, .(Observed = rads::round2(mean(percent_ami))), 
                     .(time, exit_category)]
      mod2.preds <- merge(mod2.preds, mod2.obs, by = c("time", "exit_category"), all.x = TRUE, all.y = TRUE)
      mod2.preds <- mod2.preds[, .(Quarter = rads::round2(time, 0), 
                                   `Exit Type` = exit_category, 
                                   `Predicted percent AMI (95% CI)` = paste0(
                                     prettyNum(percent_ami, big.mark = ','),
                                     "% (", 
                                     prettyNum(lower, big.mark = ','), 
                                     "%, ", 
                                     prettyNum(upper, big.mark = ','), 
                                     "%)"), 
                                   Predicted = paste0(prettyNum(percent_ami, big.mark = ','), '%'),
                                   Observed = paste0(prettyNum(Observed, big.mark = ','), '%')
      )]
      setorder(mod2.preds, `Exit Type`, Quarter)
      
    # Save plots ----
      saveplots(plot.object = plot2, plot.name = 'figure_3_pre_post_by_qtr')      
      saveplots(plot.object = plot.resid.mod2, plot.name = 'appendix_figure_5_residuals')      
      saveplots(plot.object = OvE.mod2, plot.name = 'appendix_figure_6_trends_Obs_v_Exp')      
      
      
# Save regression results ----
      # Write Tables 1 & 2 using openxlsx----
      wb <- createWorkbook() # initiate a new / empty workbook
      
      addWorksheet(wb, 'appendix_Table_3_est') 
      writeDataTable(wb, sheet = 'appendix_Table_3_est', mod1.tidy, 
                     rowNames = F, colNames = T)    
      
      addWorksheet(wb, 'appendix_Table_4_preds')
      writeDataTable(wb, sheet = 'appendix_Table_4_preds', mod1.preds, 
                     rowNames = F, colNames = T)   
      
      addWorksheet(wb, 'appendix_Table_5_est') 
      writeDataTable(wb, sheet = 'appendix_Table_5_est', mod2.tidy, 
                     rowNames = F, colNames = T)    
      
      addWorksheet(wb, 'appendix_Table_6_preds')
      writeDataTable(wb, sheet = 'appendix_Table_6_preds', mod2.preds, 
                     rowNames = F, colNames = T)   
      
      saveWorkbook(wb, file = paste0(outputdir, "Tables_regresssion.xlsx"), 
                   overwrite = T)
      

# The end ----
