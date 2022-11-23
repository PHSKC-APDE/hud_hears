# Header ----
# Author: Danny Colombara
# Date: November 22, 2022
# R version 4.2.1
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

    wb <- createWorkbook() # initiate a new / empty workbook
    
    # output folder
    outputdir <- "C:/Users/dcolombara/King County/DPH Health And Housing - Documents/HUD HEARS Study/wage_analysis/output/publication/"
    
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
        ggsave(paste0(outputdir, '/', plot.name, ".pdf"),
               plot = plot.object, 
               dpi=600, 
               width = 11, 
               height = 8.5, 
               units = "in") 
        ggsave(paste0(outputdir, '/', plot.name, ".png"),
               plot = plot.object, 
               dpi=600, 
               width = 11, 
               height = 8.5, 
               units = "in") 
      }
       
    # calculate mean values of predictions for my dataset using random draws ----
      prediction_summary <- function(predDT, ndraw = 1000){
        myest <- suppressWarnings(copy(predDT)[, c("lower", "upper") := NULL])
        
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
        if("Program" %in% names(mymod.preds)){mymod.preds[, Program := NULL]}
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
        if(!is.na(myformat) && myformat == 'dollar'){
          mymod.tidy <- mymod.tidy[, .(effect, 
                                       group, 
                                       term, 
                                       estimate = paste0("$",
                                         prettyNum(estimate, big.mark = ','),
                                         " ($", 
                                         prettyNum(conf.low, big.mark = ','), 
                                         ", $", 
                                         prettyNum(conf.high, big.mark = ','), 
                                         ")") , 
                                       'p-value' = p.value
                                       # , SE = std.error
                                       )]
        }
        if(!is.na(myformat) && myformat == 'percent'){
          mymod.tidy <- mymod.tidy[, .(effect, 
                                       group, 
                                       term, 
                                       estimate = paste0(prettyNum(estimate, big.mark = ','),
                                                         "% (", 
                                                         prettyNum(conf.low, big.mark = ','), 
                                                         "%, ", 
                                                         prettyNum(conf.high, big.mark = ','), 
                                                         "%)") , 
                                       'p-value' = p.value)]
        }        
        mymod.tidy[term == 'exit', term := "Positive exit"]
        mymod.tidy[term %like% 'AI/AN', term := "American Indian / Alaska Native"]
        mymod.tidy[term %like% 'Asian', term := "Asian"]
        mymod.tidy[term %like% 'Latino', term := "Hispanic"]
        mymod.tidy[term %like% 'Multiple', term := "Multiple"]
        mymod.tidy[term %like% 'NH/PI', term := "Native Hawaiian / Pacific Islander"]
        mymod.tidy[term %like% 'White', term := "White"]
        mymod.tidy[, term := gsub("as.integer\\(time\\)", "time", term)]
        mymod.tidy[, term := gsub("exit_year", "Exit year: ", term)]
        mymod.tidy[term == 'single_caregiverTRUE', term := "Single caregiver household"]
        mymod.tidy[term == 'housing_time_at_exit', term := "Years in public housing"]
        mymod.tidy[term == 'hh_disabilityTRUE', term := "Head of Household disabled"]
        mymod.tidy[, term := gsub("splines::bs", "spline", term)]
        mymod.tidy[, estimate := gsub("\\$-", "-\\$", estimate)]
        mymod.tidy[, estimate := gsub(" \\(\\$NA\\, \\$NA\\)", "", estimate)]
        mymod.tidy[, estimate := gsub("\\$NaN", NA, estimate)]
        
        # add referent for Exit year
        split.row <- which(mymod.tidy[,3] == "Exit year: 2017")
        mymod.tidy <- rbind(
          mymod.tidy[1:(split.row-1)], 
          data.table(effect = 'fixed', term = "Exit year: 2016", estimate = "Referent", `p-value` = NA_character_), 
          mymod.tidy[(split.row):nrow(mymod.tidy)], 
          fill = TRUE
        )
        
        # add referent for program
        split.row <- which(mymod.tidy[,3] == "prog_type_usePBV")
        if(length(split.row) != 0){
          mymod.tidy <- rbind(
            mymod.tidy[1:(split.row-1)], 
            data.table(effect = 'fixed', term = "Program: TBV", estimate = "Referent", `p-value` = NA_character_), 
            mymod.tidy[(split.row):nrow(mymod.tidy)], 
            fill = TRUE
          )
          mymod.tidy[, term := gsub("prog_type_use", "Program: ", term)]
        }
        
        setnames(mymod.tidy, "estimate", "Estimate (95% CI)")
      }

# Load data ----
    # open connection 
    hhsaw16 = create_db_connection( # this is prod
      server = "hhsaw", 
      prod = T, 
      interactive = F
    )
    
    # pull from SQL
    raw <- setDT(DBI::dbGetQuery(conn = hhsaw16, "SELECT * FROM [hudhears].[wage_analytic_table]"))
    
    # get covariates identified in step 3
    load(paste0(outputdir, "pscovariates.Rdata"))
    pscovariates = pscovariates$pscovariates
    
    load(paste0(outputdir, "confounders.Rdata"))
    confounders = confounders$confounders
    
# Preparatory data manipulation ----
    raw[, exit_year := factor(exit_year)]
    raw[, exit := as.integer(exit)]
    raw[, exit_category := factor(exit_category, levels = c("Positive", "Negative"))]
    raw[, quarter := as.factor(quarter(exit_date))]
    raw[, season := factor(quarter(exit_date), levels = 1:4, labels = c("Winter", "Spring", "Summer", "Fall"))]

    raw <- raw[qtr %in% -4:4] # limit to quarters of interest
    
    # set reference for factors
    raw[, race_eth_me := factor(race_eth_me)]
    raw$race_eth_me <- relevel(raw$race_eth_me, ref = 'Black')
    raw[, prog_type_use := factor(prog_type_use, 
                                  levels = c("All Programs", "TBV", "PBV", "PH"), 
                                  labels = c("All Programs", "Tenant-Based Vouchers", "Project-Based Vouchers", "Public Housing"))]
    raw <- raw[!is.na(prog_type_use)]

# Model 1: Model for all available quarterly wage data ----
    # Singular regression and then predict for each program type ----
      # get confounder vars ----
          myconfounders <- confounders

      # Create dt1 for complete quarterly analysis ----
          dt1 <- copy(raw)[qtr %in% -4:4]
          dt1[qtr %in% -4:4, time := qtr]
          dt1[, .N, .(exit, time)] # check to see how much data there is
          
      # model ----
          mod1.formula <- paste0("wage ~ ",
                                 "exit*splines::bs(as.integer(time), knots = c(0)) + ", 
                                 myconfounders, " + ",
                                 "(1 | id_kc_pha) + ", # random intercept for persons
                                 "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
          mod1 <- lmerTest::lmer(mod1.formula, data = setDF(copy(dt1))) # use lmerTest rather than lme4 because gives p.values but estimates and SE are identical
          mod1.tidy <- model.clean(mod1, myformat = 'dollar')
          addWorksheet(wb, paste0('wage_model')) 
          writeDataTable(wb, sheet = paste0('wage_model'), mod1.tidy, rowNames = F, colNames = T)  
          
          mod1.rsquared <- MuMIn::r.squaredGLMM(mod1) # psuedo-R squared    
          
      # test if p-value for interaction term is < 0.05 ----
          mod1.formula.alt <- paste0("wage ~ ",
                                     "exit + time + ", 
                                     myconfounders, " + ",
                                     "(1 | id_kc_pha) + ", # random intercept for persons
                                     "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
          mod1.alt <- lmerTest::lmer(mod1.formula.alt, data = setDF(copy(dt1)))
          
          mod1.test = anova(mod1, mod1.alt, test = 'LRT') # compare the two models
          
          if( mod1.test[["Pr(>Chisq)"]][2] < 0.05 ) {
            message("The exit:time interaction term is significant, so keep the full model with the interaction term")
            caption.text <- paste0("", mod1.formula)
          } else {
            message("The exit:time interaction term is NOT significant, so use more parsimonuous model.")
            mod1.formula <- paste0("wage ~ ",
                                   "exit + time + ", 
                                   myconfounders, " + ",
                                   "(1 | id_kc_pha) + ", # random intercept for persons
                                   "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
            mod1 <- lmerTest::lmer(mod1.formula, data = dt1)
            mod1.tidy <- model.clean(mod1)
            caption.text <- paste0("", mod1.formula)
          }
          
      # predictions ----
        # standard predictions ----
          mod1.preds <- as.data.table(predictions(mod1, 
                                                  newdata = setDF(copy(dt1)),
                                                  re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
          mod1.preds.summary <- data.table()
          for(prog in c("All Programs", "Tenant-Based Vouchers", "Project-Based Vouchers", "Public Housing")){
            if(prog == "All Programs"){
              temp.preds <- prediction_summary(mod1.preds, ndraw = 10000)
            } else {
              temp.preds <- prediction_summary(mod1.preds[prog_type_use == prog], ndraw = 10000)
            }
            temp.preds <- cbind(Program = prog, temp.preds)
            mod1.preds.summary <- rbind(mod1.preds.summary, temp.preds)
          }

        # calculate counterfactual (ascribe change observed in negative exits to positive exits) ----
          mod1.preds <- data.table() # wipe original table but keep it as an object to build on
          for(prog in c("All Programs", "Tenant-Based Vouchers", "Project-Based Vouchers", "Public Housing")){
            temp.counter <- calc.counterfactual(mod1.preds.summary[Program == prog])
            temp.counter <- cbind(Program = prog, temp.counter)
            mod1.preds <- rbind(mod1.preds, temp.counter)
          }

      # Add onto to summary table of predictions
          predictions.model1 <- copy(mod1.preds)
      
      # Clean copy of summary model predictions to export ----
          mod1.tidy.preds <- copy(mod1.preds)[exit_category != "Counterfactual"]
          roundvars <- c("wage", "lower", "upper", "se")
          mod1.tidy.preds[, (roundvars) := rads::round2(.SD, 0), .SDcols = roundvars]
          mod1.obs = dt1[, .(Observed = rads::round2(mean(wage))), .(time, exit_category)]
          mod1.tidy.preds <- merge(mod1.tidy.preds, mod1.obs, by = c("time", "exit_category"), all.x = TRUE, all.y = TRUE)
          mod1.tidy.preds <- mod1.tidy.preds[, .(Program, 
                                                 Quarter = rads::round2(time, 0), 
                                                 `Exit Type` = exit_category, 
                                                 Predicted = paste0("$", prettyNum(wage, big.mark = ',')),
                                                 Observed = paste0("$", prettyNum(Observed, big.mark = ',')) )]
          mod1.tidy.preds[Observed == "$NA", Observed := NA]
          setorder(mod1.tidy.preds, `Exit Type`, Quarter)
          addWorksheet(wb, paste0('wage_predictions')) 
          writeDataTable(wb, sheet = paste0('wage_predictions'), mod1.tidy.preds, rowNames = F, colNames = T)  

    
    # Export regression output ----
      # saveWorkbook(wb, file = paste0(outputdir, "Tables_regression.xlsx"), overwrite = T) # skip b/c write after model 2
    

    # Plot data four quarters before and after exit ----
      mod1.preds <- copy(predictions.model1)
      mod1.preds[, program := factor(Program, 
                                           levels = c("All Programs", "Tenant-Based Vouchers", "Project-Based Vouchers", "Public Housing"), 
                                           labels = c("All Programs", "Tenant-Based Vouchers", "Project-Based Vouchers", "Public Housing"))]
      plot1 <- ggplot() +
        geom_line(data = mod1.preds[exit_category != "Counterfactual"], aes(x = time, y = wage, color = exit_category), size = 1) +
        geom_line(data = mod1.preds[exit_category == "Counterfactual"], aes(x = time, y = wage, color = exit_category), linetype="dashed", size = 1) +
        geom_point(data = mod1.preds[exit_category != "Counterfactual"], 
                   aes(x = time, y = wage, color = exit_category), 
                   size = 2.5) +
        labs(
          x = "", 
          y = "Predicted quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      plot1 <- formatplots(plot1) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) +
        facet_wrap(~program, nrow = 2, strip.position = "top") 
      
      plot(plot1)
      
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
          x = "", 
          y = "Wage residuals") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      plot.resid.mod1 <- formatplots(plot.resid.mod1) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Negative' = '#2ca25f')) 
      message("No pattern with residuals, so evidence of autocorrelation, and no need for including lag dependent variables")
      plot(plot.resid.mod1)
      
      
    # Save plots ----
      saveplots(plot.object = plot1, plot.name = 'figure_2_predicted_wages_by_qtr')      
      saveplots(plot.object = OvE.mod1, plot.name = 'appendix_figure_3_trends_Obs_v_Exp')
      saveplots(plot.object = plot.resid.mod1, plot.name = 'appendix_figure_4_pre_post_trends_residuals')  
      
# Model 2: Model for all available quarterly % AMI data ----
    # Singular regression and then predict for each program type ----
      # get confounder vars ----
          myconfounders <- confounders

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

      # model ----
          mod2.formula <- paste0("percent_ami ~ ",
                                 "exit*splines::bs(as.integer(time), knots = c(0)) + ", 
                                 myconfounders, " + ",
                                 "(1 | id_kc_pha) + ", # random intercept for persons
                                 "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
          mod2 <- lmerTest::lmer(mod2.formula, data = setDF(copy(dt2))) # use lmerTest rather than lme4 because gives p.values but estimates and SE are identical
          mod2.tidy <- model.clean(mod2, myformat = 'dollar')
          addWorksheet(wb, paste0('ami_model')) 
          writeDataTable(wb, sheet = paste0('ami_model'), mod2.tidy, rowNames = F, colNames = T)  
          
          mod2.rsquared <- MuMIn::r.squaredGLMM(mod2) # psuedo-R squared    
          
      # test if p-value for interaction term is < 0.05 ----
          mod2.formula.alt <- paste0("percent_ami ~ ",
                                     "exit + time + ", 
                                     myconfounders, " + ",
                                     "(1 | id_kc_pha) + ", # random intercept for persons
                                     "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
          mod2.alt <- lmerTest::lmer(mod2.formula.alt, data = setDF(copy(dt2)))
          
          mod2.test = anova(mod2, mod2.alt, test = 'LRT') # compare the two models
          
          if( mod2.test[["Pr(>Chisq)"]][2] < 0.05 ) {
            message("The exit:time interaction term is significant, so keep the full model with the interaction term")
            caption.text <- paste0("", mod2.formula)
          } else {
            message("The exit:time interaction term is NOT significant, so use more parsimonuous model.")
            mod2.formula <- paste0("percent_ami ~ ",
                                   "exit + time + ", 
                                   myconfounders, " + ",
                                   "(1 | id_kc_pha) + ", # random intercept for persons
                                   "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
            mod2 <- lmerTest::lmer(mod2.formula, data = dt2)
            mod2.tidy <- model.clean(mod2)
            caption.text <- paste0("", mod2.formula)
          }
          
      # predictions ----
        # standard predictions ----
          mod2.preds <- as.data.table(predictions(mod2, 
                                                  newdata = setDF(copy(dt2)),
                                                  re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
          mod2.preds.summary <- data.table()
          for(prog in c("All Programs", "Tenant-Based Vouchers", "Project-Based Vouchers", "Public Housing")){
            if(prog == "All Programs"){
              temp.preds <- prediction_summary(mod2.preds, ndraw = 10000)
            } else {
              temp.preds <- prediction_summary(mod2.preds[prog_type_use == prog], ndraw = 10000)
            }
            temp.preds <- cbind(Program = prog, temp.preds)
            mod2.preds.summary <- rbind(mod2.preds.summary, temp.preds)
          }

        # calculate counterfactual (ascribe change observed in negative exits to positive exits) ----
          mod2.preds <- data.table() # wipe original table but keep it as an object to build on
          for(prog in c("All Programs", "Tenant-Based Vouchers", "Project-Based Vouchers", "Public Housing")){
            temp.counter <- calc.counterfactual(mod2.preds.summary[Program == prog])
            temp.counter <- cbind(Program = prog, temp.counter)
            mod2.preds <- rbind(mod2.preds, temp.counter)
          }
          setnames(mod2.preds, "wage", "percent_ami") # formally change column name to prevent confusion below

      # Add onto to summary table of predictions
          predictions.model2 <- copy(mod2.preds)
      
      # Clean copy of summary model predictions to export ----
          roundvars <- c("percent_ami", "lower", "upper", "se")
          mod2.preds[, (roundvars) := rads::round2(.SD, 0), .SDcols = roundvars]

          mod2.tidy.preds <- copy(mod2.preds)[exit_category != "Counterfactual"]
          
          mod2.obs = dt2[, .(Observed = rads::round2(mean(percent_ami))), .(time, exit_category)]
          
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

          addWorksheet(wb, paste0('ami_predictions')) 
          writeDataTable(wb, sheet = paste0('ami_predictions'), mod1.tidy.preds, rowNames = F, colNames = T)  
          
    # Export regression output ----
    saveWorkbook(wb, file = paste0(outputdir, "Tables_regression.xlsx"), overwrite = T)
    

    # Plot data four quarters before and after exit ----
      mod2.preds <- copy(predictions.model2)
      mod2.preds[, program := factor(Program, 
                                           levels = c("All Programs", "Tenant-Based Vouchers", "Project-Based Vouchers", "Public Housing"), 
                                           labels = c("All Programs", "Tenant-Based Vouchers", "Project-Based Vouchers", "Public Housing"))]
      plot2 <- ggplot() +
        geom_line(data = mod2.preds[exit_category != "Counterfactual"], aes(x = time, y = percent_ami, color = exit_category), size = 1) +
        geom_line(data = mod2.preds[exit_category == "Counterfactual"], aes(x = time, y = percent_ami, color = exit_category), linetype="dashed", size = 1) +
        geom_point(data = mod2.preds[exit_category != "Counterfactual"], 
                   aes(x = time, y = percent_ami, color = exit_category), 
                   size = 2.5) +
        labs(
          x = "", 
          y = "Predicted quarterly percentage AMI") 
      
      plot2 <- formatplots(plot2) + 
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 
                                    'Counterfactual' = '#e41a1c', 
                                    'Negative' = '#2ca25f')) +
        scale_y_continuous(labels=scales::label_percent(scale = 1)) +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4)) +
        facet_wrap(~program, nrow = 2, strip.position = "top") 
      
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

    # Save plots ----
      saveplots(plot.object = plot2, plot.name = 'figure_3_predicted_percent_ami_by_qtr')      
      saveplots(plot.object = plot.resid.mod2, plot.name = 'appendix_figure_5_residuals')      
      saveplots(plot.object = OvE.mod2, plot.name = 'appendix_figure_6_trends_Obs_v_Exp')  


# The end ----
