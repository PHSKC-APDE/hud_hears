# Header ----
# Author: Danny Colombara
# Date: October 28, 2022
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

    wb <- createWorkbook() # initiate a new / empty workbook
    
    # output folder
    outputdir <- "C:/Users/dcolombara/King County/DPH Health And Housing - Documents/HUD HEARS Study/wage_analysis/output/sha/"
    
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
        myest <- copy(predDT)[, c("lower", "upper") := NULL]
        
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
    raw <- setDT(DBI::dbGetQuery(conn = hhsaw16, 
                                 "SELECT * FROM [hudhears].[wage_analytic_table] WHERE agency = 'SHA'"))

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
    raw[, prog_type_use := factor(prog_type_use, levels = c("TBV", "PBV", "PH"))]
    raw <- raw[!is.na(prog_type_use)]

# Model 1: Model for all available quarterly wage data ----
    # Regression and prediction for each program type ----
    predictons.model1 <- data.table()
    for(ii in c("SHA", "TBV", "PBV", "PH")){
      # get confounder vars ----
          if(ii == "SHA"){confounders <- confounders <-"exit_year + hh_disability + prog_type_use + housing_time_at_exit"}
          if(ii != "SHA"){confounders <- confounders <-"exit_year + hh_disability + housing_time_at_exit"}
      
      # Create dt1 for complete quarterly analysis ----
          if(ii != "SHA"){dt1 <- copy(raw)[qtr %in% -4:4 & prog_type_use == ii]}
          if(ii == "SHA"){dt1 <- copy(raw)[qtr %in% -4:4 & prog_type_use != "All SHA"]}
          dt1[qtr %in% -4:4, time := qtr]
          dt1[, .N, .(exit, time)] # check to see how much data there is
          
      # model ----
          mod1.formula <- paste0("wage ~ ",
                                 "exit*splines::bs(as.integer(time), knots = c(0)) + ", 
                                 confounders, " + ",
                                 "(1 | id_kc_pha) + ", # random intercept for persons
                                 "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
          mod1 <- lmerTest::lmer(mod1.formula, data = setDF(copy(dt1))) # use lmerTest rather than lme4 because gives p.values but estimates and SE are identical
          mod1.tidy <- model.clean(mod1, myformat = 'dollar')
          mod1.tidy <- cbind(Program = ii, mod1.tidy)
          addWorksheet(wb, paste0('model_', ii)) 
          writeDataTable(wb, sheet = paste0('model_', ii), mod1.tidy, rowNames = F, colNames = T)  
          
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
                                                  newdata = setDF(copy(dt1)),
                                                  re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
          mod1.preds <- prediction_summary(mod1.preds, ndraw = 10000)
          
        # calculate counterfactual (ascribe change observed in negative exits to positive exits) ----
          mod1.preds <- calc.counterfactual(mod1.preds)

      # Add onto to summary table of predictions
          predictons.model1 <- rbind(predictons.model1, 
                                     cbind(data.table(program = ii), mod1.preds))
      
      # Clean copy of summary model predictions to export ----
          mod1.tidy.preds <- copy(mod1.preds)[exit_category != "Counterfactual"]
          roundvars <- c("wage", "lower", "upper", "se")
          mod1.tidy.preds[, (roundvars) := rads::round2(.SD, 0), .SDcols = roundvars]
          mod1.obs = dt1[, .(Observed = rads::round2(mean(wage))), .(time, exit_category)]
          mod1.tidy.preds <- merge(mod1.tidy.preds, mod1.obs, by = c("time", "exit_category"), all.x = TRUE, all.y = TRUE)
          mod1.tidy.preds <- mod1.tidy.preds[, .(Program = ii, 
                                                 Quarter = rads::round2(time, 0), 
                                                 `Exit Type` = exit_category, 
                                                 Predicted = paste0("$", prettyNum(wage, big.mark = ',')),
                                                 Observed = paste0("$", prettyNum(Observed, big.mark = ',')) )]
          mod1.tidy.preds[Observed == "$NA", Observed := NA]
          setorder(mod1.tidy.preds, `Exit Type`, Quarter)
          addWorksheet(wb, paste0('preds_', ii)) 
          writeDataTable(wb, sheet = paste0('preds_', ii), mod1.tidy.preds, rowNames = F, colNames = T)  
    }
    
    # Export regression output ----
    saveWorkbook(wb, file = paste0(outputdir, "Tables_regression.xlsx"), overwrite = T)
    

    # Plot data four quarters before and after exit ----
      mod1.preds <- copy(predictons.model1)
      mod1.preds[, program := factor(program, 
                                           levels = c("SHA", "TBV", "PBV", "PH"), 
                                           labels = c("All SHA", "TBV", "PBV", "PH"))]
      plot1 <- ggplot() +
        # commented out errorbars because don't trust standard error calculation from prediction_summary()
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
      
    # Save plots ----
      saveplots(plot.object = plot1, plot.name = 'figure_2_predicted_wages_by_qtr')      
    



# The end ----
