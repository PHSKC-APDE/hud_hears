# Header ----
# Author: Danny Colombara
# Date: May 23, 2022
# R verson 4.1.2
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
#

# Set up ----
    rm(list=ls())
    options(scipen = 999)
    pacman::p_load(lubridate, rads, data.table, DBI, odbc, ggplot2, lme4, margins)
    # library(lmerTest)  # commented out because want to be sure lmer function is called from lme4 by default

    # output folder
    outputdir <- "C:/Users/dcolombara/King County/DPH Health And Housing - Documents/HUD HEARS Study/wage_analysis/output/"
    
    # easy SQL connections
    devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/apde/main/R/create_db_connection.R") 
    
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
    raw[, quarter := as.factor(quarter(exit_date))]

# STEP 1: Assess parallel trend assumption from quarter -4 to quarter 0 (exit)
    step1 <- copy(raw)
    
    # create binaries for DiD ----
    step1[qtr == -4, time := -1]
    step1[qtr == -1, time := 0]
    step1[exit_category == "Negative", exit := 0]
    step1[exit_category == "Positive", exit := 1]
    
    # subset to quarters of interest ----
    step1 <- step1[!is.na(time)]
    
# Model 1: assess parallel trends prior to exit ----
    # model ----
      mod1.formula <- paste0("wage ~ 
                             exit*time + ", 
                             confounders, " + ",
                             "(1 | id_kc_pha) + ", # random intercept for persons
                             "(1 + exit | hh_id_kc_pha)") # random intercept and slope for households
      mod1 <- lme4::lmer(mod1.formula, data = step1)
      mod1.tidy <- as.data.table(broom.mixed::tidy(mod1, conf.int = T))

    # test if p-value for interaction term is < 0.05 ----
      mod1.test <- suppressWarnings(lmerTest::lmer(mod1.formula, data = step1, REML = FALSE))
      mod1.test <- as.data.table(coef(summary(mod1.test)), keep.rownames = T)    
      if( mod1.test[rn == "exit:time"][["Pr(>|t|)"]] < 0.05) {
        print(mod1.test)
        stop("\nThe exit:time interaction term is significant, so fails test of parallel trend prior to exit")
      }
        
    # average marginal effects (just for curiosity) ----
      mod1.margin.summary <- as.data.table(summary(margins::margins(mod1))) 
      mod1.margin.summary[]
    
    # create table of predictions for slopes in ggplot ... use setDF b/c can't use data.table ----
      mod1.preds <- as.data.table(summary(margins::prediction(mod1, data = setDF(copy(step1)), at = list(time=c(-1, 0), exit = c(0, 1)))))
      mod1.preds[`at(exit)` == 0, exit_category := "Negative"][`at(exit)` == 1, exit_category := "Positive"]
      mod1.preds <- mod1.preds[, .(time = `at(time)`, exit_category, wage = Prediction)]
      mod1.preds[]
      
    # Plot data prior to exit ----
      step1.plot = copy(step1)
      step1.plot[exit == 0, time := time - .015] # add tiny shift for easier visualization
      step1.plot[exit == 1, time := time + .015] # add tiny shift for easier visualization
      plot1 <- ggplot() +
        geom_point(data = step1.plot, aes(x = time, y = wage, color = exit_category)) + 
        geom_line(data = mod1.preds, aes(x = time, y = wage, color = exit_category), size = 1) +
        labs(title = paste0("Trends 1 year prior to exit"), 
             subtitle = "Positive exits had greater mean wage increases", 
             x = "Year", 
             y = "Quarterly wages") +
        scale_x_continuous(breaks = c(-1, 0)) +
        scale_color_discrete("Exit type") +
        theme(panel.grid.major = element_line(color = "white"), 
              panel.background = element_rect(fill = "white"), 
              panel.border = element_rect(colour = "black", fill=NA, size=.5),  
              plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(size=10),
              legend.position = "right",
              legend.background = element_rect(fill="white", size=0.5, linetype="solid", color ="white"), 
              legend.title = element_text(size = 12), 
              legend.key = element_rect(fill = "white", color = "white"),
              legend.text = element_text(size = 10))

      plot(plot1)
      
    # Save plot ----
      ggsave(paste0(outputdir, "figure_1_prior_trends.pdf"),
             plot = plot1, 
             dpi=600, 
             width = 6.5, 
             height = 6.5, 
             units = "in") 
      ggsave(paste0(outputdir, "figure_1_prior_trends.png"),
             plot = plot1, 
             dpi=600, 
             width = 6.5, 
             height = 6.5, 
             units = "in") 
# The end ----