# Header ----
# Author: Danny Colombara
# Date: May 20, 2022
# R verson 4.1.2
# Purpose: Two purposes:
#             Create demographic table for wage analysis (Table 1)
#             Identify potential confounders associated with exposure (exit type) and outomce (wages)
# Notes: Experiment using arsenal to make the Table 1 descriptive analysis
#

# Set up ----
    rm(list=ls())
    options(scipen = 999)
    pacman::p_load(lubridate, rads, data.table, DBI, odbc, arsenal)
    
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
    
# Preparatory data manipulation ----
    raw[, quarter := as.factor(quarter(exit_date))]
    raw[, exit_year := as.factor(exit_year)]
    
# Table 1: Descriptive statistics by Exit Type ----
  # configure arsenal::tableby ----
    my_controls <- tableby.control(
      test = T,
      total = T,
      numeric.test = "kwt", cat.test = "chisq",
      numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
      cat.stats = c("countpct", "Nmiss2"),
      stats.labels = list(
        meansd = "Mean (SD)",
        medianq1q3 = "Median (Q1, Q3)",
        range = "Min - Max",
        Nmiss2 = "Missing"
      )
    )
    
  # create table ----
    table1 <- as.data.table(summary(
      arsenal::tableby(exit_category ~ 
                         wage + 
                         wage_hourly +
                         hrs +
                         percent_ami +
                         race_eth_me +
                         gender_me +
                         age_at_exit +
                         hh_disability +
                         single_caregiver +
                         housing_time_at_exit +
                         agency +
                         major_prog +
                         quarter +
                         exit_year, 
                       data = raw[qtr == 0], 
                       control = my_controls)
      ))
    
  # save vector of vars associated with exposure (exit type) ----
    setnames(table1, "", "col1")
    table1[, pnumeric := as.numeric(gsub("<", "", `p value`))]
    exposure.associated <- gsub("\\*\\*", "", table1[pnumeric < 0.05]$col1)
    table1[, pnumeric := NULL]
    
  # Tidy table ----
    table1[, col1 := gsub("wage", "Wages", col1)]
    table1[, col1 := gsub("Wages_hourly", "Wages hourly", col1)]
    table1[, col1 := gsub("hrs", "Hours", col1)]
    table1[, col1 := gsub("percent_ami", "Percent AMI", col1)]
    table1[, col1 := gsub("race_eth_me", "Race/ethnicity", col1)]
    table1[, col1 := gsub("gender_me", "Gender", col1)]
    table1[, col1 := gsub("age_at_exit", "Age", col1)]
    table1[, col1 := gsub("hh_disability", "Household with disability", col1)]
    table1[, col1 := gsub("single_caregiver", "Single caregiver", col1)]
    table1[, col1 := gsub("housing_time_at_exit", "Years in public housing", col1)]
    table1[, col1 := gsub("agency", "Agency", col1)]
    table1[, col1 := gsub("major_prog", "Major program", col1)]
    table1[, col1 := gsub("quarter", "Quarter", col1)]
    table1[, col1 := gsub("exit_year", "Exit Year", col1)]
    
    table1[, col1 := gsub("&nbsp;|\\*\\*", "", col1)]
    table1[ !is.na(`p value`) & `p value` != "", variable := col1]
    table1[, variable := variable[1], by= .(cumsum(!is.na(variable)) ) ] # fill downward
    setnames(table1, "col1", "")
    
    # setcolorder(table1, "variable")

# Write Table 1 ----
    openxlsx::write.xlsx(table1, file = paste0(outputdir, "Table1.xlsx"), asTable = T, overwrite = T)
    
# Identify confounders (associated with exposure and outcome) ----
    # remove wage / ami since they are the outcomes of interest ----
      exposure.associated <- setdiff(exposure.associated, c('wage', 'percent_ami'))
      
    # split categorical from continuous ----
      # identify definite categorical vars ----
        categorical <- exposure.associated[sapply(raw[, ..exposure.associated], class) %in% c("character", "factor", "logical")]
        exposure.associated <- setdiff(exposure.associated, categorical) # remove definite categorical from pool
      
      # partition remaining vars into categorical and continutous ----
        continuous <- c()
        for(maybecat in exposure.associated){
          tempclass <- class(raw[[maybecat]])
          tempvals <- length(unique(raw[[maybecat]]))
          if(tempclass == 'integer' & tempvals < 6){ # assume if less than 6 unique integer values, it is actually a factor/nominal var
            categorical <- c(categorical, maybecat)
          } else {
            continuous <- c(continuous, maybecat)
          }
        }
      
    # test association of categorical vars with the continuous outcome (wages) ----
      # use Kruskal-Wallace test because does not assume normality as would one-way Anova
      # null hypothesis is that the mean *ranks* (not the means) for the groups are the same
      outcome.associated <- c() # empty vector to add on vars associated with the exposure (wages)
      if(length(categorical) > 0){
        for(mycat in categorical){
          temp.kw <- kruskal.test(wage ~ get(mycat), 
                                  data = raw[qtr==0])
          if(temp.kw$p.value < 0.05){
            outcome.associated <- c(outcome.associated, mycat)
          }
        }
      }

    # test association of continuous vars with the continuous outcome (wages) ----
      # use Spearman's rank correlation because robust to non-normality (which is what we have here)
      if(length(continuous) > 0){
        for(mycon in continuous){
          temp.spearman <- suppressWarnings(
            cor.test( ~ wage + get(mycon), 
                      data = raw[qtr==0], 
                      method = 'spearman', 
                      continuity = FALSE, 
                      conf.level = 0.95) 
            )
          if(temp.spearman$p.value < 0.05){
            outcome.associated <- c(outcome.associated, mycon)
          }
        }
      }
      
# Create final list of confounders (associated with exposure and outcome) ----
    # since we only checked for association with outcome among those associated with the exposure
    # the final list of those associated with the outcome is the list of confounders
      confounders <- copy(outcome.associated)
        
# Save confounders as an object for use in modeling ----
    # neet to have an object (e.g., a table or list) rather than a value for saving
      confounders <- data.table(confounders = confounders)
      save(confounders, file = paste0(outputdir, "confounders.Rdata"))
        
# The end ----