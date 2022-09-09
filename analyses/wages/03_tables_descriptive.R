# Header ----
# Author: Danny Colombara
# Date: May 20, 2022
# R verson 4.1.2
# Purpose: Two purposes:
#             Create demographic table for wage analysis (Table 1)
#             Identify potential confounders associated with exposure (exit type) and outcome (wages)
# Notes: Experiment using arsenal to make the Table 1 descriptive analysis
#

# Set up ----
    rm(list=ls())
    options(scipen = 999)
    pacman::p_load(lubridate, rads, data.table, DBI, odbc, arsenal, openxlsx)
    
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
    raw[, season := factor(quarter(exit_date), levels = 1:4, labels = c("Winter", "Spring", "Summer", "Fall"))]
    raw[, exit_year := as.factor(exit_year)]

# Table 0: ID counts ----
    table0_id_counts = rbind(
      data.table(category = "Persons", 
                 Total = uniqueN(raw$id_kc_pha), 
                 positive = uniqueN(raw[exit_category == "Positive"]$id_kc_pha),
                 negative = uniqueN(raw[exit_category == "Negative"]$id_kc_pha),
                 KCHA = uniqueN(raw[agency == "KCHA"]$id_kc_pha), 
                 SHA = uniqueN(raw[agency == "SHA"]$id_kc_pha)), 
      data.table(category = "Households", 
                 Total = uniqueN(raw$hh_id_kc_pha), 
                 positive = uniqueN(raw[exit_category == "Positive"]$hh_id_kc_pha),
                 negative = uniqueN(raw[exit_category == "Negative"]$hh_id_kc_pha),
                 KCHA = uniqueN(raw[agency == "KCHA"]$hh_id_kc_pha), 
                 SHA = uniqueN(raw[agency == "SHA"]$hh_id_kc_pha)), 
      fill = T)

# Table 1: Quarterly age differences Pre/Exit/Post ----
    # Data prep ----
      dt1 <- copy(raw)
      dt1[qtr == -4, time := -1]
      dt1[qtr == 0, time := 0]
      dt1[qtr == 4, time := 1]
      
    # Summary table ----
      table1_qtr_diff <- merge(dt1[!is.na(time), 
                                   .(tot.mean = mean(wage), tot.sd = sd(wage)), 
                                   time], 
                                dt1[!is.na(time) & exit_category == "Positive", 
                                    .(pos.mean = mean(wage), pos.sd = sd(wage)), 
                                    time], 
                                by = 'time', 
                                all = T)
      table1_qtr_diff <- merge(table1_qtr_diff,
                                dt1[!is.na(time) & exit_category == "Negative", 
                                    .(neg.mean = mean(wage), neg.sd = sd(wage)), 
                                    time], 
                                by = 'time', 
                                all = T)
      table1_qtr_diff[, difference := pos.mean - neg.mean]
      table1_qtr_diff[, difference.sd := sqrt((pos.sd^2) + (neg.sd^2))]
      
    # Perform t-test ----
      for(ii in c(-1, 0, 1)){
        table1_qtr_diff[time == ii, 
                       t_test_pvalue := t.test(x = dt1[!is.na(time) & exit_category == "Positive" & time == ii,]$wage,
                                               y = dt1[!is.na(time) & exit_category == "Negative" & time == ii,]$wage)$p.value]
        }
    # Tidy table ----
      table1_qtr_diff[t_test_pvalue < 0.05, significant := "*"]
      table1_qtr_diff <- table1_qtr_diff[, .(`Wage type` = 'quarterly', 
                                       `Time period` = factor(time, 
                                                              levels = c(-1, 0, 1), 
                                                              labels = c("1 year prior", "Exit", "1 year post")), 
                                       `Any exit` = paste0(prettyNum(round2(tot.mean), big.mark = ','), " (", prettyNum(round2(tot.sd), big.mark = ','), ")"), 
                                       Positive = paste0(prettyNum(round2(pos.mean), big.mark = ','), " (", prettyNum(round2(pos.sd), big.mark = ','), ")"), 
                                       Negative = paste0(prettyNum(round2(neg.mean), big.mark = ','), " (", prettyNum(round2(neg.sd), big.mark = ','), ")"),
                                       Difference = paste0(prettyNum(round2(difference), big.mark = ','), " (", prettyNum(round2(difference.sd), big.mark = ','), ")"),
                                       Significant = significant, 
                                       `p-value` = ifelse(t_test_pvalue < 0.001, "<0.001", t_test_pvalue))]    
  

# Table 2: Descriptive statistics by Exit Type ----
  # configure arsenal::tableby ----
    my_controls <- tableby.control(
      test = T,
      total = T,
      numeric.test = "kwt", cat.test = "chisq",
      # numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
      numeric.stats = c("meansd", "Nmiss2"),
      digits = 0,
      cat.stats = c("countpct", "Nmiss2"),
      stats.labels = list(
        meansd = "Mean (SD)",
        # medianq1q3 = "Median (Q1, Q3)",
        # range = "Min - Max",
        Nmiss2 = "Missing"
      )
    )
    
  # create table ----
    # temporarily multiple opportunity index by 1000 so rounding will not obscure differences
      raw[, opportunity_index1k := 1000*opportunity_index]
      
    table2 <- as.data.table(summary(
      arsenal::tableby(exit_category ~ 
                         # individual
                           age_at_exit +
                           gender_me +
                           race_eth_me +
                           race_gender +
                           wage + 
                           hrs +
                           wage_hourly +
                         # household
                           exit_year +
                           season +
                           housing_time_at_exit +
                           hh_disability +
                           single_caregiver + 
                           opportunity_index1k +
                           percent_ami +
                           agency +
                           major_prog, 
                       data = raw[qtr == 0], 
                       control = my_controls)
      ))
    
  # save vector of vars associated with exposure (exit type) ----
    setnames(table2, "", "col1")
    table2[, pnumeric := as.numeric(gsub("<", "", `p value`))]
    exposure.associated <- gsub("\\*\\*", "", table2[pnumeric < 0.05]$col1)
    table2[, pnumeric := NULL]
    
  # Tidy table ----
    table2[, col1 := gsub("wage", "Wages", col1)]
    table2[, col1 := gsub("Wages_hourly", "Wages hourly", col1)]
    table2[, col1 := gsub("hrs", "Hours", col1)]
    table2[, col1 := gsub("percent_ami", "Percent AMI", col1)]
    table2[, col1 := gsub("opportunity_index1k", "Opportunity index", col1)]
    table2[, col1 := gsub("race_eth_me", "Race/ethnicity", col1)]
    table2[, col1 := gsub("gender_me", "Gender", col1)]
    table2[, col1 := gsub("race_gender", "Race/ethnicity & Gender", col1)]
    table2[, col1 := gsub("age_at_exit", "Age", col1)]
    table2[, col1 := gsub("hh_disability", "Household with disability", col1)]
    table2[, col1 := gsub("single_caregiver", "Single caregiver", col1)]
    table2[, col1 := gsub("housing_time_at_exit", "Years in public housing", col1)]
    table2[, col1 := gsub("agency", "Agency", col1)]
    table2[, col1 := gsub("major_prog", "Major program", col1)]
    table2[, col1 := gsub("season", "Season", col1)]
    table2[, col1 := gsub("exit_year", "Exit Year", col1)]
    
    table2[, col1 := gsub("&nbsp;|\\*\\*", "", col1)]
    table2[ !is.na(`p value`) & `p value` != "", variable := col1]
    table2[, variable := variable[1], by= .(cumsum(!is.na(variable)) ) ] # fill downward
    
    # when binary true/false, collapse it down to one row
    table2 <- table2[col1 != "FALSE"]
    table2[col1 == "TRUE", col1 := variable]
    tf.vars <- table2[col1 == variable, dup := 1:.N, variable][dup == 2]$col1
    for(tf in tf.vars){
      table2[dup == 2 & col1 == tf, "p value" := table2[dup==1 & col1 == tf]$`p value`]
      table2 <- table2[!(dup == 1 & col1 == tf)]
    }

    # change opportunity index scale back to real / nominal scale
    table2[variable == 'Opportunity index' & col1 == 'Mean (SD)', `:=`
           (oi.m2 = as.integer(gsub("\\(..*", "", get(names(table2)[2])))/ 1000, 
             oi.sd2 = as.integer(gsub("\\)", "", gsub("..*\\(", "", get(names(table2)[2]))))/1000,
             oi.m3 = as.integer(gsub("\\(..*", "", get(names(table2)[3])))/1000, 
             oi.sd3 = as.integer(gsub("\\)", "", gsub("..*\\(", "", get(names(table2)[3]))))/1000,
             oi.m4 = as.integer(gsub("\\(..*", "", get(names(table2)[4])))/1000, 
             oi.sd4 = as.integer(gsub("\\)", "", gsub("..*\\(", "", get(names(table2)[4]))))/1000
           )]
    table2[variable == 'Opportunity index' & col1 == 'Mean (SD)', names(table2)[2] := paste0(oi.m2, " (", oi.sd2, ")")]
    table2[variable == 'Opportunity index' & col1 == 'Mean (SD)', names(table2)[3] := paste0(oi.m3, " (", oi.sd3, ")")]
    table2[variable == 'Opportunity index' & col1 == 'Mean (SD)', names(table2)[4] := paste0(oi.m4, " (", oi.sd4, ")")]
    
    # drop missing if all missing are zero
    table2 <- table2[!(col1 == "Missing" & get(names(table2)[2]) == 0 & get(names(table2)[3]) == 0 & get(names(table2)[4]) == 0)]
    
    table2[col1 != variable, col1 := paste0("   ", col1)]
    
    table2 <- table2[, 1:5]
    
    setnames(table2, c("col1", "p value"), c("", "P-value"))
    
    # setcolorder(table2, "variable")



# Write Tables 1 & 2 using openxlsx----
    wb <- createWorkbook() # initiate a new / empty workbook
    addWorksheet(wb, 'Table_0_counts') 
    writeDataTable(wb, sheet = 'Table_0_counts', table0_id_counts, 
                   rowNames = F, colNames = T)    
    addWorksheet(wb, 'Table_1_differences')
    writeDataTable(wb, sheet = 'Table_1_differences', table1_qtr_diff, 
                   rowNames = F, colNames = T)   
    addWorksheet(wb, 'Table_2_descriptive')
    writeDataTable(wb, sheet = 'Table_2_descriptive', table2, 
                   rowNames = F, colNames = T)  
    saveWorkbook(wb, file = paste0(outputdir, "Tables_descriptive.xlsx"), overwrite = T)

# Identify confounders (associated with exposure and outcome) ----
    # remove wage / ami since they are the outcomes of interest ----
      exposure.associated <- setdiff(exposure.associated, c('wage', 'percent_ami', "wage_hourly", "hrs"))
      pscovariates <- copy(exposure.associated) # keep a copy if want to create propensity score
      
    # split categorical from continuous ----
      # identify categorical vars & set aside ----
        categorical <- exposure.associated[sapply(raw[, ..exposure.associated], class) %in% c("character", "factor", "logical")]
        exposure.associated <- setdiff(exposure.associated, categorical) # remove definite categorical from pool
      
      # partition remaining vars into categorical and continuous ----
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

# Save confounders & pscovariates as an object for use in modeling ----
    # need to have an object (e.g., a table or list) rather than a value for saving
      confounders <- data.table(confounders = confounders)
      save(confounders, file = paste0(outputdir, "confounders.Rdata"))
      pscovariates <- data.table(pscovariates = pscovariates)
      save(pscovariates, file = paste0(outputdir, "pscovariates.Rdata"))
        
# The end ----