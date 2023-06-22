# Header ----
# Author: Danny Colombara
# Date: June 6, 2023
# R version 4.2.2
# Purpose: Two purposes:
#             Create demographic table for wage analysis (Table 1)
#             Identify potential confounders associated with exposure (exit type) and outcome (wages)
# Notes: Experiment using arsenal to make the Table 1 descriptive analysis
#

# Set up ----
    rm(list=ls())
    options(scipen = 999)
    pacman::p_load(lubridate, rads, data.table, DBI, odbc, arsenal, openxlsx, Microsoft365R)
    
    # output folder
    site <- get_team("DPH Health And Housing")
    drv <- site$get_drive("Documents")
    outputdir <- "HUD HEARS Study/wage_analysis/output/hpd_revision_1/"
    
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
    raw <- rbind(copy(raw), copy(raw)[, prog_type_use := "All Programs"])
    raw[, prog_type_use := factor(prog_type_use, 
                                  levels = c("All Programs", "TBV", "PBV", "PH"), 
                                  labels = c("All Programs", "Tenant-Based Vouchers", "Project-Based Vouchers", "Public Housing"))]
    raw[, season := factor(quarter(exit_date), levels = 1:4, labels = c("Winter", "Spring", "Summer", "Fall"))]
    raw[, exit_year := as.factor(exit_year)]

# Table 0: ID counts of positive negative exits by program type ----
    table0 <- data.table()
    for(tempx in sort(unique(raw$prog_type_use))){
      table0 <- rbind(
        table0,
        data.table(category = "Persons", 
                   program = tempx,
                   total = uniqueN(raw[prog_type_use == tempx]$id_kc_pha), 
                   positive = uniqueN(raw[exit_category == "Positive" & prog_type_use == tempx]$id_kc_pha),
                   negative = uniqueN(raw[exit_category == "Negative" & prog_type_use == tempx]$id_kc_pha)), 
        data.table(category = "Households", 
                   program = tempx,
                   total = uniqueN(raw[prog_type_use == tempx]$hh_id_kc_pha), 
                   positive = uniqueN(raw[exit_category == "Positive" & prog_type_use == tempx]$hh_id_kc_pha),
                   negative = uniqueN(raw[exit_category == "Negative" & prog_type_use == tempx]$hh_id_kc_pha)))
    }
    
    # write to Excel worksheets in memory
    wb <- createWorkbook() # initiate a new / empty workbook
    addWorksheet(wb, 'Table_0_counts') 
    writeDataTable(wb, sheet = 'Table_0_counts', table0, 
                   rowNames = F, colNames = T)   
    
# Table 0_lw: living wage ----
    table0_lw <- data.table()
    for(tempx in sort(unique(raw$prog_type_use))){
      table0_lw <- rbind(
        table0_lw,
        data.table(category = "Households at exit", 
                   program = tempx,
                   total = uniqueN(raw[qtr == 0 & !is.na(living_wage) & prog_type_use == tempx]$hh_id_kc_pha), 
                   living_wage = uniqueN(raw[qtr == 0 & !is.na(living_wage) & living_wage == "At or above" & prog_type_use == tempx]$hh_id_kc_pha),
                   positive = uniqueN(raw[exit_category == "Positive" & qtr == 0 & !is.na(living_wage) & prog_type_use == tempx]$hh_id_kc_pha), 
                   pos_living_wage = uniqueN(raw[exit_category == "Positive" & qtr == 0 & !is.na(living_wage) & living_wage == "At or above" & prog_type_use == tempx]$hh_id_kc_pha), 
                   negative = uniqueN(raw[exit_category == "Negative" & qtr == 0 & !is.na(living_wage) & prog_type_use == tempx]$hh_id_kc_pha), 
                   neg_living_wage = uniqueN(raw[exit_category == "Negative" & qtr == 0 & !is.na(living_wage) & living_wage == "At or above" & prog_type_use == tempx]$hh_id_kc_pha)), 
        data.table(category = "Households 1 year post exit", 
                   program = tempx,
                   total = uniqueN(raw[qtr == 4 & !is.na(living_wage) & prog_type_use == tempx]$hh_id_kc_pha), 
                   living_wage = uniqueN(raw[qtr == 4 & !is.na(living_wage) & living_wage == "At or above" & prog_type_use == tempx]$hh_id_kc_pha),
                   positive = uniqueN(raw[exit_category == "Positive" & qtr == 4 & !is.na(living_wage) & prog_type_use == tempx]$hh_id_kc_pha), 
                   pos_living_wage = uniqueN(raw[exit_category == "Positive" & qtr == 4 & !is.na(living_wage) & living_wage == "At or above" & prog_type_use == tempx]$hh_id_kc_pha), 
                   negative = uniqueN(raw[exit_category == "Negative" & qtr == 4 & !is.na(living_wage) & prog_type_use == tempx]$hh_id_kc_pha), 
                   neg_living_wage = uniqueN(raw[exit_category == "Negative" & qtr == 4 & !is.na(living_wage) & living_wage == "At or above" & prog_type_use == tempx]$hh_id_kc_pha)))
    }
    
    table0_lw[, living_wage_per := round2(100*living_wage/total, 1)]
    table0_lw[, pos_living_wage_per := round2(100*pos_living_wage/positive, 1)]
    table0_lw[, neg_living_wage_per := round2(100*neg_living_wage/negative, 1)]
    setorder(table0_lw, -category)

    # write to Excel worksheets in memory
    addWorksheet(wb, 'Table_0_livingwage')
    writeDataTable(wb, sheet = 'Table_0_livingwage', table0_lw, 
                   rowNames = F, colNames = T) 
    
# Table 0_buyhome: exit to homeownership ----
    table0_buyhome <- data.table()
    for(tempx in sort(unique(raw$prog_type_use))){
      table0_buyhome <- rbind(
        table0_buyhome,
        data.table(category = "Households at exit", 
                   program = tempx,
                   total = uniqueN(raw[qtr == 0 & prog_type_use == tempx]$hh_id_kc_pha), 
                   positive = uniqueN(raw[exit_category == "Positive" & qtr == 0 & prog_type_use == tempx]$hh_id_kc_pha), 
                   homeownership = uniqueN(raw[qtr == 0 & exit_reason_clean == 'Homeownership' & prog_type_use == tempx]$hh_id_kc_pha)))
    }    
    table0_buyhome[, percent_of_total := round2(100*homeownership/total, 1)]
    table0_buyhome[, percent_of_positive := round2(100*homeownership/positive, 1)]
    
    # write to Excel worksheets in memory
    addWorksheet(wb, 'Table_0_buyhome')
    writeDataTable(wb, sheet = 'Table_0_buyhome', table0_buyhome, 
                   rowNames = F, colNames = T) 
    
    
# Table 1: Quarterly age differences Pre/Exit/Post ----
    table1 <- data.table()
    for(tempx in sort(unique(raw$prog_type_use))){
      # Data prep ----
        dt1 <- copy(raw)[prog_type_use == tempx]
        dt1[qtr == -4, time := -1]
        dt1[qtr == 0, time := 0]
        dt1[qtr == 4, time := 1]
      
      # Summary table ----
        table1_qtr_diff <- merge(dt1[!is.na(time), 
                                     .(tot.mean = mean(wage), tot.sd = sd(wage)), 
                                     time], # any exit
                                  dt1[!is.na(time) & exit_category == "Positive", 
                                      .(pos.mean = mean(wage), pos.sd = sd(wage)), 
                                      time], # positive exits
                                  by = 'time', 
                                  all = T)
        table1_qtr_diff <- merge(table1_qtr_diff,
                                  dt1[!is.na(time) & exit_category == "Negative", 
                                      .(neg.mean = mean(wage), neg.sd = sd(wage)), 
                                      time], # negative exits
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
      table1_qtr_diff <- table1_qtr_diff[, .(Program = tempx,
                                             `Wage type` = 'quarterly', 
                                             `Time period` = factor(time, 
                                                                    levels = c(-1, 0, 1), 
                                                                    labels = c("1 year prior", "Exit", "1 year post")), 
                                             `Any exit` = paste0(prettyNum(round2(tot.mean), big.mark = ','), " (", prettyNum(round2(tot.sd), big.mark = ','), ")"), 
                                             Positive = paste0(prettyNum(round2(pos.mean), big.mark = ','), " (", prettyNum(round2(pos.sd), big.mark = ','), ")"), 
                                             Negative = paste0(prettyNum(round2(neg.mean), big.mark = ','), " (", prettyNum(round2(neg.sd), big.mark = ','), ")"),
                                             Difference = paste0(prettyNum(round2(difference), big.mark = ','), " (", prettyNum(round2(difference.sd), big.mark = ','), ")"),
                                             Significant = significant, 
                                             `p-value` = ifelse(t_test_pvalue < 0.001, "<0.001", rads::round2(t_test_pvalue, 3)))]
    table1 <- rbind(table1, table1_qtr_diff)
    rm(table1_qtr_diff)
    
    }
    
    # Append copy of Table 1 without SE
      table1_simple <- copy(table1)
      table1_simple[, `Any exit` := gsub(" .*", "", `Any exit`)]
      table1_simple[, `Positive` := gsub(" .*", "", `Positive`)]
      table1_simple[, `Negative` := gsub(" .*", "", `Negative`)]
      table1_simple[, Difference := prettyNum(as.integer(gsub(",", "", Positive)) - as.integer(gsub(",", "", Negative)), big.mark = ',')]
      
      table1 <- rbind(table1, table1_simple)
      rm(table1_simple)
          
    # write to Excel worksheets in memory
    addWorksheet(wb, 'Table_1_differences')
    writeDataTable(wb, sheet = 'Table_1_differences', table1, 
                   rowNames = F, colNames = T)   

# Table 2: Descriptive statistics by Exit Type ----
  # configure arsenal::tableby ----
    my_controls <- tableby.control(
      test = T,
      total = T,
      numeric.test = "kwt", cat.test = "chisq",
      # numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
      numeric.stats = c("meansd", "median", "Nmiss2"),
      digits = 0,
      cat.stats = c("countpct", "Nmiss2"),
      stats.labels = list(
        meansd = "Mean (SD)",
        # medianq1q3 = "Median (Q1, Q3)",
        # range = "Min - Max",
        Nmiss2 = "Missing"
      )
    )
    
  # loop over each program type to create series of table2 ----
    for(tempx in sort(unique(raw$prog_type_use))){
      message('Processing ', tempx)
      if(tempx == "All Programs"){
        raw.subset <- raw[prog_type_use != tempx | is.na(prog_type_use)]} else{
          raw.subset <- raw[prog_type_use == tempx]
        }
      raw.subset[, prog_type_use := as.character(prog_type_use)]
      # create table ----
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
                               percent_ami +
                               agency +
                             prog_type_use, 
                           data = raw.subset[qtr == 0], 
                           control = my_controls)
          ))
        
      # save vector of vars associated with exposure (exit type) ----
        setnames(table2, "", "col1")
        table2[, pnumeric := as.numeric(gsub("<", "", `p value`))]
        exposure.associated <- gsub("\\*\\*", "", table2[pnumeric < 0.05]$col1)
        table2[, pnumeric := NULL]
        
      # tidy table ----
        table2[, col1 := gsub("wage", "Wages", col1)]
        table2[, col1 := gsub("Wages_hourly", "Wages hourly", col1)]
        table2[, col1 := gsub("hrs", "Hours", col1)]
        table2[, col1 := gsub("percent_ami", "Percent AMI", col1)]
        table2[, col1 := gsub("race_eth_me", "Race/ethnicity", col1)]
        table2[, col1 := gsub("gender_me", "Gender", col1)]
        table2[, col1 := gsub("race_gender", "Race/ethnicity & Gender", col1)]
        table2[, col1 := gsub("age_at_exit", "Age", col1)]
        table2[, col1 := gsub("hh_disability", "Household with disability", col1)]
        table2[, col1 := gsub("single_caregiver", "Single caregiver", col1)]
        table2[, col1 := gsub("housing_time_at_exit", "Years in public housing", col1)]
        table2[, col1 := gsub("agency", "Agency", col1)]
        table2[, col1 := gsub("prog_type_use", "Program type", col1)]
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
    
        # drop missing if all missing are zero
        table2 <- table2[!(col1 == "Missing" & get(names(table2)[2]) == 0 & get(names(table2)[3]) == 0 & get(names(table2)[4]) == 0)]
        
        table2[col1 != variable, col1 := paste0("   ", col1)]
        
        table2 <- table2[, 1:5]
        
        setnames(table2, c("p value"), c("P-value"))
        
        # only keep median for wage and hours
        table2[, category := table2[['col1']]]
        table2[grepl("^ ", category), category := NA] # wipe out sub-values
        table2[, category := category[1], by= .(cumsum(!is.na(category)) ) ] # fill forward / downward
        table2 <- table2[!(grepl("Median", col1) & !category %in% c("Wages", "Hours", "Wages hourly"))]
        table2[, c("category") := NULL]
        
      # save with distinct object name ----
        assign(paste0("table2_", tempx), table2)
        
      # add to Excel file in memory ----
        addWorksheet(wb, paste0("Table_2_", gsub(" ", "_", tempx)))
        writeDataTable(wb, sheet = paste0("Table_2_", gsub(" ", "_", tempx)), table2, 
                       rowNames = F, colNames = T)  
        
      # Identify confounders ----
        if(tempx == "All Programs"){
          # Identify confounders (associated with exposure and outcome) ----
          # remove wage / ami since they are the outcomes of interest ----
          exposure.associated <- setdiff(exposure.associated, c('wage', 'percent_ami', "wage_hourly", "hrs", "race_gender"))
          pscovariates <- copy(exposure.associated) # keep a copy if want to create propensity score
          
          # split categorical from continuous ----
          # identify categorical vars & set aside ----
          categorical <- exposure.associated[sapply(raw.subset[, ..exposure.associated], class) %in% c("character", "factor", "logical")]
          exposure.associated <- setdiff(exposure.associated, categorical) # remove definite categorical from pool
          
          # partition remaining vars into categorical and continuous ----
          continuous <- c()
          for(maybecat in exposure.associated){
            tempclass <- class(raw.subset[[maybecat]])
            tempvals <- length(unique(raw.subset[[maybecat]]))
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
                                      data = raw.subset[qtr==0])
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
                          data = raw.subset[qtr==0], 
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
          tempy <- tempfile(fileext = ".Rdata") # tempfile in memory to hold Excel file
          save(confounders, file = tempy)
          drv$upload_file(src = tempy, 
                          dest = "HUD HEARS Study/wage_analysis/output/hpd_revision_1/confounders.Rdata")  
          pscovariates <- data.table(pscovariates = pscovariates)
          tempy <- tempfile(fileext = ".Rdata") # tempfile in memory to hold Excel file
          save(pscovariates, file = tempy)
          drv$upload_file(src = tempy, 
                          dest = "HUD HEARS Study/wage_analysis/output/hpd_revision_1/pscovariates.Rdata")  
        }
    }
    
# Write Tables 0, 1 & 2 using openxlsx to SharePoint ----
    tempy <- tempfile(fileext = ".xlsx") # tempfile in memory to hold Excel file
    saveWorkbook(wb, file = tempy, overwrite = T) # write to tempfile

    drv$upload_file(src = tempy, 
                    dest = "HUD HEARS Study/wage_analysis/output/hpd_revision_1/Tables_descriptive.xlsx")  


# The end ----