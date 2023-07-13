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

# Table 0: ID counts of positive, neutral, negative exits by program type ----
    table0 <- data.table()
    for(tempx in sort(unique(raw$prog_type_use))){
      table0 <- rbind(
        table0,
        data.table(category = "Persons", 
                   program = tempx,
                   total = uniqueN(raw[prog_type_use == tempx]$id_kc_pha), 
                   positive = uniqueN(raw[exit_category == "Positive" & prog_type_use == tempx]$id_kc_pha),
                   neutral = uniqueN(raw[exit_category == "Neutral" & prog_type_use == tempx]$id_kc_pha),
                   negative = uniqueN(raw[exit_category == "Negative" & prog_type_use == tempx]$id_kc_pha)), 
        data.table(category = "Households", 
                   program = tempx,
                   total = uniqueN(raw[prog_type_use == tempx]$hh_id_kc_pha), 
                   positive = uniqueN(raw[exit_category == "Positive" & prog_type_use == tempx]$hh_id_kc_pha),
                   neutral = uniqueN(raw[exit_category == "Neutral" & prog_type_use == tempx]$hh_id_kc_pha),
                   negative = uniqueN(raw[exit_category == "Negative" & prog_type_use == tempx]$hh_id_kc_pha)))
    }
    
    # write to Excel worksheets in memory
    wb <- createWorkbook() # initiate a new / empty workbook
    addWorksheet(wb, 'Table_0_counts') 
    writeDataTable(wb, sheet = 'Table_0_counts', table0, 
                   rowNames = F, colNames = T)   
    
# Table 0_lw: living wage ----
    lw = copy(raw)[qtr %in% c(0, 4) & prog_type_use == 'All Programs', .(prog_type_use = 'All Programs', qtr, living_wage, exit_category)]
    lw = rbind(copy(lw)[, exit_category := 'Total'], lw)
    table0_lw <- merge( lw[!is.na(living_wage), .(denominator = .N), .(qtr, exit_category)],
                        lw[living_wage == T, .(numerator = .N), .(qtr, exit_category)])
    table0_lw <- merge(lw[is.na(living_wage), .(missing = .N), .(qtr, exit_category)], 
                       table0_lw, by = c('qtr', 'exit_category'))
    table0_lw[, percentage := rads::round2(100*numerator / denominator, 1)]
    
    # write to Excel worksheets in memory
    addWorksheet(wb, 'Table_0_livingwage')
    writeDataTable(wb, sheet = 'Table_0_livingwage', table0_lw, 
                   rowNames = F, colNames = T) 
    
# Table 0_ami: percent AMI ----
    ami = copy(raw)[qtr %in% c(0, 4) & prog_type_use == 'All Programs', .(prog_type_use = 'All Programs', qtr, percent_ami, exit_category)]
    ami = rbind(copy(ami)[, exit_category := 'Total'], ami)
    table0_ami <- setorder(ami[, .(mean_ami = rads::round2(mean(percent_ami, na.rm = T))), .(qtr, exit_category)], qtr, exit_category)
    
    # write to Excel worksheets in memory
    addWorksheet(wb, 'Table_0_percentAMI')
    writeDataTable(wb, sheet = 'Table_0_percentAMI', table0_ami, 
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
    
    
# Table 0: Quarterly age differences Pre/Exit/Post ----
    table0 <- data.table()
    for(tempx in sort(unique(raw$prog_type_use))){
      # Data prep ----
        dt1 <- copy(raw)[prog_type_use == tempx]
        dt1[qtr == -4, time := -1]
        dt1[qtr == 0, time := 0]
        dt1[qtr == 4, time := 1]
      
      # Summary table ----
        table0_qtr_diff <- merge(dt1[!is.na(time), 
                                     .(tot.mean = mean(wage), tot.sd = sd(wage)), 
                                     time], # any exit
                                  dt1[!is.na(time) & exit_category == "Positive", 
                                      .(pos.mean = mean(wage), pos.sd = sd(wage)), 
                                      time], # positive exits
                                  by = 'time', 
                                  all = T)
        table0_qtr_diff <- merge(table0_qtr_diff,
                                 dt1[!is.na(time) & exit_category == "Neutral", 
                                     .(neut.mean = mean(wage), neut.sd = sd(wage)), 
                                     time], # neutral exits
                                 by = 'time', 
                                 all = T)
        table0_qtr_diff <- merge(table0_qtr_diff,
                                  dt1[!is.na(time) & exit_category == "Negative", 
                                      .(neg.mean = mean(wage), neg.sd = sd(wage)), 
                                      time], # negative exits
                                  by = 'time', 
                                  all = T)
        table0_qtr_diff[, diff_posneut := pos.mean - neut.mean]
        table0_qtr_diff[, diff_posneut.sd := sqrt((pos.sd^2) + (neut.sd^2))]
        table0_qtr_diff[, diff_posneg := pos.mean - neg.mean]
        table0_qtr_diff[, diff_posneg.sd := sqrt((pos.sd^2) + (neg.sd^2))]
        table0_qtr_diff[, diff_neutneg := neut.mean - neg.mean]
        table0_qtr_diff[, diff_neutneg.sd := sqrt((neut.sd^2) + (neg.sd^2))]
        
      # Perform t-test ----
        for(ii in c(-1, 0, 1)){
          table0_qtr_diff[time == ii, 
                         t_test_posneg := t.test(x = dt1[!is.na(time) & exit_category == "Positive" & time == ii,]$wage,
                                                 y = dt1[!is.na(time) & exit_category == "Negative" & time == ii,]$wage)$p.value]
          table0_qtr_diff[time == ii, 
                          t_test_posneut := t.test(x = dt1[!is.na(time) & exit_category == "Positive" & time == ii,]$wage,
                                                  y = dt1[!is.na(time) & exit_category == "Neutral" & time == ii,]$wage)$p.value]
          table0_qtr_diff[time == ii, 
                          t_test_neutneg := t.test(x = dt1[!is.na(time) & exit_category == "Neutral" & time == ii,]$wage,
                                                  y = dt1[!is.na(time) & exit_category == "Negative" & time == ii,]$wage)$p.value]
          }
    # Tidy table ----
      table0_qtr_diff[t_test_posneg < 0.05, sig_posneg := "*"]
      table0_qtr_diff[t_test_posneut < 0.05, sig_posneut := "*"]
      table0_qtr_diff[t_test_neutneg < 0.05, sig_neutneg := "*"]
      table0_qtr_diff <- table0_qtr_diff[, .(Program = tempx,
                                             `Wage type` = 'quarterly', 
                                             `Time period` = factor(time, 
                                                                    levels = c(-1, 0, 1), 
                                                                    labels = c("1 year prior", "Exit", "1 year post")), 
                                             `Any exit` = paste0(prettyNum(round2(tot.mean), big.mark = ','), " (", prettyNum(round2(tot.sd), big.mark = ','), ")"), 
                                             Positive = paste0(prettyNum(round2(pos.mean), big.mark = ','), " (", prettyNum(round2(pos.sd), big.mark = ','), ")"), 
                                             Neutral = paste0(prettyNum(round2(neut.mean), big.mark = ','), " (", prettyNum(round2(neut.sd), big.mark = ','), ")"), 
                                             Negative = paste0(prettyNum(round2(neg.mean), big.mark = ','), " (", prettyNum(round2(neg.sd), big.mark = ','), ")"),
                                             
                                             Diff.pos_neut = paste0(prettyNum(round2(diff_posneut), big.mark = ','), " (", prettyNum(round2(diff_posneut.sd), big.mark = ','), ")"),
                                             Sig.pos_neut = sig_posneut, 
                                             `p-value.pos_neut` = ifelse(t_test_posneut < 0.001, "<0.001", rads::round2(t_test_posneut, 3)),
                                             
                                             Diff.pos_neg = paste0(prettyNum(round2(diff_posneg), big.mark = ','), " (", prettyNum(round2(diff_posneg.sd), big.mark = ','), ")"),
                                             Sig.pos_neg = sig_posneg, 
                                             `p-value.pos_neg` = ifelse(t_test_posneg < 0.001, "<0.001", rads::round2(t_test_posneg, 3)),
                                             
                                             Diff.neut_neg = paste0(prettyNum(round2(diff_neutneg), big.mark = ','), " (", prettyNum(round2(diff_neutneg.sd), big.mark = ','), ")"),
                                             Sig.neut_neg = sig_neutneg, 
                                             `p-value.neut_neg` = ifelse(t_test_neutneg < 0.001, "<0.001", rads::round2(t_test_neutneg, 3)))]
    table0 <- rbind(table0, table0_qtr_diff)
    rm(table0_qtr_diff)
    
    }
    
    # Append copy of Table 1 without SE
      table0_simple <- copy(table0)
      table0_simple[, `Any exit` := gsub(" .*", "", `Any exit`)]
      table0_simple[, `Positive` := gsub(" .*", "", `Positive`)]
      table0_simple[, `Neutral` := gsub(" .*", "", `Neutral`)]
      table0_simple[, `Negative` := gsub(" .*", "", `Negative`)]
      table0_simple[, Diff.pos_neut := prettyNum(as.integer(gsub(",", "", Positive)) - as.integer(gsub(",", "", Neutral)), big.mark = ',')]
      table0_simple[, Diff.pos_neg := prettyNum(as.integer(gsub(",", "", Positive)) - as.integer(gsub(",", "", Negative)), big.mark = ',')]
      table0_simple[, Diff.neut_neg := prettyNum(as.integer(gsub(",", "", Neutral)) - as.integer(gsub(",", "", Negative)), big.mark = ',')]
      
      table0 <- rbind(table0, table0_simple)
      rm(table0_simple)
          
    # write to Excel worksheets in memory
    addWorksheet(wb, 'Table_0_differences')
    writeDataTable(wb, sheet = 'Table_0_differences', table0, 
                   rowNames = F, colNames = T)   

# Table 0: All exit types used ----
    table0_types <- copy(raw)[qtr == 0 & prog_type_use == 'All Programs', .(Count = .N), .(`Exit type` = exit_category, `Detailed exit reason` = exit_reason_clean)]
    setorder(table0_types, -`Exit type`, -Count)
    
    
    # write to Excel worksheets in memory
    addWorksheet(wb, 'Table_0_exit_types')
    writeDataTable(wb, sheet = 'Table_0_exit_types', table0_types, 
                   rowNames = F, colNames = T)   
    
# Table 1: Descriptive statistics by Exit Type ----
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
    
  # loop over each program type to create series of table1 ----
    for(tempx in sort(unique(raw$prog_type_use))){
      message('Processing ', tempx)
      if(tempx == "All Programs"){
        raw.subset <- raw[prog_type_use != tempx | is.na(prog_type_use)]} else{
          raw.subset <- raw[prog_type_use == tempx]
        }
      raw.subset[, prog_type_use := as.character(prog_type_use)]
      raw.subset[, exit_category := factor(exit_category, levels = c('Positive', 'Neutral', 'Negative'))]
      # create table ----
        table1 <- as.data.table(summary(
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
                               hh_gt_1_worker +
                               single_caregiver + 
                               hh_size + 
                               percent_ami +
                               living_wage +
                               agency +
                             prog_type_use, 
                           data = raw.subset[qtr == 0], # at exit!
                           control = my_controls)
          ))
          
          # strip attributes
              attr(table1, 'align') <- NULL
              attr(table1, 'ylabel') <- NULL
              #attr(table1, '.internal.selfref') <- NULL
              attr(table1, 'control.list') <- NULL
              table1 <- setDT(copy(as.data.frame(table1)))

      # save vector of vars associated with exposure (exit type) ----
        setnames(table1, "", "col1")
        table1[, pnumeric := as.numeric(gsub("<", "", `p value`))]
        table1[, pnumeric := NULL]
        
      # Format big numbers ---- 
        for(ii in 2:5){
          orig.colname <- names(table1)[ii]
          table1[, tempy := unlist(gsub("\\(|\\)", "", get(orig.colname)))] # drop parenthesis
          table1[, c('tempy1', 'tempy2') := tstrsplit(tempy, split = ' ', fixed=TRUE)]
          table1[, tempy1 := as.integer(tempy1)]
          table1[tempy1 > 999, tempy1.big := format(tempy1, big.mark = ',')]
          table1[, tempy2 := suppressWarnings(as.integer(tempy2))]
          table1[tempy2 > 999, tempy2.big := format(tempy2, big.mark = ',')]
          table1[!is.na(tempy1.big) & is.na(tempy2.big) & grepl(" ", tempy), tempy2.big := gsub("^.* ", "", tempy)] # to keep 2nd half as is when only first part is a big number
          table1[, tempybig := gsub(" \\(NA\\)", "", paste0(tempy1.big, " (", tempy2.big, ")"))]
          # table1[, tempybig := fcase(grepl(",", tempy1.big) & grepl(",", tempy2.big), paste0(tempy1.big, " (", tempy2.big, ")"), 
          #                            grepl(",", tempy1.big) & grepl(",", tempy2.big)==F, tempy1.big)]
          table1[!is.na(tempybig) & tempybig != 'NA', paste0(orig.colname) := tempybig]
          table1[, grep('tempy', names(table1)) := NULL]
        }        

      # tidy table ----
        table1[, col1 := gsub("\\*wage\\*", "\\*Wages, quarterly\\*", col1)]
        table1[, col1 := gsub("wage_hourly", "Wages, hourly", col1)]
        table1[, col1 := gsub("hrs", "Hours, quarterly", col1)]
        table1[, col1 := gsub("percent_ami", "Percent AMI", col1)]
        table1[, col1 := gsub("race_eth_me", "Race/ethnicity", col1)]
        table1[, col1 := gsub("gender_me", "Gender", col1)]
        table1[, col1 := gsub("race_gender", "Race/ethnicity & Gender", col1)]
        table1[, col1 := gsub("age_at_exit", "Age", col1)]
        table1[, col1 := gsub("hh_disability", "Household with disability", col1)]
        table1[, col1 := gsub("hh_gt_1_worker", "Household ≥ 2 wage earners", col1)]
        table1[, col1 := gsub("single_caregiver", "Single caregiver", col1)]
        table1[, col1 := gsub("housing_time_at_exit", "Years of housing assistance", col1)]
        table1[, col1 := gsub("living_wage", "Living wage", col1)]
        table1[, col1 := gsub("agency", "Agency", col1)]
        table1[, col1 := gsub("prog_type_use", "Program type", col1)]
        table1[, col1 := gsub("season", "Season", col1)]
        table1[, col1 := gsub("exit_year", "Exit Year", col1)]
        table1[, col1 := gsub("hh_size", "Household size", col1)]
        
        table1[, col1 := gsub("&nbsp;|\\*\\*", "", col1)]
        table1[ !is.na(`p value`) & `p value` != "", variable := col1]
        table1[, variable := variable[1], by= .(cumsum(!is.na(variable)) ) ] # fill downward
        
        # when binary true/false, collapse it down to one row ----
        tf.vars <- table1[col1 == 'TRUE']$variable # identify true / false variables
        table1 <- table1[col1 != "FALSE"]
        table1[col1 == "TRUE", col1 := variable]
        for(tf in tf.vars){ # collapse the header and the TRUE row down to one row
          table1[col1 == tf, dup := 1:.N, col1]
          table1[dup == 2 & col1 == tf, "p value" := table1[dup==1 & col1 == tf]$`p value`]
          table1 <- table1[!(dup == 1 & col1 == tf)]
          table1[, dup := NULL]
        }
    
        table1 <- table1[!(col1 == "Missing" & get(names(table1)[2]) == 0 & get(names(table1)[3]) == 0 & get(names(table1)[4]) == 0)] # drop if missing always zero
        
        table1[col1 != variable, col1 := paste0("   ", col1)] # add indent for categories within a variable
        
        #table1 <- table1[!(grepl("Median", col1) & !variable %in% c("Wages", "Hours", "Wages hourly"))] # drop Median for wage data
        
        table1 <- table1[, 1:6]
        
        setnames(table1, c("p value"), c("P-value"))

      # save with distinct object name ----
        assign(paste0("table1_", tempx), table1)
        
      # add to Excel file in memory ----
        addWorksheet(wb, paste0("Table_1_", gsub(" ", "_", tempx)))
        writeDataTable(wb, sheet = paste0("Table_1_", gsub(" ", "_", tempx)), table1, 
                       rowNames = F, colNames = T)  
        
    }
    
# Write Tables 0 & 1 using openxlsx to SharePoint ----
    tempy <- tempfile(fileext = ".xlsx") # tempfile in memory to hold Excel file
    saveWorkbook(wb, file = tempy, overwrite = T) # write to tempfile

    drv$upload_file(src = tempy, 
                    dest = "HUD HEARS Study/wage_analysis/output/hpd_revision_1/Tables_descriptive.xlsx")  


# The end ----