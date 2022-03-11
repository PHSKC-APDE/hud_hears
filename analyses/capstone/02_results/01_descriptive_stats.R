## Script name: 01_descriptive_stats.R
##
## Purpose of script: - generate descriptive statistics tables
##                    - visualizations of missingness patterns
##
## Author: Capstone Group 2
## Date: 3/11/2022
##
## R version: 4.1.2
##
## Notes:
##    - Table 1 uses table1 package, which generates a html table. This cannot 
##      be saved using the "Export" option. Instead, please use "Show in new window"
##      option at the right end of the Viewer tool bar
##

# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, ggplot2, viridis, hrbrthemes,
               knitr, kableExtra, rmarkdown, table1, VIM)

# Uncomment the following line to create a key (run when you change KCIT pwd)
# Put your KC working email address inside the "" of the username = ""
# When a window pops up, please enter your standard KCIT password
# keyring::key_set(service = "hhsaw", username = "")

# Create a connection to hhs_analytics_workspace on Azure server 16 ----
db_hhsaw <- DBI::dbConnect(odbc::odbc(), 
                        driver = "ODBC Driver 17 for SQL Server", 
                        server = "kcitazrhpasqlprp16.azds.kingcounty.gov", 
                        database = "hhs_analytics_workspace", 
                        uid = keyring::key_list("hhsaw")[["username"]], 
                        pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]), 
                        Encrypt = "yes", 
                        TrustServerCertificate = "yes", 
                        Authentication = "ActiveDirectoryPassword")

# Select the table needed for performing descriptive statistics
df <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[capstone_data_3]"))

### Part 1: Creating Table 1
## Three descriptive statistics tables (by exit type) are created using the code below
## 1) For censored individuals
## 2) For individuals who have homelessness events
## 3) For the overall population
##
## Variables in all tables:
## gender_me, race_eth_me, age_at_exit, housing_time_at_exit, agency
## major_prog, hh_size, hh_disability, single_caregiver, kc_opp_index_score
##
## Variables only in overall population
## tt_homeless, event
##
## ----------------------------------------------------------------------------
# 1. Adjust labels and values of variables to make descriptive statistics look better
# Create df.table so original data is not altered
df.table <- df

# Re-factorize hh_disability, single_caregiver, event, to show a clearer interpretation
# hh_disability: 0 -> no; 1-> yes
df.table$hh_disability <- factor(df.table$hh_disability, 
                                 levels = c(0,1), 
                                 labels = c("No","Yes"))
# single_caregiver: 0 -> no; 1-> yes
df.table$single_caregiver <- factor(df.table$single_caregiver,
                                    levels = c(0,1),
                                    labels = c("No","Yes"))
# event (relabel to censored): 0 -> yes; 1-> no
# Use with caution! 0 in event means is censored; 1 in event means not censored
df.table$event <- factor(df.table$event,
                         levels = c(0,1),
                         labels = c("Yes","No"))

# Label variables to include info and units
label(df.table$gender_me) <- "Gender"
label(df.table$race_eth_me) <- "Race/Ethnicity"
label(df.table$age_at_exit) <- "Age at exit (year)"
label(df.table$housing_time_at_exit) <- "Time in housing at exit (year)"
label(df.table$agency) <- "Agency"
label(df.table$major_prog) <- "Subsidy type"
label(df.table$hh_size) <- "Household size"
label(df.table$hh_disability) <- "Head of household disabled"
label(df.table$single_caregiver) <- "Single caregiver in household"
label(df.table$tt_homeless) <- "Time to homeless (day)"
label(df.table$event) <- "Censored"
label(df.table$kc_opp_index_score) <- "Opportunity index score"

## -----------------------------------------------------------------------
# 2. Make table 1
# How to use table1:
# table1(~ "variables to see in rows, separated by +" | "variables to see in columns",
#        data = "desired data")
#
# 1) For censored individuals
table1(~gender_me+race_eth_me+age_at_exit+housing_time_at_exit+agency+
               major_prog+hh_size+hh_disability+single_caregiver+
               kc_opp_index_score|exit_category, 
       data=(df.table %>% subset(event=="Yes")))

# 2) For individuals who have homelessness events
table1(~gender_me+race_eth_me+age_at_exit+housing_time_at_exit+agency+
         major_prog+hh_size+hh_disability+single_caregiver+
         kc_opp_index_score|exit_category, 
       data=(df.table %>% subset(event=="No")))

# 3) For the overall population
table1(~tt_homeless+event+gender_me+race_eth_me+age_at_exit+housing_time_at_exit+
         agency+major_prog+hh_size+hh_disability+single_caregiver+
         kc_opp_index_score|exit_category, 
       data=df.table)


### Part 2: Missingness pattern
## A missingness frequency histogram and a missingness pattern plot are created using the code below
##
## Variables included:
## gender_me, race_eth_me, age_at_exit, housing_time_at_exit, agency
## major_prog, hh_size, hh_disability, single_caregiver, kc_opp_index_score,
## exit_reason_clean, exit_category
##
## ----------------------------------------------------------------------------
# 1. Select variables that needs to be explored
# Create df.miss so original data is not altered
df.miss <- df 

# Select variables
df.miss <- df.miss %>% select(gender_me, race_eth_me, age_at_exit, 
                              housing_time_at_exit, agency, major_prog, hh_size, 
                              hh_disability, single_caregiver, kc_opp_index_score, 
                              exit_reason_clean, exit_category)

# For variables with a long name, need to make a shorter version or else the label
# will not show in the plot
df.miss <- rename(df.miss, c(opp_score=kc_opp_index_score, 
                             housing_time=housing_time_at_exit,
                             single_care=single_caregiver,
                             exit_reason=exit_reason_clean))

# -----------------------------------------------------------------------------
# 2. Create plot (2 plots in one)
aggr_plot <- aggr(df.miss,
                  col=c("navyblue","red"),        # Can change color, first non-missing, second missing
                  bars=F,prop=F,combined=F,
                  numbers=T,
                  sortVars=F,
                  cex.lab=1.5,                    # Y axis label font size
                  cex.axis=0.8,                   # X axis label font size
                  ylab=c("Histogram of missing data", "Pattern"))
