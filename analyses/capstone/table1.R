# Header ----
# Author: Capstone Group 2
# Date: Jan 12, 2021
# R version: 4.1.2
# Purpose: Code for deriving Table 1
#

### Import data
rm(list=ls())
pacman::p_load(data.table, DBI, keyring, glue)

# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, ggplot2, viridis, hrbrthemes,
               knitr, kableExtra, rmarkdown)

# Uncomment the following line to create a key (run when you change KCIT pwd)
# When a window pops up, please enter your standard KCIT password
keyring::key_set(service = "hhsaw", username = "n-hhu@kingcounty.gov")

message("Hint! There are different flavors of SQL. When googling for how to do something, 
            be sure to look for TSQL | T-SQL | Transact-SQL")

# Create a connection to hhs_analytics_workspace on Azure server 16 ----
cxn16 <- DBI::dbConnect(odbc::odbc(), 
                        driver = "ODBC Driver 17 for SQL Server", 
                        server = "kcitazrhpasqlprp16.azds.kingcounty.gov", 
                        database = "hhs_analytics_workspace", 
                        uid = keyring::key_list("hhsaw")[["username"]], 
                        pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]), 
                        Encrypt = "yes", 
                        TrustServerCertificate = "yes", 
                        Authentication = "ActiveDirectoryPassword")

df <- setDT(DBI::dbGetQuery(conn = cxn16, "SELECT * FROM [hudhears].[capstone_data_2]"))
# df1 <- setDT(DBI::dbGetQuery(conn = cxn16, "SELECT * FROM [hudhears].[capstone_data_1]"))
# head(df1)
head(df)

### Descriptive stats
## Variables in the table:
## exit_date?, gender_me, race_eth_me, age_at_exit, housing_time_at_exit, agency,
## major_prog, prog_type, hh_size, hh_disability, single_caregiver, 
## exit_category, tt_homeless, event

library(table1)
df.table <- df

df.table$hh_disability <- factor(df.table$hh_disability, 
                                 levels = c(0,1), 
                                 labels = c("No","Yes"))
df.table$single_caregiver <- factor(df.table$single_caregiver,
                                    levels = c(0,1),
                                    labels = c("No","Yes"))
df.table$event <- factor(df.table$event,
                         levels = c(0,1),
                         labels = c("Yes","No"))

label(df.table$gender_me) <- "Sex"
label(df.table$race_eth_me) <- "Race/Ethnicity"
label(df.table$age_at_exit) <- "Age at exit (year)"
label(df.table$housing_time_at_exit) <- "Time in housing at exit (year)"
label(df.table$agency) <- "Agency"
label(df.table$major_prog) <- "Subsidy type"
label(df.table$prog_type) <- "Specific program type"
label(df.table$hh_size) <- "Household size"
label(df.table$hh_disability) <- "Head of household disabled"
label(df.table$single_caregiver) <- "Single caregiver in household"
label(df.table$tt_homeless) <- "Time to homeless (day)"
label(df.table$event) <- "Censored"

# Censored
table1(~gender_me+race_eth_me+age_at_exit+housing_time_at_exit+agency
       +major_prog+prog_type+hh_size+hh_disability+single_caregiver|exit_category, 
       data=(df.table %>% subset(event=="Yes")))

# Non-censored
table1(~gender_me+race_eth_me+age_at_exit+housing_time_at_exit+agency
       +major_prog+prog_type+hh_size+hh_disability+single_caregiver|exit_category, 
       data=(df.table %>% subset(event=="No")))

# Overall
table1(~tt_homeless+event+gender_me+race_eth_me+age_at_exit+housing_time_at_exit+agency
       +major_prog+prog_type+hh_size+hh_disability+single_caregiver|exit_category, 
       data=df.table)

df <- rename(df, housing_time=housing_time_at_exit)
library(VIM)
aggr_plot <- aggr(df[,c(6:15,17,21,22)],
                  col=c("navyblue","red"),
                  bars=F,prop=F,combined=T,
                  numbers=T,
                  sortVars=F,
                  cex.lab=1.2,
                  cex.axis=0.54,
                  #labels=names(df[,c(6:15,17,21,22)]),
                  ylab=c("Histogram of missing data", "Pattern"))
labs <- paste(names(df[,c(6:15,17,21,22)]))
text(cex=1, x=-.25, y=-1.25, labs, xpd=TRUE, srt=45)
