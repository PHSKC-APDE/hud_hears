# Header ----
# Author: Capstone Group 2 - Hantong Hu
# Date: Jan 27, 2021
# R version: 4.1.2
# Purpose: Code for deriving program type and housing exit variable
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
db_hhsaw <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = "kcitazrhpasqlprp16.azds.kingcounty.gov",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryPassword")

df <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[capstone_data_3]"))
head(df)

# Derive program type
df <- df %>% 
  mutate(new_prog_type = case_when(
    prog_type %in% c("PH", "SHA OWNED AND MANAGED")   ~ "Public Housing",
    prog_type %in% c("COLLABORATIVE HOUSING", "PBS8")   ~ "Project-based Housing",
    prog_type %in% c("PORT", "TBS8", "TENANT BASED")   ~ "Housing Choice Voucher",
    TRUE ~ prog_type))

# Derive housing time var
# summary(df$housing_time_at_exit)
# unique(df$housing_time_at_exit)
# df$housing_time_at_exit[df$housing_time_at_exit<=0,] <- NA

start_date_info <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT hh_id_kc_pha, admit_date FROM [pha].[final_calyear]"))

start_clean <- start_date_info %>% group_by(hh_id_kc_pha) %>% 
  arrange(admit_date) %>% filter(row_number ()==1)
df.start <- left_join(df, start_clean, by="hh_id_kc_pha")

diff.day <- difftime(as.Date(df.start$exit_date), as.Date(df.start$admit_date), units = "days")
admit.chart <- cbind(diff.day, round(diff.day/365, 2), df$housing_time_at_exit)
max.time <- apply(admit.chart[,-1], MARGIN = 1, FUN = function(x) max(x, na.rm=T))
max.time[which(max.time==-Inf)] <- NA
max.time[which(max.time<0)] <- NA

df$new_housing_time <- max.time
head(df)

#------------------------------------------------------------
## Write Temporary table to SQL Server

DBI::dbWriteTable(conn = db_hhsaw, 
                  name = DBI::Id(schema = "hudhears", table = "capstone_data_4"), 
                  value = setDF(copy(df)), 
                  append = F, 
                  overwrite = T)




