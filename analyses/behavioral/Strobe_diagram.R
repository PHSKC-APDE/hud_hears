# Summarizing exclusions code ----
#Summarize exclusions and data processing to create a Strobe Diagram
#Last Update April 28, 2022

##SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, DiagrammeR, DiagrammeRsvg, rsvg, dplyr, lubridate)
#Need to load packages
library(tidyverse)
library(odbc)
library(glue)
library(data.table)
# library(DiagrammeR)
# library(DiagrammeRsvg)
library(rsvg)
library(lubridate)

# Connect to HHSAW
db_hhsaw <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = "tcp:kcitazrhpasqlprp16.azds.kingcounty.gov,1433",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryPassword")

## Get total N's--all study IDs eligible----
pha_id_xwalk <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[pha_id_xwalk]"))

#Total number of unique PHA ids
length(unique(pha_id_xwalk$id_kc_pha))
#Total number of unique HUD HEARS IDs
length(unique(pha_id_xwalk$id_hudhears))


#Identify duplicates of each ID
pha_n_occur <- data.frame(table(pha_id_xwalk$id_kc_pha))
pha_dupes<- pha_n_occur[pha_n_occur$Freq > 1,]

n_occur <- data.frame(table(pha_id_xwalk$id_hudhears))
dupes<- n_occur[n_occur$Freq >1,] #385
# table(dupes$Freq)
# 2   3   4 
# 381   3   1 

##225633 unique PHA IDs
##225243 unique HUDHEARS IDs
##385 duplicate HUDHEARS IDs
##0 duplicate PHA IDs

##exclude duplicate 385 HUDHEARS IDs



# Identify those with exit during time period of interest----
exit_timevar <- dbGetQuery(db_hhsaw,
                           "SELECT a.*, c.geo_tractce10 FROM
                           (SELECT * FROM pha.stage_pha_exit_timevar
                           WHERE chooser = chooser_max) a
                           LEFT JOIN
                           (SELECT DISTINCT geo_hash_clean, geo_hash_geocode FROM ref.address_clean) b
                           ON a.geo_hash_clean = b.geo_hash_clean
                           LEFT JOIN
                           (SELECT DISTINCT geo_hash_geocode, geo_tractce10 FROM ref.address_geocode) c
                           ON b.geo_hash_geocode = c.geo_hash_geocode")

#Check number with multiple exits first (for housekeeping purposes)----
exits <- exit_timevar %>%
  filter(true_exit == 1 &
           ((agency == "SHA" & act_date >= "2012-01-01" & act_date <= "2018-12-31") |
              (agency == "KCHA" & act_date >= "2016-01-01" & act_date <= "2018-12-31"))) %>%
  distinct(id_kc_pha, act_date) %>%
  arrange(id_kc_pha, act_date) %>% group_by(id_kc_pha) %>% mutate(exit_cnt = n())



totals <- exits %>% group_by(id_kc_pha)%>% summarize(Count=n())
table(totals$Count)
# 1     2     3
# 19329   393     7



# length(unique(exits$id_kc_pha))#19329 unique IDs
# #Excluding 19736-19329=407 exits that were duplicates
#400 second exits and 7 3rd exits

# Get number of exits that excludes duplicate (2nd and 3rd exits)----
# # Set up list of IDs, exits, and dates
exits <- exit_timevar %>%
  filter(true_exit == 1 &
           ((agency == "SHA" & act_date >= "2012-01-01" & act_date <= "2018-12-31") |
              (agency == "KCHA" & act_date >= "2016-01-01" & act_date <= "2018-12-31"))) %>%
  distinct(id_kc_pha, act_date) %>%
  arrange(id_kc_pha, act_date) %>%
# Take the most recent exit for the 400 people with multiple exits
  group_by(id_kc_pha) %>%
  mutate(exit_cnt = n(), exit_order = row_number()) %>% ungroup() %>%
  filter(exit_cnt == exit_order)

#total number without exit in time period
225243-19329


#Bring in matched controls and get Ns ----
# # Note: Control matching was not rerun due to long run time. 
# #Read in pre-generated table of exits and matched controls
control_match_long <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[control_match_long]"))
# #Tabulate Ids to check number of matched controls
table(control_match_long$id_type)
# id_control    id_exit 
# 77316      19329 


# Summarize number with and without medicaid coverage at different time points----
##Left join this table with Medicaid ID table
control_match_id_mcaid <- dbGetQuery(db_hhsaw,
                                     "SELECT a.*, b.id_mcaid
                                     FROM
(SELECT * FROM hudhears.control_match_covariate) a
LEFT JOIN
(SELECT DISTINCT id_hudhears, id_mcaid
FROM claims.hudhears_id_xwalk WHERE id_mcaid is not null) b
ON a.id_hudhears = b.id_hudhears") %>%
  mutate(exit_1yr_prior = exit_date - years(1) + days(1),
         exit_1yr_after = exit_date + years(1) - days(1))

#Summarize number with medicaid coverage 7 months before and after exit



## Observations off by 300
# # Do antijoin to see why this is happening
# 
# mis_match<- anti_join(control_match_id_mcaid, control_match_covariate, by=c("id_hudhears"))
# mis_match2<- as.data.frame(mis_match)
# 
# #Several Medicaid IDs match to a single HUD HEARS ID. Will take first record
# 
# 
# # control_match_covariate %>% select(id_hudhears, starts_with("full_")) %>% head() %>% mutate(chk = full_cov_7_prior * full_cov_7_after)


