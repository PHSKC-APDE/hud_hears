#Creating STROBE Diagram
#February 24, 2022

# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, DiagrammeR, DiagrammeRsvg, rsvg, dplyr, lubridate)
#Need to load packages
library(tidyverse)
library(odbc)
library(glue)
library(data.table)
library(DiagrammeR)
library(DiagrammeRsvg)
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

##Get N--all study IDs
pha_id_xwalk <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[pha_id_xwalk]"))


# length(pha_id_xwalk$id_kc_pha)
# length(unique(pha_id_xwalk$id_kc_pha))
# pha_n_occur <- data.frame(table(pha_id_xwalk$id_kc_pha))
# pha_dupes<- pha_n_occur[pha_n_occur$Freq > 1,]
# sum(is.na(pha_id_xwalk$id_kc_pha))
# sum(is.na(pha_id_xwalk$id_hudhears))



# length(unique(pha_id_xwalk$id_hudhears))
# n_occur <- data.frame(table(pha_id_xwalk$id_hudhears))
# dupes<- n_occur[n_occur$Freq >1,] #385
# table(dupes$Freq)
# 2   3   4 
# 381   3   1 

##225633 unique PHA IDs
##225243 unique HUDHEARS IDs
##385 duplicate HUDHEARS IDs
##0 duplicate PHA IDs

##exclude duplicate 385 HUDHEARS IDs



#Identify date
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

# #Check number with multiple exits first (for housekeeping purposes)
# exits <- exit_timevar %>% 
#   filter(true_exit == 1 & 
#            ((agency == "SHA" & act_date >= "2012-01-01" & act_date <= "2018-12-31") | 
#               (agency == "KCHA" & act_date >= "2016-01-01" & act_date <= "2018-12-31"))) %>%
#   distinct(id_kc_pha, act_date) %>%
#   arrange(id_kc_pha, act_date) %>% group_by(id_kc_pha) %>% mutate(exit_cnt = n())
# exits %>% group_by(id_kc_pha)%>% table(exit_cnt)
# 
# 
# totals <- exits %>% group_by(id_kc_pha)%>% summarize(Count=n())
# table(totals$Count)
# # 1     2     3 
# # 18929   393     7 
# 
# 
# length(unique(exits$id_kc_pha))#19329 unique IDs
# #Excluding 19736-19329=407 exits that were duplicates


##Write over this with correct code (or just run this part)
# Set up list of IDs, exits, and dates
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

# Note: Control matching was not rerun due to long run time. 
#Read in pre-generated table of exits and matched controls
control_match_long <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[control_match_long]"))
#Tabulate Ids to check number of matched controls
table(control_match_long$id_type)
# id_control    id_exit 
# 76936      19234 

#19234 vs. 19329

#Read in table with covariates
control_match_covariate <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[control_match_covariate]"))
table(control_match_covariate$id_type)
# id_control    id_exit 
# 76936      19234 

##join this table with medicaid data to get n's

##Left join this table with Medicaid ID table
control_match_id_mcaid <- dbGetQuery(db_hhsaw,
                                     "SELECT a.*, b.id_mcaid
                                     FROM
(SELECT * FROM hudhears.control_match_long) a
LEFT JOIN
(SELECT DISTINCT id_hudhears, id_mcaid
FROM claims.hudhears_id_xwalk WHERE id_mcaid is not null) b
ON a.id_hudhears = b.id_hudhears") %>%
  mutate(exit_1yr_prior = exit_date - years(1) + days(1),
         exit_1yr_after = exit_date + years(1) - days(1))

table(control_match_id_mcaid$id_type)



library(DiagrammeR)
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']

      # edge definitions with the node IDs
      tab1 -> tab2 
      tab1 -> tab 3 
      -> tab3 -> tab4 -> tab5;
      }

      [1]: 'PHA Clients n=312,397'
      [2]: 'Exit from public housing n=38,961'
      [3]: 'No exit from public housing '
      [4]: 'Participants eligible for the study n=600'
      [5]: 'Study sample n=600'
      ")
