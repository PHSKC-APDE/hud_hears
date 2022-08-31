## HUD HEARS Behavioral Health Exit Type Code ----
#Code updated on June 3 2022
#Author: Megan Suter 

#Code Purpose: Process data for HUD HEARS Behavioral Health Analysis

# 1) Pull data from SQL tables to define crisis outcomes: Program codes, ITS, Medicaid ED visits
# 2) Pull data from SQL tables to count and define behavioral health outpatient care
# 3) Pull data from SQL tables to define behavioral health conditions
# 4) Join the above to full covariates table
# Note: Items 1-4 limited to those with an exit only



# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, DiagrammeR, DiagrammeRsvg, rsvg, dplyr, lubridate, MASS)
#Need to load packages
library(tidyverse)
library(odbc)
library(glue)
library(data.table)
# library(DiagrammeR)
# library(DiagrammeRsvg)
library(rsvg)
library(lubridate)

db_hhsaw <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = "tcp:kcitazrhpasqlprp16.azds.kingcounty.gov,1433",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryPassword")

# Pull data from SQL server ----

##Crisis events (From program codes, ITAs, ED visits----

##Join IDs to BH crisis events, ITAs, ED visits (from Medicaid)
crisis_events <- dbGetQuery(db_hhsaw,
                         "SELECT a.id_hudhears, a.gender_me, a.exit_date, a.exit_category, DATEADD(year, 1, a.exit_date) AS yr_after, b.crisis_date AS event_date, b.source
FROM
(SELECT id_hudhears, exit_date, gender_me, exit_category
  FROM hudhears.control_match_covariate 
  WHERE id_type = 'id_exit' AND exit_death = 0) a
LEFT JOIN
(SELECT DISTINCT id_hudhears, program, description, crisis_date, 'crisis' AS source
  FROM hudhears.bh_crisis_events WHERE crisis_date IS NOT NULL) b
ON a.id_hudhears = b.id_hudhears
AND b.crisis_date >= a.exit_date AND b.crisis_date <= DATEADD(year, 1, a.exit_date)
WHERE b.source IS NOT NULL


UNION

SELECT x.id_hudhears, x.gender_me, x.exit_date, x.exit_category, DATEADD(year, 1, x.exit_date) AS yr_after, y.call_date AS event_date, y.source
FROM
(SELECT id_hudhears, exit_date, gender_me, exit_category
  FROM hudhears.control_match_covariate 
  WHERE id_type = 'id_exit' AND exit_death = 0)  x
LEFT JOIN
(SELECT DISTINCT id_hudhears, call_date, 'ita' AS source FROM hudhears.bh_ita_events WHERE call_date IS NOT NULL) y
ON x.id_hudhears = y.id_hudhears
AND y.call_date >= x.exit_date AND y.call_date <= DATEADD(year, 1, x.exit_date)
WHERE y.source IS NOT NULL

UNION

SELECT z.id_hudhears, z.gender_me, z.exit_date, z.exit_category, DATEADD(year, 1, z.exit_date) AS yr_after, m.first_service_date AS event_date, m.source
FROM
(SELECT id_hudhears, exit_date, gender_me, exit_category
  FROM hudhears.control_match_covariate 
  WHERE id_type = 'id_exit' AND exit_death = 0) z
LEFT JOIN
(SELECT DISTINCT id_hudhears, id_mcaid FROM claims.hudhears_id_xwalk) aa
ON z.id_hudhears= aa.id_hudhears
LEFT JOIN
(SELECT DISTINCT id_mcaid, first_service_date, ed_bh, 'mcaid' AS source FROM claims.final_mcaid_claim_header WHERE ed_bh = 1) m
ON aa.id_mcaid = m.id_mcaid
AND m.first_service_date >= z.exit_date AND m.first_service_date <= DATEADD(year, 1, z.exit_date)
WHERE m.source IS NOT NULL
")

#Create two individual-level counts and indicators for crisis events
# First set DOES NOT include Medicaid events (crisis_any, crisis_num)
# Second DOES include Medicaid events (crisis_any_mcaid, crisis_num_mcaid)

crisis_count <- left_join(crisis_events %>% group_by(id_hudhears, exit_date) %>% summarize(crisis_num_mcaid=n_distinct(event_date)),
                          crisis_events %>% filter(source != "mcaid") %>% group_by(id_hudhears, exit_date) %>% summarize(crisis_num=n_distinct(event_date)),
                          by=c("id_hudhears", "exit_date")) %>% 
mutate(crisis_any_mcaid=ifelse(crisis_num_mcaid >=1, 1, 0), 
       crisis_any=ifelse(crisis_num >=1, 1, 0))

#### Repeat for BEFORE exit (not limiting to 1 year----
##Join IDs to BH crisis events, ITAs, ED visits (from Medicaid)
crisis_events_before <- dbGetQuery(db_hhsaw,
                            "SELECT a.id_hudhears, a.gender_me, a.exit_date, a.exit_category, b.crisis_date AS event_date, b.source
FROM
(SELECT id_hudhears, exit_date, gender_me, exit_category
  FROM hudhears.control_match_covariate 
  WHERE id_type = 'id_exit' AND exit_death = 0) a
LEFT JOIN
(SELECT DISTINCT id_hudhears, program, description, crisis_date, 'crisis' AS source
  FROM hudhears.bh_crisis_events WHERE crisis_date IS NOT NULL) b
ON a.id_hudhears = b.id_hudhears
AND b.crisis_date <= a.exit_date
WHERE b.source IS NOT NULL


UNION

SELECT x.id_hudhears, x.gender_me, x.exit_date, x.exit_category, y.call_date AS event_date, y.source
FROM
(SELECT id_hudhears, exit_date, gender_me, exit_category
  FROM hudhears.control_match_covariate 
  WHERE id_type = 'id_exit' AND exit_death = 0)  x
LEFT JOIN
(SELECT DISTINCT id_hudhears, call_date, 'ita' AS source FROM hudhears.bh_ita_events WHERE call_date IS NOT NULL) y
ON x.id_hudhears = y.id_hudhears
AND y.call_date <= x.exit_date
WHERE y.source IS NOT NULL

UNION

SELECT z.id_hudhears, z.gender_me, z.exit_date, z.exit_category, m.first_service_date AS event_date, m.source
FROM
(SELECT id_hudhears, exit_date, gender_me, exit_category
  FROM hudhears.control_match_covariate 
  WHERE id_type = 'id_exit' AND exit_death = 0) z
LEFT JOIN
(SELECT DISTINCT id_hudhears, id_mcaid FROM claims.hudhears_id_xwalk) aa
ON z.id_hudhears= aa.id_hudhears
LEFT JOIN
(SELECT DISTINCT id_mcaid, first_service_date, ed_bh, 'mcaid' AS source FROM claims.final_mcaid_claim_header WHERE ed_bh = 1) m
ON aa.id_mcaid = m.id_mcaid
AND m.first_service_date <= z.exit_date
WHERE m.source IS NOT NULL
")
#Create two individual-level counts and indicators for crisis events
# First set DOES NOT include Medicaid events (crisis_any_before, crisis_num_before)
# Second DOES include Medicaid events (crisis_any_mcaid_before, crisis_num_mcaid_before)

crisis_count_before <- left_join(crisis_events_before %>% group_by(id_hudhears, exit_date) %>% summarize(crisis_num_mcaid_before=n_distinct(event_date)),
                          crisis_events_before %>% filter(source != "mcaid") %>% group_by(id_hudhears, exit_date) %>% summarize(crisis_num_before=n_distinct(event_date)),
                          by=c("id_hudhears", "exit_date")) %>% 
  mutate(crisis_any_mcaid_before=ifelse(crisis_num_mcaid_before >=1, 1, 0), 
         crisis_any_before=ifelse(crisis_num_before >=1, 1, 0))



## Behavioral Health Conditions----

##Note: This is based on Medicaid data
bh_conditions <- dbGetQuery(db_hhsaw,
                            "SELECT cov.id_hudhears, cov.gender_me, cov.exit_date, cov.exit_category, bh.from_date, bh.bh_cond
FROM
(SELECT id_hudhears, exit_date, gender_me, exit_category
  FROM hudhears.control_match_covariate 
  WHERE id_type = 'id_exit' AND exit_death = 0) cov

LEFT JOIN

(SELECT DISTINCT id_hudhears, id_mcaid FROM claims.hudhears_id_xwalk) walk
ON cov.id_hudhears= walk.id_hudhears
LEFT JOIN
(SELECT DISTINCT id_mcaid, from_date, bh_cond
FROM claims.final_mcaid_claim_bh) bh
ON walk.id_mcaid =bh.id_mcaid
AND bh.from_date <= cov.exit_date
WHERE bh.from_date IS NOT NULL
")

#Number of conditions by person (N=6878 with at least 1 condition)--Only includes Medicaid conditions
condition_count_bh <- bh_conditions %>% group_by(id_hudhears, exit_date) %>% summarize(con_count=n_distinct(bh_conditions)) %>% mutate(any_cond=ifelse(con_count >=1, 1, 0))

## Outpatient visits ----
#Includes service minutes
#Condition considered "managed" if client meets service hours for "medium" need in that month
outpt_bh <- dbGetQuery(db_hhsaw,
                       "

SELECT z.id_hudhears, z.exit_date, z.exit_category, d.event_date_mth, 
d.event_date_yr, 'outpt' AS source, d.auth_no, d.program, d.event_date, d.service_minutes, d.description
FROM
(SELECT id_hudhears, exit_date, exit_category, MONTH(DATEADD(day, 1, exit_date)) AS month_after_exit
  FROM hudhears.control_match_covariate 
  WHERE id_type = 'id_exit' AND exit_death = 0) z
LEFT JOIN
(SELECT id_hudhears, auth_no, program, event_date, service_minutes, description, MONTH(event_date) AS event_date_mth, 
YEAR(event_date) AS event_date_yr FROM hudhears.bh_outpatient_events WHERE event_date IS NOT NULL) d
ON z.id_hudhears = d.id_hudhears
AND d.event_date <= z.exit_date AND d.event_date >= DATEADD(month, -13, z.exit_date)
WHERE d.event_date IS NOT NULL
")

# #All events are from 13 months before exit
# #No visits in 13 months before exit for these programs 
# 
outpt_hours <- outpt_bh %>% group_by(id_hudhears, event_date_mth, event_date_yr, auth_no) %>%
mutate(month_hrs=(sum(service_minutes, na.rm=T)/60))





##OLD CODE (Where number of outpatient visits is counted
#note- This is not dependent on Medicaid data

# #Count behavioral health outpatient visits per person and make indicator for people with 2+ visits
# #These variables group visits by auth number
# #reg_care2 is indicator for 2+ visits; out_count2 is count of outpt visits by auth number
# bh_outpt_count <- outpt_bh %>% group_by(id_hudhears, exit_date, auth_no) %>% 
#   summarize(out_count=n_distinct(event_date)) %>% group_by(id_hudhears, exit_date)%>%
#  mutate(out_count2=max(out_count, na.rm=T), 
#         reg_care2=ifelse(out_count2 >=2, 1, 0), 
#         reg_care=ifelse(out_count >=2, 1, 0)) %>% ungroup()





## Join all of the above with covariate table made by Alastair----

#Read in covariate table for exits only
control_match_covariate <- dbGetQuery(db_hhsaw, "SELECT *, DATEADD(year, -1, exit_date) as yr_before, DATEADD(year, 1, exit_date) AS yr_later FROM hudhears.control_match_covariate
                                                 WHERE id_type = 'id_exit' AND exit_death = 0
                                      ")

#Join control match covariate with NON MEDICAID CRISIS EVENTS----
all_pop <-left_join(control_match_covariate, crisis_count, by=c("id_hudhears", "exit_date"))

#fix number and indicator variable NOT INCLUDING Medicaid events (Set NAs to zero)
all_pop <- all_pop %>% mutate(crisis_any=ifelse(is.na(crisis_any), 0,crisis_any))%>% mutate(crisis_num=ifelse(is.na(crisis_num), 0,crisis_num))

#fix number and indicator variable for crises INCLUDING Medicaid events
#Only include individuals who have full coverage 7 months before and after exit, everyone else is set to NA
all_pop <- all_pop %>%
  mutate(crisis_any_mcaid = case_when(is.na(crisis_any_mcaid) & full_cov_7_prior == T & full_cov_7_after == T ~ 0L,
                                      full_cov_7_prior == F | full_cov_7_after == F ~ NA_integer_,
                                      TRUE ~ as.integer(crisis_any_mcaid)),
         crisis_num_mcaid = case_when(is.na(crisis_num_mcaid) ~ 0L,
                                      TRUE ~ as.integer(crisis_num_mcaid)))

####Add in crisis events before exit----
#Join with NON MEDICAID CRISIS EVENTS
all_pop <-left_join(all_pop, crisis_count_before, by=c("id_hudhears", "exit_date"))

#fix number and indicator variable NOT INCLUDING Medicaid events (Set NAs to zero)
all_pop <- all_pop %>% mutate(crisis_any_before=ifelse(is.na(crisis_any_before), 0,crisis_any_before))%>% mutate(crisis_num_before=ifelse(is.na(crisis_num_before), 0,crisis_num_before))

#fix number and indicator variable for crises INCLUDING Medicaid events
#Only include individuals who have full coverage 7 months before and after exit, everyone else is set to NA
all_pop <- all_pop %>%
  mutate(crisis_any_mcaid_before = case_when(is.na(crisis_any_mcaid_before) & full_cov_7_prior == T & full_cov_7_after == T ~ 0L,
                                      full_cov_7_prior == F | full_cov_7_after == F ~ NA_integer_,
                                      TRUE ~ as.integer(crisis_any_mcaid_before)),
         crisis_num_mcaid_before = case_when(is.na(crisis_num_mcaid_before) ~ 0L,
                                      TRUE ~ as.integer(crisis_num_mcaid_before)))

#Join control match covariate with BEHAVIORAL HEALTH CONDITIONS----
#fix number and indicator variable for BH conditions
#Only include individuals who have full coverage 7 months before and after exit, everyone else is set to NA
all_pop <-left_join(all_pop, condition_count_bh, by=c("id_hudhears", "exit_date"))
all_pop <- all_pop %>% 
  mutate(any_cond= case_when(is.na(any_cond) & full_cov_7_prior == T & full_cov_7_after == T ~ 0L,
                                      full_cov_7_prior == F | full_cov_7_after == F ~ NA_integer_,
                                      TRUE ~ as.integer(any_cond)),
         condition_count_bh = case_when(is.na(con_count) ~ 0L,
                                      TRUE ~ as.integer(con_count)))



#No longer using this code (due to change in definition of outpatient care)
# #Join control match covariate with BH OUTPATIENT visits----
# all_pop <-left_join(all_pop, bh_outpt_count, by=c("id_hudhears", "exit_date"))
# #fix number and indicator variables for NAs
# #Assume anyone without an entry has 0 outpatient visits because outpatient visits are not from Medicaid data
# # all_pop <- all_pop %>% mutate(out_count=ifelse(is.na(out_count), 0, out_count))%>% mutate(reg_care=ifelse(is.na(reg_care), 0, reg_care))