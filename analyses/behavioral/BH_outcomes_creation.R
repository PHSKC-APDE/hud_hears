# Code goal:
# Df with covariates for all participants who had a BH crisis event, ready for summary in tables
# #Code steps:
# 1)Summarize exclusions in data
# 2)Join outcome tables (crisis events, ITAs, ED hospitalizations)
# 3)Make indicator variables for main analysis
# 4)Get BH conditions and routine BH care tables
# 5)Join all tables above to covariate table
# 
#Last Update March 25, 2022 

# SET OPTIONS AND BRING IN PACKAGES ----
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

# ##Get N--all study IDs
# pha_id_xwalk <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[pha_id_xwalk]"))


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



# # Identify exits
# exit_timevar <- dbGetQuery(db_hhsaw,
#                            "SELECT a.*, c.geo_tractce10 FROM
#                            (SELECT * FROM pha.stage_pha_exit_timevar
#                            WHERE chooser = chooser_max) a
#                            LEFT JOIN
#                            (SELECT DISTINCT geo_hash_clean, geo_hash_geocode FROM ref.address_clean) b
#                            ON a.geo_hash_clean = b.geo_hash_clean
#                            LEFT JOIN
#                            (SELECT DISTINCT geo_hash_geocode, geo_tractce10 FROM ref.address_geocode) c
#                            ON b.geo_hash_geocode = c.geo_hash_geocode")

# # #Check number with multiple exits first (for housekeeping purposes)
# exits <- exit_timevar %>%
#   filter(true_exit == 1 &
#            ((agency == "SHA" & act_date >= "2012-01-01" & act_date <= "2018-12-31") |
#               (agency == "KCHA" & act_date >= "2016-01-01" & act_date <= "2018-12-31"))) %>%
#   distinct(id_kc_pha, act_date) %>%
#   arrange(id_kc_pha, act_date) %>% group_by(id_kc_pha) %>% mutate(exit_cnt = n())
# exits %>% group_by(id_kc_pha)%>% table(exit_cnt)


# totals <- exits %>% group_by(id_kc_pha)%>% summarize(Count=n())
# table(totals$Count)
# 1     2     3
# 18929   393     7


# length(unique(exits$id_kc_pha))#19329 unique IDs
# #Excluding 19736-19329=407 exits that were duplicates


# ##Write over this with correct code (or just run this part)
# # Set up list of IDs, exits, and dates
# exits <- exit_timevar %>% 
#   filter(true_exit == 1 & 
#            ((agency == "SHA" & act_date >= "2012-01-01" & act_date <= "2018-12-31") | 
#               (agency == "KCHA" & act_date >= "2016-01-01" & act_date <= "2018-12-31"))) %>%
#   distinct(id_kc_pha, act_date) %>%
#   arrange(id_kc_pha, act_date) %>%
# # Take the most recent exit for the 400 people with multiple exits
#   group_by(id_kc_pha) %>%
#   mutate(exit_cnt = n(), exit_order = row_number()) %>% ungroup() %>%
#   filter(exit_cnt == exit_order)
# 
# # Note: Control matching was not rerun due to long run time. 
# #Read in pre-generated table of exits and matched controls
# control_match_long <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[control_match_long]"))
# #Tabulate Ids to check number of matched controls
# table(control_match_long$id_type)
# # id_control    id_exit 
# # 76936      19234 
# 
# #19234 vs. 19329

# #Read in table with covariates
# control_match_covariate <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[control_match_covariate]"))
# table(control_match_covariate$id_type)
# # id_control    id_exit 
# # 76936      19234 
# 
# ##join this table with medicaid data to get n's
# 
# ##Left join this table with Medicaid ID table
# control_match_id_mcaid <- dbGetQuery(db_hhsaw,
#                                      "SELECT a.*, b.id_mcaid
#                                      FROM
# (SELECT * FROM hudhears.control_match_long) a
# LEFT JOIN
# (SELECT DISTINCT id_hudhears, id_mcaid
# FROM claims.hudhears_id_xwalk WHERE id_mcaid is not null) b
# ON a.id_hudhears = b.id_hudhears") %>%
#   mutate(exit_1yr_prior = exit_date - years(1) + days(1),
#          exit_1yr_after = exit_date + years(1) - days(1))


## Observations off by 300
# Do antijoin to see why this is happening

# mis_match<- anti_join(control_match_id_mcaid, control_match_covariate, by=c("id_hudhears"))
# mis_match2<- as.data.frame(mis_match)

#Several Medicaid IDs match to a single HUD HEARS ID. Will take first record


# control_match_covariate %>% select(id_hudhears, starts_with("full_")) %>% head() %>% mutate(chk = full_cov_7_prior * full_cov_7_after)


################################################################################
# INDIVIDUALS WITH THE OUTCOME (i.e. at least 1 crisis event within 1 year of exit)
################################################################################

##Join covariate table to BH crisis events, ITAs, ED visits
crisis_pop <- dbGetQuery(db_hhsaw,
                       "SELECT a.id_hudhears, a.gender_me, a.exit_date, a.exit_category, NULL as yr_before, DATEADD(year, 1, a.exit_date) AS yr_later, b.crisis_date AS event_date, b.source
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

SELECT x.id_hudhears, x.gender_me, x.exit_date, x.exit_category, NULL as yr_before, DATEADD(year, 1, x.exit_date) AS yr_later, y.call_date AS event_date, y.source
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

SELECT z.id_hudhears, z.gender_me, z.exit_date, z.exit_category, DATEADD(year, -1, z.exit_date) AS yr_before, NULL as yr_after, m.first_service_date AS event_date, m.source
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
AND m.first_service_date <= z.exit_date AND m.first_service_date >= DATEADD(year, -1, z.exit_date)
WHERE m.source IS NOT NULL
")

outpt_bh <- dbGetQuery(db_hhsaw,
                       "

SELECT z.id_hudhears, z.gender_me, z.exit_date, z.exit_category, DATEADD(year, -1, z.exit_date) AS yr_before, NULL as yr_after, d.event_date AS event_date, d.source
FROM
(SELECT id_hudhears, exit_date, gender_me, exit_category
  FROM hudhears.control_match_covariate 
  WHERE id_type = 'id_exit' AND exit_death = 0) z
LEFT JOIN
(SELECT DISTINCT id_hudhears, event_date, 'outpt' AS source FROM hudhears.bh_outpatient_events WHERE event_date IS NOT NULL) d
ON z.id_hudhears = d.id_hudhears
AND d.event_date <= z.exit_date AND d.event_date >= DATEADD(year, -1, z.exit_date)

")



#Summary Statistics (of crisis events)
# Number of events of each type (can be multiple for single ID)
crisis_pop %>% distinct() %>% count(source)
#Number of IDs with any event
crisis_pop %>% summarize(events=n_distinct (id_hudhears))
#Count of events, grouped by exit type and ID
crisis_pop %>% group_by(exit_category) %>% summarize(events=n_distinct (id_hudhears))%>% ungroup()
# exit_category events
# 
# 1 Negative         721
# 2 Neutral          828
# 3 Positive         106


##Make crisis event indicator variable
#Filter out people with any crisis event, make var of number of events
crisis_count <- crisis_pop %>% group_by(id_hudhears, exit_date) %>% summarize(crisis_num=n_distinct(event_date))

#Code indicator variable (Note everyone here should have a value of 1)
crisis_count$crisis_any <-ifelse(crisis_count$crisis_num >=1, 1, 0)
#table(crisis_count$crisis_any) #Confirmed that it worked

##Use this code later when joining with other table
#Recode variable (change NA to 0, by ID)
# crisis_pop <- crisis_pop %>% mutate(crisis_any=ifelse(is.na(crisis_any), 0,crisis_any)) 

#Join this with other data frame
crisis_pop <- left_join(crisis_pop, distinct(crisis_count, id_hudhears, exit_date, crisis_num, crisis_any), by=c("id_hudhears", "exit_date")) 

########
###OUTPATIENT EVENTS
#Make a data frame with number of events by person (note: includes people with and without outpatient events
bh_outpt_count <- outpt_bh %>% group_by(id_hudhears, exit_date) %>% summarize(out_count=n_distinct(event_date))


#Make indicator for people with 2+ visits
bh_outpt_count$reg_care <-ifelse(bh_outpt_count$out_count >=2, 1, 0)

#This will be used later when joining with the larger table to include people without outpt events
# #Recode variable (change NA to 0, by ID)
# bh_outpt_count <- bh_outpt_count %>% mutate(reg_care=ifelse(is.na(reg_care), 0,reg_care)) 

#Check whether this worked
bh_outpt_count %>% group_by(reg_care) %>% summarise_at(vars(out_count),list(name =max))

#Join this with other data frame
crisis_pop <- left_join(crisis_pop, distinct(bh_outpt_count, id_hudhears, exit_date, out_count, reg_care), by=c("id_hudhears", "exit_date")) 

# #Summarize for all data (including people without crisis event)
bh_outpt_count %>% group_by(reg_care) %>% summarize(n_distinct(id_hudhears))
# #Note that 174 were in the larger dataset who were duplicated IDs! (n=174)

#Look at exit category by receiving regular care
crisis_pop %>% group_by(exit_category, reg_care) %>% summarize(n_distinct (id_hudhears))

#Look at regular care by crisis event and exit category
crisis_pop %>% group_by(reg_care, crisis_any) %>% summarize(n_distinct (id_hudhears))
# reg_care crisis_any `n_distinct(id_hudhears)`
# 1        0          1                       804
# 2        1          1                       845

##Get table of behavioral health conditions
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

#Number of conditions by person (N=6878 with at least 1 condition)
conditions_count_bh <- bh_conditions %>% group_by(id_hudhears, exit_date) %>% summarize(con_count=n_distinct(bh_cond))
#Make indicator for people with 2+ visits
conditions_count_bh$any_cond <-ifelse(conditions_count_bh$con_count >=1, 1, 0)

##Joining this with crisis_pop
crisis_pop <- left_join(crisis_pop, conditions_count_bh, by=c("id_hudhears", "exit_date"))

#Change NAs for BH conditions variables
crisis_pop <- crisis_pop %>% mutate(con_count=ifelse(is.na(con_count), 0, con_count)) 
crisis_pop <- crisis_pop %>% mutate(any_cond=ifelse(is.na(any_cond), 0,any_cond)) 


##Join with covariate table, make two separate dfs of each population (ones with and without exits)
control_match_covariate <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[control_match_covariate]"))

all_pop <- left_join(control_match_covariate, select(crisis_pop, -gender_me, -exit_category), by=c("id_hudhears", "exit_date"))


##Data frame that includes all people with crisis events
all_pop <- all_pop %>% group_by(id_hudhears) %>% mutate(crisis_any=ifelse(is.na(crisis_any), 0,crisis_any)) 
all_pop <- all_pop %>% group_by(id_hudhears) %>% mutate(crisis_count=ifelse(is.na(crisis_count), 0, crisis_count))
                                                        
all_pop <- all_pop %>% ungroup %>% left_join(all_pop, conditions_count, by=c("id_hudhears", "exit_date"))
all_pop <- all_pop %>% left_join(all_pop, bh_oupt_count, by=c("id_hudhears", "exit_date"))
# #Attempts at flow chart
# library(DiagrammeR)
# grViz("digraph flowchart {
#       # node definitions with substituted label text
#       node [fontname = Helvetica, shape = rectangle]        
#       tab1 [label = '@@1']
#       tab2 [label = '@@2']
#       tab3 [label = '@@3']
#       tab4 [label = '@@4']
#       tab5 [label = '@@5']
# 
#       # edge definitions with the node IDs
#       tab1 -> tab2 
#       tab1 -> tab 3 
#       -> tab3 -> tab4 -> tab5;
#       }
# 
#       [1]: 'PHA Clients n=312,397'
#       [2]: 'Exit from public housing n=38,961'
#       [3]: 'No exit from public housing '
#       [4]: 'Participants eligible for the study n=600'
#       [5]: 'Study sample n=600'
#       ")
