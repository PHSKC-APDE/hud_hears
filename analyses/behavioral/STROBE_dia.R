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


## Observations off by 300
# Do antijoin to see why this is happening

# mis_match<- anti_join(control_match_id_mcaid, control_match_covariate, by=c("id_hudhears"))
# mis_match2<- as.data.frame(mis_match)

#Several Medicaid IDs match to a single HUD HEARS ID. Will take first record


# control_match_covariate %>% select(id_hudhears, starts_with("full_")) %>% head() %>% mutate(chk = full_cov_7_prior * full_cov_7_after)

##Join covariate table to BH crisis events, ITAs, and outpatient events (all non-Medicaid)--then union these 3 new tables
covariate <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.control_match_covariate WHERE id_type = 'id_exit'")
exits_bh <- dbGetQuery(db_hhsaw,
                       "SELECT a.id_hudhears, a.gender_me, a.exit_date, NULL as yr_before, DATEADD(year, 1, a.exit_date) AS yr_later, b.crisis_date AS event_date, b.source
FROM
(SELECT id_hudhears, exit_date, gender_me 
  FROM hudhears.control_match_covariate 
  WHERE id_type = 'id_exit' AND exit_death = 0) a
LEFT JOIN
(SELECT DISTINCT id_hudhears, program, description, crisis_date, 'crisis' AS source
  FROM hudhears.bh_crisis_events) b
ON a.id_hudhears = b.id_hudhears
AND b.crisis_date >= a.exit_date AND b.crisis_date <= DATEADD(year, 1, a.exit_date)
WHERE b.crisis_date IS NOT NULL

UNION

SELECT x.id_hudhears, x.gender_me, x.exit_date, NULL as yr_before, DATEADD(year, 1, x.exit_date) AS yr_later, y.call_date AS event_date, y.source
FROM
(SELECT id_hudhears, exit_date, gender_me 
  FROM hudhears.control_match_covariate 
  WHERE id_type = 'id_exit' AND exit_death = 0)  x
LEFT JOIN
(SELECT DISTINCT id_hudhears, call_date, 'ita' AS source FROM hudhears.bh_ita_events) y
ON x.id_hudhears = y.id_hudhears
AND y.call_date >= x.exit_date AND y.call_date <= DATEADD(year, 1, x.exit_date)
WHERE y.call_date IS NOT NULL
UNION

SELECT z.id_hudhears, z.gender_me, z.exit_date, DATEADD(year, -1, z.exit_date) AS yr_before, NULL as yr_after, d.event_date AS event_date, d.source
FROM
(SELECT id_hudhears, exit_date, gender_me 
  FROM hudhears.control_match_covariate 
  WHERE id_type = 'id_exit' AND exit_death = 0) z
LEFT JOIN
(SELECT DISTINCT id_hudhears, event_date, 'outpt' AS source FROM hudhears.bh_outpatient_events) d
ON z.id_hudhears = d.id_hudhears
AND d.event_date <= z.exit_date AND d.event_date >= DATEADD(year, -1, z.exit_date)
WHERE d.event_date IS NOT NULL
")




#Get exit type variable from relevant table
#Note: only getting that one variable

pha_stage <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.stage_pha_exit_timevar WHERE act_date IS NOT NULL")

exits_bh_type <- left_join(exits_bh, distinct(pha_stage, id_hudhears, act_date, exit_category), by=c("id_hudhears", "exit_date"="act_date"))



#Summary Statistics
# Number of events of each type (can be multiple for single ID)
exits_bh %>% distinct() %>% count(source)
#Number of IDs with any event
exits_bh_type %>% filter(source %in% c("crisis", "ita"))%>%summarize(events=n_distinct (id_hudhears))
#Count of events, grouped by exit type and ID
exits_bh_type %>% filter(source %in% c("crisis", "ita"))%>% group_by(exit_category) %>% summarize(events=n_distinct (id_hudhears))%>% ungroup()
# exit_category events
# 1 Negative         212
# 2 Neutral          241
# 3 Positive          26


#Get denominators
exits_bh_type %>% group_by(exit_category) %>% summarize(n_distinct (id_hudhears))
# A tibble: 3 x 2
# exit_category `n_distinct(id_hudhears)`
# 1 Negative                           5896
# 2 Neutral                            8251
# 3 Positive                           3058


##Tabulate proportions
# exits_bh_type %>% group_by(exit_category) %>% mutate(totals=n_distinct (id_hudhears)) %>% filter(source %in% c("crisis", "ita"))%>% mutate(events=n_distinct (id_hudhears))%>% mutate(prop=events/totals)%>% count(prop)

# #Total numbers of events
# exit_category    prop     n
# 
# 1 Negative      0.0360    568
# 2 Neutral       0.0292    638
# 3 Positive      0.00850    97

#Make a data frame with number of events by person

bh_count <- exits_bh_type %>% group_by(id_hudhears, exit_date) %>% filter(source %in% c("outpt")) %>% count()
# summary(bh_count$n)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0     7.0    17.0    33.8    36.0   424.0 

# hist(bh_count$n)

#Make indicator for people with 2+ visits
bh_count$reg_care <-ifelse(bh_count$n > 1, 1, 0)

#Join this with other data frame
bh_routine <- left_join(exits_bh_type, distinct(bh_count, id_hudhears, exit_date, n, reg_care), by=c("id_hudhears", "exit_date")) 

#Summarize for all data
bh_routine %>% group_by(reg_care) %>% summarize(n_distinct(id_hudhears))
#Note that 174 were in the larger dataset who were duplicated IDs! (n=174)

#Make recoded version of variable (change NA to 0, by ID)
bh_routine$recode_reg_care <- replace(bh_routine$reg_care, is.na(bh_routine$reg_care),0)


#Look at exit category by receiving regular care
bh_routine %>% group_by(exit_category, recode_reg_care) %>% summarize(n_distinct (id_hudhears))








#################################################################################

#Attempts at flow chart
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
