# Code updated on 03/29/2022
# 
# #Make data frame for HUD HEARS Analysis
# 
# 1) Build crisis event outcome table
# 2) Build BH regular care table
# 3) Build BH conditions table
# 4) Join the above to full covariates table (for exits only)



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

##Crisis events

##Join IDs to BH crisis events, ITAs, ED visits
crisis_events <- dbGetQuery(db_hhsaw,
                         "SELECT a.id_hudhears, a.gender_me, a.exit_date, a.exit_category, NULL as yr_before, DATEADD(year, 1, a.exit_date) AS yr_after, b.crisis_date AS event_date, b.source
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

SELECT x.id_hudhears, x.gender_me, x.exit_date, x.exit_category, NULL as yr_before, DATEADD(year, 1, x.exit_date) AS yr_after, y.call_date AS event_date, y.source
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

crisis_count <- crisis_events %>% group_by(id_hudhears, exit_date) %>% summarize(crisis_num=n_distinct(event_date))
#Code indicator variable (Note everyone here should have a value of 1)
crisis_count$crisis_any <-ifelse(crisis_count$crisis_num >=1, 1, 0)


#Outpatient care
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
WHERE d.source IS NOT NULL
")

##Events per person
bh_outpt_count <- outpt_bh %>% group_by(id_hudhears, exit_date) %>% summarize(out_count=n_distinct(event_date))

#Make indicator for people with 2+ visits
bh_outpt_count$reg_care <-ifelse(bh_outpt_count$out_count >=2, 1, 0)


#####Behavioral Health Conditions
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
condition_count_bh <- bh_conditions %>% group_by(id_hudhears, exit_date) %>% summarize(con_count=n_distinct(bh_cond))
condition_count_bh$any_condition <-ifelse(condition_count_bh$con_count >=1, 1, 0)


#####################################

##Join all of the above with covariate table##
#Read in covariate table for exits only
control_match_covariate <- dbGetQuery(db_hhsaw, "SELECT *, DATEADD(year, -1, exit_date) as yr_before, DATEADD(year, 1, exit_date) AS yr_later FROM hudhears.control_match_covariate
                                                 WHERE id_type = 'id_exit' AND exit_death = 0
                                      ")

#Join control match covariate with CRISIS EVENTS
all_pop <-left_join(control_match_covariate, crisis_count, by=c("id_hudhears", "exit_date"))
#fix number and indicator variables for NAs
all_pop <- all_pop %>% mutate(crisis_any=ifelse(is.na(crisis_any), 0,crisis_any))%>% mutate(crisis_num=ifelse(is.na(crisis_num), 0,crisis_num))


#Join control match covariate with BH OUTPATIENT visits
all_pop <-left_join(all_pop, bh_outpt_count, by=c("id_hudhears", "exit_date"))
#fix number and indicator variables for NAs
all_pop <- all_pop %>% mutate(out_count=ifelse(is.na(out_count), 0, out_count))%>% mutate(reg_care=ifelse(is.na(reg_care), 0, reg_care))

#Join control match covariate with BEHAVIORAL HEALTH CONDITIONS
all_pop <-left_join(all_pop, condition_count_bh, by=c("id_hudhears", "exit_date"))
#fix number and indicator variables for NAs
all_pop <- all_pop %>% mutate(con_count=ifelse(is.na(con_count), 0, con_count))%>% mutate(any_condition=ifelse(is.na(any_condition), 0, any_condition))




#This will be used later when joining with the larger table to include people without outpt events
# #Recode variable (change NA to 0, by ID)
# bh_outpt_count <- bh_outpt_count %>% mutate(reg_care=ifelse(is.na(reg_care), 0,reg_care)) 


#Make indicator for any condition
condition_count_bh$any_cond <-ifelse(condition_count_bh$con_count >=1, 1, 0)


##Run preliminary model (unadjusted)
model1 <- glm(crisis_any ~ exit_category*reg_care + reg_care + gender_me + age_at_exit + race_eth_me  + hh_size + any_condition, family="binomial", data=all_pop)
exp(cbind(OR = coef(model1), confint(model1)))