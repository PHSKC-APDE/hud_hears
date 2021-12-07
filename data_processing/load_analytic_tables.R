## Script name: load_analytic_tables
##
## Purpose of script: Use the ID linkage table to create analytic tables for the 
##   HUD HEARS Study
##
## Author: Alastair Matheson, Public Health - Seattle & King County
## Date Created: 2021-11-24
## Email: alastair.matheson@kingcounty.gov
##
## Notes:
##   
##

# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, lubridate, sqldf)

db_hhsaw <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = "tcp:kcitazrhpasqlprp16.azds.kingcounty.gov,1433",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryPassword")


# PHA ID XWALK ----
# Make a cross-walk view of id_hudhears and id_kc_pha and set a flag for people who
# had an exit

# First drop the existing table
try(DBI::dbRemoveTable(db_hhsaw, DBI::Id(schema = "hudhears", table = "pha_id_xwalk")))

dbExecute(db_hhsaw,
          "SELECT a.id_hudhears, a.id_kc_pha, b.true_exit 
          INTO hudhears.pha_id_xwalk
          FROM 
          (SELECT id_hudhears, id_kc_pha FROM amatheson.hudhears_xwalk_ids
          WHERE id_kc_pha IS NOT NULL) a 
          LEFT JOIN
          (SELECT DISTINCT id_kc_pha, true_exit FROM pha.stage_pha_exit_timevar
          WHERE true_exit = 1 AND possible_false_exit = 0) b
          ON a.id_kc_pha = b.id_kc_pha")


# MEDICAID ID XWALK ----
# Make a cross-walk view of id_hudhears and id_medicaid

# First drop the existing table
try(DBI::dbRemoveTable(db_hhsaw, DBI::Id(schema = "claims", table = "hudhears_id_xwalk")))

DBI::dbExecute(db_hhsaw,
          "SELECT DISTINCT id_hudhears, id_mcaid INTO claims.hudhears_id_xwalk
          FROM amatheson.hudhears_xwalk_ids
          WHERE id_mcaid IS NOT NULL")



# HOMELESS TABLE ----
# Find people in PHA who matched to the integrated data hub IDs and are in the 
# table that tracks homelessness status

# Try to drop first 
try(DBI::dbRemoveTable(db_hhsaw, DBI::Id(schema = "hudhears", table = "pha_homeless_status")))

dbExecute(db_hhsaw,
          "SELECT a.id_hudhears, h.start_date, h.housing_status, h.status_code, h.sourcesystemnm
          INTO hudhears.pha_homeless_status
          FROM
          (SELECT DISTINCT id_hudhears FROM amatheson.hudhears_xwalk_ids
            WHERE id_kc_pha IS NOT NULL) a
          INNER JOIN
          (SELECT DISTINCT id_hudhears, id_kcmaster FROM amatheson.hudhears_xwalk_ids
            WHERE id_kcmaster IS NOT NULL) b
          ON a.id_hudhears = b.id_hudhears
          INNER JOIN
          (SELECT DISTINCT masterclientkey, masterclientcd FROM dwhealth.v_client_xref_im) x
          ON b.id_kcmaster = x.masterclientcd
          INNER JOIN dwhealth.v_housing_status_im h
          ON x.masterclientkey = h.masterclientkey")


# NON-EXIT MATCHED TABLE ----
# Set up a set of people who did not exit to serve as controls for aim 2
# Need to do this before making the covariate table 

# Bring in complete pha_timevar_exit table and go from there
timevar_exit <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.stage_pha_exit_timevar")

# Set up list of IDs, exits, and dates
exits <- timevar_exit %>% 
  filter(true_exit == 1 & act_date >= "2012-01-01" & act_date <= "2018-12-31" & possible_false_exit == F) %>%
  distinct(id_kc_pha, act_date) %>%
  # Take the most recent exit for the ~260 people with multiple exits
  arrange(id_kc_pha, act_date) %>%
  group_by(id_kc_pha) %>%
  mutate(exit_cnt = n(), exit_order = row_number()) %>% ungroup()

# Set up a list of controls
# This will be everyone at first (including people who eventually exit) 
# but as controls are sampled they will be removed from this data frame
controls <- timevar_exit %>% select(id_kc_pha, from_date, to_date, period, true_exit, act_date) %>%
  # Add in the max date by a person's period in housing to make things easier
  group_by(id_kc_pha, period) %>%
  mutate(max_period_date = max(to_date)) %>%
  ungroup()

# For each exit, find a person in housing at the time of exit who did not exit for at 
# least 12 months following the exit date
match_f <- function(exit_id, exit_dt) {
  # Randomly select 4 controls
  set.seed(202198104)
  match <- controls %>%
    filter(exit_id != id_kc_pha & 
             exit_dt >= from_date & exit_dt <= to_date & 
             interval(start = exit_dt, end = max_period_date) / years(1) >= 1) %>%
    distinct(id_kc_pha) %>%
    sample_n(4)
  
  # Remove these controls from the list of future controls
  controls <<- anti_join(controls, match, by = "id_kc_pha")
  
  output <- match %>%
    mutate(id_exit = rep(exit_id, 4),
           exit_date = rep(exit_dt, 4),
           control_num = row_number()) %>%
    rename(id_control = id_kc_pha) %>%
    select(id_exit, exit_date, control_num, id_control)
  
  output
}

control_match <- map2(exits$id_kc_pha, exits$act_date, ~ match_f(exit_id = .x, exit_dt = .y))

# Add an exit ID so that controls can be linked up later
control_match <- control_match %>%
  bind_rows() %>%
  group_by(id_exit, exit_date) %>%
  mutate(exit_uid = cur_group_id()) %>%
  ungroup()


# Make long version of the data for easier joining
control_match_long <- control_match %>%
  select(-control_num) %>%
  pivot_longer(cols = starts_with("id"), names_to = "id_type", values_to = "id_kc_pha") %>%
  distinct()


# Add in id_hudhears
if (!exists("pha_id_xwalk")) {
  pha_id_xwalk <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.pha_id_xwalk") 
}

control_match_long <- control_match_long %>%
  left_join(., distinct(pha_id_xwalk, id_hudhears, id_kc_pha), by = "id_kc_pha") %>%
  select(id_hudhears, id_kc_pha, id_type, exit_uid, exit_date)

# Load to SQL
dbWriteTable(db_hhsaw, 
             name = DBI::Id(schema = "hudhears", table = "control_match"),
             value = control_match,
             overwrite = T)

dbWriteTable(db_hhsaw, 
             name = DBI::Id(schema = "hudhears", table = "control_match_long"),
             value = control_match_long,
             overwrite = T)




# COVARIATE TABLE (DON'T RUN UNTIL NON-EXIT TABLES MADE) ----
# NB: Do not set this table up until non-exiting controls are selected for aim 2
# (may also need to include a non-exiting comparison group for aim 3)

# Summarize all relevant demographics and other covariates for a person at the time of exit:
# Demographics: age, gender, race
# Housing: household size, household crowding, single caregiver, time in housing, history of unstable housing
# Mental health: Prior receipt of BH services
# Physical health: # ED visits in past 12 months, # hospitalizations in past 12 months, Medicaid coverage


## Pull in control_match table if needed ----
if (!exists("control_match_long")) {
  control_match_long <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.control_match_long")
}


## Household demographics ----





## Medicaid eligibility ----
# Find people who had 11 months of full coverage prior to/after the exit date

# First pull in all IDs in the exit/control table and join to Medicaid IDs
control_match_id_mcaid <- dbGetQuery(db_hhsaw, 
                                     "SELECT a.*, b.id_mcaid
                                   FROM
                                   (SELECT * FROM hudhears.control_match_long) a
                                   LEFT JOIN
                                   (SELECT DISTINCT id_hudhears, id_mcaid
                                     FROM claims.hudhears_id_xwalk) b
                                   ON a.id_hudhears = b.id_hudhears") %>%
  mutate(exit_1yr_prior = exit_date - years(1) + days(1),
         exit_1yr_after = exit_date + years(1) - days(1))

mcaid_elig <- claims::elig_timevar_collapse(db_hhsaw, server = "hhsaw", source = "mcaid",
                                            full_benefit = T, dual = T,
                                            ids = unique(control_match_id_mcaid$id_mcaid))


# Join and keep Mcaid rows that partially overlap in the past
# Use sqldf for non-equi join
mcaid_elig_prior <- sqldf("SELECT a.*, b.from_date, b.to_date, b.dual, b.full_benefit
                          FROM 
                          (SELECT * FROM control_match_id_mcaid) a
                          LEFT JOIN
                          (SELECT * FROM mcaid_elig) b
                          ON a.id_mcaid = b.id_mcaid AND 
                          (a.exit_date >= b.from_date AND a.exit_date <= b.to_date OR
                            a.exit_1yr_prior >= b.from_date AND a.exit_1yr_prior <= b.to_date)") %>%
  mutate(full_coverage = case_when(is.na(from_date) ~ 0,
                                   dual == 1 | full_benefit == 0 ~ 0,
                                   TRUE ~ intersect(interval(exit_1yr_prior, exit_date),
                                                    interval(from_date, to_date)) / ddays(1) + 1)) %>%
  group_by(id_hudhears, exit_date) %>%
  summarise(full_cov_days = sum(full_coverage)) %>%
  ungroup() %>%
  mutate(full_cov_prior = full_cov_days >= 11/12 * 365)


# Join and keep Mcaid rows that partially overlap in the past
# Use sqldf for non-equi join
mcaid_elig_after <- sqldf("SELECT a.*, b.from_date, b.to_date, b.dual, b.full_benefit
                          FROM 
                          (SELECT * FROM control_match_id_mcaid) a
                          LEFT JOIN
                          (SELECT * FROM mcaid_elig) b
                          ON a.id_mcaid = b.id_mcaid AND 
                          (a.exit_date >= b.from_date AND a.exit_date <= b.to_date OR
                            a.exit_1yr_after >= b.from_date AND a.exit_1yr_after <= b.to_date)") %>%
  mutate(full_coverage = case_when(is.na(from_date) ~ 0,
                                   dual == 1 | full_benefit == 0 ~ 0,
                                   TRUE ~ intersect(interval(exit_date, exit_1yr_after),
                                                    interval(from_date, to_date)) / ddays(1) + 1)) %>%
  group_by(id_hudhears, exit_date) %>%
  summarise(full_cov_days = sum(full_coverage)) %>%
  ungroup() %>%
  mutate(full_cov_after = full_cov_days >= 11/12 * 365)


## Medicaid ED visits and hospitalizations ----
mcaid_visits_prior <- dbGetQuery(db_hhsaw,
                           "SELECT x.id_hudhears, x.exit_date, 
                           COUNT(x.ed_pophealth_id) AS ed_cnt_prior, COUNT(x.inpatient_id) AS hosp_cnt_prior
                           FROM
                           (SELECT DISTINCT a.id_hudhears, a.exit_date, c.ed_pophealth_id, c.inpatient_id
                             FROM
                             (SELECT DISTINCT id_hudhears, exit_date 
                               FROM hudhears.control_match_long) a
                             LEFT JOIN
                             (SELECT DISTINCT id_hudhears, id_mcaid 
                               FROM claims.hudhears_id_xwalk) b
                             ON a.id_hudhears = b.id_hudhears
                             LEFT JOIN
                             (SELECT DISTINCT id_mcaid, first_service_date, ed_pophealth_id, inpatient_id
                               FROM claims.final_mcaid_claim_header
                               WHERE ed_pophealth_id is not null OR inpatient_id is not null) c
                             ON b.id_mcaid = c.id_mcaid AND
                             c.first_service_date <= a.exit_date AND 
                             c.first_service_date >= DATEADD(year, -1, a.exit_date)
                           ) x
                           GROUP BY x.id_hudhears, x.exit_date")


mcaid_visits_after <- dbGetQuery(db_hhsaw,
                                 "SELECT x.id_hudhears, x.exit_date, 
                           COUNT(x.ed_pophealth_id) AS ed_cnt_after, COUNT(x.inpatient_id) AS hosp_cnt_after
                           FROM
                           (SELECT DISTINCT a.id_hudhears, a.exit_date, c.ed_pophealth_id, c.inpatient_id
                             FROM
                             (SELECT DISTINCT id_hudhears, exit_date 
                               FROM hudhears.control_match_long) a
                             LEFT JOIN
                             (SELECT DISTINCT id_hudhears, id_mcaid 
                               FROM claims.hudhears_id_xwalk) b
                             ON a.id_hudhears = b.id_hudhears
                             LEFT JOIN
                             (SELECT DISTINCT id_mcaid, first_service_date, ed_pophealth_id, inpatient_id
                               FROM claims.final_mcaid_claim_header
                               WHERE ed_pophealth_id is not null OR inpatient_id is not null) c
                             ON b.id_mcaid = c.id_mcaid AND
                             c.first_service_date >= a.exit_date AND 
                             c.first_service_date <= DATEADD(year, 1, a.exit_date)
                           ) x
                           GROUP BY x.id_hudhears, x.exit_date")


## Final table ----
covariate <- control_match_long %>%
  left_join(., select(mcaid_elig_prior, id_hudhears, exit_date, full_cov_prior), 
            by = c("id_hudhears", "exit_date")) %>%
  left_join(., select(mcaid_elig_after, id_hudhears, exit_date, full_cov_after), 
            by = c("id_hudhears", "exit_date")) %>%
  left_join(., mcaid_visits_prior, by = c("id_hudhears", "exit_date")) %>%
  left_join(., mcaid_visits_after, by = c("id_hudhears", "exit_date")) 


