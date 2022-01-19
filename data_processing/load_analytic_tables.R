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


# ID XWALK TABLES ----
# Claims/Medicaid ID xwalk ----
# Make a cross-walk view of id_hudhears and id_medicaid

# First drop the existing table
try(DBI::dbRemoveTable(db_hhsaw, DBI::Id(schema = "claims", table = "hudhears_id_xwalk")))

DBI::dbExecute(db_hhsaw,
               "SELECT DISTINCT id_hudhears, id_mcaid INTO claims.hudhears_id_xwalk
          FROM amatheson.hudhears_xwalk_ids
          WHERE id_mcaid IS NOT NULL")


## ESD ID xwalk ----
# First drop the existing table
try(DBI::dbRemoveTable(db_hhsaw, DBI::Id(schema = "esd", table = "hudhears_id_xwalk")))

DBI::dbExecute(db_hhsaw,
               "SELECT DISTINCT id_hudhears, customerkey INTO esd.hudhears_id_xwalk
               FROM amatheson.hudhears_xwalk_ids
               WHERE customerkey IS NOT NULL")


## PHA ID xwalk ----
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
          WHERE true_exit = 1) b
          ON a.id_kc_pha = b.id_kc_pha")


# NON-EXIT MATCHED TABLE ----
# Set up a set of people who did not exit to serve as controls for aim 2
# Need to do this before making the covariate table 

# Bring in complete pha_exit_timevar table and go from there
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


# Set up list of IDs, exits, and dates
exits <- exit_timevar %>% 
  filter(true_exit == 1 & 
           ((agency == "SHA" & act_date >= "2012-01-01" & act_date <= "2018-12-31") | 
              (agency == "KCHA" & act_date >= "2016-01-01" & act_date <= "2018-12-31"))) %>%
  distinct(id_kc_pha, act_date) %>%
  arrange(id_kc_pha, act_date) %>%
  # Take the most recent exit for the ~200 people with multiple exits
  group_by(id_kc_pha) %>%
  mutate(exit_cnt = n(), exit_order = row_number()) %>% ungroup() %>%
  filter(exit_cnt == exit_order)


# Set up a list of controls
# This will be everyone at first (including people who eventually exit) 
# but as controls are sampled they will be removed from this data frame
controls <- setDT(exit_timevar %>% select(id_kc_pha, from_date, to_date, period, true_exit, act_date))
  # Add in the max date by a person's period in housing to make things easier
controls[, max_period_date := max(to_date), by = .(id_kc_pha, period)]

# For each exit, find a person in housing at the time of exit who did not exit for at 
# least 12 months following the exit date
match_f <- function(exit_id, exit_dt) {
  # Randomly select 4 controls
  match <- setDT(controls)
  match <- match[exit_id != id_kc_pha & 
                   exit_dt >= from_date & exit_dt <= to_date & 
                   interval(start = exit_dt, end = max_period_date) / years(1) >= 1]
  match <- unique(match[, .(id_kc_pha)])
  
  set.seed(202198104)
  match <- match %>% sample_n(4)
  
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

system.time(control_match <- map2(exits$id_kc_pha, exits$act_date, ~ match_f(exit_id = .x, exit_dt = .y)))

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


# Add in id_hudhears and hh_id_kc_pha (makes joining easier later)
if (!exists("pha_id_xwalk")) {
  pha_id_xwalk <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.pha_id_xwalk") 
}

control_match_long <- control_match_long %>%
  left_join(., distinct(pha_id_xwalk, id_hudhears, id_kc_pha), by = "id_kc_pha") %>%
  select(id_hudhears, id_kc_pha, id_type, exit_uid, exit_date)


control_match_long <- sqldf("SELECT a.*, b.hh_id_kc_pha
               FROM
               (SELECT id_hudhears, id_kc_pha, id_type, exit_uid, exit_date
                 FROM control_match_long
                 WHERE id_type = 'id_exit') a
               LEFT JOIN
               (SELECT DISTINCT id_kc_pha, hh_id_kc_pha, act_date 
                 FROM exit_timevar) b
               ON a.id_kc_pha = b.id_kc_pha AND a.exit_date = b.act_date
               UNION 
               SELECT c.*, d.hh_id_kc_pha
               FROM
               (SELECT id_hudhears, id_kc_pha, id_type, exit_uid, exit_date
                 FROM control_match_long
                 WHERE id_type = 'id_control') c
               LEFT JOIN
               (SELECT DISTINCT id_kc_pha, hh_id_kc_pha, from_date, to_date 
                 FROM exit_timevar) d
               ON c.id_kc_pha = d.id_kc_pha AND 
               c.exit_date >= d.from_date AND c.exit_date <= d.to_date")


# Load to SQL
dbWriteTable(db_hhsaw, 
             name = DBI::Id(schema = "hudhears", table = "control_match"),
             value = control_match,
             overwrite = T)

dbWriteTable(db_hhsaw, 
             name = DBI::Id(schema = "hudhears", table = "control_match_long"),
             value = control_match_long,
             overwrite = T)

# Add indices
dbExecute(db_hhsaw, "CREATE CLUSTERED COLUMNSTORE INDEX hudhears_control_match_long_idx 
          ON hudhears.control_match_long")


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


## Demographics ----
demogs <- dbGetQuery(db_hhsaw,
                     "SELECT a.*, b.dob, b.admit_date, b.gender_me, b.race_latino,
                     b.race_me, b.race_eth_me
                     FROM
                     (SELECT id_hudhears, id_kc_pha, exit_date
                       FROM hudhears.control_match_long) a
                     LEFT JOIN
                     (SELECT id_kc_pha, dob, admit_date, gender_me, race_me, race_eth_me, race_latino
                       FROM pha.final_demo) b
                     ON a.id_kc_pha = b.id_kc_pha")

# Set up age and time in housing at exit 
demogs <- demogs %>%
  mutate(age_at_exit = floor(interval(start = dob, end = exit_date) / years(1)),
         housing_time_at_exit = floor(interval(start = admit_date, end = exit_date) / years(1)),
         race_eth_me = ifelse(race_latino == 1, "Latino", race_eth_me))


## Also use individual time-varying characteristics from pha.final_timevar because
#   not all heads of household are captured in the head of household table
#   (usually when a person was HoH for one day)
if (!exists("exit_timevar")) {
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
}


## Household demographics ----
# Join to pre-populated hh_demogs table
# This table summarizes households by week so need to join to the week prior to exit date
# However, need to get the hh_id_kc_pha for exits from the exit_timevar table because sometimes
# the exit is after a person's last to_date (and so wouldn't match in the normal timevar table)

hh_demogs <- dbGetQuery(db_hhsaw,
                        "SELECT a.*, b.agency, b.major_prog, b.subsidy_type, b.prog_type, 
                        b.operator_type, b.vouch_type_final, b.portfolio_final,
                        b.geo_tractce10, b.hh_size, b.hh_senior, b.hh_disability, 
                        b.n_child, b.n_adult, b.n_senior, b.n_disability, b.single_caregiver
                        FROM
                        (SELECT x.*, 
                          (SELECT TOP 1 date FROM pha.stage_hh_demogs_weekly y
                           WHERE y.hh_id_kc_pha = x.hh_id_kc_pha AND y.date <= x.exit_date
                           ORDER BY y.date desc) AS hh_demog_date		
                          FROM
                          (SELECT * FROM hudhears.control_match_long) x) a
                        LEFT JOIN
                        (SELECT hh_id_kc_pha, date, agency, major_prog, subsidy_type, 
                          prog_type, operator_type, vouch_type_final, portfolio_final,
                          geo_tractce10, hh_size, hh_senior, hh_disability, n_child, n_adult, n_senior, n_disability, single_caregiver
                          FROM pha.stage_hh_demogs_weekly) b
                        ON a.hh_id_kc_pha = b.hh_id_kc_pha AND a.hh_demog_date = b.date
                        order by a.exit_uid, a.id_type, a.id_kc_pha")


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
  mutate(exit_1yr_prior = case_when(month(exit_date) == 2 & day(exit_date) == 29 ~ exit_date + days(1) - years(1),
                                    TRUE ~ exit_date - years(1) + days(1)),
         exit_1yr_after = case_when(month(exit_date) == 2 & day(exit_date) == 29 ~ exit_date + days(1) + years(1),
                                    TRUE ~ exit_date + years(1) - days(1)))

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
  mutate(full_cov_11_prior = full_cov_days >= 11/12 * 365,
         full_cov_7_prior = full_cov_days >= 7/12 * 365)


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
  mutate(full_cov_11_after = full_cov_days >= 11/12 * 365,
         full_cov_7_after = full_cov_days >= 7/12 * 365)


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


## Medicaid chronic conditions ----
mcaid_ccw <- dbGetQuery(db_hhsaw,
                        "SELECT DISTINCT a.id_hudhears, a.exit_date, c.ccw_code, c.ccw_desc
                          FROM
                          (SELECT DISTINCT id_hudhears, exit_date 
                            FROM hudhears.control_match_long) a
                          LEFT JOIN
                          (SELECT DISTINCT id_hudhears, id_mcaid 
                            FROM claims.hudhears_id_xwalk) b
                          ON a.id_hudhears = b.id_hudhears
                          LEFT JOIN
                          (SELECT DISTINCT id_mcaid, from_date, ccw_code, ccw_desc
                            FROM claims.final_mcaid_claim_ccw) c
                          ON b.id_mcaid = c.id_mcaid AND
                          c.from_date <= a.exit_date")

mcaid_ccw_summ <- mcaid_ccw %>%
  group_by(id_hudhears, exit_date) %>%
  summarise(ccw_cnt = n_distinct(ccw_code, na.rm = T)) %>%
  ungroup() %>%
  mutate(ccw_cnt = ifelse(is.infinite(ccw_cnt), 0L, ccw_cnt),
         ccw_flag = ifelse(ccw_cnt >= 2, 1L, 0L))



## Medicaid well-child visits ----
wc_visits_prior <- dbGetQuery(db_hhsaw,
                                 "SELECT x.id_hudhears, x.exit_date, COUNT(x.wc_visit) AS wc_cnt_prior
                           FROM
                           (SELECT DISTINCT a.id_hudhears, a.exit_date, c.wc_visit
                             FROM
                             (SELECT DISTINCT id_hudhears, exit_date 
                               FROM hudhears.control_match_long) a
                             LEFT JOIN
                             (SELECT DISTINCT id_hudhears, id_mcaid 
                               FROM claims.hudhears_id_xwalk) b
                             ON a.id_hudhears = b.id_hudhears
                             LEFT JOIN
                             (SELECT DISTINCT id_mcaid, first_service_date, 1 AS wc_visit 
                               FROM claims.final_mcaid_claim_header
                               WHERE clm_type_mcaid_id = 27) c
                             ON b.id_mcaid = c.id_mcaid AND
                             c.first_service_date <= a.exit_date AND 
                             c.first_service_date >= DATEADD(year, -1, a.exit_date)
                           ) x
                           GROUP BY x.id_hudhears, x.exit_date")


wc_visits_after <- dbGetQuery(db_hhsaw,
                              "SELECT x.id_hudhears, x.exit_date, COUNT(x.wc_visit) AS wc_cnt_after
                           FROM
                           (SELECT DISTINCT a.id_hudhears, a.exit_date, c.wc_visit
                             FROM
                             (SELECT DISTINCT id_hudhears, exit_date 
                               FROM hudhears.control_match_long) a
                             LEFT JOIN
                             (SELECT DISTINCT id_hudhears, id_mcaid 
                               FROM claims.hudhears_id_xwalk) b
                             ON a.id_hudhears = b.id_hudhears
                             LEFT JOIN
                             (SELECT DISTINCT id_mcaid, first_service_date, 1 AS wc_visit 
                               FROM claims.final_mcaid_claim_header
                               WHERE clm_type_mcaid_id = 27) c
                             ON b.id_mcaid = c.id_mcaid AND
                             c.first_service_date >= a.exit_date AND 
                             c.first_service_date <= DATEADD(year, 1, a.exit_date)
                           ) x
                           GROUP BY x.id_hudhears, x.exit_date")



## Final table ----
covariate <- control_match_long %>%
  arrange(exit_uid, id_type, id_hudhears) %>%
  # Join demogs and hh_demog on id_kc_pha as well because there are two IDs that 
  # point to the same id_hudhears, possibly a false positive match
  left_join(., select(demogs, id_hudhears, id_kc_pha, exit_date, gender_me, race_eth_me, 
                      age_at_exit, housing_time_at_exit),
            by = c("id_hudhears", "id_kc_pha", "exit_date")) %>%
  left_join(., select(hh_demogs, id_hudhears, id_kc_pha, exit_date, hh_demog_date:single_caregiver),
            by = c("id_hudhears", "id_kc_pha", "exit_date")) %>%
  # Use individual-level time-varying demogs to fill in gaps in HH-level
  # Usually happens when an exiting person becomes their own HoH for a day or so
  left_join(., filter(exit_timevar, chooser == chooser_max) %>%
              select(id_hudhears, id_kc_pha, act_date, exit_reason_clean, agency, 
                      major_prog:vouch_type_final, portfolio_final, geo_tractce10),
            by = c("id_hudhears", "id_kc_pha", "exit_date" = "act_date")) %>%
  # Make flag for death exit, apply to controls as well
  mutate(exit_death = case_when(is.na(exit_reason_clean) ~ NA_integer_,
                                exit_reason_clean == "Deceased" ~ 1L,
                                TRUE ~ 0L)) %>%
  group_by(exit_uid) %>%
  mutate(exit_death = max(exit_death, na.rm = T)) %>%
  ungroup() %>%
  mutate(exit_death = ifelse(is.infinite(exit_death), NA_integer_, exit_death)) %>%
  mutate(agency = coalesce(agency.x, agency.y),
         major_prog = coalesce(major_prog.x, major_prog.y),
         subsidy_type = coalesce(subsidy_type.x, subsidy_type.y),
         prog_type = coalesce(prog_type.x, prog_type.y),
         operator_type = coalesce(operator_type.x, operator_type.y),
         vouch_type_final = coalesce(vouch_type_final.x, vouch_type_final.y),
         portfolio_final = coalesce(portfolio_final.x, portfolio_final.y),
         geo_tractce10 = coalesce(geo_tractce10.x, geo_tractce10.y)) %>%
  select(id_hudhears:exit_date, exit_death, gender_me:housing_time_at_exit, 
         hh_id_kc_pha, hh_demog_date, agency:geo_tractce10, hh_size:single_caregiver) %>%
  left_join(., select(mcaid_elig_prior, id_hudhears, exit_date, full_cov_11_prior, full_cov_7_prior), 
            by = c("id_hudhears", "exit_date")) %>%
  left_join(., select(mcaid_elig_after, id_hudhears, exit_date, full_cov_11_after, full_cov_7_after), 
            by = c("id_hudhears", "exit_date")) %>%
  left_join(., mcaid_visits_prior, by = c("id_hudhears", "exit_date")) %>%
  left_join(., mcaid_visits_after, by = c("id_hudhears", "exit_date")) %>%
  left_join(., mcaid_ccw_summ, by = c("id_hudhears", "exit_date")) %>%
  left_join(., wc_visits_prior, by = c("id_hudhears", "exit_date")) %>%
  left_join(., wc_visits_after, by = c("id_hudhears", "exit_date"))


# Load to SQL
dbWriteTable(db_hhsaw,
             name = DBI::Id(schema = "hudhears", table = "control_match_covariate"),
             value = covariate,
             overwrite = T)
