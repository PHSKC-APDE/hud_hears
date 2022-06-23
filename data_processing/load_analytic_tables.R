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
                           Authentication = "ActiveDirectoryInteractive")


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
          (SELECT DISTINCT id_hudhears, id_kc_pha FROM amatheson.hudhears_xwalk_ids
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
  filter(true_exit == 1 & exit_type_keep == 1 & 
           exit_order_study == exit_order_max_study & !is.na(exit_order_study)) %>%
  distinct(id_kc_pha, act_date) %>%
  arrange(id_kc_pha, act_date) %>%
  # Take the most recent exit for the ~200 people with multiple exits
  group_by(id_kc_pha) %>%
  mutate(exit_cnt = n(), exit_order = row_number()) %>% ungroup() %>%
  filter(exit_cnt == exit_order)


# Set up a list of controls
# This will be everyone at first (including people who eventually exit) 
# but as controls are sampled they will be removed from this data frame
controls <- setDT(exit_timevar %>% distinct(id_kc_pha, from_date, to_date, period, true_exit, act_date))
  # Add in the max date by a person's period in housing to make things easier
controls[, max_period_date := max(to_date), by = .(id_kc_pha, period)]

# For each exit, find a person in housing at the time of exit who did not exit for at 
# least 12 months following the exit date
match_f <- function(exit_id, exit_dt, n_control = 4) {
  # Randomly select 4 controls
  match <- setDT(controls)
  match <- match[exit_id != id_kc_pha & 
                   exit_dt >= from_date & exit_dt <= to_date & 
                   interval(start = exit_dt, end = max_period_date) / years(1) >= 1]
  match <- unique(match[, .(id_kc_pha)])
  
  if (nrow(match) > 0) {
    set.seed(202198104)
    match <- match %>% slice_sample(n = n_control, replace = T)  
    
    # Remove these controls from the list of future controls
    controls <<- anti_join(controls, match, by = "id_kc_pha")
    
    output <- match %>%
      mutate(id_exit = rep(exit_id, n_control),
             exit_date = rep(exit_dt, n_control),
             control_num = row_number()) %>%
      rename(id_control = id_kc_pha) %>%
      select(id_exit, exit_date, control_num, id_control)
  } else {
    output <- data.frame(id_exit = exit_id,
                         exit_date = exit_dt,
                         control_num = 1,
                         id_control = "no_match")
  }
  
  output
}

system.time(control_match <- map2(exits$id_kc_pha, exits$act_date, ~ match_f(exit_id = .x, exit_dt = .y)))

# Add an exit ID so that controls can be linked up later
control_match <- control_match %>%
  bind_rows() %>%
  distinct() %>%
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



## Repeat but only at head of household level ----
# Decided that it makes most sense for the exit vs not analysis to restrict to 
# head of household, given that the exit reason most directly applies to them
# (other HH members may exit but the reason is not recorded and other HH 
# members may remain in housing because the exit reason only impacts the HoH)
# Set up list of IDs, exits, and dates
exits_hh <- exit_timevar %>% 
  filter(true_exit == 1 & exit_type_keep == 1 & 
           exit_order_study == exit_order_max_study & !is.na(exit_order_study) &
           id_kc_pha == hh_id_kc_pha) %>%
  distinct(id_kc_pha, act_date) %>%
  arrange(id_kc_pha, act_date) %>%
  # Take the most recent exit for the ~200 people with multiple exits
  group_by(id_kc_pha) %>%
  mutate(exit_cnt = n(), exit_order = row_number()) %>% ungroup() %>%
  filter(exit_cnt == exit_order)

controls <- setDT(exit_timevar %>% 
                    filter(id_kc_pha == hh_id_kc_pha) %>%
                    distinct(id_kc_pha, from_date, to_date, period, true_exit, act_date))
# Add in the max date by a person's period in housing to make things easier
controls[, max_period_date := max(to_date), by = .(id_kc_pha, period)]

system.time(control_match_hh <- map2(exits_hh$id_kc_pha, exits_hh$act_date, 
                                     ~ match_f(exit_id = .x, exit_dt = .y, n_control = 3)))


# Add an exit ID so that controls can be linked up later
control_match_hh <- control_match_hh %>%
  bind_rows() %>%
  distinct() %>%
  group_by(id_exit, exit_date) %>%
  mutate(exit_uid = cur_group_id()) %>%
  ungroup()


# Make long version of the data for easier joining
control_match_hh_long <- control_match_hh %>%
  select(-control_num) %>%
  pivot_longer(cols = starts_with("id"), names_to = "id_type", values_to = "id_kc_pha") %>%
  distinct()


# Add in id_hudhears and hh_id_kc_pha (makes joining easier later)
if (!exists("pha_id_xwalk")) {
  pha_id_xwalk <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.pha_id_xwalk") 
}

control_match_hh_long <- control_match_hh_long %>%
  left_join(., distinct(pha_id_xwalk, id_hudhears, id_kc_pha), by = "id_kc_pha") %>%
  select(id_hudhears, id_kc_pha, id_type, exit_uid, exit_date) %>%
  filter(id_kc_pha != "no_match") %>%
  mutate(hh_id_kc_pha = id_kc_pha,
         level = "hh")


# Load to SQL
dbWriteTable(db_hhsaw, 
             name = DBI::Id(schema = "hudhears", table = "control_match_hh"),
             value = control_match_hh,
             overwrite = T)

dbWriteTable(db_hhsaw, 
             name = DBI::Id(schema = "hudhears", table = "control_match_hh_long"),
             value = control_match_hh_long,
             overwrite = T)

# Add indices
dbExecute(db_hhsaw, "CREATE CLUSTERED COLUMNSTORE INDEX hudhears_control_match_hh_long_idx 
          ON hudhears.control_match_hh_long")


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

# Most efficient to do joins etc on a union of individual- and hh-level tables and then
# split into each control_match_covariate table at the end (based on 'level' variable)
# Note, for some _hh-level tables, need to look at everyone in hhold to create a flag
# to indicate if anyone in the hhold had that (specifically: BH crisis, Mcaid, prior homelessness)


## Pull in control_match table if needed ----
if (!exists("control_match_long")) {
  control_match_long <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.control_match_long")
}
if (!exists("control_match_hh_long")) {
  control_match_hh_long <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.control_match_hh_long")
}


## Demographics ----
demogs <- dbGetQuery(db_hhsaw,
                     "SELECT a.*, b.dob, b.admit_date_all, b.admit_date_kcha, b.admit_date_sha, 
                     b.gender_me, b.race_latino, b.race_me, b.race_eth_me
                     FROM
                     (SELECT level, id_hudhears, id_kc_pha, exit_date
                       FROM hudhears.control_match_long) a
                     LEFT JOIN
                     (SELECT id_kc_pha, dob, admit_date_all, admit_date_kcha, admit_date_sha, 
                     gender_me, race_me, race_eth_me, race_latino
                       FROM pha.final_demo) b
                     ON a.id_kc_pha = b.id_kc_pha
                     
                     UNION
                     
                     SELECT y.*, z.dob, z.admit_date_all, z.admit_date_kcha, z.admit_date_sha, 
                     z.gender_me, z.race_latino, z.race_me, z.race_eth_me
                     FROM
                     (SELECT level, id_hudhears, id_kc_pha, exit_date
                       FROM hudhears.control_match_hh_long) y
                     LEFT JOIN
                     (SELECT id_kc_pha, dob, admit_date_all, admit_date_kcha, admit_date_sha, 
                     gender_me, race_me, race_eth_me, race_latino
                       FROM pha.final_demo) z
                     ON y.id_kc_pha = z.id_kc_pha
                     ")

# Set up age and consolidated race/eth 
demogs <- demogs %>%
  mutate(age_at_exit = floor(interval(start = dob, end = exit_date) / years(1)),
         race_eth_me = ifelse(race_latino == 1, "Latino", race_eth_me))


## Also use individual time-varying characteristics from pha.final_timevar (via pha_exit_timevar) because
#   not all heads of household are captured in the head of household table
#   (usually when a person was HoH for one day)
if (!exists("exit_timevar")) {
  exit_timevar <- dbGetQuery(db_hhsaw, 
                             "SELECT a.*, d.geo_tractce10 FROM
                           (SELECT * FROM pha.stage_pha_exit_timevar
                           WHERE chooser = chooser_max) a
                           LEFT JOIN
                           (SELECT DISTINCT geo_hash_clean, geo_hash_geocode FROM ref.address_clean) c
                           ON a.geo_hash_clean = c.geo_hash_clean
                           LEFT JOIN
                           (SELECT DISTINCT geo_hash_geocode, geo_tractce10 FROM ref.address_geocode) d
                           ON c.geo_hash_geocode = d.geo_hash_geocode
                             ")
}


## Household demographics ----
# Join to pre-populated hh_demogs table
# This table summarizes households by week so need to join to the week prior to exit date
# However, need to get the hh_id_kc_pha for exits from the exit_timevar table because sometimes
# the exit is after a person's last to_date (and so wouldn't match in the normal timevar table)

hh_demogs_ind <- dbGetQuery(db_hhsaw,
                        "SELECT a.*, b.agency, b.major_prog, b.subsidy_type, b.prog_type, 
                        b.operator_type, b.vouch_type_final, b.portfolio_final,
                        b.geo_tractce10, b.hh_size, b.hh_senior, b.hh_disability, 
                        b.n_child, b.n_adult, b.n_senior, b.n_disability, b.single_caregiver, 
                        b.housing_time_at_exit
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
                          geo_tractce10, hh_size, hh_senior, hh_disability, n_child, 
                          n_adult, n_senior, n_disability, single_caregiver, 
                          length_period AS housing_time_at_exit
                          FROM pha.stage_hh_demogs_weekly) b
                        ON a.hh_id_kc_pha = b.hh_id_kc_pha AND a.hh_demog_date = b.date
                        order by a.exit_uid, a.id_type, a.id_kc_pha")

# Because the controls are different for the _hh data, need to run twice
hh_demogs_hh <- dbGetQuery(db_hhsaw,
                        "SELECT a.*, b.agency, b.major_prog, b.subsidy_type, b.prog_type, 
                        b.operator_type, b.vouch_type_final, b.portfolio_final,
                        b.geo_tractce10, b.hh_size, b.hh_senior, b.hh_disability, 
                        b.n_child, b.n_adult, b.n_senior, b.n_disability, b.single_caregiver, 
                        b.housing_time_at_exit
                        FROM
                        (SELECT x.*, 
                          (SELECT TOP 1 date FROM pha.stage_hh_demogs_weekly y
                           WHERE y.hh_id_kc_pha = x.hh_id_kc_pha AND y.date <= x.exit_date
                           ORDER BY y.date desc) AS hh_demog_date		
                          FROM
                          (SELECT * FROM hudhears.control_match_hh_long) x) a
                        LEFT JOIN
                        (SELECT hh_id_kc_pha, date, agency, major_prog, subsidy_type, 
                          prog_type, operator_type, vouch_type_final, portfolio_final,
                          geo_tractce10, hh_size, hh_senior, hh_disability, n_child, 
                          n_adult, n_senior, n_disability, single_caregiver, 
                          length_period AS housing_time_at_exit
                          FROM pha.stage_hh_demogs_weekly) b
                        ON a.hh_id_kc_pha = b.hh_id_kc_pha AND a.hh_demog_date = b.date
                        order by a.exit_uid, a.id_type, a.id_kc_pha")

hh_demogs <- bind_rows(hh_demogs_ind, hh_demogs_hh)


## Medicaid eligibility ----
# Find people who had 11 months of full coverage prior to/after the exit date

# First pull in all IDs in the exit/control table and join to Medicaid IDs
# For _hh-level, need to join to timevar to get all hhold members
control_match_id_mcaid <- dbGetQuery(db_hhsaw, 
                                     "SELECT a.*, b.id_mcaid
                                   FROM
                                   -- Individual-level exits and controls
                                   (SELECT * FROM hudhears.control_match_long 
                                   UNION
                                   -- Heads of households exits and controls
                                   SELECT * FROM hudhears.control_match_hh_long
                                   UNION
                                   -- Other household members for HoH level
                                    (SELECT y.id_hudhears, y.id_kc_pha, x.id_type,
                                      x.exit_uid, x.exit_date, x.hh_id_kc_pha, x.level
                                      FROM
                                      (SELECT * FROM hudhears.control_match_hh_long) x
                                      INNER JOIN
                                      (SELECT DISTINCT id_hudhears, id_kc_pha, 
                                              hh_id_kc_pha, from_date, to_date
                                      FROM pha.stage_pha_exit_timevar) y
                                      ON x.hh_id_kc_pha = y.hh_id_kc_pha AND
                                        x.exit_date >= y.from_date AND x.exit_date <= y.to_date AND
                                        x.id_kc_pha <> y.id_kc_pha)
                                    ) a
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
                                            ids = unique(control_match_id_mcaid$id_mcaid[!is.na(control_match_id_mcaid$id_mcaid)]))

# Join and count overlapping time with non-dual full coverage
mcaid_elig_overlap <- left_join(control_match_id_mcaid, mcaid_elig, by = "id_mcaid") %>%
  mutate(full_coverage_prior = case_when(is.na(from_date) ~ 0,
                                   dual == 1 | full_benefit == 0 ~ 0,
                                   TRUE ~ intersect(interval(exit_1yr_prior, exit_date),
                                                    interval(from_date, to_date)) / ddays(1) + 1),
         full_coverage_after = case_when(is.na(from_date) ~ 0,
                                   dual == 1 | full_benefit == 0 ~ 0,
                                   TRUE ~ intersect(interval(exit_date, exit_1yr_after),
                                                    interval(from_date, to_date)) / ddays(1) + 1)) %>%
  group_by(level, id_hudhears, exit_date) %>%
  summarise(full_cov_days_prior = sum(full_coverage_prior, na.rm = T),
            full_cov_days_after = sum(full_coverage_after, na.rm = T)) %>%
  ungroup() %>%
  mutate(full_cov_11_prior = full_cov_days_prior >= 11/12 * 365,
         full_cov_7_prior = full_cov_days_prior >= 7/12 * 365,
         full_cov_11_after = full_cov_days_after >= 11/12 * 365,
         full_cov_7_after = full_cov_days_after >= 7/12 * 365)


## Medicaid ED visits and hospitalizations ----
mcaid_visits_prior <- dbGetQuery(db_hhsaw,
                           "SELECT ed.level, ed.id_hudhears, ed.exit_date, 
                           COUNT(ed.ed_pophealth_id) AS ed_cnt_prior, COUNT(ed.inpatient_id) AS hosp_cnt_prior
                           FROM
                           (SELECT DISTINCT a.level, a.id_hudhears, a.exit_date, c.ed_pophealth_id, c.inpatient_id
                             FROM
                             -- Individual-level exits and controls
                                   (SELECT DISTINCT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_long 
                                   UNION
                                   -- Heads of households exits and controls
                                   SELECT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_hh_long
                                   UNION
                                   -- Other household members for HoH level
                                    (SELECT DISTINCT x.level, y.id_hudhears, x.exit_date
                                      FROM
                                      (SELECT * FROM hudhears.control_match_hh_long) x
                                      INNER JOIN
                                      (SELECT DISTINCT id_hudhears, id_kc_pha, 
                                              hh_id_kc_pha, from_date, to_date
                                      FROM pha.stage_pha_exit_timevar) y
                                      ON x.hh_id_kc_pha = y.hh_id_kc_pha AND
                                        x.exit_date >= y.from_date AND x.exit_date <= y.to_date AND
                                        x.id_kc_pha <> y.id_kc_pha)
                                    ) a
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
                           ) ed
                           GROUP BY ed.level, ed.id_hudhears, ed.exit_date")

mcaid_visits_after <- dbGetQuery(db_hhsaw,
                                 "SELECT ed.level, ed.id_hudhears, ed.exit_date, 
                           COUNT(ed.ed_pophealth_id) AS ed_cnt_afterr, COUNT(ed.inpatient_id) AS hosp_cnt_after
                           FROM
                           (SELECT DISTINCT a.level, a.id_hudhears, a.exit_date, c.ed_pophealth_id, c.inpatient_id
                             FROM
                             -- Individual-level exits and controls
                                   (SELECT DISTINCT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_long 
                                   UNION
                                   -- Heads of households exits and controls
                                   SELECT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_hh_long
                                   UNION
                                   -- Other household members for HoH level
                                    (SELECT DISTINCT x.level, y.id_hudhears, x.exit_date
                                      FROM
                                      (SELECT * FROM hudhears.control_match_hh_long) x
                                      INNER JOIN
                                      (SELECT DISTINCT id_hudhears, id_kc_pha, 
                                              hh_id_kc_pha, from_date, to_date
                                      FROM pha.stage_pha_exit_timevar) y
                                      ON x.hh_id_kc_pha = y.hh_id_kc_pha AND
                                        x.exit_date >= y.from_date AND x.exit_date <= y.to_date AND
                                        x.id_kc_pha <> y.id_kc_pha)
                                    ) a
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
                           ) ed
                           GROUP BY ed.level, ed.id_hudhears, ed.exit_date")


## Medicaid chronic conditions ----
mcaid_ccw <- dbGetQuery(db_hhsaw,
                        "SELECT DISTINCT a.level, a.id_hudhears, a.exit_date, c.ccw_code, c.ccw_desc
                          FROM
                             -- Individual-level exits and controls
                                   (SELECT DISTINCT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_long 
                                   UNION
                                   -- Heads of households exits and controls
                                   SELECT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_hh_long
                                   UNION
                                   -- Other household members for HoH level
                                    (SELECT DISTINCT x.level, y.id_hudhears, x.exit_date
                                      FROM
                                      (SELECT * FROM hudhears.control_match_hh_long) x
                                      INNER JOIN
                                      (SELECT DISTINCT id_hudhears, id_kc_pha, 
                                              hh_id_kc_pha, from_date, to_date
                                      FROM pha.stage_pha_exit_timevar) y
                                      ON x.hh_id_kc_pha = y.hh_id_kc_pha AND
                                        x.exit_date >= y.from_date AND x.exit_date <= y.to_date AND
                                        x.id_kc_pha <> y.id_kc_pha)
                                    ) a
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
  group_by(level, id_hudhears, exit_date) %>%
  summarise(ccw_cnt = n_distinct(ccw_code, na.rm = T)) %>%
  ungroup() %>%
  mutate(ccw_cnt = ifelse(is.infinite(ccw_cnt), 0L, ccw_cnt),
         ccw_flag = ifelse(ccw_cnt >= 2, 1L, 0L))


## Medicaid well-child visits ----
wc_visits_prior <- dbGetQuery(db_hhsaw,
                                 "SELECT wc.level, wc.id_hudhears, wc.exit_date, COUNT(wc.wc_visit) AS wc_cnt_prior
                           FROM
                           (SELECT DISTINCT a.level, a.id_hudhears, a.exit_date, c.wc_visit
                             FROM
                             -- Individual-level exits and controls
                                   (SELECT DISTINCT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_long 
                                   UNION
                                   -- Heads of households exits and controls
                                   SELECT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_hh_long
                                   UNION
                                   -- Other household members for HoH level
                                    (SELECT DISTINCT x.level, y.id_hudhears, x.exit_date
                                      FROM
                                      (SELECT * FROM hudhears.control_match_hh_long) x
                                      INNER JOIN
                                      (SELECT DISTINCT id_hudhears, id_kc_pha, 
                                              hh_id_kc_pha, from_date, to_date
                                      FROM pha.stage_pha_exit_timevar) y
                                      ON x.hh_id_kc_pha = y.hh_id_kc_pha AND
                                        x.exit_date >= y.from_date AND x.exit_date <= y.to_date AND
                                        x.id_kc_pha <> y.id_kc_pha)
                                    ) a
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
                           ) wc
                           GROUP BY wc.level, wc.id_hudhears, wc.exit_date")


wc_visits_after <- dbGetQuery(db_hhsaw,
                              "SELECT wc.level, wc.id_hudhears, wc.exit_date, COUNT(wc.wc_visit) AS wc_cnt_after
                           FROM
                           (SELECT DISTINCT a.level, a.id_hudhears, a.exit_date, c.wc_visit
                             FROM
                             -- Individual-level exits and controls
                                   (SELECT DISTINCT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_long 
                                   UNION
                                   -- Heads of households exits and controls
                                   SELECT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_hh_long
                                   UNION
                                   -- Other household members for HoH level
                                    (SELECT DISTINCT x.level, y.id_hudhears, x.exit_date
                                      FROM
                                      (SELECT * FROM hudhears.control_match_hh_long) x
                                      INNER JOIN
                                      (SELECT DISTINCT id_hudhears, id_kc_pha, 
                                              hh_id_kc_pha, from_date, to_date
                                      FROM pha.stage_pha_exit_timevar) y
                                      ON x.hh_id_kc_pha = y.hh_id_kc_pha AND
                                        x.exit_date >= y.from_date AND x.exit_date <= y.to_date AND
                                        x.id_kc_pha <> y.id_kc_pha)
                                    ) a
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
                           ) wc
                           GROUP BY wc.level, wc.id_hudhears, wc.exit_date")


## BH crisis events prior ----
bh_crisis_prior <- dbGetQuery(db_hhsaw, "SELECT DISTINCT a.level, a.id_hudhears, a.exit_date, 
                                            b.event_date, b.source
                                      FROM
                             -- Individual-level exits and controls
                                   (SELECT DISTINCT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_long 
                                   UNION
                                   -- Heads of households exits and controls
                                   SELECT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_hh_long
                                   UNION
                                   -- Other household members for HoH level
                                    (SELECT DISTINCT x.level, y.id_hudhears, x.exit_date
                                      FROM
                                      (SELECT * FROM hudhears.control_match_hh_long) x
                                      INNER JOIN
                                      (SELECT DISTINCT id_hudhears, id_kc_pha, 
                                              hh_id_kc_pha, from_date, to_date
                                      FROM pha.stage_pha_exit_timevar) y
                                      ON x.hh_id_kc_pha = y.hh_id_kc_pha AND
                                        x.exit_date >= y.from_date AND x.exit_date <= y.to_date AND
                                        x.id_kc_pha <> y.id_kc_pha)
                                    ) a
                          INNER JOIN
                          (SELECT DISTINCT id_hudhears, program, description, crisis_date AS event_date, 'crisis' AS source
                         FROM hudhears.bh_crisis_events) b
                       ON a.id_hudhears = b.id_hudhears
                       AND b.event_date <= a.exit_date AND b.event_date >= DATEADD(year, -1, a.exit_date)")

bh_ita_prior <- dbGetQuery(db_hhsaw, "SELECT DISTINCT a.level, a.id_hudhears, a.exit_date, 
                                            b.event_date, b.source
                                      FROM
                             -- Individual-level exits and controls
                                   (SELECT DISTINCT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_long 
                                   UNION
                                   -- Heads of households exits and controls
                                   SELECT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_hh_long
                                   UNION
                                   -- Other household members for HoH level
                                    (SELECT DISTINCT x.level, y.id_hudhears, x.exit_date
                                      FROM
                                      (SELECT * FROM hudhears.control_match_hh_long) x
                                      INNER JOIN
                                      (SELECT DISTINCT id_hudhears, id_kc_pha, 
                                              hh_id_kc_pha, from_date, to_date
                                      FROM pha.stage_pha_exit_timevar) y
                                      ON x.hh_id_kc_pha = y.hh_id_kc_pha AND
                                        x.exit_date >= y.from_date AND x.exit_date <= y.to_date AND
                                        x.id_kc_pha <> y.id_kc_pha)
                                    ) a
                          INNER JOIN
                          (SELECT DISTINCT id_hudhears, call_date AS event_date, 'ita' AS source 
                          FROM hudhears.bh_ita_events) b
                       ON a.id_hudhears = b.id_hudhears
                       AND b.event_date <= a.exit_date AND b.event_date >= DATEADD(year, -1, a.exit_date)")

bh_ed_prior <- dbGetQuery(db_hhsaw, "SELECT DISTINCT a.level, a.id_hudhears, a.exit_date, 
                                            c.event_date, c.source
                                      FROM
                             -- Individual-level exits and controls
                                   (SELECT DISTINCT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_long 
                                   UNION
                                   -- Heads of households exits and controls
                                   SELECT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_hh_long
                                   UNION
                                   -- Other household members for HoH level
                                    (SELECT DISTINCT x.level, y.id_hudhears, x.exit_date
                                      FROM
                                      (SELECT * FROM hudhears.control_match_hh_long) x
                                      INNER JOIN
                                      (SELECT DISTINCT id_hudhears, id_kc_pha, 
                                              hh_id_kc_pha, from_date, to_date
                                      FROM pha.stage_pha_exit_timevar) y
                                      ON x.hh_id_kc_pha = y.hh_id_kc_pha AND
                                        x.exit_date >= y.from_date AND x.exit_date <= y.to_date AND
                                        x.id_kc_pha <> y.id_kc_pha)
                                    ) a
                          INNER JOIN
                          (SELECT DISTINCT id_hudhears, id_mcaid FROM claims.hudhears_id_xwalk) b
                             ON a.id_hudhears = b.id_hudhears
                          LEFT JOIN
                          (SELECT DISTINCT id_mcaid, first_service_date AS event_date, ed_bh, 'mcaid' AS source 
                          FROM claims.final_mcaid_claim_header WHERE ed_bh = 1) c
                       ON b.id_mcaid = c.id_mcaid
                       AND c.event_date <= a.exit_date AND c.event_date >= DATEADD(year, -1, a.exit_date)")


# Make two counts, one with Medicaid ED visits and one without
bh_events_prior <- bind_rows(bh_crisis_prior, bh_ita_prior) %>%
  group_by(level, id_hudhears, exit_date) %>%
  summarise(crisis_prior = n_distinct(event_date)) %>% ungroup() %>%
  left_join(., bind_rows(bh_crisis_prior, bh_ita_prior, bh_ed_prior) %>%
              group_by(level, id_hudhears, exit_date) %>%
              summarise(crisis_ed_prior = n_distinct(event_date)) %>% ungroup(),
            by = c("level", "id_hudhears", "exit_date"))
  

## Prior homelessness ----
homeless <- dbGetQuery(db_hhsaw,
                       "SELECT DISTINCT a.*, b.start_date, b.housing_status, 
                       b.status_code, b.sourcesystemnm
                       FROM
                             -- Individual-level exits and controls
                                   (SELECT DISTINCT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_long 
                                   UNION
                                   -- Heads of households exits and controls
                                   SELECT level, id_hudhears, exit_date 
                                    FROM hudhears.control_match_hh_long
                                   UNION
                                   -- Other household members for HoH level
                                    (SELECT DISTINCT x.level, y.id_hudhears, x.exit_date
                                      FROM
                                      (SELECT * FROM hudhears.control_match_hh_long) x
                                      INNER JOIN
                                      (SELECT DISTINCT id_hudhears, id_kc_pha, 
                                              hh_id_kc_pha, from_date, to_date
                                      FROM pha.stage_pha_exit_timevar) y
                                      ON x.hh_id_kc_pha = y.hh_id_kc_pha AND
                                        x.exit_date >= y.from_date AND x.exit_date <= y.to_date AND
                                        x.id_kc_pha <> y.id_kc_pha)
                                    ) a
                        INNER JOIN
                        (SELECT * FROM hudhears.pha_homeless_status 
                        WHERE housing_status <> 'housed' AND housing_status <> 'institutional') b
                        ON a.id_hudhears = b.id_hudhears
                        AND b.start_date < a.exit_date
                       ")

# Add to time-varying data and restrict to when the homelessness was recorded before
# the latest to_date in that period (max_in_period)
# In some cases it looks like a person was homeless while they were in housing
# Based on the very limited exploration, these might be errors in the way the PHA data were 
#   collapsed to make the timevar table.
# Assume any homelessness within 3 years of the exit date is accurate
# Why 3 years? Could not find much on recurrent homelessness in the literature and 
# the studies I did used 20-24 months of follow up (but partly because of data limitations).
homeless_prior <- inner_join(distinct(exit_timevar, id_hudhears, act_date, from_date, to_date, period, max_in_period),
                             homeless, by = "id_hudhears") %>%
  filter(start_date < max_in_period) %>%
  left_join(., distinct(demogs, id_hudhears, exit_date, admit_date_all, admit_date_kcha, admit_date_sha),
            by = c("id_hudhears", "exit_date")) %>%
  mutate(gap = (exit_date - start_date) / dyears(3),
         recent_homeless = case_when(gap < 3 ~ 1L,
                                     gap >= 3 ~ 0L)) %>%
  group_by(level, id_hudhears, exit_date) %>%
  summarise(recent_homeless = max(recent_homeless, na.rm = T)) %>%
  ungroup()


## Opportunity index ----
# load opportunity index data (version standardized in King County)
kc_opp_index_data <- read_csv(file.path(here::here(), "analyses/capstone/00_opportunity_index",
                                        "kc_opp_indices_scaled.csv")) %>%
  # create variables for state, county, and tract identifies
  mutate(GEO_STATE= substr(GEOID10, 1, 2),
         GEO_COUNTY= substr(GEOID10, 3, 5),
         GEO_TRACT= substr(GEOID10, 6, 11)) %>%
  select(GEOID10, GEO_STATE, GEO_COUNTY, GEO_TRACT, everything()) %>%
  rename(kc_opp_index_score = OPP_Z)




## Final table ----
covariate <- bind_rows(control_match_long, control_match_hh_long) %>%
  arrange(level, exit_uid, id_type, id_hudhears) %>%
  # Join demogs and hh_demog on id_kc_pha as well because there are two IDs that 
  # point to the same id_hudhears, possibly a false positive match
  left_join(., select(demogs, level, id_hudhears, id_kc_pha, exit_date, admit_date_all, 
                      admit_date_kcha, admit_date_sha, gender_me, race_eth_me, 
                      age_at_exit),
            by = c("level", "id_hudhears", "id_kc_pha", "exit_date")) %>%
  left_join(., select(hh_demogs, level, id_hudhears, id_kc_pha, exit_date, hh_demog_date:housing_time_at_exit),
            by = c("level", "id_hudhears", "id_kc_pha", "exit_date")) %>%
  # Use individual-level time-varying demogs to fill in gaps in HH-level
  # Usually happens when an exiting person becomes their own HoH for a day or so
  left_join(., filter(exit_timevar, chooser == chooser_max) %>%
              select(id_hudhears, id_kc_pha, act_date, agency, 
                     major_prog:vouch_type_final, portfolio_final, geo_tractce10),
            by = c("id_hudhears", "id_kc_pha", "exit_date" = "act_date"))


# Note, some controls (~34) have an exit reason because of data issues
# Pull out exit-related fields from only the exiting person and join back
# Make flag for death exit
exit_info <- bind_rows(control_match_long, control_match_hh_long) %>%
  filter(id_type == "id_exit") %>%
  select(level, id_hudhears, id_kc_pha, exit_date, exit_uid) %>%
  left_join(., filter(exit_timevar, chooser == chooser_max) %>%
              select(id_hudhears, id_kc_pha, act_date, exit_reason_clean, exit_category),
            by = c("id_hudhears", "id_kc_pha", "exit_date" = "act_date")) %>%  
  mutate(exit_death = case_when(is.na(exit_reason_clean) ~ NA_integer_,
                                exit_reason_clean == "Deceased" ~ 1L,
                                TRUE ~ 0L)) %>%
  select(level, exit_uid, exit_reason_clean, exit_category, exit_death)

# Join back to covariate data
covariate <- covariate %>%
  left_join(., exit_info, by = c("level", "exit_uid")) %>%
  mutate(agency = coalesce(agency.x, agency.y),
         major_prog = coalesce(major_prog.x, major_prog.y),
         subsidy_type = coalesce(subsidy_type.x, subsidy_type.y),
         prog_type = coalesce(prog_type.x, prog_type.y),
         operator_type = coalesce(operator_type.x, operator_type.y),
         vouch_type_final = coalesce(vouch_type_final.x, vouch_type_final.y),
         portfolio_final = coalesce(portfolio_final.x, portfolio_final.y),
         geo_tractce10 = coalesce(geo_tractce10.x, geo_tractce10.y)) %>%
  left_join(., distinct(kc_opp_index_data, GEO_TRACT, kc_opp_index_score),
            by= c("geo_tractce10" = "GEO_TRACT")) %>%
  select(level, id_hudhears:exit_date, exit_reason_clean, exit_category, exit_death, gender_me:age_at_exit,  
         housing_time_at_exit, hh_id_kc_pha, hh_demog_date, agency:geo_tractce10, kc_opp_index_score, 
         hh_size:single_caregiver) %>%
  left_join(., 
            select(mcaid_elig_overlap, level, id_hudhears, exit_date, full_cov_11_prior, full_cov_7_prior, 
                   full_cov_11_after, full_cov_7_after), 
            by = c("level", "id_hudhears", "exit_date")) %>%
  left_join(., mcaid_visits_prior, by = c("level", "id_hudhears", "exit_date")) %>%
  left_join(., mcaid_visits_after, by = c("level", "id_hudhears", "exit_date")) %>%
  left_join(., mcaid_ccw_summ, by = c("level", "id_hudhears", "exit_date")) %>%
  left_join(., wc_visits_prior, by = c("level", "id_hudhears", "exit_date")) %>%
  left_join(., wc_visits_after, by = c("level", "id_hudhears", "exit_date")) %>%
  left_join(., bh_events_prior, by = c("level", "id_hudhears", "exit_date")) %>%
  left_join(., homeless_prior, by = c("level", "id_hudhears", "exit_date")) %>%
  mutate(across(c("crisis_prior", "crisis_ed_prior", "recent_homeless"),
                ~ replace_na(., 0)))


# Load to SQL
dbWriteTable(db_hhsaw,
             name = DBI::Id(schema = "hudhears", table = "control_match_covariate"),
             value = filter(covariate, level == "ind"),
             overwrite = T)

dbWriteTable(db_hhsaw,
             name = DBI::Id(schema = "hudhears", table = "control_match_covariate_hh"),
             value = filter(covariate, level == "hh"),
             overwrite = T)



covariate_current <- dbGetQuery(db_hhsaw, "select * from hudhears.control_match_covariate")
covariate_now <- covariate %>% filter(level == "ind") %>% select(id_hudhears:wc_cnt_after)
chk2 <- anti_join(covariate_current, covariate_now)
