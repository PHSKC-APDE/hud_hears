## Script name: pha_exit_outcomes_mcaid
##
## Purpose of script: Look at Medicaid-derived exit outcomes 
##
## Author: Alastair Matheson, Public Health - Seattle & King County
## Date Created: 2022-01-03
## Email: alastair.matheson@kingcounty.gov
##
## Notes:
##   
##


# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, scales, multgee, 
               ggplot2, ggrepel, viridis, hrbrthemes, knitr, rmarkdown, flextable,
               scales, gt)

# Connect to HHSAW
db_hhsaw <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = "tcp:kcitazrhpasqlprp16.azds.kingcounty.gov,1433",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           #pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryInteractive")


# BRING IN DATA ----
covariate <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.control_match_covariate") %>%
  mutate(exit = case_when(id_type == "id_exit" ~ 1L,
                          id_type == "id_control" ~ 0L))

exits <- dbGetQuery(db_hhsaw, "SELECT DISTINCT agency, id_hudhears, act_date, exit_reason, 
                    exit_reason_clean, exit_category_pha, exit_category
                    FROM pha.stage_pha_exit_timevar
                    WHERE chooser = chooser_max AND
                    true_exit = 1 AND act_date IS NOT NULL AND
                    exit_type_keep = 1 AND 
                    exit_order_study = exit_order_max_study AND exit_order_study IS NOT NULL")


## Add exit type and other factors ----
covariate_nodeath <- covariate %>% filter(exit_death != 1) %>%
  left_join(., exits, 
            by = c("agency", "id_hudhears", "exit_date" = "act_date", "exit_reason_clean", "exit_category")) %>%
  mutate(
    ed_any_prior = ifelse(ed_cnt_prior >= 1, 1L, 0L),
    ed_any_after = ifelse(ed_cnt_after >= 1, 1L, 0L),
    hosp_any_prior = ifelse(hosp_cnt_prior >= 1, 1L, 0L),
    hosp_any_after = ifelse(hosp_cnt_after >= 1, 1L, 0L),
    wc_any_prior = ifelse(wc_cnt_prior >= 1, 1L, 0L),
    wc_any_after = ifelse(wc_cnt_after >= 1, 1L, 0L),
    crisis_any_prior = case_when(crisis_prior == 0 ~ 0L,
                                 crisis_prior >= 1 ~ 1L),
    crisis_ed_any_prior = case_when(crisis_ed_prior == 0 ~ 0L,
                                    crisis_ed_prior >= 1 ~ 1L),
    # Remove older ages
    age_at_exit = ifelse(age_at_exit > 125, NA_integer_, age_at_exit),
    # Flag people with coverage before and after exit
    include_cov = full_cov_7_prior == T & full_cov_7_after == T,
    include_cov_age = full_cov_7_prior == T & full_cov_7_after == T & age_at_exit < 62,
    # Collapse voucher types
    vouch_type_use = case_when(vouch_type_final == "GENERAL TENANT-BASED VOUCHER" ~ "General tenant-based",
                               vouch_type_final == "PARTNER PROJECT-BASED VOUCHER" ~ "Partner project-based",
                               vouch_type_final %in% c("AGENCY TENANT-BASED VOUCHER",
                                                       "PHA OPERATED VOUCHER") ~ "Agency/PHA tenant-based",
                               vouch_type_final == "MOD REHAB" ~ "Mod rehab",
                               vouch_type_final %in% c("FUP", "HASP", "OTHER (TI/DV)") ~ 
                                 "Other (DV/FUP/HASP/TI)",
                               is.na(vouch_type_final) & prog_type %in% c("COLLABORATIVE HOUSING", "PBS8") ~ 
                                 "Partner project-based",
                               TRUE ~ vouch_type_final),
    # Flag anyone with missing covariates since they will be dropped from the propensity scores
    include_demog = (!(is.na(exit_category) | is.na(age_at_exit) | is.na(gender_me) | 
                         is.na(race_eth_me) | race_eth_me == "Unknown" |
                         is.na(agency) | is.na(single_caregiver) | 
                         is.na(hh_size) | is.na(hh_disability) | is.na(housing_time_at_exit) | is.na(major_prog) | 
                         is.na(kc_opp_index_score)))
    )


## Set up numbers for CONSORT diagram ----
# Controls and their Medicaid coverage
covariate_nodeath %>% filter(include_demog == T) %>% count(exit, include_cov_age) %>%
  group_by(exit) %>%
  mutate(tot = sum(n)) %>%
  ungroup() %>%
  mutate(pct = round(n / tot * 100, 1))

# Exit types as a percent of all exits
covariate_nodeath %>% filter(include_demog == T & exit == 1) %>% count(exit_category) %>%
  mutate(tot = sum(n), pct = round(n / tot * 100, 1))

# Exit types and their Medicaid coverage
covariate_nodeath %>% filter(include_demog == T & exit == 1) %>% count(exit_category, include_cov_age) %>%
  group_by(exit_category) %>%
  mutate(tot = sum(n)) %>%
  ungroup() %>%
  mutate(pct = round(n / tot * 100, 1))

# Numbers for well-child checks
covariate_nodeath %>% 
  filter(include_demog == T & include_cov_age == T & age_at_exit <= 6) %>% 
  count(exit, exit_category) %>%
  group_by(exit) %>%
  mutate(tot_exit = sum(n)) %>%
  ungroup() %>%
  mutate(pct_cat = round(n / tot_exit * 100, 1))


## Set up exit only and control data ----
exit_control <- covariate_nodeath %>%
  # Remove anyone with missing covariates since they will be dropped from the propensity scores
  # Also remove anyone aged 62+ because so few will have non-dual Medicaid
  # Also remove people without full coverage
  filter(include_demog == T & include_cov_age == T)

exit_nodeath <- exit_control %>% filter(exit == 1)


# DEMOGRAPHICS ----
# Functions are found in the pha_exit_factors.R code
## Demogs for exit vs not ----
# Age
exit_any_age <- age_sum(exit_control, full_demog = T, id_type)

# Gender
exit_any_gender <- demog_pct_sum(exit_control, full_demog = T, level = "ind", 
                                  demog = "gender", id_type)

# Race/eth
exit_any_race <- demog_pct_sum(exit_control, full_demog = T, level = "ind", 
                                demog = "race", id_type)

# Time in housing (this is based on HH data)
exit_any_hh_los <- demog_num_sum(exit_control, full_demog = T, demog = "los", id_type)

# HH size and composition
exit_any_hh_demogs <- hh_demogs_sum(exit_control, full_demog = T, level = "hh", id_type)

# Program type
exit_any_hh_prog <- demog_pct_sum(exit_control, full_demog = T, level = "hh", demog = "program", id_type)

# Medicaid outcomes
exit_any_mcaid_7_prior <- mcaid_outcomes_sum(exit_control, full_demog = T, 
                                              time = "prior", cov_time = "7_mth", show_num = T, id_type)
exit_any_mcaid_7_after <- mcaid_outcomes_sum(exit_control, full_demog = T, 
                                              time = "after", cov_time = "7_mth", show_num = T, id_type)

# Combine for R markdown
exit_any_demogs <- bind_rows(exit_any_age, exit_any_gender, exit_any_race, exit_any_hh_los,
                             exit_any_hh_demogs, exit_any_hh_prog, exit_any_mcaid_7_prior,
                             exit_any_mcaid_7_after)

rm(exit_any_age, exit_any_gender, exit_any_race, exit_any_hh_los,
   exit_any_hh_demogs, exit_any_hh_prog, exit_any_mcaid_7_prior,
   exit_any_mcaid_7_after)


## Demogs by exit ----
# Age
exit_type_age <- age_sum(exit_nodeath, full_demog = T, exit_category)

# Gender
exit_type_gender <- demog_pct_sum(exit_nodeath, full_demog = T, level = "ind", 
                                  demog = "gender", exit_category)

# Race/eth
exit_type_race <- demog_pct_sum(exit_nodeath, full_demog = T, level = "ind", 
                                demog = "race", exit_category)

# Time in housing (this is based on HH data)
exit_type_hh_los <- demog_num_sum(exit_nodeath, full_demog = T, demog = "los", exit_category)

# HH size and composition
exit_type_hh_demogs <- hh_demogs_sum(exit_nodeath, full_demog = T, level = "hh", exit_category)

# Program type
exit_type_hh_prog <- demog_pct_sum(exit_nodeath, full_demog = T, level = "hh", demog = "program", exit_category)

# Medicaid outcomes
exit_type_mcaid_7_prior <- mcaid_outcomes_sum(exit_nodeath, full_demog = T, 
                                             time = "prior", cov_time = "7_mth", show_num = T, exit_category)
exit_type_mcaid_7_after <- mcaid_outcomes_sum(exit_nodeath, full_demog = T, 
                                              time = "after", cov_time = "7_mth", show_num = T, exit_category)


# Combine for R markdown
exit_type_demogs <- bind_rows(exit_type_age, exit_type_gender, exit_type_race, exit_type_hh_los,
                           exit_type_hh_demogs, exit_type_hh_prog, exit_type_mcaid_7_prior,
                           exit_type_mcaid_7_after)

rm(exit_type_age, exit_type_gender, exit_type_race, exit_type_hh_los,
   exit_type_hh_demogs, exit_type_hh_prog, exit_type_mcaid_7_prior,
   exit_type_mcaid_7_after)


# OUTCOMES ----
outcome_sum <- function(df, 
                        outcome = c("ed_prior", "ed_after", 
                                    "hosp_prior", "hosp_after",
                                    "wc_prior", "wc_after"), 
                        ...) {
  # Set things up to select in pivot_ functions
  # There is probably a better way to do this but it works
  col_names <- df %>% select(...) %>% colnames()
  
  outcome <- match.arg(outcome)
  
  if (outcome == "ed_prior") {
    cat_text <- "ED visits prior to exit"
    output <- df %>% 
      distinct(id_hudhears, exit_date, ..., ed_cnt_prior, ed_any_prior) %>%
      mutate(cnt = ed_cnt_prior, any = ed_any_prior)
  } else if (outcome == "ed_after") {
    cat_text <- "ED visits after exit"
    output <- df %>% 
      distinct(id_hudhears, exit_date, ..., ed_cnt_after, ed_any_after) %>%
      mutate(cnt = ed_cnt_after, any = ed_any_after)
  } else if (outcome == "hosp_prior") {
    cat_text <- "Hospitalizations prior to exit"
    output <- df %>% 
      distinct(id_hudhears, exit_date, ..., hosp_cnt_prior, hosp_any_prior) %>%
      mutate(cnt = hosp_cnt_prior, any = hosp_any_prior)
  } else if (outcome == "hosp_after") {
    cat_text <- "Hospitalizations after exit"
    output <- df %>% 
      distinct(id_hudhears, exit_date, ..., hosp_cnt_after, hosp_any_after) %>%
      mutate(cnt = hosp_cnt_after, any = hosp_any_after)
  } else if (outcome == "wc_prior") {
    # Note use 5 as cutoff so that 6 is max age in yr after exit (keep consistent for both)
    cat_text <- "Well child checks prior to exit"
    output <- df %>% 
      filter(age_at_exit < 6) %>%
      distinct(id_hudhears, exit_date, ..., wc_cnt_prior, wc_any_prior) %>%
      mutate(cnt = wc_cnt_prior, any = wc_any_prior)
  } else if (outcome == "wc_after") {
    # Note use 5 as cutoff so that 6 is max age in yr after exit
    cat_text <- "Well child checks after exit"
    output <- df %>% 
      filter(age_at_exit < 6) %>%
      distinct(id_hudhears, exit_date, ..., wc_cnt_after, wc_any_after) %>%
      mutate(cnt = wc_cnt_after, any = wc_any_after)
  }
  
  if (str_detect(outcome, "hosp")) {
    multiplier <- 100
  } else {
    multiplier <- 1
  }
  
  output <- output %>% 
    group_by(...) %>%
    summarise(n = number(n(), big.mark = ","), 
              any = scales::percent(mean(any, na.rm = T), accuracy = 0.1),
              visit_mean = round(mean(cnt * multiplier, na.rm = T), 1), 
              vist_med = median(cnt * multiplier, na.rm = T),
              visit_range = paste0(min(cnt, na.rm = T), "-", max(cnt, na.rm = T))) %>% 
    pivot_longer(cols = !col_names, values_transform = list(value = as.character)) %>%
    pivot_wider(id_cols = "name", names_from = col_names) %>%
    mutate(category = cat_text, .before = "name",
           name = case_when(name == "any" ~ "Proportion with 1+ event",
                            str_detect(outcome, "hosp") & name == "visit_mean" ~ "Mean number events (per 100)",
                            name == "visit_mean" ~ "Mean number events",
                            str_detect(outcome, "hosp") & name == "vist_med" ~ "Median number events (per 100)",
                            name == "vist_med" ~ "Median number events",
                            name == "visit_range" ~ "Range of event numbers",
                            TRUE ~ name)) %>%
    rename("group" = "name")
  
  output
}

## Outcomes by exit type ----
ed_type_prior <- outcome_sum(df = exit_nodeath %>% filter(include_cov == T & include_demog == T),
                             outcome = "ed_prior", exit_category)

ed_type_after <- outcome_sum(df = exit_nodeath %>% filter(include_cov == T & include_demog == T),
                             outcome = "ed_after", exit_category)

hosp_type_prior <- outcome_sum(df = exit_nodeath %>% filter(include_cov == T & include_demog == T),
                             outcome = "hosp_prior", exit_category)

hosp_type_after <- outcome_sum(df = exit_nodeath %>% filter(include_cov == T & include_demog == T),
                             outcome = "hosp_after", exit_category)

wc_type_prior <- outcome_sum(df = exit_nodeath %>% filter(include_cov == T & include_demog == T),
                             outcome = "wc_prior", exit_category)

wc_type_after <- outcome_sum(df = exit_nodeath %>% filter(include_cov == T & include_demog == T),
                             outcome = "wc_after", exit_category)


# Combine outcomes
outcomes_type <- bind_rows(ed_type_prior, ed_type_after, hosp_type_prior,
                           hosp_type_after, wc_type_prior, wc_type_after) %>%
  filter(!group == "n" | str_detect(category, "Well child"))

rm(ed_type_prior, ed_type_after, hosp_type_prior, hosp_type_after, wc_type_prior, wc_type_after)


# REGRESSION MODEL: EXIT TYPES ----
## Set up data ----
model_data_mcaid <- exit_nodeath %>%
  mutate(across(c("gender_me", "major_prog", "vouch_type_use"), ~ as_factor(.))) %>%
  # Relevel factors as they are made
  mutate(race_eth_me = fct_relevel(race_eth_me, c("White")),
         age_grp = fct_relevel(as_factor(case_when(
           age_at_exit < 25 ~ "<25",
           data.table::between(age_at_exit, 25, 44.99, NAbounds = NA) ~ "25-44",
           data.table::between(age_at_exit, 45, 61.99, NAbounds = NA) ~ "45-<62")),
           "<25"),
         los = fct_relevel(
           as_factor(case_when(housing_time_at_exit < 3 ~ "<3",
                               between(housing_time_at_exit, 3, 5.999, NAbounds = NA) ~ "3-5.99",
                               between(housing_time_at_exit, 6, 9.999, NAbounds = NA) ~ "6-9.99",
                               housing_time_at_exit >= 10 ~ "10+")),
           "<3", "3-5.99"),
         exit_category = as_factor(exit_category),
         exit_category_neg = fct_relevel(exit_category, "Negative")
  )


# Set up numeric IDs for households
# Needed for geeglm to work properly
hh_ids <- model_data_mcaid %>% filter(!is.na(hh_id_kc_pha)) %>%
  distinct(hh_id_kc_pha) %>%
  arrange(hh_id_kc_pha) %>%
  mutate(id_hh = row_number())


model_data_mcaid <- model_data_mcaid %>%
  left_join(., hh_ids, by = "hh_id_kc_pha")

# Make well-child specific set that drops multiple gender because of low n
model_data_mcaid_wc <- model_data_mcaid %>%
  filter(age_at_exit < 6 & gender_me != "Multiple") %>%
  mutate(gender_me = fct_drop(gender_me))


## Set up generic function ----
model_run <- function(outcome = c("ed", "hosp", "wc"), neutral = T) {
  outcome <- match.arg(outcome)
  
  # Set up propensity scores
  if (outcome == "ed") {
    ps_model <- nomLORgee(formula = exit_category ~ age_grp + gender_me + 
                            race_eth_me + single_caregiver + hh_size +
                            los + major_prog +
                            ed_cnt_prior + ccw_flag + kc_opp_index_score,
                          data = model_data_mcaid,
                          id = id_hh,
                          LORstr = "independence")
  } else if (outcome == "hosp") {
    ps_model <- nomLORgee(formula = exit_category ~ age_grp + gender_me + 
                            race_eth_me + single_caregiver + hh_size +
                            los + major_prog +
                            hosp_cnt_prior + ccw_flag + kc_opp_index_score,
                          data = model_data_mcaid,
                          id = hh_id_kc_pha,
                          LORstr = "independence")
  } else if (outcome == "wc") {
    model_data_mcaid <- model_data_mcaid %>% filter(age_at_exit < 6))
    
    ps_model <- nomLORgee(formula = exit_category ~ age_grp + gender_me + 
                            race_eth_me + single_caregiver + hh_size +
                            los + major_prog +
                            wc_cnt_prior + ccw_flag + kc_opp_index_score,
                          data = model_data_mcaid,
                          id = hh_id_kc_pha,
                          LORstr = "independence")
  }
  
  # Next, calculate generalized propensity scores
  ps <- as.data.frame(fitted(ps_model))
  colnames(ps) <- c("neutral", "negative", "positive")
  ps <- cbind("id_kc_pha" = sort(model_data_mcaid$id_kc_pha), ps)
  
  
  model_data_mcaid <- model_data_mcaid %>%
    left_join(., ps, by = "id_kc_pha") %>%
    mutate(iptw = case_when(exit_category == "Neutral" ~ 1/neutral,
                            exit_category == "Negative" ~ 1/negative,
                            exit_category == "Positive" ~ 1/positive))
  
  
  if (neutral == F) {
    # Set up version with no neutral and reset factor
    model_data_mcaid <- model_data_mcaid %>% filter(exit_category != "Neutral") %>%
      mutate(exit_category = fct_drop(exit_category))
  }
  
  # Run model
  if (outcome == "ed") {
    output <- geepack::geeglm(formula = ed_any_after ~ exit_category,
                              weights = iptw,
                              data = model_data_mcaid,
                              id = id_hh,
                              family = "binomial")
  } else if (outcome == "hosp") {
    output <- geepack::geeglm(formula = hosp_any_after ~ exit_category,
                              weights = iptw,
                              data = model_data_mcaid,
                              id = id_hh,
                              family = "binomial")
  } else if (outcome == "wc") {
    output <- geepack::geeglm(formula = wc_any_after ~ exit_category,
                              weights = iptw,
                              data = model_data_mcaid,
                              id = id_hh,
                              family = "binomial")
  }
  
  output
}




## Evaluate propensity scores ----
ps_chk1_model <- nomLORgee(formula = exit_category ~ age_grp + gender_me + 
                        race_eth_me + single_caregiver + hh_size +
                        los + major_prog +
                        ed_cnt_prior + ccw_flag + kc_opp_index_score,
                      data = model_data_mcaid,
                      id = id_hh,
                      LORstr = "independence")
ps_chk1 <- as.data.frame(fitted(ps_chk1_model))
colnames(ps_chk1) <- c("neutral", "negative", "positive")

summarytools::dfSummary(ps_chk1)

ps_chk2_model <- nomLORgee(formula = exit_category ~ age_grp + gender_me + 
                        race_eth_me + single_caregiver + hh_size +
                        los + major_prog +
                        ed_cnt_prior + ccw_flag + kc_opp_index_score,
                      data = model_data_mcaid,
                      id = hh_id_kc_pha,
                      LORstr = "independence")
ps_chk2 <- as.data.frame(fitted(ps_chk2_model))
colnames(ps_chk2) <- c("neutral", "negative", "positive")

summarytools::dfSummary(ps_chk2)



## Testing ----
# Test with PS cluster = hh_id_kc_pha
test <- model_run(outcome = "ed")
summary(test)
broom::tidy(test, conf.int = TRUE, exponentiate = T)

# Test with PS cluster = id_hh
test2 <- model_run(outcome = "ed")
summary(test2)
broom::tidy(test2, conf.int = TRUE, exponentiate = T)


# Test running things manually
ps_chk1_bind <- cbind("id_kc_pha" = sort(model_data_mcaid$id_kc_pha), ps_chk1)

chk <-  left_join(model_data_mcaid, ps_chk1_bind, by = "id_kc_pha") %>%
  mutate(iptw = case_when(exit_category == "Neutral" ~ 1/neutral,
                          exit_category == "Negative" ~ 1/negative,
                          exit_category == "Positive" ~ 1/positive)) %>%
  arrange(id_hh, id_kc_pha)

chk$exit_category <- relevel(factor(chk$exit_category), ref = "Negative")

chk_model <- geepack::geeglm(formula = ed_any_after ~ exit_category,
                             weights = iptw,
                             data = chk,
                             id = id_hh,
                             family = "binomial")

summary(chk_model)
broom::tidy(chk_model, conf.int = TRUE, exponentiate = T)


## ED visits ----
### Propensity score ----
ed <- model_run(outcome = "ed")
summary(ed)
broom::tidy(ed, conf.int = TRUE, exponentiate = T)


### Crude ----
ed_crude <- geepack::geeglm(ed_any_after ~ exit_category, 
                data = model_data_mcaid,
                id = id_hh,
                family = "binomial")

summary(ed_crude)
broom::tidy(ed_crude, conf.int = TRUE, exponentiate = T)


### Adjusted ----
# Multiple categories model
ed_adj_mult <- geepack::geeglm(ed_any_after ~ exit_category_neg + gender_me + race_eth_me + 
                                 age_grp + los + major_prog + hh_size + single_caregiver + 
                                 hh_disability + ed_cnt_prior + ccw_flag, 
                               data = model_data_mcaid, 
                               id = id_hh,
                               family = "binomial")

summary(ed_adj_mult)
broom::tidy(ed_adj_mult, conf.int = TRUE, exponentiate = T) %>% as.data.frame()


# Poisson model
ed_adj_pois <- geepack::geeglm(ed_cnt_after ~ exit_category_neg + gender_me + race_eth_me + 
                                 age_grp + los + major_prog + hh_size + single_caregiver + 
                                 hh_disability + ed_cnt_prior + ccw_flag, 
                               data = model_data_mcaid, 
                               id = id_hh,
                               family = "poisson")

summary(ed_adj_pois)
broom::tidy(ed_adj_pois, conf.int = TRUE, exponentiate = T) %>% as.data.frame()


## Hospitalizations visits ----
### Propensity score ----
hosp <- model_run(outcome = "hosp")
summary(hosp)
broom::tidy(hosp, conf.int = TRUE, exponentiate = T)


### Crude ----
hosp_crude <- geepack::geeglm(hosp_any_after ~ exit_category, 
                            data = model_data_mcaid_pn,
                            id = id_hh,
                            family = "binomial")

summary(hosp_crude)
broom::tidy(hosp_crude, conf.int = TRUE, exponentiate = T)


### Adjusted ----
# Multiple categories model
hosp_adj_mult <- geepack::geeglm(hosp_any_after ~ exit_category_neg + gender_me + race_eth_me + 
                                   age_grp + los + major_prog + hh_size + single_caregiver + 
                                   hh_disability + hosp_cnt_prior + ccw_flag, 
                               data = model_data_mcaid, 
                               id = id_hh,
                               family = "binomial")

broom::tidy(hosp_adj_mult, conf.int = TRUE, exponentiate = T)

# Poisson model
hosp_adj_pois <- geepack::geeglm(hosp_cnt_after ~ exit_category_neg + gender_me + race_eth_me + 
                                   age_grp + los + major_prog + hh_size + single_caregiver + 
                                   hh_disability + hosp_cnt_prior + ccw_flag, 
                                 data = model_data_mcaid, 
                                 id = id_hh,
                                 family = "poisson")

broom::tidy(hosp_adj_pois, conf.int = TRUE, exponentiate = T)



## Well-child visits ----

### Propensity score ----
wc <- model_run(outcome = "wc")
summary(wc)
broom::tidy(wc, conf.int = TRUE, exponentiate = T)


### Crude ----
wc_crude <- geepack::geeglm(wc_any_after ~ exit_category, 
                            data = model_data_mcaid_pn,
                            id = id_hh,
                            family = "binomial")

summary(wc_crude)
broom::tidy(wc_crude, conf.int = TRUE, exponentiate = T)


### Adjusted ----
# Multiple categories model
wc_adj_mult <- geepack::geeglm(wc_any_after ~ exit_category_neg + gender_me + race_eth_me + 
                                 age_at_exit + los + major_prog + hh_size + single_caregiver + 
                                 hh_disability + wc_cnt_prior, 
                               data = model_data_mcaid_wc, 
                               id = id_hh,
                               family = "binomial")

broom::tidy(wc_adj_mult, conf.int = TRUE, exponentiate = T)


### Stratified: prior visits ----
# Crude
wc_strat_wc <- geepack::geeglm(wc_any_after ~ exit_category,
                               data = model_data_mcaid_wc[model_data_mcaid_wc$wc_cnt_prior >= 1, ], 
                               id = id_hh,
                               family = "binomial")

summary(wc_strat_wc)
broom::tidy(wc_strat_wc, conf.int = TRUE, exponentiate = T)


# Adjusted
wc_strat_wc_adj <- geepack::geeglm(wc_any_after ~ exit_category_neg + gender_me + race_eth_me + age_at_exit + 
                                     los + major_prog + hh_size + single_caregiver + hh_disability, 
                               data = model_data_mcaid_wc[model_data_mcaid_wc$wc_cnt_prior >= 1, ], 
                               id = id_hh,
                               family = "binomial")

broom::tidy(wc_strat_wc_adj, conf.int = TRUE, exponentiate = T)


### Stratified: no prior visits ----
# Crude
wc_strat_no_wc <- geepack::geeglm(wc_any_after ~ exit_category,
                               data = model_data_mcaid_wc[model_data_mcaid_wc$wc_cnt_prior < 1, ], 
                               id = id_hh,
                               family = "binomial")

summary(wc_strat_no_wc)
broom::tidy(wc_strat_no_wc, conf.int = TRUE, exponentiate = T)


# Adjusted
wc_strat_no_wc_adj <- geepack::geeglm(wc_any_after ~ exit_category_neg + gender_me + race_eth_me + age_at_exit + 
                                     los + major_prog + hh_size + single_caregiver + hh_disability, 
                                   data = model_data_mcaid_wc[model_data_mcaid_wc$wc_cnt_prior < 1, ], 
                                   id = id_hh,
                                   family = "binomial")

broom::tidy(wc_strat_no_wc_adj, conf.int = TRUE, exponentiate = T)


## Combine results ----
# Make it easier to see all outcomes in one place
model_outcomes <- bind_rows(broom::tidy(ed_adj_mult, conf.int = TRUE, exponentiate = T) %>%
                              mutate(outcome = "ED visits"),
                            broom::tidy(hosp_adj_mult, conf.int = TRUE, exponentiate = T) %>%
                              mutate(outcome = "Hospitalizations"),
                            broom::tidy(wc_strat_wc_adj, conf.int = TRUE, exponentiate = T) %>%
                              mutate(outcome = "Well-child checks \n(with previous well-child checks)"),
                            broom::tidy(wc_strat_no_wc_adj, conf.int = TRUE, exponentiate = T) %>% 
                              mutate(outcome = "Well-child checks \n(without previous well-child checks)")
                            )

model_outcomes_graph <- model_outcomes %>%
  filter(str_detect(term, "exit_")) %>%
  mutate(term = case_when(str_detect(term, "Negative") ~ "Negative vs. remaining",
                          str_detect(term, "Neutral") ~ "Neutral vs. remaining",
                          str_detect(term, "Positive") ~ "Positive vs. remaining"),
         height = c(1.1, 1.6, 1, 1.5, 0.9, 1.4, 0.8, 1.3))




# REGRESSION MODEL: EXIT VS NOT ----
## Set up data ----
any_exit_mcaid <- exit_control %>%
  mutate(across(c("gender_me", "major_prog", "vouch_type_use"), ~ as_factor(.))) %>%
  # Relevel factors as they are made
  mutate(race_eth_me = fct_relevel(race_eth_me, c("White")),
         age_grp = fct_relevel(as_factor(case_when(
           age_at_exit < 25 ~ "<25",
           data.table::between(age_at_exit, 25, 44.99, NAbounds = NA) ~ "25-44",
           data.table::between(age_at_exit, 45, 61.99, NAbounds = NA) ~ "45-<62")),
           "<25"),
         los = fct_relevel(
           as_factor(case_when(housing_time_at_exit < 3 ~ "<3",
                               between(housing_time_at_exit, 3, 5.999, NAbounds = NA) ~ "3-5.99",
                               between(housing_time_at_exit, 6, 9.999, NAbounds = NA) ~ "6-9.99",
                               housing_time_at_exit >= 10 ~ "10+")),
           "<3", "3-5.99"),
         exit_category = fct_relevel(as_factor(
           case_when(id_type == "id_control" ~ "Remained",
                     TRUE ~ exit_category)),
           "Remained", "Negative", "Neutral", "Positive")
  )

# Set up numeric IDs for households
# Needed for geeglm to work properly
hh_ids <- any_exit_mcaid %>% filter(!is.na(hh_id_kc_pha)) %>%
  distinct(hh_id_kc_pha) %>%
  arrange(hh_id_kc_pha) %>%
  mutate(id_hh = row_number())

any_exit_mcaid <- any_exit_mcaid %>%
  left_join(., hh_ids, by = "hh_id_kc_pha")

# Make well-child specific set that drops multiple gender because of low n
any_exit_mcaid_wc <- any_exit_mcaid %>%
  filter(age_at_exit < 6 & gender_me != "Multiple") %>%
  mutate(gender_me = fct_drop(gender_me))


## ED visits ----
### Adjusted ----
# Multiple categories model
any_ed_adj_mult <- geepack::geeglm(ed_any_after ~ exit_category + gender_me + race_eth_me + 
                                 age_grp + los + major_prog + hh_size + single_caregiver + 
                                   hh_disability + ed_cnt_prior + ccw_flag, 
                               data = any_exit_mcaid, 
                               id = id_hh,
                               family = "binomial")

summary(any_ed_adj_mult)
broom::tidy(any_ed_adj_mult, conf.int = TRUE, exponentiate = T)



## Hospitalizations visits ----
### Adjusted ----
# Multiple categories model
any_hosp_adj_mult <- geepack::geeglm(hosp_any_after ~ exit_category + gender_me + race_eth_me + 
                                   age_grp + los + major_prog + hh_size + single_caregiver + 
                                     hh_disability + hosp_cnt_prior + ccw_flag, 
                                 data = any_exit_mcaid, 
                                 id = id_hh,
                                 family = "binomial")

broom::tidy(any_hosp_adj_mult, conf.int = TRUE, exponentiate = T) %>% as.data.frame()


## Well-child visits ----
### Adjusted ----
# Multiple categories model
any_wc_adj_mult <- geepack::geeglm(wc_any_after ~ exit_category + gender_me + race_eth_me + 
                                 age_at_exit + los + major_prog + hh_size + single_caregiver + 
                                   hh_disability + wc_cnt_prior, 
                               data = any_exit_mcaid_wc, 
                               id = id_hh,
                               family = "binomial")

broom::tidy(any_wc_adj_mult, conf.int = TRUE, exponentiate = T)


### Stratified: prior visits ----
# Adjusted
any_wc_strat_wc_adj <- geepack::geeglm(wc_any_after ~ exit_category + gender_me + race_eth_me + age_at_exit + 
                                     los + major_prog + hh_size + single_caregiver + hh_disability, 
                                   data = any_exit_mcaid_wc[any_exit_mcaid_wc$wc_cnt_prior >= 1, ], 
                                   id = id_hh,
                                   family = "binomial")

broom::tidy(any_wc_strat_wc_adj, conf.int = TRUE, exponentiate = T)


### Stratified: no prior visits ----
# Adjusted
any_wc_strat_no_wc_adj <- geepack::geeglm(wc_any_after ~ exit_category + gender_me + race_eth_me + age_at_exit + 
                                        los + major_prog + hh_size + single_caregiver + hh_disability, 
                                      data = any_exit_mcaid_wc[any_exit_mcaid_wc$wc_cnt_prior < 1, ], 
                                      id = id_hh,
                                      family = "binomial")

broom::tidy(any_wc_strat_no_wc_adj, conf.int = TRUE, exponentiate = T)


## Combine results ----
# Make it easier to see all outcomes in one place
model_outcomes_any <- bind_rows(broom::tidy(any_ed_adj_mult, conf.int = TRUE, exponentiate = T) %>%
                                  mutate(outcome = "ED visits"),
                            broom::tidy(any_hosp_adj_mult, conf.int = TRUE, exponentiate = T) %>%
                              mutate(outcome = "Hospitalizations"),
                            broom::tidy(any_wc_strat_wc_adj, conf.int = TRUE, exponentiate = T) %>%
                              mutate(outcome = "Well-child checks \n(with previous well-child checks)"),
                            broom::tidy(any_wc_strat_no_wc_adj, conf.int = TRUE, exponentiate = T) %>%
                              mutate(outcome = "Well-child checks \n(without previous well-child checks)"))

model_outcomes_any_graph <- model_outcomes_any %>%
  filter(str_detect(term, "exit_")) %>%
  mutate(term = case_when(str_detect(term, "Negative") ~ "Negative vs. remaining",
                          str_detect(term, "Neutral") ~ "Neutral vs. remaining",
                          str_detect(term, "Positive") ~ "Positive vs. remaining"),
         height = c(1.1, 1.6, 2.1, 
                    1, 1.5, 2, 
                    0.9, 1.4, 1.9, 
                    0.8, 1.3, 1.8))




# RUN MARKDOWN FILE ----
render(input = file.path(here::here(), "analyses/health/pha_exit_outcomes_mcaid.Rmd"), 
       output_format = "word_document",
       output_file = "CSTE_2022_health_outcomes",
       output_dir = "C:/Users/mathesal/King County/DPH Health And Housing - Documents/HUD HEARS Study/Presentations and papers/")


# MAKE IMAGES FOR MANUSCRIPT ----
## Exit type ----
ggplot(data = model_outcomes_graph) +
  # Point estimates
  geom_point(aes(x = estimate, y = height, color = outcome), size = 3) +
  # Add labels under point estimates
  #geom_text_repel(aes(label = estimate)) +
  geom_text(aes(x = estimate, y = height, label = round(estimate, 2)),
            nudge_y = -0.03, size = 2.5, color = "gray33") +
  
  # Confidence intervals
  geom_errorbarh(height = 0.02, 
                 aes(y = height, xmin = conf.low, xmax = conf.high, color = outcome), 
                 size = 1, alpha = 0.5) +
  
  # Hazard ratio = 1 line
  geom_vline(xintercept = 1, color = "black", size = 0.7, alpha = 0.5, linetype = "longdash") +
  
  # Other settings
  scale_y_continuous(breaks = c(0, 0.95, 1.45, 1.6),
                     labels = c("", "Neutral exit\n(vs. negative)", 
                                "Positive exit\n(vs. negative)", "")) +
  scale_x_continuous(breaks = c(0, 0.5, 1, 2, 3)) +
  labs(x = "Odds ratio", color = "Outcome") +
  scale_color_brewer(type = "qual", palette = "Dark2") + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(margin = margin(t = 4, b = 4)))

ggsave(filename = "health_outcomes_exit_type_plot.png",
       path = file.path(here::here(), "analyses/health"),
       device = "png", width = 20, height = 12, units = "cm"
)


## Exit vs. remain ----
ggplot(data = model_outcomes_any_graph) +
  # Point estimates
  geom_point(aes(x = estimate, y = height, color = outcome), size = 3) +
  # Add labels under point estimates
  #geom_text_repel(aes(label = estimate)) +
  geom_text(aes(x = estimate, y = height, label = round(estimate, 2)),
            nudge_y = -0.03, size = 2.5, color = "gray33") +
  
  # Confidence intervals
  geom_errorbarh(height = 0.02, 
                 aes(y = height, xmin = conf.low, xmax = conf.high, color = outcome), 
                 size = 1, alpha = 0.5) +
  
  # Hazard ratio = 1 line
  geom_vline(xintercept = 1, color = "black", size = 0.7, alpha = 0.5, linetype = "longdash") +
  
  # Other settings
  scale_y_continuous(breaks = c(0, 0.95, 1.45, 1.95, 2.1),
                     labels = c("", 
                                "Negative exit\n(vs. remaining)", 
                                "Neutral exit\n(vs. remaining)", 
                                "Positive exit\n(vs. remaining)", "")) +
  scale_x_continuous(breaks = c(0, 0.4, 0.8, 1, 1.2, 1.6)) +
  labs(x = "Odds ratio", color = "Outcome") +
  scale_color_brewer(type = "qual", palette = "Dark2") + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(margin = margin(t = 4, b = 4)))

ggsave(filename = "health_outcomes_remain_plot.png",
       path = file.path(here::here(), "analyses/health"),
       device = "png", width = 20, height = 12, units = "cm"
)



# MAKE TABLES FOR MANUSCRIPT ----
# Constants
c_col = c("#1e3048", "#274060", "#2f5375", "#4073a0", "#5088b9")

# Function for formatting
# Adapted from here: https://towardsdatascience.com/create-flawless-tables-from-your-dataframe-ready-for-publication-7e3fe2d63a52
table_formatter <- function(tbl) {
  output <- tbl %>%
    tab_options(table.font.name = "Optima",
                table.font.color = c_col[1],
                table.border.top.style = "none",
                table.border.bottom.style = "solid",
                table.border.bottom.color = c_col[2],
                table.border.bottom.width = px(3),
                column_labels.border.top.color = "white",
                column_labels.border.top.width = px(3),
                column_labels.border.bottom.color = c_col[2],
                column_labels.border.bottom.width = px(3),
                data_row.padding = px(1),
                footnotes.font.size = px(10)
    ) %>%
    tab_style(style = list(
      cell_text(size = px(11))),
      locations = list(cells_body(gt::everything()))
    ) %>% 
    tab_style(style = list(
      cell_text(size = px(14),
                color = "#2f5375",
                font = "Mangal")),
      locations = list(cells_column_labels(everything()),
                       cells_column_spanners(everything()))
    ) %>% 
    tab_style(
      style = "padding-left:10px;padding-right:10px;",
      locations = cells_column_spanners()
    ) %>%
    tab_style(style = list(
      cell_text(size = px(14),
                color = "#2f5375",
                font = "Mangal"),
      cell_borders(sides = c("bottom"),
                   style = "solid",
                   color = c_col[1],
                   weight = px(2))),
      locations = list(cells_row_groups(gt::everything()))
    ) %>% 
    tab_style(style = list(
      cell_text(size = px(12),
                color = "#2f5375",
                font = "Mangal"),
      cell_borders(sides = c("bottom", "right"),
                   style = "solid",
                   color = "white",
                   weight = px(1))),
      locations = list(
        cells_stub(gt::everything()),
        cells_stubhead())
    ) %>% 
    tab_style(style = list(
      cell_text(font = "Mangal", 
                size = px(12), 
                color = "#2f5375")),
      location = list(cells_body(columns = c("group")))
    )
  
  output
}


table_regression <- function(tbl, type = c("any_exit", "exit_type")) {
  # Do some basic setup
  output <- tbl %>%
    rename(group = term) %>%
    mutate(outcome = case_when(outcome == "ED visits" ~ "ed",
                               outcome == "Hospitalizations" ~ "h",
                               str_detect(outcome, "with previous") ~ "wc_w",
                               str_detect(outcome, "without previous") ~ "wc_wo"),
           estimate = as.character(number(estimate, accuracy = 0.01)),
           p.value = case_when(p.value < 0.001 ~ "<0.001",
                               p.value < 0.01 ~ "0.01",
                               p.value < 0.05 ~ "<0.05",
                               TRUE ~ as.character(round(p.value, 3))),
           ci = paste0(number(conf.low, accuracy = 0.01), "–", 
                       number(conf.high, accuracy = 0.01)),
           order = 2L,
           category = case_when(str_detect(group, "exit_category") ~ "Exit category",
                                str_detect(group, "age_") ~ "Age",
                                str_detect(group, "gender_") ~ "Gender",
                                str_detect(group, "race_") ~ "Race/ethnicity",
                                str_detect(group, "^los") ~ "Time in housing",
                                group %in% c("hh_size", "single_caregiver", "hh_disability") ~ 
                                  "Household characteristics",
                                str_detect(group, "major_prog") ~ "Program type",
                                group %in% c("crisis_any_prior", "crisis_ed_any_prior", 
                                             "recent_homeless_grp", 
                                             "ed_cnt_prior", "ed_any_prior",
                                             "hosp_cnt_prior", "hosp_any_prior",
                                             "ccw_flag") ~ "Health",
                                group == "kc_opp_index_score" ~ "Neighborhood opportunity")) %>%
    select(order, outcome, category, group, estimate, ci, p.value) %>%
    pivot_wider(id_cols = c(category, group, order), 
                names_from = outcome, 
                values_from = c(estimate, ci, p.value))
  
  # Make and bind the reference rows
  ref_rows <- data.frame(category = c("Exit category", "Gender", "Race/ethnicity",
                                      "Age", "Time in housing", "Program type"),
                         group = c("exit_categoryRemained", "gender_meFemale", "race_eth_meWhite",
                                   "age_grp<25", "los<3", "major_progHCV"),
                         estimate_ed = rep("ref", 6), 
                         estimate_h = rep("ref", 6), 
                         estimate_wc_w = c(rep("ref", 3), NA_character_, "ref", "ref"), 
                         estimate_wc_wo = c(rep("ref", 3), NA_character_, "ref", "ref"), 
                         order = rep(1L, 6))
  
  if (type == "exit_type") {
    ref_rows <- ref_rows %>%
      mutate(group = case_when(group == "exit_categoryRemained" ~ "exit_category_negNegative",
                               TRUE ~ group))
  }
  
  output <- output %>%
    bind_rows(., ref_rows) %>%
    mutate(cat_order = case_when(category == "Exit category" ~ 1L,
                                 category == "Age" ~ 2L,
                                 category == "Gender" ~ 3L,
                                 category == "Race/ethnicity" ~ 4L,
                                 category == "Time in housing" ~ 5L,
                                 category == "Household characteristics" ~ 6L,
                                 category == "Program type" ~ 7L,
                                 category == "Health" ~ 8L),
           group_order = case_when(group %in% c("recent_homeless_grp", "los<3", 
                                                "hh_size", "age_grp25-44", 
                                                "exit_categoryPositive", "exit_category_negPositive") ~ 1L,
                                   group %in% c("crisis_any_prior", "los3-5.99", 
                                                "single_caregiver", "age_grp45-64",
                                                "exit_categoryNeutral", "exit_category_negNeutral") ~ 2L,
                                   group %in% c("crisis_ed_any_prior", "los6-9.99", "hh_disability",
                                                "exit_categoryNegative") ~ 3L,
                                   group %in% c("ed_cnt_prior", "ed_any_prior", "los10+") ~ 4L,
                                   group %in% c("hosp_cnt_prior", "hosp_any_prior") ~ 5L,
                                   group %in% c("ccw_cnt", "ccw_flag", "age_at_exit") ~ 6L)) %>%
    arrange(cat_order, order, group_order, group) %>%
    filter(group != "(Intercept)") %>%
    select(-ends_with("order")) %>%
    mutate(group = case_when(group == "hh_size" ~ "Household size",
                            group == "single_caregiver" ~ "Single caregiver",
                            group == "hh_disability" ~ "HoH disability",
                            group == "kc_opp_index_score" ~ "Neighborhood opportunity",
                            group == "recent_homeless_grp" ~ "Experienced recent homelessness",
                            group == "crisis_any_prior" ~ 
                              paste0("Experienced 1+ crisis event in year prior to exit ",
                                     "(excl. ED visits)"),
                            group == "crisis_ed_any_prior" ~ 
                              paste0("Experienced 1+ crisis event in year prior to exit ",
                                     "(incl. ED visits)"),
                            group == "ed_any_prior" ~ "Experienced 1+ ED visit in year prior to exit",
                            group == "ed_cnt_prior" ~ "No. ED visits in year prior to exit",
                            group == "hosp_any_prior" ~ "Experienced 1+ hospitalization in year prior to exit",
                            group == "hosp_cnt_prior" ~ "No. hospitalizations in year prior to exit",
                            group == "ccw_flag" ~ "2+ chronic conditions",
                            group == "age_at_exit" ~ "Age at exit (years)",
                            TRUE ~ str_remove(group, "age_grp|gender_me|los|major_prog|race_eth_me|exit_category_neg|exit_category")))
  
  # Turn into gt table
  output <- output %>%
    gt(groupname_col = "category", rowname_col = "group") %>%
    tab_spanner(label = md("ED visits"), columns = ends_with("_ed")) %>%
    tab_spanner(label = md("Hospitalizations"), columns = ends_with("_h")) %>%
    tab_spanner(label = md("Well-child checks <br>(with previous visit)"), 
                columns = ends_with("_wc_w")) %>%
    tab_spanner(label = md("Well-child checks <br>(without previous visit)"), 
                columns = ends_with("wc_wo")) %>%
    cols_label(category = md("Category"),
               group = md("Group"),
               estimate_ed = md("Odds ratio"),
               ci_ed = md("95% CI"),
               p.value_ed = md("p-value"),
               estimate_h = md("Odds ratio"),
               ci_h = md("95% CI"),
               p.value_h = md("p-value"),
               estimate_wc_w = md("Odds ratio"),
               ci_wc_w = md("95% CI"),
               p.value_wc_w = md("p-value"),
               estimate_wc_wo = md("Odds ratio"),
               ci_wc_wo = md("95% CI"),
               p.value_wc_wo = md("p-value")) %>%
    tab_footnote(footnote = "Too few with multiple gender to include in model for well-child checks", 
                 locations = cells_row_groups(groups = "Gender")) %>%
    tab_footnote(footnote = "AI/AN = American Indian/Alaskan Native, NH/PI = Native Hawaiian/Pacific Islander", 
                 locations = cells_row_groups(groups = "Race/ethnicity")) %>%
    tab_footnote(footnote = "HoH = Head of household", 
                 locations = cells_stub(rows = str_detect(group, "HoH"))) %>%
    tab_footnote(footnote = "HCV = Housing Choice Voucher, PH = Public housing", 
                 locations = cells_row_groups(groups = "Program type")) %>%
    sub_missing()
    
  output
}


## Table 1: demographics by exit and exit type ----
# Set up n for col names
n_exit_any <- exit_control %>% count(id_type) %>% deframe()
n_exit_type <- exit_nodeath %>% count(exit_category) %>% deframe()

n_exit_any_hh <- exit_control %>% distinct(hh_id_kc_pha, exit_date, id_type) %>% 
  count(id_type) %>% deframe()
n_exit_type_hh <- exit_nodeath %>% distinct(hh_id_kc_pha, exit_date, exit_category) %>% 
  count(exit_category) %>% deframe()

n_exit_any_wc <- exit_control %>% filter(age_at_exit < 6) %>% 
  distinct(id_hudhears, exit_date, id_type) %>% 
  count(id_type) %>% deframe()
n_exit_type_wc <- exit_nodeath %>% filter(age_at_exit < 6) %>% 
  distinct(id_hudhears, exit_date, exit_category) %>% 
  count(exit_category) %>% deframe()


# Make table
table_1_demogs <- left_join(exit_any_demogs, exit_type_demogs,
                            by = c("category", "group")) %>%
  # Remove unwanted groups
  filter(!group %in% c("n", "Range (years)", "Child (aged <18)", "Senior (aged 62+)")) %>% 
  filter(str_detect(group, "Did not experience", negate = T)) %>%
  filter(str_detect(group, "crisis", negate = T)) %>%
  # Do some renaming
  rename("Remained" = "id_control", "Exited" = "id_exit") %>%
  mutate(category = str_replace_all(category, "HoH time", "Time"),
         group = str_replace_all(group, " time in housing \\(years\\)", " time (years)")) %>%
  distinct() %>%
  # Set up table
  gt(groupname_col = "category", rowname_col = "group") %>%
  tab_footnote(footnote = "AI/AN = American Indian/Alaskan Native, NH/PI = Native Hawaiian/Pacific Islander", 
               locations = cells_row_groups(groups = "Race/ethnicity")) %>%
  tab_footnote(footnote = md(glue("At household level (",
                                  "Remained N={number(n_exit_any_hh[1], big.mark = ',')}, ",
                                  "Exited N={number(n_exit_any_hh[2], big.mark = ',')}, ",
                                  "Negative N={number(n_exit_type_hh[1], big.mark = ',')}, ",
                                  "Neutral N={number(n_exit_type_hh[2], big.mark = ',')}, ",
                                  "Positive N={number(n_exit_type_hh[3], big.mark = ',')}",
                                  ")")), 
               locations = cells_row_groups(groups = c("Time in housing", "Household characteristics",
                                          "Program type"))) %>%
  tab_footnote(footnote = "HCV = Housing Choice Voucher, PH = Public housing", 
               locations = cells_row_groups(groups = "Program type")) %>%
  tab_footnote(footnote = md(glue("Ages <6 (",
                                  "Remained N={number(n_exit_any_wc[1], big.mark = ',')}, ",
                                  "Exited N={number(n_exit_any_wc[2], big.mark = ',')}, ",
                                  "Negative N={number(n_exit_type_wc[1], big.mark = ',')}, ",
                                  "Neutral N={number(n_exit_type_wc[2], big.mark = ',')}, ",
                                  "Positive N={number(n_exit_type_wc[3], big.mark = ',')}",
                                  ")")),
               locations = cells_stub(rows = str_detect(group, "well-child"))) %>%
  cols_label(category = md("Category"),
             group = md("Group"),
             Remained = md(paste0("Remained (N=", number(n_exit_any[1], big.mark = ","), ")")),
             Exited = md(paste0("Exited (N=", number(n_exit_any[2], big.mark = ","), ")")),
             Negative = md(paste0("Negative exit (N=", number(n_exit_type[1], big.mark = ","), ")")),
             Neutral = md(paste0("Neutral exit (N=", number(n_exit_type[2], big.mark = ","), ")")),
             Positive = md(paste0("Positive exit (N=", number(n_exit_type[3], big.mark = ","), ")")))

table_1_demogs <- table_formatter(table_1_demogs)

# Save output
gtsave(table_1_demogs, filename = "health_manuscript_table1.png",
       path = file.path(here::here(), "analyses/health"))


## Supplemental tables: regression output ----
# Exit type regression
table_2_regression_type <- table_regression(model_outcomes, type = "exit_type")
table_2_regression_type <- table_formatter(table_2_regression_type)

gtsave(table_2_regression_type, filename = "health_manuscript_table_supp1.png",
       path = file.path(here::here(), "analyses/health"))


# Any exit regression
table_3_regression_any <- table_regression(model_outcomes_any, type = "any_exit")
table_3_regression_any <- table_formatter(table_3_regression_any)

gtsave(table_3_regression_any, filename = "health_manuscript_table_supp2.png",
       path = file.path(here::here(), "analyses/health"))
