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
               ggplot2, ggrepel, viridis, hrbrthemes, knitr, rmarkdown, flextable)

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


## Add exit type and opportunity index ----
exit_nodeath <- covariate %>% filter(exit_death != 1 & exit == 1) %>%
  left_join(., exits, by = c("agency", "id_hudhears", "exit_date" = "act_date", "exit_reason_clean", "exit_category"))


## Set up outcomes and inclusion flag ----
exit_nodeath <- exit_nodeath %>%
  mutate(
    ed_any_prior = ifelse(ed_cnt_prior >= 1, 1L, 0L),
    ed_any_after = ifelse(ed_cnt_after >= 1, 1L, 0L),
    hosp_any_prior = ifelse(hosp_cnt_prior >= 1, 1L, 0L),
    hosp_any_after = ifelse(hosp_cnt_after >= 1, 1L, 0L),
    wc_any_prior = ifelse(wc_cnt_prior >= 1, 1L, 0L),
    wc_any_after = ifelse(wc_cnt_after >= 1, 1L, 0L),
    # Remove spurious ages
    age_at_exit = ifelse(age_at_exit > 125, NA_integer_, age_at_exit),
    # Flag people with coverage before and after exit
    include_cov = full_cov_7_prior == T & full_cov_7_after == T,
    # Flag anyone with missing covariates since they will be dropped from the propensity scores
    include_demog = (!(is.na(exit_category) | is.na(age_at_exit) | is.na(gender_me) | 
                         is.na(race_eth_me) | race_eth_me == "Unknown" |
                         is.na(agency) | is.na(single_caregiver) | 
                         is.na(hh_size) | is.na(hh_disability) | is.na(housing_time_at_exit) | is.na(major_prog) | 
                         is.na(kc_opp_index_score))
    ))


# DEMOGRAPHICS ----
# Functions are found in the pha_exit_factors.R code

## Demogs by Medicaid status ----
# Age
exit_age <- age_sum(exit_nodeath, full_cov_7_prior, full_cov_7_after)

# Gender
exit_gender <- demog_pct_sum(exit_nodeath, level = "ind", demog = "gender", full_cov_7_prior, full_cov_7_after)

# Race/eth
exit_race <- demog_pct_sum(exit_nodeath, level = "ind", demog = "race", full_cov_7_prior, full_cov_7_after)

# Program type
exit_prog <- demog_pct_sum(exit_nodeath, level = "ind", demog = "program", full_cov_7_prior, full_cov_7_after)

# Time in housing (measured for head of household)
exit_los <- hh_los_sum(exit_nodeath, full_cov_7_prior, full_cov_7_after)


## Demogs by exit ----
# Age
exit_type_age <- age_sum(exit_nodeath %>% filter(include_cov == T & include_demog == T), exit_category)

# Gender
exit_type_gender <- demog_pct_sum(exit_nodeath %>% filter(include_cov == T & include_demog == T), 
                                  level = "ind", demog = "gender", exit_category)

# Race/eth
exit_type_race <- demog_pct_sum(exit_nodeath %>% filter(include_cov == T & include_demog == T), 
                                level = "ind", demog = "race", exit_category)

# Time in housing (this is based on HH data)
exit_type_hh_los <- hh_los_sum(exit_nodeath %>% filter(include_cov == T & include_demog == T), exit_category)

# Combine for R markdown
exit_type_ind <- bind_rows(exit_type_age, exit_type_gender, exit_type_race, exit_type_hh_los)


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
      distinct(id_kc_pha, exit_date, ..., ed_cnt_prior, ed_any_prior) %>%
      mutate(cnt = ed_cnt_prior, any = ed_any_prior)
  } else if (outcome == "ed_after") {
    cat_text <- "ED visits after exit"
    output <- df %>% 
      distinct(id_kc_pha, exit_date, ..., ed_cnt_after, ed_any_after) %>%
      mutate(cnt = ed_cnt_after, any = ed_any_after)
  } else if (outcome == "hosp_prior") {
    cat_text <- "Hospitalizations prior to exit"
    output <- df %>% 
      distinct(id_kc_pha, exit_date, ..., hosp_cnt_prior, hosp_any_prior) %>%
      mutate(cnt = hosp_cnt_prior, any = hosp_any_prior)
  } else if (outcome == "hosp_after") {
    cat_text <- "Hospitalizations after exit"
    output <- df %>% 
      distinct(id_kc_pha, exit_date, ..., hosp_cnt_after, hosp_any_after) %>%
      mutate(cnt = hosp_cnt_after, any = hosp_any_after)
  } else if (outcome == "wc_prior") {
    cat_text <- "Well child checks prior to exit"
    output <- df %>% 
      filter(between(age_at_exit, 2, 6)) %>%
      distinct(id_kc_pha, exit_date, ..., wc_cnt_prior, wc_any_prior) %>%
      mutate(cnt = wc_cnt_prior, any = wc_any_prior)
  } else if (outcome == "wc_after") {
    cat_text <- "Well child checks after exit"
    output <- df %>% 
      filter(between(age_at_exit, 2, 6)) %>%
      distinct(id_kc_pha, exit_date, ..., wc_cnt_after, wc_any_after) %>%
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
              any = scales::percent(mean(any, na.rm = T), accuracy = 0.1L),
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

## Outcomes by exit ----
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



# REGRESSION MODEL ----
## Set up data ----
model_data_mcaid <- exit_nodeath %>%
  # Remove anyone with missing covariates since they will be dropped from the propensity scores
  filter(include_cov == T & include_demog == T) %>%
  # Also remove people without full coverage
  filter(full_cov_7_prior == T & full_cov_7_after == T) %>%
  mutate(agegrp = case_when(age_at_exit < 18 ~ "<18",
                            data.table::between(age_at_exit, 18, 24.99, NAbounds = NA) ~ "18-24",
                            data.table::between(age_at_exit, 25, 44.99, NAbounds = NA) ~ "25-44",
                            data.table::between(age_at_exit, 45, 64.99, NAbounds = NA) ~ "45-64",
                            age_at_exit >= 65 ~ "65+",
                            is.na(age_at_exit) ~ NA_character_),
         los = case_when(housing_time_at_exit < 3 ~ "<3",
                         between(housing_time_at_exit, 3, 5.999, NAbounds = NA) ~ "3-5.99",
                         between(housing_time_at_exit, 6, 9.999, NAbounds = NA) ~ "6-9.99",
                         housing_time_at_exit >= 10 ~ "10+"))


# Set up numeric IDs for households
# Needed for geeglm to work properly
hh_ids <- model_data_mcaid %>% filter(!is.na(hh_id_kc_pha)) %>%
  distinct(hh_id_kc_pha) %>%
  arrange(hh_id_kc_pha) %>%
  mutate(id_hh = row_number())

model_data_mcaid <- model_data_mcaid %>%
  left_join(., hh_ids, by = "hh_id_kc_pha")


## Set up generic function ----
model_run <- function(outcome = c("ed", "hosp", "wc"), neutral = T) {
  outcome <- match.arg(outcome)
  
  # Set up propensity scores
  if (outcome == "ed") {
    ps_model <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + 
                            race_eth_me + agency + single_caregiver + hh_size +
                            hh_disability + housing_time_at_exit + major_prog +
                            ed_cnt_prior + ccw_flag + kc_opp_index_score,
                          data = model_data_mcaid,
                          id = hh_id_kc_pha,
                          LORstr = "independence")
  } else if (outcome == "hosp") {
    ps_model <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + 
                            race_eth_me + agency + single_caregiver + hh_size +
                            hh_disability + housing_time_at_exit + major_prog +
                            hosp_cnt_prior + ccw_flag + kc_opp_index_score,
                          data = model_data_mcaid,
                          id = hh_id_kc_pha,
                          LORstr = "independence")
  } else if (outcome == "wc") {
    model_data_mcaid <- model_data_mcaid %>% filter(between(age_at_exit, 2, 6))
    
    ps_model <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + 
                            race_eth_me + agency + single_caregiver + hh_size +
                            hh_disability + housing_time_at_exit + major_prog +
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
    # Set up version with no neutral
    model_data_mcaid <- model_data_mcaid %>% filter(exit_category != "Neutral")
  }
  
  # Set exit category up as factor
  model_data_mcaid$exit_category <- relevel(factor(model_data_mcaid$exit_category), ref = "Negative")
  
  
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


test <- model_run(outcome = "ed")
summary(test)
broom::tidy(test, conf.int = TRUE, exponentiate = T)

test2 <- model_run(outcome = "ed", neutral = F)
summary(test2)
broom::tidy(test2, conf.int = TRUE, exponentiate = T)



## Evaluate propensity scores ----
ps_chk1 <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + 
                        race_eth_me + agency + single_caregiver + hh_size +
                        hh_disability + housing_time_at_exit + major_prog +
                        ed_cnt_prior + ccw_flag + kc_opp_index_score,
                      data = model_data_mcaid,
                      id = id_hh,
                      LORstr = "independence")
ps_chk1 <- as.data.frame(fitted(ps_chk1))
colnames(ps_chk1) <- c("neutral", "negative", "positive")

summarytools::dfSummary(ps_chk1)

ps_chk2 <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + 
                        race_eth_me + agency + single_caregiver + hh_size +
                        hh_disability + housing_time_at_exit + major_prog +
                        ed_cnt_prior + ccw_flag + kc_opp_index_score,
                      data = model_data_mcaid,
                      id = hh_id_kc_pha,
                      LORstr = "independence")
ps_chk2 <- as.data.frame(fitted(ps_chk2))
colnames(ps_chk2) <- c("neutral", "negative", "positive")

summarytools::dfSummary(ps_chk2)


## ED visits ----
### Propensity score ----
ed <- model_run(outcome = "ed")
summary(ed)
broom::tidy(ed, conf.int = TRUE, exponentiate = T)


### Crude ----
ed_crude <- geepack::geeglm(ed_any_after ~ exit_category, 
                data = model_data_mcaid_pn,
                id = id_hh,
                family = "binomial")

summary(ed_crude)
broom::tidy(ed_crude, conf.int = TRUE, exponentiate = T)


### Adjusted ----
# Positive vs negative model
ed_adj_bin <- geepack::geeglm(ed_any_after ~ exit_category + gender_me + race_eth_me + age_at_exit + housing_time_at_exit + 
                                 agency + major_prog + hh_size + single_caregiver + hh_disability + 
                                 ed_cnt_prior + ccw_flag, 
                               data = model_data_mcaid[model_data_mcaid$exit_category != "Neutral" &
                                                         model_data_mcaid$full_cov_7_prior == T & 
                                                         model_data_mcaid$full_cov_7_after == T, ], 
                               id = id_hh,
                               family = "binomial")

broom::tidy(ed_adj_bin, conf.int = TRUE, exponentiate = T)

# Multinomial model
ed_adj_mult <- geepack::geeglm(ed_any_after ~ exit_category + gender_me + race_eth_me + age_at_exit + housing_time_at_exit + 
                                 agency + major_prog + hh_size + single_caregiver + hh_disability + 
                                 ed_cnt_prior + ccw_flag, 
                               data = model_data_mcaid[model_data_mcaid$full_cov_7_prior == T & 
                                                         model_data_mcaid$full_cov_7_after == T, ], 
                               id = id_hh,
                               family = "binomial")

broom::tidy(ed_adj_mult, conf.int = TRUE, exponentiate = T)



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
# Positive vs negative model
hosp_adj_bin <- geepack::geeglm(hosp_any_after ~ exit_category + gender_me + race_eth_me + age_at_exit + housing_time_at_exit + 
                                agency + major_prog + hh_size + single_caregiver + hh_disability + 
                                hosp_cnt_prior + ccw_flag, 
                              data = model_data_mcaid[model_data_mcaid$exit_category != "Neutral" &
                                                        model_data_mcaid$full_cov_7_prior == T & 
                                                        model_data_mcaid$full_cov_7_after == T, ], 
                              id = id_hh,
                              family = "binomial")

broom::tidy(hosp_adj_bin, conf.int = TRUE, exponentiate = T)

# Multinomial model
hosp_adj_mult <- geepack::geeglm(hosp_any_after ~ exit_category + gender_me + race_eth_me + age_at_exit + housing_time_at_exit + 
                                 agency + major_prog + hh_size + single_caregiver + hh_disability + 
                                 hosp_cnt_prior + ccw_flag, 
                               data = model_data_mcaid[model_data_mcaid$full_cov_7_prior == T & 
                                                         model_data_mcaid$full_cov_7_after == T, ], 
                               id = id_hh,
                               family = "binomial")

broom::tidy(hosp_adj_mult, conf.int = TRUE, exponentiate = T)


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
# Positive vs negative model
wc_adj_bin <- geepack::geeglm(wc_any_after ~ exit_category + gender_me + race_eth_me + age_at_exit + housing_time_at_exit + 
                                agency + major_prog + hh_size + single_caregiver + hh_disability + 
                                wc_cnt_prior + ccw_flag, 
                              data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6) & 
                                                        model_data_mcaid$exit_category != "Neutral" &
                                                        model_data_mcaid$full_cov_7_prior == T & 
                                                        model_data_mcaid$full_cov_7_after == T, ], 
                              id = id_hh,
                              family = "binomial")

broom::tidy(wc_adj_bin, conf.int = TRUE, exponentiate = T)

# Multinomial model
wc_adj_mult <- geepack::geeglm(wc_any_after ~ exit_category + gender_me + race_eth_me + age_at_exit + housing_time_at_exit + 
                                 agency + major_prog + hh_size + single_caregiver + hh_disability + 
                                 wc_cnt_prior + ccw_flag, 
                               data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6) & 
                                                         model_data_mcaid$full_cov_7_prior == T & 
                                                         model_data_mcaid$full_cov_7_after == T, ], 
                               id = id_hh,
                               family = "binomial")

broom::tidy(wc_adj_mult, conf.int = TRUE, exponentiate = T)


### Stratified: prior visits ----
# Crude
wc_strat_wc <- geepack::geeglm(wc_any_after ~ exit_category,
                               data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6) & 
                                                         model_data_mcaid$exit_category != "Neutral" &
                                                         model_data_mcaid$wc_cnt_prior >= 1, ], 
                               id = id_hh,
                               family = "binomial")

summary(wc_strat_wc)
broom::tidy(wc_strat_wc, conf.int = TRUE, exponentiate = T)


# Adjusted
wc_strat_wc_adj <- geepack::geeglm(wc_any_after ~ exit_category + gender_me + race_eth_me + age_at_exit + 
                                     housing_time_at_exit + agency + major_prog + hh_size + 
                                     single_caregiver + hh_disability + wc_cnt_prior + ccw_flag, 
                               data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6) & 
                                                         model_data_mcaid$full_cov_7_prior == T & 
                                                         model_data_mcaid$full_cov_7_after == T &
                                                         model_data_mcaid$wc_cnt_prior >= 1, ], 
                               id = id_hh,
                               family = "binomial")

broom::tidy(wc_strat_wc_adj, conf.int = TRUE, exponentiate = T)


### Stratified: prior visits ----
# Crude
wc_strat_no_wc <- geepack::geeglm(wc_any_after ~ exit_category,
                               data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6) & 
                                                         model_data_mcaid$exit_category != "Neutral" &
                                                         model_data_mcaid$wc_cnt_prior < 1, ], 
                               id = id_hh,
                               family = "binomial")

summary(wc_strat_no_wc)
broom::tidy(wc_strat_no_wc, conf.int = TRUE, exponentiate = T)


# Adjusted
wc_strat_no_wc_adj <- geepack::geeglm(wc_any_after ~ exit_category + gender_me + race_eth_me + age_at_exit + 
                                     housing_time_at_exit + agency + major_prog + hh_size + 
                                     single_caregiver + hh_disability + wc_cnt_prior + ccw_flag, 
                                   data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6) & 
                                                             model_data_mcaid$full_cov_7_prior == T & 
                                                             model_data_mcaid$full_cov_7_after == T &
                                                             model_data_mcaid$wc_cnt_prior < 1, ], 
                                   id = id_hh,
                                   family = "binomial")

broom::tidy(wc_strat_no_wc_adj, conf.int = TRUE, exponentiate = T)




## Combine results ----
# Make it easier to see all outcomes in one place
model_outcomes <- bind_rows(broom::tidy(ed_adj_mult, conf.int = TRUE, exponentiate = T) %>%
                              filter(str_detect(term, "exit_")) %>% mutate(outcome = "ED visits"),
                            broom::tidy(hosp_adj_mult, conf.int = TRUE, exponentiate = T) %>%
                              filter(str_detect(term, "exit_")) %>% mutate(outcome = "Hospitalizations"),
                            broom::tidy(wc_adj_mult, conf.int = TRUE, exponentiate = T) %>%
                              filter(str_detect(term, "exit_")) %>% mutate(outcome = "Well-child checks")
                            ) %>%
  mutate(term = case_when(str_detect(term, "Neutral") ~ "Neutral vs. negative",
                          str_detect(term, "Positive") ~ "Positive vs. negative"),
         height = c(1.1, 1.6, 1, 1.5, 0.9, 1.4))

# Make plot
# Create a boxplot with ggplot
ggplot(data = model_outcomes) +
  # Point estimates
  geom_point(aes(x = estimate, y = height, color = outcome), size = 3) +
  # Add labels under point estimates
  #geom_text_repel(aes(label = estimate)) +
  geom_text(aes(x = estimate, y = height, label = round(estimate, 2)),
            nudge_y = -0.02, size = 3, color = "gray33") +
  
  # Confidence intervals
  geom_errorbarh(height = 0.02, 
                 aes(y = height, xmin = conf.low, xmax = conf.high, color = outcome), 
                 size = 1, alpha = 0.5) +
  
  # Hazard ratio = 1 line
  geom_vline(xintercept = 1, color = "black", size = 1, alpha = 0.5, linetype = "longdash") +
  
  # Other settings
  scale_y_continuous(breaks = c(0, 1, 1.5, 1.6),
                     labels = c("", "Neutral exit\n(v. negative)", 
                                "Positive exit\n(v. negative)", "")) +
  labs(x = "Odds ratio", color = "Outcome") +
  scale_color_viridis(discrete = TRUE, option = "plasma") + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line())



# RUN MARKDOWN FILE ----
render(input = file.path(here::here(), "analyses/health/pha_exit_outcomes_mcaid.Rmd"), 
       output_format = "word_document",
       output_file = "CSTE_2022_health_outcomes",
       output_dir = "C:/Users/mathesal/King County/DPH Health And Housing - Documents/HUD HEARS Study/Presentations and papers/")
