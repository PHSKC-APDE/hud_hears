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
               ggplot2, viridis, hrbrthemes, knitr, rmarkdown, flextable)

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


# BRING IN DATA ----
covariate <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.control_match_covariate") %>%
  mutate(exit = case_when(id_type == "id_exit" ~ 1L,
                          id_type == "id_control" ~ 0L))

exits <- dbGetQuery(db_hhsaw, "SELECT DISTINCT agency, id_hudhears, act_date, exit_reason, 
                    exit_reason_clean, exit_category_pha, exit_category
                    FROM pha.stage_pha_exit_timevar
                    WHERE chooser = chooser_max AND
                    true_exit = 1 AND act_date IS NOT NULL AND
                    ((act_date >= '2016-01-01' AND act_date <= '2018-12-31' AND agency = 'KCHA') OR
                    (act_date >= '2012-01-01' AND act_date <= '2018-12-31' AND agency = 'SHA'))")

# load opportunity index data (version standardized in King County)
kc_opp_index_data <- read_csv(file.path(here::here(), "analyses/capstone/00_opportunity_index",
                                        "kc_opp_indices_scaled.csv")) %>%
  # create variables for state, county, and tract identifies
  mutate(GEO_STATE= substr(kc_opp_index_data$GEOID10, 1, 2),
         GEO_COUNTY= substr(kc_opp_index_data$GEOID10, 3, 5),
         GEO_TRACT= substr(kc_opp_index_data$GEOID10, 6, 11)) %>%
  select(GEOID10, GEO_STATE, GEO_COUNTY, GEO_TRACT, everything()) %>%
  rename(kc_opp_index_score = OPP_Z)


## Sort out multiple exit types ----
set.seed(98104)
exits <- exits %>% 
  mutate(exit_pos = case_when(is.na(exit_category) ~ NA_integer_,
                              exit_category == "Positive" ~ 1L,
                              TRUE ~ 0L),
         exit_neg = case_when(is.na(exit_category) ~ NA_integer_,
                              exit_category == "Negative" ~ 1L,
                              TRUE ~ 0L),
         exit_neu = case_when(is.na(exit_category) ~ NA_integer_,
                              exit_category == "Neutral" ~ 1L,
                              TRUE ~ 0L),
         decider = runif(nrow(exits), 0, 1)) %>%
  group_by(id_hudhears, act_date) %>%
  mutate(row_n = n(),
         exit_pos_sum = sum(exit_pos, na.rm = T),
         exit_neg_sum = sum(exit_neg, na.rm = T),
         exit_neu_sum = sum(exit_neu, na.rm = T),
         decider_max = max(decider)) %>%
  ungroup()

# Logic for keeping rows (there are only max 2 in a duplicate set)
# 1) If one positive/negative and one neutral, keep the positive/negative
#    (from review, these appear to be more descriptive)
# 2) If one positive and one negative, take positive 
#    (mostly over income/moved to non-subsidized rental vs. vaguer negative reason)
# 3) If both one category, randomly select one
exits <- exits %>%
  mutate(drop = case_when(row_n == 1 ~ 0L,
                          (exit_pos_sum == 1 | exit_neg_sum == 1) & exit_neu_sum == 1 &
                            exit_category %in% c("Positive", "Negative") ~ 0L,
                          (exit_pos_sum == 1 | exit_neg_sum == 1) & exit_neu_sum == 1 &
                            exit_category == "Neutral" ~ 1L,
                          exit_pos_sum == 1 & exit_neg_sum == 1 & exit_category == "Positive" ~ 0L,
                          exit_pos_sum == 1 & exit_neg_sum == 1 & exit_category == "Negative" ~ 1L,
                          (exit_pos_sum == 2 | exit_neg_sum == 2 | exit_neu_sum == 2) &
                            decider == decider_max ~ 0L,
                          (exit_pos_sum == 2 | exit_neg_sum == 2 | exit_neu_sum == 2) &
                            decider != decider_max ~ 1L
                          )) %>%
  filter(drop == 0) %>%
  select(id_hudhears:exit_neu)


## Add exit type and opportunity index ----
exit_nodeath <- covariate %>% filter(exit_death != 1 & exit == 1) %>%
  left_join(., exits, by = c("id_hudhears", "exit_date" = "act_date", "exit_reason_clean"))

exit_nodeath <- left_join(exit_nodeath,
                          distinct(kc_opp_index_data, GEO_TRACT, kc_opp_index_score),
                          by= c("geo_tractce10" = "GEO_TRACT"))


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
## Demogs by Medicaid status ----
# Age
exit_age <- exit_nodeath %>% 
  mutate(senior = case_when(age_at_exit >= 62 ~ 1L, age_at_exit < 62 ~ 0L),
         child = case_when(age_at_exit < 18 ~ 1L, age_at_exit >= 18 ~ 0L)) %>%
  filter(!is.na(age_at_exit)) %>%
  group_by(full_cov_7_prior, full_cov_7_after) %>%
  summarise(n = n(), 
            age_mean = mean(age_at_exit),
            age_med = median(age_at_exit),
            age_min = min(age_at_exit),
            age_max = max(age_at_exit),
            senior = mean(senior),
            child = mean(child))

# Gender
exit_gender <- exit_nodeath %>% 
  count(full_cov_7_prior, full_cov_7_after, gender_me) %>%
  group_by(full_cov_7_prior, full_cov_7_after) %>%
  mutate(tot = sum(n), pct = round(n/tot*100,1))

# Race/eth
exit_race <- exit_nodeath %>% 
  count(full_cov_7_prior, full_cov_7_after, race_eth_me) %>%
  group_by(full_cov_7_prior, full_cov_7_after) %>%
  mutate(tot = sum(n), pct = round(n/tot*100,1)) %>%
  as.data.frame()

# Time in housing
exit_los <- exit_nodeath %>% 
  filter(!is.na(housing_time_at_exit)) %>%
  group_by(full_cov_7_prior, full_cov_7_after) %>%
  summarise(n = n(),
            los_mean = mean(housing_time_at_exit),
            los_med = median(housing_time_at_exit))


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
      mutate(cnt = hosp_cnt_prior, any = hosp_any_prior)
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
  
  output <- output %>% 
    group_by(...) %>%
    summarise(n = number(n(), big.mark = ","), 
              any = scales::percent(mean(any, na.rm = T), accuracy = 0.1L),
              visit_mean = round(mean(cnt, na.rm = T), 1), 
              vist_med = median(cnt, na.rm = T),
              visit_range = paste0(min(cnt, na.rm = T), "-", max(cnt, na.rm = T))) %>% 
    pivot_longer(cols = !col_names, values_transform = list(value = as.character)) %>%
    pivot_wider(id_cols = "name", names_from = col_names) %>%
    mutate(category = cat_text, .before = "name",
           name = case_when(name == "any" ~ "Proportion with 1+ event",
                            name == "visit_mean" ~ "Mean number events",
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
                             outcome = "hosp_prior", exit_category)

wc_type_prior <- outcome_sum(df = exit_nodeath %>% filter(include_cov == T & include_demog == T),
                             outcome = "wc_prior", exit_category)

wc_type_after <- outcome_sum(df = exit_nodeath %>% filter(include_cov == T & include_demog == T),
                             outcome = "wc_after", exit_category)


# Combine outcomes
outcomes_type <- bind_rows(ed_type_prior, ed_type_after, hosp_type_prior,
                           hosp_type_after, wc_type_prior, wc_type_after) %>%
  filter(!group == "n")


# REGRESSION MODEL ----
## Set up data ----
model_data_mcaid <- exit_nodeath %>%
  # Remove anyone with missing covariates since they will be dropped from the propensity scores
  filter(include_cov == T & include_demog == T) %>%
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

## Set up propensity scores ----
ps_model <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + 
                  race_eth_me + agency + single_caregiver + hh_size +
                  hh_disability + housing_time_at_exit + major_prog +
                  kc_opp_index_score,
                data = model_data_mcaid,
                id = hh_id_kc_pha,
                LORstr = "independence")


# Next, calculate generalized propensity scores
ps <- as.data.frame(fitted(ps_model))
colnames(ps) <- c("neutral", "negative", "positive")
ps <- cbind("id_kc_pha" = model_data_mcaid$id_kc_pha, ps)


model_data_mcaid <- model_data_mcaid %>%
  left_join(., ps, by = "id_kc_pha") %>%
  filter(full_cov_7_prior == T & full_cov_7_after == T) %>%
  mutate(iptw = case_when(exit_category == "Neutral" ~ 1/neutral,
                          exit_category == "Negative" ~ 1/negative,
                          exit_category == "Positive" ~ 1/positive))
 

model_data_mcaid$exit_category <- relevel(factor(model_data_mcaid$exit_category), ref = "Neutral")


## ED visits ----
### PS/GEE ----
ed <- geepack::geeglm(formula = ed_any_after ~ exit_category,
                      weights = iptw,
                      data = model_data_mcaid,
                      id = hh_id_kc_pha,
                      family = "binomial")

### Crude ----
ed_crude <- glm(ed_any_after ~ exit_category, 
                data = model_data_mcaid[model_data_mcaid$exit_category != "Neutral", ], family = "binomial")

summary(ed_crude)
exp(cbind(OR = coef(ed_crude), confint(ed_crude)))


### Adjusted ----
ed_adj <- glm(ed_any_after ~ exit_category + gender_me + race_eth_me + agegrp + los + 
                       major_prog + hh_size + single_caregiver + hh_disability + 
                       ed_cnt_prior + ccw_flag, 
                     data = model_data_mcaid[model_data_mcaid$exit_category != "Neutral", ], family = "binomial")

summary(ed_adj)
ed_adj_p <- summary(ed_adj)[["coefficients"]][2, 4]
ed_adj_results <- data.frame(Category = names(coef(ed_adj)), 
                             OR = exp(coef(ed_adj)), 
                             exp(confint(ed_adj)), 
                             p_value = summary(ed_adj)[["coefficients"]][, 4]) %>%
  rename(ci_lb = X2.5.., ci_ub = X97.5..)


## Hospitalizations visits ----
### Crude ----
hosp_crude <- glm(hosp_any_after ~ exit_category, 
                  data = model_data_mcaid[model_data_mcaid$exit_category != "Neutral", ], 
                  family = "binomial")

summary(hosp_crude)
exp(cbind(OR = coef(hosp_crude), confint(hosp_crude)))


### Adjusted ----
hosp_adj <- glm(hosp_any_after ~ exit_category + gender_me + race_eth_me + agegrp + los + 
                major_prog + hh_size + single_caregiver + hh_disability + 
                hosp_cnt_prior + ccw_flag, 
              data = model_data_mcaid[model_data_mcaid$exit_category != "Neutral", ], family = "binomial")

summary(hosp_adj)
hosp_adj_p <- summary(hosp_adj)[["coefficients"]][2, 4]
hosp_adj_results <- cbind(OR = exp(coef(hosp_adj)), exp(confint(hosp_adj)), p_value = summary(hosp_adj)[["coefficients"]][, 4])


## Well-child visits ----
### Crude ----
wc_crude <- glm(wc_any_after ~ exit_category, 
                data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6) & 
                                          model_data_mcaid$exit_category != "Neutral", ], 
                family = "binomial")

summary(wc_crude)
exp(cbind(OR = coef(wc_crude), confint(wc_crude)))


### Adjusted ----
wc_adj <- glm(wc_any_after ~ exit_category + gender_me + race_eth_me + los + 
                  major_prog + hh_size + single_caregiver + hh_disability + 
                  wc_cnt_prior + ccw_flag, 
                data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6) & 
                                          model_data_mcaid$exit_category != "Neutral", ], 
              family = "binomial")

summary(wc_adj)
wc_adj_p <- summary(wc_adj)[["coefficients"]][2, 4]
wc_adj_results <- cbind(OR = exp(coef(wc_adj)), exp(confint(wc_adj)), p_value = summary(wc_adj)[["coefficients"]][, 4])


### Stratified: prior visits ----
# Crude
wc_strat_wc <- glm(wc_any_after ~ exit_category, 
                   data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6) & 
                                             model_data_mcaid$exit_category != "Neutral" &
                                             model_data_mcaid$wc_cnt_prior >= 1, ], 
                   family = "binomial")

summary(wc_strat_wc)
exp(cbind(OR = coef(wc_strat_wc), confint(wc_strat_wc)))

# Adjusted
wc_strat_wc_adj <- glm(wc_any_after ~ exit_category + gender_me + race_eth_me + los + 
                         major_prog + hh_size + single_caregiver + hh_disability + ccw_flag, 
                       data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6) & 
                                                 model_data_mcaid$exit_category != "Neutral" &
                                                 model_data_mcaid$wc_cnt_prior >= 1, ], 
                       family = "binomial")

summary(wc_strat_wc_adj)
wc_strat_wc_adj_p <- summary(wc_strat_wc_adj)[["coefficients"]][2, 4]
wc_strat_wc_adj_results <- cbind(OR = exp(coef(wc_strat_wc_adj)), exp(confint(wc_strat_wc_adj)), 
                                 p_value = summary(wc_strat_wc_adj)[["coefficients"]][, 4])


### Stratified: prior visits ----
# Crude
wc_strat_no_wc <- glm(wc_any_after ~ exit_category, 
                   data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6) & 
                                             model_data_mcaid$exit_category != "Neutral" &
                                             model_data_mcaid$wc_cnt_prior == 0, ], 
                   family = "binomial")

summary(wc_strat_no_wc)
exp(cbind(OR = coef(wc_strat_no_wc), confint(wc_strat_no_wc)))

# Adjusted
wc_strat_no_wc_adj <- glm(wc_any_after ~ exit_category + gender_me + race_eth_me + los + 
                     major_prog + hh_size + single_caregiver + hh_disability + ccw_flag, 
                   data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6) & 
                                             model_data_mcaid$exit_category != "Neutral" &
                                             model_data_mcaid$wc_cnt_prior == 0, ], 
                   family = "binomial")

summary(wc_strat_no_wc_adj)
wc_strat_no_wc_adj_p <- summary(wc_strat_no_wc_adj)[["coefficients"]][2, 4]
wc_strat_no_wc_adj_results <- cbind(OR = exp(coef(wc_strat_no_wc_adj)), exp(confint(wc_strat_no_wc_adj)), 
                                p_value = summary(wc_strat_no_wc_adj)[["coefficients"]][, 4])



# RUN MARKDOWN FILE ----
render(input = file.path(here::here(), "analyses/health/pha_exit_outcomes_mcaid.Rmd"), 
       output_format = "word_document",
       output_file = "CSTE_2022_health_outcomes",
       output_dir = "C:/Users/mathesal/King County/DPH Health And Housing - Documents/HUD HEARS Study/Presentations and papers/")
