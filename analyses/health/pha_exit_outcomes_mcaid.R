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
pacman::p_load(tidyverse, odbc, glue, data.table, ggplot2, viridis, hrbrthemes,
               knitr, kableExtra, rmarkdown)

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

exits <- dbGetQuery(db_hhsaw, "SELECT DISTINCT id_hudhears, act_date, exit_reason, 
                    exit_reason_clean, exit_category_pha, exit_category
                    FROM pha.stage_pha_exit_timevar
                    WHERE true_exit = 1 AND act_date IS NOT NULL")


## Sort out multiple exit types ----
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


## Add exit type ----
exit_nodeath <- covariate %>% filter(exit_death != 1 & exit == 1) %>%
  left_join(., exits, by = c("id_hudhears", "exit_date" = "act_date"))



# DEMOGRAPHICS ----
## Demogs by Medicaid status ----
# Age
exit_age <- exit_nodeath %>% 
  mutate(# Remove spurious ages
    age_at_exit = ifelse(age_at_exit > 125, NA_integer_, age_at_exit),
    senior = case_when(age_at_exit >= 62 ~ 1L, age_at_exit < 62 ~ 0L),
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




# REGRESSION MODEL ----
model_data_mcaid <- exit_nodeath %>%
  filter(full_cov_7_prior == T & full_cov_7_after == T & exit_category != "Neutral") %>%
  mutate(agegrp = case_when(age_at_exit < 18 ~ "<18",
                            data.table::between(age_at_exit, 18, 24.99, NAbounds = NA) ~ "18-24",
                            data.table::between(age_at_exit, 25, 44.99, NAbounds = NA) ~ "25-44",
                            data.table::between(age_at_exit, 45, 64.99, NAbounds = NA) ~ "45-64",
                            age_at_exit >= 65 ~ "65+",
                            is.na(age_at_exit) ~ NA_character_),
         los = case_when(housing_time_at_exit < 3 ~ "<3",
                         between(housing_time_at_exit, 3, 5.999, NAbounds = NA) ~ "3-5.99",
                         between(housing_time_at_exit, 6, 9.999, NAbounds = NA) ~ "6-9.99",
                         housing_time_at_exit >= 10 ~ "10+"),
         ed_any_prior = ifelse(ed_cnt_prior >= 1, 1L, 0L),
         ed_any_after = ifelse(ed_cnt_after >= 1, 1L, 0L),
         hosp_any_prior = ifelse(hosp_cnt_prior >= 1, 1L, 0L),
         hosp_any_after = ifelse(hosp_cnt_after >= 1, 1L, 0L))
 

## ED visits ----
### Crude ----
ed_crude <- glm(ed_any_after ~ exit_pos, data = model_data_mcaid, family = "binomial")

summary(ed_crude)
exp(cbind(OR = coef(ed_crude), confint(ed_crude)))


### Adjusted ----
ed_adj <- glm(ed_any_after ~ exit_pos + gender_me + race_eth_me + agegrp + los + 
                       major_prog + hh_size + single_caregiver + hh_disability + 
                       ed_cnt_prior + hosp_cnt_prior + ccw_flag, 
                     data = model_data_mcaid, family = "binomial")

summary(ed_adj)
ed_adj_p <- summary(ed_adj)[["coefficients"]][2, 4]
ed_adj_results <- exp(cbind(OR = coef(ed_adj), confint(ed_adj)))


## Hospitalizations visits ----
### Crude ----
hosp_crude <- glm(hosp_any_after ~ exit_pos, data = model_data_mcaid, family = "binomial")

summary(hosp_crude)
exp(cbind(OR = coef(hosp_crude), confint(hosp_crude)))


### Adjusted ----
hosp_adj <- glm(hosp_any_after ~ exit_pos + gender_me + race_eth_me + agegrp + los + 
                major_prog + hh_size + single_caregiver + hh_disability + 
                ed_cnt_prior + hosp_cnt_prior + ccw_flag, 
              data = model_data_mcaid, family = "binomial")

summary(hosp_adj)
hosp_adj_p <- summary(hosp_adj)[["coefficients"]][2, 4]
hosp_adj_results <- exp(cbind(OR = coef(hosp_adj), confint(hosp_adj)))


## Well-child visits ----
### Crude ----
wc_crude <- glm(wc_cnt_after ~ exit_pos, data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6), ], 
                family = "binomial")

summary(wc_crude)
exp(cbind(OR = coef(wc_crude), confint(wc_crude)))


### Adjusted ----
wc_adj <- glm(wc_cnt_after ~ exit_pos + gender_me + race_eth_me + los + 
                  major_prog + hh_size + single_caregiver + hh_disability + 
                  wc_cnt_prior + ccw_flag + wc_cnt_prior, 
                data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6), ], family = "binomial")

summary(wc_adj)
wc_adj_p <- summary(wc_adj)[["coefficients"]][2, 4]
wc_adj_results <- exp(cbind(OR = coef(wc_adj), confint(wc_adj)))


### Stratified ----
wc_strat_wc <- glm(wc_cnt_after ~ exit_pos + gender_me + race_eth_me + los + 
                major_prog + hh_size + single_caregiver + hh_disability + 
                wc_cnt_prior + ccw_flag, 
              data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6) & 
                                        model_data_mcaid$wc_cnt_prior == 1, ], 
              family = "binomial")

summary(wc_strat_wc)
wc_strat_wc_p <- summary(wc_strat_wc)[["coefficients"]][2, 4]
wc_strat_wc_results <- exp(cbind(OR = coef(wc_strat_wc), confint(wc_strat_wc)))

wc_strat_no_wc <- glm(wc_cnt_after ~ exit_pos + gender_me + race_eth_me + los + 
                     major_prog + hh_size + single_caregiver + hh_disability + 
                     wc_cnt_prior + ccw_flag, 
                   data = model_data_mcaid[between(model_data_mcaid$age_at_exit, 2, 6) & 
                                             model_data_mcaid$wc_cnt_prior == 0, ], 
                   family = "binomial")

summary(wc_strat_no_wc)
wc_strat_no_wc_p <- summary(wc_strat_no_wc)[["coefficients"]][2, 4]
wc_strat_no_wc_results <- exp(cbind(OR = coef(wc_strat_no_wc), confint(wc_strat_no_wc)))



# RUN MARKDOWN FILE ----
render(input = file.path(here::here(), "analyses/health/pha_exit_outcomes_mcaid.Rmd"), 
       output_format = "word_document",
       output_file = "CSTE_2022_health_outcomes",
       output_dir = "C:/Users/mathesal/King County/DPH Health And Housing - Documents/HUD HEARS Study/Presentations and papers/")
