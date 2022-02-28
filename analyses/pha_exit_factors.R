## Script name: pha_exit_factors
##
## Purpose of script: Factors associated with exiting housing
##
## Author: Alastair Matheson, Public Health - Seattle & King County
## Date Created: 2021-12-09
## Email: alastair.matheson@kingcounty.gov
##
## Notes:
##   
##


# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, ggplot2, viridis, hrbrthemes,
               knitr, kableExtra, rmarkdown, nnet)

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
# Covariate table with exit reason
covariate <- dbGetQuery(db_hhsaw, 
                        "SELECT a.*, b.exit_category FROM
                        (SELECT * FROM hudhears.control_match_covariate) a
                        LEFT JOIN
                        (SELECT DISTINCT id_hudhears, act_date, exit_reason_clean, exit_category
                        FROM pha.stage_pha_exit_timevar
                        WHERE act_date IS NOT NULL) b
                        ON a.id_hudhears = b.id_hudhears AND a.exit_date = b.act_date AND
                        a.exit_reason_clean = b.exit_reason_clean
                        ") %>%
  mutate(exit = case_when(id_type == "id_exit" ~ 1L,
                          id_type == "id_control" ~ 0L))

covariate_nodeath <- covariate %>% filter(exit_death != 1)

covariate_exits <- covariate_nodeath %>% filter(id_type == "id_exit")


# FACTORS ASSOCIATED WITH EXIT VS NOT -----
covariate_nodeath %>% head()

covariate_nodeath %>% count(id_type)

## Demogs ----
# Age
covariate_nodeath %>% 
  mutate(# Remove spurious ages
         age_at_exit = ifelse(age_at_exit > 125, NA_integer_, age_at_exit),
         senior = case_when(age_at_exit >= 62 ~ 1L, age_at_exit < 62 ~ 0L),
         child = case_when(age_at_exit < 18 ~ 1L, age_at_exit >= 18 ~ 0L)) %>%
  filter(!is.na(age_at_exit)) %>%
  group_by(id_type) %>%
  summarise(n = n(), 
            age_mean = mean(age_at_exit),
            age_med = median(age_at_exit),
            age_min = min(age_at_exit),
            age_max = max(age_at_exit),
            senior = mean(senior),
            child = mean(child))

# Gender
covariate_nodeath %>% 
  count(id_type, gender_me) %>%
  group_by(id_type) %>%
  mutate(tot = sum(n), pct = round(n/tot*100,1))

# Race/eth
covariate_nodeath %>% 
  count(id_type, race_eth_me) %>%
  group_by(id_type) %>%
  mutate(tot = sum(n), pct = round(n/tot*100,1))



## HH demogs ----
# Time in housing (this is based on HH data)
covariate_nodeath %>% 
  filter(!is.na(housing_time_at_exit)) %>%
  group_by(id_type) %>%
  summarise(n = n(),
            los_mean = mean(housing_time_at_exit),
            los_med = median(housing_time_at_exit))

# Size and composition
covariate_nodeath %>% 
  distinct(hh_id_kc_pha, exit_date, id_type, hh_size, single_caregiver, hh_disability) %>%
  group_by(id_type) %>%
  summarise(n = n(),
            hh_size_mean = mean(hh_size, na.rm = T),
            hh_size_med = median(hh_size, na.rm = T),
            single_caregiver = mean(single_caregiver, na.rm = T),
            hh_disability = mean(hh_disability, na.rm = T))


# Program type
covariate_nodeath %>% 
  distinct(hh_id_kc_pha, exit_date, id_type, major_prog) %>%
  filter(!is.na(major_prog)) %>%
  count(id_type, major_prog) %>%
  group_by(id_type) %>%
  ungroup() %>%
  group_by(id_type) %>%
  mutate(tot = sum(n),
         pct = round(n/tot*100, 1))


# Voucher type
covariate_nodeath %>% 
  distinct(hh_id_kc_pha, exit_date, id_type, vouch_type_final) %>%
  mutate(vouch_type_final = case_when(vouch_type_final == "GENERAL TENANT-BASED VOUCHER" ~ "General tenant-based",
                                      vouch_type_final == "PARTNER PROJECT-BASED VOUCHER" ~ "Partner project-based",
                                      vouch_type_final %in% c("AGENCY TENANT-BASED VOUCHER",
                                                              "PHA OPERATED VOUCHER") ~ "Agency/PHA tenant-based",
                                      vouch_type_final == "MOD REHAB" ~ "Mod rehab",
                                      vouch_type_final == "OTHER (TI/DV)" ~ "Other (TI/DV)",
                                      TRUE ~ vouch_type_final)) %>%
  filter(!is.na(vouch_type_final)) %>%
  count(id_type, vouch_type_final) %>%
  group_by(id_type) %>%
  ungroup() %>%
  group_by(id_type) %>%
  mutate(tot = sum(n),
         pct = round(n/tot*100, 1))

# Agency - this doesn't make sense to do because KCHA only runs 2016-2018
covariate_nodeath %>% 
  distinct(hh_id_kc_pha, exit_date, id_type, agency) %>%
  filter(!is.na(agency)) %>%
  count(id_type, agency) %>%
  group_by(id_type) %>%
  ungroup() %>%
  group_by(id_type) %>%
  mutate(tot = sum(n),
         pct = round(n/tot*100, 1))



## Repeat but at individual level ----
# Size and composition
covariate_nodeath %>% 
  distinct(id_kc_pha, exit_date, id_type, hh_size, single_caregiver, hh_disability) %>%
  group_by(id_type) %>%
  summarise(n = n(),
            hh_size_mean = mean(hh_size, na.rm = T),
            hh_size_med = median(hh_size, na.rm = T),
            single_caregiver = mean(single_caregiver, na.rm = T),
            hh_disability = mean(hh_disability, na.rm = T))


# Program type
covariate_nodeath %>% 
  distinct(id_kc_pha, exit_date, id_type, major_prog) %>%
  filter(!is.na(major_prog)) %>%
  count(id_type, major_prog) %>%
  group_by(id_type) %>%
  ungroup() %>%
  group_by(id_type) %>%
  mutate(tot = sum(n),
         pct = round(n/tot*100, 1))


# Voucher type
covariate_nodeath %>% 
  distinct(id_kc_pha, exit_date, id_type, vouch_type_final) %>%
  mutate(vouch_type_final = case_when(vouch_type_final == "GENERAL TENANT-BASED VOUCHER" ~ "General tenant-based",
                                      vouch_type_final == "PARTNER PROJECT-BASED VOUCHER" ~ "Partner project-based",
                                      vouch_type_final %in% c("AGENCY TENANT-BASED VOUCHER",
                                                              "PHA OPERATED VOUCHER") ~ "Agency/PHA tenant-based",
                                      vouch_type_final == "MOD REHAB" ~ "Mod rehab",
                                      vouch_type_final == "OTHER (TI/DV)" ~ "Other (TI/DV)",
                                      TRUE ~ vouch_type_final)) %>%
  filter(!is.na(vouch_type_final)) %>%
  count(id_type, vouch_type_final) %>%
  group_by(id_type) %>%
  ungroup() %>%
  group_by(id_type) %>%
  mutate(tot = sum(n),
         pct = round(n/tot*100, 1))


## Medicaid demogs ----
covariate_nodeath %>% 
  group_by(id_type) %>%
  summarise(n = n(),
            full_cov_11_prior = mean(full_cov_11_prior, na.rm = T),
            full_cov_7_prior = mean(full_cov_7_prior, na.rm = T),
            full_cov_11_after = mean(full_cov_11_after, na.rm = T),
            full_cov_7_after = mean(full_cov_7_after, na.rm = T))


covariate_nodeath %>% 
  filter(full_cov_11_prior == T) %>%
  group_by(id_type) %>%
  summarise(n = n(),
            ed_cnt_prior = mean(ed_cnt_prior, na.rm = T),
            hosp_cnt_prior = mean(hosp_cnt_prior, na.rm = T),
            ccw_cnt = mean(ccw_cnt, na.rm = T))

covariate_nodeath %>% 
  filter(full_cov_7_prior == T) %>%
  group_by(id_type) %>%
  summarise(n = n(),
            ed_cnt_prior = mean(ed_cnt_prior, na.rm = T),
            hosp_cnt_prior = mean(hosp_cnt_prior, na.rm = T),
            ccw_cnt = mean(ccw_cnt, na.rm = T))



## Demogs by Medicaid status ----
# Age
covariate_nodeath %>% 
  filter(id_type == "id_exit") %>%
  mutate(# Remove spurious ages
    age_at_exit = ifelse(age_at_exit > 125, NA_integer_, age_at_exit),
    senior = case_when(age_at_exit >= 62 ~ 1L, age_at_exit < 62 ~ 0L),
    child = case_when(age_at_exit < 18 ~ 1L, age_at_exit >= 18 ~ 0L)) %>%
  filter(!is.na(age_at_exit)) %>%
  group_by(full_cov_7_prior) %>%
  summarise(n = n(), 
            age_mean = mean(age_at_exit),
            age_med = median(age_at_exit),
            age_min = min(age_at_exit),
            age_max = max(age_at_exit),
            senior = mean(senior),
            child = mean(child))

# Gender
covariate_nodeath %>% 
  filter(id_type == "id_exit") %>%
  count(full_cov_7_prior, gender_me) %>%
  group_by(full_cov_7_prior) %>%
  mutate(tot = sum(n), pct = round(n/tot*100,1))

# Race/eth
covariate_nodeath %>% 
  filter(id_type == "id_exit") %>%
  count(full_cov_7_prior, race_eth_me) %>%
  group_by(full_cov_7_prior) %>%
  mutate(tot = sum(n), pct = round(n/tot*100,1)) %>%
  as.data.frame()

# Time in housing
covariate_nodeath %>% 
  filter(id_type == "id_exit") %>%
  filter(!is.na(housing_time_at_exit)) %>%
  group_by(full_cov_7_prior) %>%
  summarise(n = n(),
            los_mean = mean(housing_time_at_exit),
            los_med = median(housing_time_at_exit))



# REGRESSION MODEL FOR EXIT VS NOT ----
## Not including Medicaid factors ----
model_data <- covariate_nodeath %>%
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

anyexit <- glm(exit ~ gender_me + race_eth_me + agegrp + los + 
                 major_prog + hh_size + single_caregiver + hh_disability, 
               data = model_data, family = "binomial")

summary(anyexit)

exp(cbind(OR = coef(anyexit), confint(anyexit)))


## Including Medicaid factors ----
model_data_mcaid <- model_data %>% filter(full_cov_7_prior == T)


anyexit_mcaid <- glm(exit ~ gender_me + race_eth_me + agegrp + los + 
                       major_prog + hh_size + single_caregiver + hh_disability + 
                       ed_cnt_prior + hosp_cnt_prior + ccw_flag, 
               data = model_data_mcaid, family = "binomial")

summary(anyexit_mcaid)

exp(cbind(OR = coef(anyexit_mcaid), confint(anyexit_mcaid)))



# FACTORS ASSOCIATED WITH EXIT TYPE -----
## Demogs ----
# Age
covariate_exits %>% 
  mutate(# Remove spurious ages
    age_at_exit = ifelse(age_at_exit > 125, NA_integer_, age_at_exit),
    senior = case_when(age_at_exit >= 62 ~ 1L, age_at_exit < 62 ~ 0L),
    child = case_when(age_at_exit < 18 ~ 1L, age_at_exit >= 18 ~ 0L)) %>%
  filter(!is.na(age_at_exit)) %>%
  group_by(exit_category) %>%
  summarise(n = n(), 
            age_mean = mean(age_at_exit),
            age_med = median(age_at_exit),
            age_min = min(age_at_exit),
            age_max = max(age_at_exit),
            senior = mean(senior),
            child = mean(child))

# Gender
covariate_exits %>% 
  count(exit_category, gender_me) %>%
  group_by(exit_category) %>%
  mutate(tot = sum(n), pct = round(n/tot*100,1))

# Race/eth
covariate_exits %>% 
  count(exit_category, race_eth_me) %>%
  group_by(exit_category) %>%
  mutate(tot = sum(n), pct = round(n/tot*100,1))



## HH demogs ----
# Time in housing (this is based on HH data)
covariate_exits %>% 
  filter(!is.na(housing_time_at_exit)) %>%
  group_by(exit_category) %>%
  summarise(n = n(),
            los_mean = mean(housing_time_at_exit),
            los_med = median(housing_time_at_exit))

# Size and composition
covariate_exits %>% 
  distinct(hh_id_kc_pha, exit_date, exit_category, hh_size, single_caregiver, hh_disability) %>%
  group_by(exit_category) %>%
  summarise(n = n(),
            hh_size_mean = mean(hh_size, na.rm = T),
            hh_size_med = median(hh_size, na.rm = T),
            single_caregiver = mean(single_caregiver, na.rm = T),
            hh_disability = mean(hh_disability, na.rm = T))


# Program type
covariate_exits %>% 
  distinct(hh_id_kc_pha, exit_date, exit_category, major_prog) %>%
  filter(!is.na(major_prog)) %>%
  count(exit_category, major_prog) %>%
  group_by(exit_category) %>%
  ungroup() %>%
  group_by(exit_category) %>%
  mutate(tot = sum(n),
         pct = round(n/tot*100, 1))


# Voucher type
covariate_exits %>% 
  distinct(hh_id_kc_pha, exit_date, exit_category, vouch_type_final) %>%
  mutate(vouch_type_final = case_when(vouch_type_final == "GENERAL TENANT-BASED VOUCHER" ~ "General tenant-based",
                                      vouch_type_final == "PARTNER PROJECT-BASED VOUCHER" ~ "Partner project-based",
                                      vouch_type_final %in% c("AGENCY TENANT-BASED VOUCHER",
                                                              "PHA OPERATED VOUCHER") ~ "Agency/PHA tenant-based",
                                      vouch_type_final == "MOD REHAB" ~ "Mod rehab",
                                      vouch_type_final == "OTHER (TI/DV)" ~ "Other (TI/DV)",
                                      TRUE ~ vouch_type_final)) %>%
  filter(!is.na(vouch_type_final)) %>%
  count(exit_category, vouch_type_final) %>%
  group_by(exit_category) %>%
  ungroup() %>%
  group_by(exit_category) %>%
  mutate(tot = sum(n),
         pct = round(n/tot*100, 1))

# Agency - this doesn't make sense to do because KCHA only runs 2016-2018
covariate_exits %>% 
  distinct(hh_id_kc_pha, exit_date, exit_category, agency) %>%
  filter(!is.na(agency)) %>%
  count(exit_category, agency) %>%
  group_by(exit_category) %>%
  ungroup() %>%
  group_by(exit_category) %>%
  mutate(tot = sum(n),
         pct = round(n/tot*100, 1))



## Repeat but at individual level ----
# Size and composition
covariate_exits %>% 
  distinct(id_kc_pha, exit_date, exit_category, hh_size, single_caregiver, hh_disability) %>%
  group_by(exit_category) %>%
  summarise(n = n(),
            hh_size_mean = mean(hh_size, na.rm = T),
            hh_size_med = median(hh_size, na.rm = T),
            single_caregiver = mean(single_caregiver, na.rm = T),
            hh_disability = mean(hh_disability, na.rm = T))


# Program type
covariate_exits %>% 
  distinct(id_kc_pha, exit_date, exit_category, major_prog) %>%
  filter(!is.na(major_prog)) %>%
  count(exit_category, major_prog) %>%
  group_by(exit_category) %>%
  ungroup() %>%
  group_by(exit_category) %>%
  mutate(tot = sum(n),
         pct = round(n/tot*100, 1))


# Voucher type
covariate_exits %>% 
  distinct(id_kc_pha, exit_date, exit_category, vouch_type_final) %>%
  mutate(vouch_type_final = case_when(vouch_type_final == "GENERAL TENANT-BASED VOUCHER" ~ "General tenant-based",
                                      vouch_type_final == "PARTNER PROJECT-BASED VOUCHER" ~ "Partner project-based",
                                      vouch_type_final %in% c("AGENCY TENANT-BASED VOUCHER",
                                                              "PHA OPERATED VOUCHER") ~ "Agency/PHA tenant-based",
                                      vouch_type_final == "MOD REHAB" ~ "Mod rehab",
                                      vouch_type_final == "OTHER (TI/DV)" ~ "Other (TI/DV)",
                                      TRUE ~ vouch_type_final)) %>%
  filter(!is.na(vouch_type_final)) %>%
  count(exit_category, vouch_type_final) %>%
  group_by(exit_category) %>%
  ungroup() %>%
  group_by(exit_category) %>%
  mutate(tot = sum(n),
         pct = round(n/tot*100, 1))


## Medicaid demogs ----
covariate_exits %>% 
  group_by(exit_category) %>%
  summarise(n = n(),
            full_cov_11_prior = mean(full_cov_11_prior, na.rm = T),
            full_cov_7_prior = mean(full_cov_7_prior, na.rm = T),
            full_cov_11_after = mean(full_cov_11_after, na.rm = T),
            full_cov_7_after = mean(full_cov_7_after, na.rm = T))


covariate_exits %>% 
  filter(full_cov_11_prior == T) %>%
  group_by(exit_category) %>%
  summarise(n = n(),
            ed_cnt_prior = mean(ed_cnt_prior, na.rm = T),
            hosp_cnt_prior = mean(hosp_cnt_prior, na.rm = T),
            ccw_cnt = mean(ccw_cnt, na.rm = T))

covariate_exits %>% 
  filter(full_cov_7_prior == T) %>%
  group_by(exit_category) %>%
  summarise(n = n(),
            ed_cnt_prior = mean(ed_cnt_prior, na.rm = T),
            hosp_cnt_prior = mean(hosp_cnt_prior, na.rm = T),
            ccw_cnt = mean(ccw_cnt, na.rm = T))



## Demogs by Medicaid status ----
# Age
covariate_exits %>% 
  filter(exit_category == "id_exit") %>%
  mutate(# Remove spurious ages
    age_at_exit = ifelse(age_at_exit > 125, NA_integer_, age_at_exit),
    senior = case_when(age_at_exit >= 62 ~ 1L, age_at_exit < 62 ~ 0L),
    child = case_when(age_at_exit < 18 ~ 1L, age_at_exit >= 18 ~ 0L)) %>%
  filter(!is.na(age_at_exit)) %>%
  group_by(full_cov_7_prior) %>%
  summarise(n = n(), 
            age_mean = mean(age_at_exit),
            age_med = median(age_at_exit),
            age_min = min(age_at_exit),
            age_max = max(age_at_exit),
            senior = mean(senior),
            child = mean(child))

# Gender
covariate_exits %>% 
  filter(exit_category == "id_exit") %>%
  count(full_cov_7_prior, gender_me) %>%
  group_by(full_cov_7_prior) %>%
  mutate(tot = sum(n), pct = round(n/tot*100,1))

# Race/eth
covariate_exits %>% 
  filter(exit_category == "id_exit") %>%
  count(full_cov_7_prior, race_eth_me) %>%
  group_by(full_cov_7_prior) %>%
  mutate(tot = sum(n), pct = round(n/tot*100,1)) %>%
  as.data.frame()

# Time in housing
covariate_exits %>% 
  filter(exit_category == "id_exit") %>%
  filter(!is.na(housing_time_at_exit)) %>%
  group_by(full_cov_7_prior) %>%
  summarise(n = n(),
            los_mean = mean(housing_time_at_exit),
            los_med = median(housing_time_at_exit))


# REGRESSION MODEL FOR EXIT TYPE ----
## Not including Medicaid factors ----
model_data_exits <- covariate_exits %>%
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
         agegrp = factor(agegrp, levels = c("<18", "18-24", "25-44", "45-64", "65+")),
         los = factor(los, levels = c("<3", "3-5.99", "6-9.99", "10+")),
         exit_pos = case_when(exit_category == "Neutral" ~ 0L,
                              exit_category == "Positive" ~ 1L),
         exit_neg = case_when(exit_category == "Neutral" ~ 0L,
                              exit_category == "Negative" ~ 1L),
         exit_category = factor(exit_category, levels = c("Negative", "Neutral", "Positive"))
  )

model_data_exits$exit_category2 <- relevel(model_data_exits$exit_category, ref = "Neutral")

### Multinomial approach ----
exit_type <- multinom(exit_category2 ~ gender_me + race_eth_me + agegrp + los + 
                 major_prog + hh_size + single_caregiver + hh_disability, 
               data = model_data_exits)

summary(exit_type)


exit_type_coef <- coef(exit_type)
exit_type_ci <- as.data.frame(confint(exit_type))

exit_type_results <- data.frame("variable" = colnames(exit_type_coef), 
           "negative" = exp(exit_type_coef[1,]),
           "negative_lb" = exp(exit_type_ci[,1]),
           "negative_ub" = exp(exit_type_ci[,2]),
           "positive" = exp(exit_type_coef[2,]),
           "positive_lb" = exp(exit_type_ci[,3]),
           "positive_ub" = exp(exit_type_ci[,4])
           ) %>% as_tibble()


### Separated binomial approach ----
exit_type_neg <- glm(exit_neg ~ gender_me + race_eth_me + agegrp + los + 
                       major_prog + hh_size + single_caregiver + hh_disability, 
                     data = model_data_exits[!is.na(model_data_exits$exit_neg),], 
                     family = "binomial")

summary(exit_type_neg)
exit_type_neg_results <- cbind(OR = exp(coef(exit_type_neg)),
                                     exp(confint(exit_type_neg)),
                                     p = as.numeric(sprintf("%.4f", summary(exit_type_neg)[["coefficients"]][,4])))


exit_type_pos <- glm(exit_pos ~ gender_me + race_eth_me + agegrp + los + 
                       major_prog + hh_size + single_caregiver + hh_disability, 
                     data = model_data_exits[!is.na(model_data_exits$exit_pos),], 
                     family = "binomial")

summary(exit_type_pos)
exit_type_pos_results <- cbind(OR = exp(coef(exit_type_pos)),
                                     exp(confint(exit_type_pos)),
                                     p = as.numeric(sprintf("%.4f", summary(exit_type_pos)[["coefficients"]][,4])))



## Including Medicaid factors ----
model_data_exits_mcaid <- model_data_exits %>% 
  filter(full_cov_7_prior == T & full_cov_7_after == T) %>%
  # Also drop groups with  very small counts
  filter(race_eth_me != "Unknown") %>%
  filter(agegrp != "65+")

### Multinomial approach ----
exit_type_mcaid <- multinom(exit_category2 ~ gender_me + race_eth_me + agegrp + los + 
                        major_prog + hh_size + single_caregiver + hh_disability, 
                      data = model_data_exits_mcaid)

summary(exit_type_mcaid)


exit_type_mcaid_coef <- coef(exit_type_mcaid)
exit_type_mcaid_ci <- as.data.frame(confint(exit_type_mcaid))

exit_type_mcaid_results <- data.frame("variable" = colnames(exit_type_mcaid_coef), 
                                "negative" = exp(exit_type_mcaid_coef[1,]),
                                "negative_lb" = exp(exit_type_mcaid_ci[,1]),
                                "negative_ub" = exp(exit_type_mcaid_ci[,2]),
                                "positive" = exp(exit_type_mcaid_coef[2,]),
                                "positive_lb" = exp(exit_type_mcaid_ci[,3]),
                                "positive_ub" = exp(exit_type_mcaid_ci[,4])
) %>% as_tibble()


### Separated binomial approach ----
exit_type_mcaid_neg <- glm(exit_neg ~ gender_me + race_eth_me + agegrp + los + 
                       major_prog + hh_size + single_caregiver + hh_disability, 
                     data = model_data_exits_mcaid[!is.na(model_data_exits_mcaid$exit_neg),], 
                     family = "binomial")

summary(exit_type_mcaid_neg)
exit_type_mcaid_neg_results <- cbind(OR = exp(coef(exit_type_mcaid_neg)),
                                     exp(confint(exit_type_mcaid_neg)),
                                     p = as.numeric(sprintf("%.4f", summary(exit_type_mcaid_neg)[["coefficients"]][,4])))


exit_type_mcaid_pos <- glm(exit_pos ~ gender_me + race_eth_me + agegrp + los + 
                       major_prog + hh_size + single_caregiver + hh_disability, 
                     data = model_data_exits_mcaid[!is.na(model_data_exits_mcaid$exit_pos),], 
                     family = "binomial")

summary(exit_type_mcaid_pos)
exit_type_mcaid_pos_results <- cbind(OR = exp(coef(exit_type_mcaid_pos)),
                                     exp(confint(exit_type_mcaid_pos)),
                                     p = as.numeric(sprintf("%.4f", summary(exit_type_mcaid_pos)[["coefficients"]][,4])))
