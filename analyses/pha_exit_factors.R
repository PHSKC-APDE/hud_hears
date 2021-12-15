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
# Covariate table
covariate <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.control_match_covariate") %>%
  mutate(exit = case_when(id_type == "id_exit" ~ 1L,
                          id_type == "id_control" ~ 0L))

covariate_nodeath <- covariate %>% filter(exit_death != 1)
  


# FACTORS ASSOCIATED WITH EXIT -----
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

# Time in housing
covariate_nodeath %>% 
  filter(!is.na(housing_time_at_exit)) %>%
  group_by(id_type) %>%
  summarise(n = n(),
            los_mean = mean(housing_time_at_exit),
            los_med = median(housing_time_at_exit))



## HH demogs ----
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



## Repeat but at individual level
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



# REGRESSION MODEL ----
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

