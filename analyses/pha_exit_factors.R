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
               knitr, kableExtra, rmarkdown, nnet, scales)

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


# SET UP GENERIC SUMMARY FUNCTIONS ----
age_sum <- function(df, ...) {
  # Set things up to select in pivot_ functions
  # There is probably a better way to do this but it works
  col_names <- df %>% select(...) %>% colnames()
  
  output <- df %>% 
    mutate(# Remove spurious ages
      age_at_exit = ifelse(age_at_exit > 125, NA_integer_, age_at_exit),
      senior = case_when(age_at_exit >= 62 ~ 1L, age_at_exit < 62 ~ 0L),
      child = case_when(age_at_exit < 18 ~ 1L, age_at_exit >= 18 ~ 0L)) %>%
    filter(!is.na(age_at_exit)) %>%
    group_by(...) %>%
    summarise(n = number(n(), big.mark = ","), 
              age_mean = round(mean(age_at_exit), 1),
              age_med = median(age_at_exit),
              age_range = paste0(min(age_at_exit), "-", max(age_at_exit)),
              senior = scales::percent(mean(senior, na.rm = T), accuracy = 0.1L),
              child = scales::percent(mean(child, na.rm = T), accuracy = 0.1L)) %>% 
    pivot_longer(cols = !col_names, values_transform = list(value = as.character)) %>%
    pivot_wider(id_cols = "name", names_from = col_names) %>%
    mutate(category = "Age", .before = "name",
           name = case_when(name == "age_mean" ~ "Mean (years)",
                            name == "age_med" ~ "Median (years)",
                            name == "age_range" ~ "Range (years)",
                            name == "senior" ~ "Senior (aged 65+)",
                            name == "child" ~ "Child (aged <18)",
                            TRUE ~ name)) %>%
    rename("group" = "name")
  
  output
}

# Can take a common approach to summaries that are simple percents
demog_pct_sum <- function(df, 
                          level = c("ind", "hh"),
                          demog = c("gender", "race", "program", "voucher", 
                                "program_ind", "voucher_ind"), 
                      ...) {
  # Set things up to select in pivot_ functions
  # There is probably a better way to do this but it works
  col_names <- df %>% select(...) %>% colnames()
  
  level <- match.arg(level)
  demog <- match.arg(demog)
  
  if (level == "ind") {
    output <- df %>% mutate(id_var = id_hudhears)
  } else if (level == "hh") {
    output <- df %>% mutate(id_var = hh_id_kc_pha)
  }
  
  if (demog == "gender") {
    cat_text <- "Gender"
    output <- output %>% 
      distinct(id_var, exit_date, ..., gender_me) %>% 
      mutate(group = gender_me)
  } else if (demog == "race") {
    cat_text <- "Race/ethnicity"
    output <- output %>% 
      distinct(id_var, exit_date, ..., race_eth_me) %>%
      filter(!is.na(race_eth_me) & race_eth_me != "Unknown") %>%
      mutate(group = ifelse(race_eth_me == "Latino", "Latina/o/x", race_eth_me))
  } else if (demog == "program") {
    cat_text <- "Program type"
    output <- output %>% 
      distinct(id_var, exit_date, ..., major_prog) %>%
      filter(!is.na(major_prog)) %>%
      mutate(group = major_prog)
  } else if (demog == "voucher") {
    cat_text = "Voucher type"
    output <- output %>% 
      distinct(id_var, exit_date, ..., vouch_type_final) %>%
      mutate(group = case_when(vouch_type_final == "GENERAL TENANT-BASED VOUCHER" ~ "General tenant-based",
                               vouch_type_final == "PARTNER PROJECT-BASED VOUCHER" ~ "Partner project-based",
                               vouch_type_final %in% c("AGENCY TENANT-BASED VOUCHER",
                                                       "PHA OPERATED VOUCHER") ~ "Agency/PHA tenant-based",
                               vouch_type_final == "MOD REHAB" ~ "Mod rehab",
                               vouch_type_final == "OTHER (TI/DV)" ~ "Other (TI/DV)",
                               TRUE ~ vouch_type_final)) %>%
      filter(!is.na(group))
  }
  
  # Common reshaping approach
  output <- output %>%
    count(..., group) %>%
    group_by(...) %>%
    mutate(tot = sum(n), pct = round(n/tot*100,1)) %>%
    ungroup() %>%
    mutate(category = cat_text,
           val = ifelse(n < 10, "<10",
                        paste0(number(n, big.mark = ",", accuracy = 1L), " (", pct, "%)"))) %>%
    select(col_names, category, group, val) %>%
    pivot_wider(id_cols = c("category", "group"), names_from = col_names, values_from = "val")
  
  output
}


hh_los_sum <- function(df, ...) {
  col_names <- df %>% distinct(...) %>% colnames()
  
  output <- df %>% 
    filter(!is.na(housing_time_at_exit)) %>%
    distinct(hh_id_kc_pha, housing_time_at_exit, ...) %>%
    group_by(...) %>%
    summarise(n = number(n(), big.mark = ","), 
              los_mean = round(mean(housing_time_at_exit), 1),
              los_med = median(housing_time_at_exit)) %>%
    pivot_longer(cols = !col_names, values_transform = list(value = as.character)) %>%
    pivot_wider(id_cols = "name", names_from = col_names) %>%
    mutate(category = "HoH time in housing", .before = "name",
           name = case_when(name == "los_mean" ~ "Mean time in housing (years)",
                            name == "los_med" ~ "Median time in housing (years)",
                            TRUE ~ name)) %>%
    rename("group" = "name")

  output
}

hh_demogs_sum <- function(df, level = c("hh", "ind"), ...) {
  col_names <- df %>% select(...) %>% colnames()
  
  # Set things up for household- or individual-level analyses
  level <- match.arg(level)
  if (level == "ind") {
    output <- df %>% mutate(id_var = id_hudhears)
  } else if (level == "hh") {
    output <- df %>% mutate(id_var = hh_id_kc_pha)
  }
  
  output <- output %>% 
    distinct(id_var, exit_date, ..., hh_size, single_caregiver, hh_disability) %>%
    group_by(...) %>%
    summarise(n = number(n(), big.mark = ","), 
              hh_size_mean = round(mean(hh_size, na.rm = T), 1),
              hh_size_med = median(hh_size, na.rm = T),
              single_caregiver = scales::percent(mean(single_caregiver, na.rm = T), accuracy = 0.1L),
              hh_disability = scales::percent(mean(hh_disability, na.rm = T), accuracy = 0.1L)) %>%
    pivot_longer(cols = !col_names, values_transform = list(value = as.character)) %>%
    pivot_wider(id_cols = "name", names_from = col_names) %>%
    mutate(category = "Household characteristics", .before = "name",
           name = case_when(name == "hh_size_mean" ~ "Mean household size",
                            name == "hh_size_med" ~ "Median household size",
                            name == "single_caregiver" ~ "Single caregiver",
                            name == "hh_disability" ~ "HoH disability",
                            TRUE ~ name)) %>%
    rename("group" = "name")
  
  output
}


mcaid_outcomes_sum <- function(df, 
                               time = c("prior", "after"), 
                               cov_time = c("7_mth", "11_mth"), 
                               ...) {
  col_names <- df %>% select(...) %>% colnames()
  
  time <- match.arg(time)
  cov_time<- match.arg(cov_time)
  
  
  if (time == "prior") {
    if (cov_time == "7_mth") {
      output <- df %>% filter(full_cov_7_prior == T)
    } else if (cov_time == "11_mth") {
      output <- df %>% filter(full_cov_11_prior == T)
    }
    # Set up ED and hospitalization vars
    output <- output %>%
      mutate(ed_cnt = ed_cnt_prior,
             hosp_cnt = hosp_cnt_prior)
    ed_text <- "Average # ED visits in year prior to exit"
    hosp_text <- "Average # hospitalizations in year prior to exit"
  } else if (time == "after") {
    if (cov_time == "7_mth") {
      output <- df %>% filter(full_cov_7_after == T)
    } else if (cov_time == "11_mth") {
      output <- df %>% filter(full_cov_11_after == T)
    }
    # Set up ED and hospitalization vars
    output <- output %>%
      mutate(ed_cnt = ed_cnt_after,
             hosp_cnt = hosp_cnt_after)
    ed_text <- "Average # ED visits in year after exit"
    hosp_text <- "Average # hospitalizations in year after exit"
  }
  
  output <- output %>% 
    distinct(id_hudhears, ..., ed_cnt, hosp_cnt, ccw_cnt) %>%
    group_by(...) %>%
    summarise(n = number(n(), big.mark = ","), 
              ed_cnt = round(mean(ed_cnt, na.rm = T), 2),
              hosp_cnt = round(mean(hosp_cnt, na.rm = T), 3),
              ccw_cnt = round(mean(ccw_cnt, na.rm = T), 2)) %>%
    pivot_longer(cols = !col_names, values_transform = list(value = as.character)) %>%
    pivot_wider(id_cols = "name", names_from = col_names) %>%
    mutate(category = "Healthcare outcomes", .before = "name",
           name = case_when(name == "ed_cnt" ~ ed_text,
                            name == "hosp_cnt" ~ hosp_text,
                            name == "ccw_cnt" ~ "Average # of chronic conditions",
                            TRUE ~ name)) %>%
    rename("group" = "name")
  
  output
}


# FACTORS ASSOCIATED WITH EXIT VS NOT -----
covariate_nodeath %>% head()

covariate_nodeath %>% count(id_type)

## Demogs ----
# Age
exit_any_age <- age_sum(covariate_nodeath, id_type)

# Gender
exit_any_gender <- demog_pct_sum(covariate_nodeath, level = "ind", demog = "gender", id_type)

# Race/eth
exit_any_race <- demog_pct_sum(covariate_nodeath, level = "ind", demog = "race", id_type)

# Combine for R markdown
exit_any_ind <- bind_rows(exit_any_age, exit_any_gender, exit_any_race) %>%
  rename("Remained" = "id_control", "Exited" = "id_exit") %>%
  filter(!group == "n")


## HH demogs ----
# Time in housing (this is based on HH data)
exit_any_hh_los <- hh_los_sum(covariate_nodeath, id_type)

# Size and composition
exit_any_hh_demogs <- hh_demogs_sum(covariate_nodeath, level = "hh", id_type)

# Program type
exit_any_hh_prog <- demog_pct_sum(covariate_nodeath, level = "hh", demog = "program", id_type)

# Voucher type
exit_any_hh_vouch <- demog_pct_sum(covariate_nodeath, level = "hh", demog = "voucher", id_type)

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


# Combine for R markdown
exit_any_hh <- bind_rows(exit_any_hh_los, exit_any_hh_demogs, exit_any_hh_prog, exit_any_hh_vouch) %>%
  rename("Remained" = "id_control", "Exited" = "id_exit") %>%
  filter(!group == "n")


## Repeat but at individual level ----
# Not used in markdown doc currently

# Size and composition
exit_any_ind_demogs <- hh_demogs_sum(covariate_nodeath, level = "ind", id_type)

# Program type
exit_any_ind_prog <- demog_pct_sum(covariate_nodeath, level = "ind", demog = "program", id_type)

# Voucher type
exit_any_ind_vouch <- demog_pct_sum(covariate_nodeath, level = "ind", demog = "voucher", id_type)


## Medicaid outcomes ----
covariate_nodeath %>% 
  group_by(id_type) %>%
  summarise(n = n(),
            full_cov_11_prior = mean(full_cov_11_prior, na.rm = T),
            full_cov_7_prior = mean(full_cov_7_prior, na.rm = T),
            full_cov_11_after = mean(full_cov_11_after, na.rm = T),
            full_cov_7_after = mean(full_cov_7_after, na.rm = T))


exit_any_mcaid_11_prior <- mcaid_outcomes_sum(covariate_nodeath, time = "prior", cov_time = "11_mth", id_type)

exit_any_mcaid_7_prior <- mcaid_outcomes_sum(covariate_nodeath, time = "prior", cov_time = "7_mth", id_type)



## Demogs by exit and Medicaid status ----
# Age
exit_any_mcaid_age <- age_sum(covariate_nodeath, id_type, full_cov_7_prior)

# Gender
exit_any_mcaid_gender <- demog_pct_sum(covariate_nodeath, level = "ind", demog = "gender", id_type, full_cov_7_prior)

# Race/eth
exit_any_mcaid_race <- demog_pct_sum(covariate_nodeath, level = "ind", demog = "race", id_type, full_cov_7_prior)

# Time in housing
exit_any_mcaid_hh_los <- hh_los_sum(covariate_nodeath, id_type, full_cov_7_prior)



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
exit_type_age <- age_sum(covariate_exits, exit_category)

# Gender
exit_type_gender <- demog_pct_sum(covariate_exits, level = "ind", demog = "gender", exit_category)

# Race/eth
exit_type_race <- demog_pct_sum(covariate_exits, level = "ind", demog = "race", exit_category)

# Combine for R markdown
exit_type_ind <- bind_rows(exit_type_age, exit_type_gender, exit_type_race) %>%
  filter(!group == "n")


## HH demogs ----
# Time in housing (this is based on HH data)
exit_type_hh_los <- hh_los_sum(covariate_exits, exit_category)

# Size and composition
exit_type_hh_demogs <- hh_demogs_sum(covariate_exits, level = "hh", exit_category)

# Program type
exit_type_hh_prog <- demog_pct_sum(covariate_exits, level = "hh", demog = "program", exit_category)

# Voucher type
exit_type_hh_vouch <- demog_pct_sum(covariate_exits, level = "hh", demog = "voucher", exit_category)


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


# Combine for R markdown
exit_type_hh <- bind_rows(exit_type_hh_los, exit_type_hh_demogs, exit_type_hh_prog, exit_type_hh_vouch) %>%
  filter(!group == "n")


## Repeat but at individual level ----
# Not used in markdown doc currently

# Size and composition
exit_type_ind_demogs <- hh_demogs_sum(covariate_exits, level = "ind", exit_category)

# Program type
exit_type_ind_prog <- demog_pct_sum(covariate_exits, level = "ind", demog = "program", exit_category)

# Voucher type
exit_type_ind_vouch <- demog_pct_sum(covariate_exits, level = "ind", demog = "voucher", exit_category)


## Medicaid outcomes ----
covariate_exits %>% 
  group_by(exit_category) %>%
  summarise(n = n(),
            full_cov_11_prior = mean(full_cov_11_prior, na.rm = T),
            full_cov_7_prior = mean(full_cov_7_prior, na.rm = T),
            full_cov_11_after = mean(full_cov_11_after, na.rm = T),
            full_cov_7_after = mean(full_cov_7_after, na.rm = T))


exit_type_mcaid_11_prior <- mcaid_outcomes_sum(covariate_exits, time = "prior", cov_time = "11_mth", exit_category)

exit_type_mcaid_7_prior <- mcaid_outcomes_sum(covariate_exits, time = "prior", cov_time = "7_mth", exit_category)



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



# DEMOGS BY MEDICAID COVERAGE AMONG THOSE EXITING ----
### Demogs (prior) ----
# Age
exit_mcaid_7prior_age <- age_sum(covariate_exits, full_cov_7_prior)
# Gender
exit_mcaid_7prior_gender <- demog_pct_sum(covariate_exits, level = "ind", demog = "gender", full_cov_7_prior)
# Race/eth
exit_mcaid_7prior_race <- demog_pct_sum(covariate_exits, level = "ind", demog = "race", full_cov_7_prior)

# Combine for Rmarkdown
exit_mcaid_7prior_demogs <- bind_rows(exit_mcaid_7prior_age, exit_mcaid_7prior_gender, exit_mcaid_7prior_race) %>%
  rename("no_7mth_cov_prior" = "FALSE", "had_7mth_cov_prior" = "TRUE") %>%
  filter(!group == "n")


### Demogs (after) ----
# Age
exit_mcaid_7after_age <- age_sum(covariate_exits, full_cov_7_after)
# Gender
exit_mcaid_7after_gender <- demog_pct_sum(covariate_exits, level = "ind", demog = "gender", full_cov_7_after)
# Race/eth
exit_mcaid_7after_race <- demog_pct_sum(covariate_exits, level = "ind", demog = "race", full_cov_7_after)

# Combine for Rmarkdown
exit_mcaid_7after_demogs <- bind_rows(exit_mcaid_7after_age, exit_mcaid_7after_gender, exit_mcaid_7after_race) %>%
  rename("no_7mth_cov_after" = "FALSE", "had_7mth_cov_after" = "TRUE") %>%
  filter(!group == "n")


### HH demogs (prior) ----
# Time in housing (this is based on HH data)
exit_mcaid_7prior_hh_los <- hh_los_sum(covariate_exits, full_cov_7_prior)
# Size and composition
exit_mcaid_7prior_hh_demogs <- hh_demogs_sum(covariate_exits, level = "hh", full_cov_7_prior)
# Program type
exit_mcaid_7prior_hh_prog <- demog_pct_sum(covariate_exits, level = "hh", demog = "program", full_cov_7_prior)
# Voucher type
exit_mcaid_7prior_hh_vouch <- demog_pct_sum(covariate_exits, level = "hh", demog = "voucher", full_cov_7_prior)

# Combine for Rmarkdown
exit_mcaid_7prior_hh <- bind_rows(exit_mcaid_7prior_hh_los, exit_mcaid_7prior_hh_demogs, 
                               exit_mcaid_7prior_hh_prog, exit_mcaid_7prior_hh_vouch) %>%
  rename("no_7mth_cov_prior" = "FALSE", "had_7mth_cov_prior" = "TRUE") %>%
  filter(!group == "n")


### HH demogs (after) ----
# Time in housing (this is based on HH data)
exit_mcaid_7after_hh_los <- hh_los_sum(covariate_exits, full_cov_7_after)
# Size and composition
exit_mcaid_7after_hh_demogs <- hh_demogs_sum(covariate_exits, level = "hh", full_cov_7_after)
# Program type
exit_mcaid_7after_hh_prog <- demog_pct_sum(covariate_exits, level = "hh", demog = "program", full_cov_7_after)
# Voucher type
exit_mcaid_7after_hh_vouch <- demog_pct_sum(covariate_exits, level = "hh", demog = "voucher", full_cov_7_after)

# Combine for Rmarkdown
exit_mcaid_7after_hh <- bind_rows(exit_mcaid_7after_hh_los, exit_mcaid_7after_hh_demogs, 
                                  exit_mcaid_7after_hh_prog, exit_mcaid_7after_hh_vouch) %>%
  rename("no_7mth_cov_after" = "FALSE", "had_7mth_cov_after" = "TRUE") %>%
  filter(!group == "n")



# MAKE MARKDOWN DOC ----
render(file.path(here::here(), "analyses/pha_exit_factors.Rmd"), "html_document")
