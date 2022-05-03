##Descriptive Tables Code----
##Updated March 25, 2022

#Steps
#1) Write functions to summarize variables of interest
# age_sum
# condition_sum
# outpatient_sum
# demog_pct_sum
# hh_los_sum
# hh_demogs_sum
# mcaid_outcomes

#2 Demographic tables included:
# 1)Comparison of people with and without the outcome
# -basic demographics
# -mental health conditions
# -regular care before and after the exit
# -exit type and top
# 2)Comparison of people with and without the outcome by exit type
# -Tabulation of exit type
# -Top exit reasons by outcome status


##NOTE: Run Outcomes_code_cleaned.R code first to build appropriate data frames

#######################################


# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, ggplot2, viridis, hrbrthemes,
               knitr, kableExtra, rmarkdown, nnet, scales)

# ##Note: all_pop data frame contains outcome and covariate information for all with and without crisis event
# BRING IN DATA ----
all_pop <- all_pop %>% mutate(outcome = case_when(crisis_any == "1" ~ 1L,crisis_any == "0" ~ 0L))



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


#Condition count
condition_sum <- function(df, ...) {
  col_names <- df %>% select(...) %>% colnames()
  output <- df %>% 
    filter(!is.na(con_count)) %>%
    group_by(...) %>%
    summarise(n = number(n(), big.mark = ","), 
              condition_avg = round(mean(con_count), 1),
              condition_med = median(con_count),
              condition_range = paste0(min(con_count), "-", max(con_count)))%>% 
    pivot_longer(cols = !col_names, values_transform = list(value = as.character)) %>%
    pivot_wider(id_cols = "name", names_from = col_names) %>%
    mutate(category = "Number of Behavioral Health Conditions", .before = "name",
           name = case_when(name == "condition_avg" ~ "Mean",
                            name == "condition_med" ~ "Median",
                            name == "condition_range" ~ "Range",
                            TRUE ~ name)) %>%
    rename("group" = "name")
  
  output
}

#Outpatient visits (year before)
outpatient_sum <- function(df, ...) {
  col_names <- df %>% select(...) %>% colnames()
  output <- df %>% 
    filter(!is.na(out_count)) %>%
    group_by(...) %>%
    summarise(n = number(n(), big.mark = ","), 
              out_avg = round(mean(out_count), 1),
              out_med = median(out_count),
              out_range = paste0(min(out_count), "-", max(out_count)))%>% 
    pivot_longer(cols = !col_names, values_transform = list(value = as.character)) %>%
    pivot_wider(id_cols = "name", names_from = col_names) %>%
    mutate(category = "Number of Outpatient Visits Year Prior", .before = "name",
           name = case_when(name == "out_avg" ~ "Mean",
                            name == "out_med" ~ "Median",
                            name == "out_range" ~ "Range",
                            TRUE ~ name)) %>%
    rename("group" = "name")
  
  output
}

# Can take a common approach to summaries that are simple percents
demog_pct_sum <- function(df, 
                          level = c("ind", "hh"),
                          demog = c("gender", "race", "reg_care", "bh_condition", "program", "voucher", 
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
  } else if (demog=="reg_care") {
    cat_text <= "Routine Behavioral Care"
    output <- output %>% 
      distinct(id_var, exit_date, ..., reg_care) %>%
      filter(!is.na(reg_care)) %>%
      mutate(group=reg_care)
  }else if (demog=="bh_condition") {
    cat_text <= "Behavioral Health Condition"
    output <- output %>% 
      distinct(id_var, exit_date, ..., any_condition) %>%
      filter(!is.na(any_condition)) %>%
      mutate(group=any_condition)
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
  col_names <- df %>% select(...) %>% colnames()
  
  output <- df %>% 
    filter(!is.na(housing_time_at_exit)) %>%
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
    distinct(id_var, exit_date, ..., n_child, hh_size, single_caregiver, hh_disability) %>%
    group_by(...) %>%
    summarise(n = number(n(), big.mark = ","),
              n_child_mean = round(mean(n_child, na.rm = T), 1),
              n_child_med = median(n_child, na.rm = T),
              hh_size_mean = round(mean(hh_size, na.rm = T), 1),
              hh_size_med = median(hh_size, na.rm = T),
              single_caregiver = scales::percent(mean(single_caregiver, na.rm = T), accuracy = 0.1L),
              hh_disability = scales::percent(mean(hh_disability, na.rm = T), accuracy = 0.1L)) %>%
    pivot_longer(cols = !col_names, values_transform = list(value = as.character)) %>%
    pivot_wider(id_cols = "name", names_from = col_names) %>%
    mutate(category = "Household characteristics", .before = "name",
           name = case_when(name == "n_child_mean" ~ "Mean number of children",
                            name == "n_child_med" ~ "Median number of children",
                            name == "hh_size_mean" ~ "Mean household size",
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


# DEMOGRAPHIC FACTORS  -----
all_pop %>% head()

## Demogs ----
# Age
crisis_age <- age_sum(all_pop, crisis_any)

# Gender
crisis_gender <- demog_pct_sum(all_pop, level = "ind", demog = "gender", outcome)

# Race/eth
crisis_race <- demog_pct_sum(all_pop, level = "ind", demog = "race", outcome)

#Any BH condition
crisis_cond_any <- demog_pct_sum(all_pop, level= "ind", demog = "bh_condition", outcome)

#Number of conditions
crisis_cond <- condition_count(all_pop, crisis_any)

#Regular BH care
crisis_reg_care <- demog_pct_sum (all_pop, level= "ind", demog = "reg_care", crisis_any)

#Number of outpatient visits
crisis_out <- outpatient_sum(all_pop, crisis_any)


# Combine for R markdown
crisis_ind <- bind_rows(crisis_age, crisis_gender, crisis_race, crisis_cond_any, crisis_cond, crisis_reg_care, crisis_out) %>%
  rename("Had Crisis Event" = "1", "No Crisis Event" = "0") %>%
  filter(!group == "n")


## HH demogs ----
# Time in housing (this is based on HH data)
crisis_hh_los <- hh_los_sum(all_pop, crisis_any)

# Size and composition
crisis_hh_demogs <- hh_demogs_sum(all_pop, level = "hh", demog= "crisis_any")

# Program type
crisis_hh_prog <- demog_pct_sum(all_pop, level = "hh", demog = "program", crisis_any)

# Voucher type
crisis_hh_vouch <- demog_pct_sum(all_pop, level = "hh", demog = "voucher", crisis_any)

# Agency - this doesn't make sense to do because KCHA only runs 2016-2018
all_pop %>% 
  distinct(hh_id_kc_pha, exit_date, crisis_any, agency) %>%
  filter(!is.na(agency)) %>%
  count(crisis_any, agency) %>%
  group_by(crisis_any) %>%
  ungroup() %>%
  group_by(crisis_any) %>%
  mutate(tot = sum(n),
         pct = round(n/tot*100, 1))


# Combine for R markdown
crisis_hh <- bind_rows(crisis_hh_los, crisis_hh_demogs, crisis_hh_prog, crisis_hh_vouch) %>%
  rename("Had Crisis Event" = "1", "No Crisis Event" = "0") %>%
  filter(!group == "n")


# ## Repeat but at individual level ----
# # Not used in markdown doc currently
# 
# # Size and composition
# crisis_ind_demogs <- hh_demogs_sum(all_pop, level = "ind", crisis_any)
# 
# # Program type
# crisis_ind_prog <- demog_pct_sum(all_pop, level = "ind", demog = "program", crisis_any)
# 
# # Voucher type
# crisis_ind_vouch <- demog_pct_sum(all_pop, level = "ind", demog = "voucher", crisis_any)
# 
# 
# ## Medicaid outcomes ----
# all_pop %>% 
#   group_by(crisis_any) %>%
#   summarise(n = n(),
#             full_cov_11_prior = mean(full_cov_11_prior, na.rm = T),
#             full_cov_7_prior = mean(full_cov_7_prior, na.rm = T),
#             full_cov_11_after = mean(full_cov_11_after, na.rm = T),
#             full_cov_7_after = mean(full_cov_7_after, na.rm = T))
# 
# 
# crisis_mcaid_11_prior <- mcaid_outcomes_sum(all_pop, time = "prior", cov_time = "11_mth", crisis_any)
# 
# crisis_mcaid_7_prior <- mcaid_outcomes_sum(all_pop, time = "prior", cov_time = "7_mth", crisis_any)
# 
# 

## Demogs by Outcome and Medicaid status ----
# Age
crisis_mcaid_age <- age_sum(all_pop, crisis_any, full_cov_7_prior)

# Gender
crisis_mcaid_gender <- demog_pct_sum(all_pop, level = "ind", demog = "gender", crisis_any, full_cov_7_prior)

# Race/eth
crisis_mcaid_race <- demog_pct_sum(all_pop, level = "ind", demog = "race", crisis_any, full_cov_7_prior)

# Time in housing
crisis_mcaid_hh_los <- hh_los_sum(all_pop, crisis_any, full_cov_7_prior)



# REGRESSION MODEL FOR Outcome vs not ----
## Not including Medicaid factors ----
model_data <- all_pop %>%
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

anycrisis <- glm(crisis_any ~ gender_me + race_eth_me + agegrp + los + 
                 major_prog + hh_size + single_caregiver + hh_disability, 
               data = model_data, family = "binomial")

summary(anycrisis)

exp(cbind(OR = coef(anycrisis), confint(anycrisis)))


## Including Medicaid factors ----
model_data_mcaid <- model_data %>% filter(full_cov_7_prior == T)


anycrisis_mcaid <- glm(exit ~ gender_me + race_eth_me + agegrp + los + 
                       major_prog + hh_size + single_caregiver + hh_disability + 
                       ed_cnt_prior + hosp_cnt_prior + ccw_flag, 
                     data = model_data_mcaid, family = "binomial")

summary(anycrisis_mcaid)

exp(cbind(OR = coef(anycrisis_mcaid), confint(anycrisis_mcaid)))

