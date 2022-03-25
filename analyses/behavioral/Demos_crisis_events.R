##Making descriptive tables code
##Updated March 25, 2022

# #Demographic tables included:
# 1)Comparison of people with and without the outcome
# -basic demographics
# -mental health conditions
# -regular care before and after the exit
# -exit type and top
# 2)Comparison of people with and without the outcome by exit type
# -Tabulation of exit type
# -Top exit reasons by outcome status


##NOTE: Run BH_outcomes_creation.R code first to build appropriate data frames

#######################################

# ##Note: exits_bh data frame contains outcome and covariate information for all with
# a behavioral health crisis event


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


# DEMOGRAPHIC FACTORS  -----
exits_bh %>% head()

exits_bh %>% count(id_type)

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
exit_type_gender <- demog_pct_sum(covariate_exits, demog = "gender", exit_category)

# Race/eth
exit_type_race <- demog_pct_sum(covariate_exits, demog = "race", exit_category)

# Combine for R markdown
exit_type_ind <- bind_rows(exit_type_age, exit_type_gender, exit_type_race) %>%
  filter(!group == "n")


## HH demogs ----
# Time in housing (this is based on HH data)
exit_type_hh_los <- hh_los_sum(covariate_exits, exit_category)

# Size and composition
exit_type_hh_demogs <- hh_demogs_sum(covariate_exits, level = "hh", exit_category)

# Program type
exit_type_hh_prog <- demog_pct_sum(covariate_exits, demog = "program", exit_category)

# Voucher type
exit_type_hh_vouch <- demog_pct_sum(covariate_exits, demog = "voucher", exit_category)


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