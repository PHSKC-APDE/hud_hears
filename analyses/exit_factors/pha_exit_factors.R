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
               knitr, kableExtra, rmarkdown, nnet, scales, gt, DHARMa)

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
covariate <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.control_match_covariate") %>%
  mutate(exit = case_when(id_type == "id_exit" ~ 1L,
                          id_type == "id_control" ~ 0L),
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
         # Clean up program types
         prog_type_use = case_when(prog_type %in% c("PBS8", "COLLABORATIVE HOUSING") ~ "PBV",
                                   prog_type %in% c("PH", "SHA OWNED AND MANAGED") ~ "PH",
                                   prog_type %in% c("PORT", "TBS8", "TENANT BASED") ~ "TBV"),
         # Flag anyone with missing covariates (to get numbers to align with consort diagram)
         full_demog = (!(is.na(exit_category) | is.na(age_at_exit) | is.na(gender_me) | 
                              is.na(race_eth_me) | race_eth_me == "Unknown" |
                              is.na(agency) | is.na(single_caregiver) | 
                              is.na(hh_size) | is.na(hh_disability) | is.na(housing_time_at_exit) | 
                           is.na(prog_type_use)))
  )

covariate_hh <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.control_match_covariate_hh") %>%
  mutate(exit = case_when(id_type == "id_exit" ~ 1L,
                          id_type == "id_control" ~ 0L),
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
         # Clean up program types
         prog_type_use = case_when(prog_type %in% c("PBS8", "COLLABORATIVE HOUSING") ~ "PBV",
                                   prog_type %in% c("PH", "SHA OWNED AND MANAGED") ~ "PH",
                                   prog_type %in% c("PORT", "TBS8", "TENANT BASED") ~ "TBV"),
         # Flag anyone with missing covariates (to get numbers to align with consort diagram)
         full_demog = (!(is.na(exit_category) | is.na(age_at_exit) | is.na(gender_me) | 
                              is.na(race_eth_me) | race_eth_me == "Unknown" |
                              is.na(agency) | is.na(single_caregiver) | 
                              is.na(hh_size) | is.na(hh_disability) | is.na(housing_time_at_exit) | 
                           is.na(prog_type_use)))
         )

covariate_nodeath <- covariate %>% filter(exit_death != 1) %>%
  mutate(crisis_any_prior = case_when(crisis_prior == 0 ~ 0L,
                                crisis_prior >= 1 ~ 1L),
         crisis_ed_any_prior = case_when(crisis_ed_prior == 0 ~ 0L,
                                   crisis_ed_prior >= 1 ~ 1L),
         recent_homeless_grp = case_when(recent_homeless == 0 ~ 0L,
                                         recent_homeless >= 1 ~ 1L))

covariate_nodeath_hh <- covariate_hh %>% filter(exit_death != 1) %>%
  mutate(crisis_any_prior = case_when(crisis_prior == 0 ~ 0L,
                                crisis_prior >= 1 ~ 1L),
         crisis_ed_any_prior = case_when(crisis_ed_prior == 0 ~ 0L,
                                   crisis_ed_prior >= 1 ~ 1L),
         hhold_crisis_any_prior = case_when(hhold_crisis_prior == 0 ~ 0L,
                                      hhold_crisis_prior >= 1 ~ 1L),
         hhold_crisis_ed_any_prior = case_when(hhold_crisis_ed_prior == 0 ~ 0L,
                                         hhold_crisis_ed_prior >= 1 ~ 1L),
         recent_homeless_grp = case_when(recent_homeless == 0 ~ 0L,
                                         recent_homeless >= 1 ~ 1L),
         hhold_recent_homeless_grp = case_when(hhold_recent_homeless == 0 ~ 0L,
                                               hhold_recent_homeless >= 1 ~ 1L))

covariate_exits <- covariate_nodeath %>% filter(id_type == "id_exit")
covariate_exits_hh <- covariate_nodeath_hh %>% filter(id_type == "id_exit")

# Create dfs with additional criteria for Medicaid includsion
covariate_nodeath_hh_mcaid <- covariate_nodeath_hh %>% filter(age_at_exit < 62) 
covariate_exits_hh_mcaid <- covariate_exits_hh %>% filter(age_at_exit < 62)

covariate_nodeath_mcaid <- covariate_nodeath %>% filter(age_at_exit < 62) 
covariate_exits_mcaid <- covariate_exits %>% filter(age_at_exit < 62)


# SET UP GENERIC SUMMARY FUNCTIONS ----
age_sum <- function(df, full_demog = F, ...) {
  # Set things up to select in pivot_ functions
  # There is probably a better way to do this but it works
  col_names <- df %>% select(...) %>% colnames()
  
  if (full_demog == T) {
    df <- df %>% filter(full_demog == T)
  }
  
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
                            name == "senior" ~ "Senior (aged 62+)",
                            name == "child" ~ "Child (aged <18)",
                            TRUE ~ name)) %>%
    rename("group" = "name")
  
  output
}

# Can take a common approach to summaries that are simple percents
demog_pct_sum <- function(df, 
                          full_demog = F,
                          level = c("ind", "hh"),
                          demog = c("gender", "race", "program_major", "program_type", "voucher", 
                                    "program_ind", "voucher_ind", "crisis_any_prior",
                                    "homeless_grp"), 
                      ...) {
  # Set things up to select in pivot_ functions
  # There is probably a better way to do this but it works
  col_names <- df %>% select(...) %>% colnames()
  
  level <- match.arg(level)
  demog <- match.arg(demog)
  
  if (full_demog == T) {
    df <- df %>% filter(full_demog == T)
  }
  
  if (level == "ind") {
    output <- df %>% mutate(id_var = id_hudhears)
  } else if (level == "hh") {
    output <- df %>% mutate(id_var = hh_id_kc_pha)
  }
  
  if (demog == "gender") {
    cat_text <- "Gender"
    output <- output %>% 
      distinct(id_var, exit_date, ..., gender_me) %>% 
      mutate(group = case_when(gender_me == "Multiple" ~ "Another gender",
                               TRUE ~ gender_me))
  } else if (demog == "race") {
    cat_text <- "Race/ethnicity"
    output <- output %>% 
      distinct(id_var, exit_date, ..., race_eth_me) %>%
      filter(!is.na(race_eth_me) & race_eth_me != "Unknown") %>%
      mutate(group = ifelse(race_eth_me == "Latino", "Latina/o/x", race_eth_me))
  } else if (demog == "program_major") {
    cat_text <- "Program type"
    output <- output %>% 
      distinct(id_var, exit_date, ..., major_prog) %>%
      filter(!is.na(major_prog)) %>%
      mutate(group = major_prog)
  } else if (demog == "program_type") {
    cat_text <- "Program type"
    output <- output %>% 
      distinct(id_var, exit_date, ..., prog_type_use) %>%
      filter(!is.na(prog_type_use)) %>%
      mutate(group = prog_type_use)
  } else if (demog == "voucher") {
    cat_text <- "Voucher type"
    output <- output %>% 
      filter(major_prog == "HCV") %>%
      distinct(id_var, exit_date, ..., vouch_type_use) %>%
      mutate(group = vouch_type_use) %>%
      filter(!is.na(group))
  } else if (demog == "crisis_any_prior") {
    cat_text <- "Health and homelessness events"
    output <- output %>% 
      distinct(id_var, exit_date, ..., crisis_any_prior) %>%
      filter(!is.na(crisis_any_prior)) %>%
      mutate(group = case_when(crisis_any_prior == 1 ~ paste0("Experienced 1+ behavioral health crisis events ", 
                                                              "in year prior to exit (excl. Medicaid ED visits)"),
                               crisis_any_prior == 0 ~ paste0("Did not experience a behavioral health crisis ", 
                                                              "in year prior to exit (excl. Medicaid ED visits)")))
  } else if (demog == "homeless_grp") {
    cat_text <- "Health and homelessness events"
    output <- output %>% 
      distinct(id_var, exit_date, ..., recent_homeless_grp) %>%
      filter(!is.na(recent_homeless_grp)) %>%
      mutate(group = case_when(recent_homeless_grp == 1 ~ "Experienced recent homelessness",
                               recent_homeless_grp == 0 ~ "Did not experience recent homelessness"))
  }
  
  # Common reshaping approach
  output <- output %>%
    count(..., group) %>%
    group_by(...) %>%
    mutate(tot = sum(n), pct = round(n/tot*100,1)) %>%
    ungroup() %>%
    mutate(category = cat_text,
           val = paste0(number(n, big.mark = ",", accuracy = 1L), " (", pct, "%)")) %>%
    select(col_names, category, group, val) %>%
    pivot_wider(id_cols = c("category", "group"), names_from = col_names, values_from = "val")
  
  output
}


demog_num_sum <- function(df, 
                          full_demog = F, 
                          level = c("ind", "hh"),
                          demog = c("los", "opp_index"),
                          ...) {
  
  level <- match.arg(level)
  demog <- match.arg(demog)
  
  col_names <- df %>% distinct(...) %>% colnames()
  
  if (full_demog == T) {
    df <- df %>% filter(full_demog == T)
  }
  
  if (level == "ind") {
    df <- df %>% mutate(id_var = id_hudhears)
  } else if (level == "hh") {
    df <- df %>% mutate(id_var = hh_id_kc_pha)
  }
  
  if (demog == "los") {
    cat_text <- "HoH time in housing"
    scale <- 1
    df <- df %>% 
      filter(!is.na(housing_time_at_exit)) %>%
      distinct(id_var, housing_time_at_exit, ...) %>%
      mutate(group = housing_time_at_exit)
  } else if (demog == "opp_index") {
    cat_text <- "Opportunity index"
    scale <- 2
    df <- df %>% 
      filter(!is.na(kc_opp_index_score)) %>%
      distinct(id_var, kc_opp_index_score, ...) %>%
      mutate(group = kc_opp_index_score)
  }
  
  output <- df %>% 
    group_by(...) %>%
    summarise(n = number(n(), big.mark = ","), 
              grp_mean = round(mean(group), scale),
              grp_med = round(median(group), scale)) %>%
    pivot_longer(cols = !col_names, values_transform = list(value = as.character)) %>%
    pivot_wider(id_cols = "name", names_from = col_names) %>%
    mutate(category = cat_text, .before = "name",
           name = case_when(demog == "los" & name == "grp_mean" ~ "Mean time (years)",
                            demog == "los" & name == "grp_med" ~ "Median time (years)",
                            demog == "opp_index" & name == "grp_mean" ~ "Mean score",
                            demog == "opp_index" & name == "grp_med" ~ "Median score",
                            TRUE ~ name)) %>%
    rename("group" = "name")

  output
}

hh_demogs_sum <- function(df, full_demog = F, level = c("hh", "ind"), ...) {
  col_names <- df %>% select(...) %>% colnames()
  
  if (full_demog == T) {
    df <- df %>% filter(full_demog == T)
  }
  
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
                            name == "hh_disability" ~ "Head of household disability",
                            TRUE ~ name)) %>%
    rename("group" = "name")
  
  output
}


mcaid_outcomes_sum <- function(df, 
                               full_demog = F,
                               time = c("prior", "after", "both"), 
                               cov_time = c("7_mth", "11_mth"),
                               show_num = T,
                               ...) {
  col_names <- df %>% select(...) %>% colnames()
  
  time <- match.arg(time)
  cov_time<- match.arg(cov_time)
  
  if (full_demog == T) {
    df <- df %>% filter(full_demog == T)
  }
  
  if (time == "prior" | time == "both") {
    if (time == "prior" & cov_time == "7_mth") {
      output <- df %>% filter(full_cov_7_prior == T)
    } else if (time == "prior" & cov_time == "11_mth") {
      output <- df %>% filter(full_cov_11_prior == T)
    } else if (time == "both" & cov_time == "7_mth") {
      output <- df %>% filter(full_cov_7_prior == T & full_cov_7_after == T)
    } else if (time == "both" & cov_time == "11_mth") {
      output <- df %>% filter(full_cov_11_prior == T & full_cov_11_after == T)
    }
    # Set up ED and hospitalization vars
    output <- output %>%
      mutate(ed_cnt = ed_cnt_prior,
             ed_any = case_when(ed_cnt_prior >= 1 ~ 1L,
                                ed_cnt_prior == 0 ~ 0L),
             hosp_cnt = hosp_cnt_prior * 100,
             hosp_any = case_when(hosp_cnt_prior >= 1 ~ 1L,
                                  hosp_cnt_prior == 0 ~ 0L),
             wc_any = case_when(wc_cnt_prior >= 1 ~ 1L,
                                wc_cnt_prior == 0 ~ 0L),
             crisis = crisis_ed_any_prior)
    ed_cnt_text <- "Average # ED visits in year prior to exit"
    ed_any_text <- "Experienced 1+ ED visits in year prior to exit"
    hosp_cnt_text <- "Average # hospitalizations in year prior to exit (per 100 people)"
    hosp_any_text <- "Experienced 1+ hospitalizations in year prior to exit"
    wc_text <- "Completed 1+ well-child visits in the year prior to exit (ages <6)"
    crisis_text <- "Experienced 1+ behavioral health crisis events in year prior to exit (inc. ED visits)"
  } else if (time == "after") {
    if (cov_time == "7_mth") {
      output <- df %>% filter(full_cov_7_after == T)
    } else if (cov_time == "11_mth") {
      output <- df %>% filter(full_cov_11_after == T)
    }
    # Set up ED and hospitalization vars
    output <- output %>%
      mutate(ed_cnt = ed_cnt_after,
             ed_any = case_when(ed_cnt_after >= 1 ~ 1L,
                                ed_cnt_after == 0 ~ 0L),
             hosp_cnt = hosp_cnt_after * 100,
             hosp_any = case_when(hosp_cnt_after >= 1 ~ 1L,
                                  hosp_cnt_after == 0 ~ 0L),
             wc_any = case_when(wc_cnt_after >= 1 ~ 1L,
                                wc_cnt_after == 0 ~ 0L),
             crisis = crisis_ed_any_prior)
    ed_cnt_text <- "Average # ED visits in year after exit"
    ed_any_text <- "Experienced 1+ ED visits in year after exit"
    hosp_cnt_text <- "Average # hospitalizations in year after exit (per 100 people)"
    hosp_any_text <- "Experienced 1+ hospitalizations in year after exit"
    wc_text <- "Completed 1+ well-child visits in the year after exit (ages <6)"
    crisis_text <- "Crisis events not captured for year after exit - DELETE"
  } else if (time == "both") {
    if (cov_time == "7_mth") {
      output <- df %>% filter(full_cov_7_prior == T & full_cov_7_after == T)
    } else if (cov_time == "11_mth") {
      output <- df %>% filter(full_cov_11_prior == T & full_cov_11_after == T)
    }
    # Set up ED and hospitalization vars
    output <- output %>%
      mutate(ed_cnt = ed_cnt_after,
             ed_any = case_when(ed_cnt_after >= 1 ~ 1L,
                                ed_cnt_after == 0 ~ 0L),
             hosp_cnt = hosp_cnt_after * 100,
             hosp_any = case_when(hosp_cnt_after >= 1 ~ 1L,
                                  hosp_cnt_after == 0 ~ 0L),
             wc_any = case_when(wc_cnt_after >= 1 ~ 1L,
                                wc_cnt_after == 0 ~ 0L),
             crisis = crisis_ed_any_prior)
    ed_cnt_text <- "Average # ED visits in year after exit"
    ed_any_text <- "Experienced 1+ ED visits in year after exit"
    hosp_cnt_text <- "Average # hospitalizations in year after exit (per 100 people)"
    hosp_any_text <- "Experienced 1+ hospitalizations in year after exit"
    wc_text <- "Completed 1+ well-child visits in the year after exit (ages <6)"
    crisis_text <- "Crisis events not captured for year after exit - DELETE"
  }
  
  output <- output %>% 
    distinct(id_hudhears, ..., ed_cnt, ed_any, hosp_cnt, hosp_any, age_at_exit,
             wc_any, crisis, ccw_cnt) %>%
    group_by(...)
  
  output_1 <- output %>%
    summarise(n = number(n(), big.mark = ","), 
              ccw_cnt = round(mean(ccw_cnt, na.rm = T), 1),
              ed_cnt = round(mean(ed_cnt, na.rm = T), 1),
              hosp_cnt = round(mean(hosp_cnt, na.rm = T), 1))
  
  if (show_num == F) {
    output_2 <- output %>%
      summarise(ed_any = number(mean(ed_any, na.rm = T) * 100, 0.1, suffix = "%"),
                hosp_any = number(mean(hosp_any, na.rm = T) * 100, 0.1, suffix = "%"),
                wc_any = number(mean(cur_data() %>% as.data.frame() %>% 
                                       filter(age_at_exit <6) %>% 
                                       pull(wc_any), 
                                     na.rm = T) * 100, 0.1, suffix = "%"),
                crisis = number(mean(crisis, na.rm = T) * 100, 0.1, suffix = "%"))
  } else if (show_num == T) {
    output_2 <- output %>%
      summarise(ed_any = paste0(number(sum(ed_any, na.rm = T), big.mark = ","), " (",
                                number(mean(ed_any, na.rm = T) * 100, 0.1, suffix = "%"), ")"),
                hosp_any = paste0(number(sum(hosp_any, na.rm = T), big.mark = ","), " (",
                                number(mean(hosp_any, na.rm = T) * 100, 0.1, suffix = "%"), ")"),
                wc_any = paste0(number(sum(cur_data() %>% as.data.frame() %>% 
                                              filter(age_at_exit <6) %>% 
                                              pull(wc_any), 
                                            na.rm = T),
                                       big.mark = ","), " (",
                                number(mean(cur_data() %>% as.data.frame() %>% 
                                       filter(age_at_exit <6) %>% 
                                       pull(wc_any), 
                                     na.rm = T) * 100, 0.1, suffix = "%"), ")"),
                crisis = paste0(number(sum(crisis, na.rm = T), big.mark = ","), " (",
                                number(mean(crisis, na.rm = T) * 100, 0.1, suffix = "%"), ")"))
  }
  
  # Bring pieces together and reshape so each group is in its own column
  output <- left_join(output_1, output_2, by = col_names) %>%
    pivot_longer(cols = !col_names, values_transform = list(value = as.character)) %>%
    pivot_wider(id_cols = "name", names_from = col_names) %>%
    mutate(category = "Health and homelessness events", .before = "name",
           name = case_when(name == "ed_cnt" ~ ed_cnt_text,
                            name == "ed_any" ~ ed_any_text,
                            name == "hosp_cnt" ~ hosp_cnt_text,
                            name == "hosp_any" ~ hosp_any_text,
                            name == "wc_any" ~ wc_text,
                            name == "crisis" ~ crisis_text,
                            name == "ccw_cnt" ~ "Average # of chronic conditions",
                            TRUE ~ name)) %>%
    rename("group" = "name")
  
  if (time == "after") {
    output <- output %>% filter(str_detect(group, "DELETE", negate = T))
  }
  
  output
}


# FACTORS ASSOCIATED WITH EXIT VS NOT -----
## Demogs ----
# Age
exit_any_age_hh <- age_sum(covariate_nodeath_hh, full_demog = T, id_type)
exit_any_age_ind <- age_sum(covariate_nodeath, full_demog = T, id_type)

# Gender
exit_any_gender_hh <- demog_pct_sum(covariate_nodeath_hh, full_demog = T, level = "hh", demog = "gender", id_type)
exit_any_gender_ind <- demog_pct_sum(covariate_nodeath, full_demog = T, level = "ind", demog = "gender", id_type)

# Race/eth
exit_any_race_hh <- demog_pct_sum(covariate_nodeath_hh, full_demog = T, level = "hh", demog = "race", id_type)
exit_any_race_ind <- demog_pct_sum(covariate_nodeath, full_demog = T, level = "ind", demog = "race", id_type)

# Combine for R markdown
exit_any_ind_hh <- bind_rows(exit_any_age_hh, exit_any_gender_hh, exit_any_race_hh)
exit_any_ind_ind <- bind_rows(exit_any_age_ind, exit_any_gender_ind, exit_any_race_ind)
rm(exit_any_age_hh, exit_any_gender_hh, exit_any_race_hh,
   exit_any_age_ind, exit_any_gender_ind, exit_any_race_ind)


## HH demogs ----
# Time in housing (this is based on HH data)
exit_any_hh_los_hh <- demog_num_sum(covariate_nodeath_hh, demog = "los", full_demog = T, level = "hh", id_type)
exit_any_hh_los_ind <- demog_num_sum(covariate_nodeath, demog = "los", full_demog = T, level = "ind", id_type)

# Size and composition
exit_any_hh_demogs_hh <- hh_demogs_sum(covariate_nodeath_hh, full_demog = T, level = "hh", id_type)
exit_any_hh_demogs_ind <- hh_demogs_sum(covariate_nodeath, full_demog = T, level = "ind", id_type)

# Program type
exit_any_hh_prog_hh <- demog_pct_sum(covariate_nodeath_hh, full_demog = T, level = "hh", demog = "program_type", id_type)
exit_any_hh_prog_ind <- demog_pct_sum(covariate_nodeath, full_demog = T, level = "ind", demog = "program_type", id_type)

# Voucher type
exit_any_hh_vouch_hh <- demog_pct_sum(covariate_nodeath_hh, full_demog = T, level = "hh", demog = "voucher", id_type)
exit_any_hh_vouch_ind <- demog_pct_sum(covariate_nodeath, full_demog = T, level = "ind", demog = "voucher", id_type)

# Combine for R markdown
exit_any_hh_hh <- bind_rows(exit_any_hh_los_hh, exit_any_hh_demogs_hh, 
                            exit_any_hh_prog_hh, exit_any_hh_vouch_hh)
exit_any_hh_ind <- bind_rows(exit_any_hh_los_ind, exit_any_hh_demogs_ind, 
                             exit_any_hh_prog_ind, exit_any_hh_vouch_ind)
rm(exit_any_hh_los_hh, exit_any_hh_demogs_hh, exit_any_hh_prog_hh, exit_any_hh_vouch_hh,
   exit_any_hh_los_ind, exit_any_hh_demogs_ind, exit_any_hh_prog_ind, exit_any_hh_vouch_ind)


## Medicaid outcomes ----
exit_any_mcaid_11_prior_hh <- mcaid_outcomes_sum(covariate_nodeath_hh_mcaid, full_demog = T, 
                                                 time = "prior", cov_time = "11_mth", show_num = T, id_type)
exit_any_mcaid_7_prior_hh <- mcaid_outcomes_sum(covariate_nodeath_mcaid, full_demog = T, 
                                                time = "prior", cov_time = "7_mth", show_num = T, id_type)

# Restrict individual to covered prior and after because the individual table
# is used in a different part of the final report
exit_any_mcaid_11_prior_ind <- mcaid_outcomes_sum(covariate_nodeath_mcaid, full_demog = T, 
                                                  time = "both", cov_time = "11_mth", show_num = T, id_type)
exit_any_mcaid_7_prior_ind <- mcaid_outcomes_sum(covariate_nodeath_mcaid, full_demog = T, 
                                                time = "both", cov_time = "7_mth", show_num = T, id_type)


## BH and homelessness ----
exit_any_homeless_hh <- demog_pct_sum(covariate_nodeath_hh, full_demog = T, level = "hh", 
                                   demog = "homeless_grp", id_type)
exit_any_crisis_hh <- demog_pct_sum(covariate_nodeath_hh, full_demog = T, level = "hh", 
                                   demog = "crisis_any_prior", id_type)


exit_any_homeless_ind <- demog_pct_sum(covariate_nodeath, full_demog = T, level = "ind", 
                                      demog = "homeless_grp", id_type)
exit_any_crisis_ind <- demog_pct_sum(covariate_nodeath, full_demog = T, level = "ind", 
                                    demog = "crisis_any_prior", id_type)



## Demogs by exit and Medicaid status ----
# Age
exit_any_mcaid_age_hh <- age_sum(covariate_nodeath_hh, full_demog = T, id_type, full_cov_7_prior)

# Gender
exit_any_mcaid_gender_hh <- demog_pct_sum(covariate_nodeath_hh, full_demog = T,
                                          level = "hh", demog = "gender", id_type, full_cov_7_prior)

# Race/eth
exit_any_mcaid_race_hh <- demog_pct_sum(covariate_nodeath_hh, full_demog = T, 
                                     level = "hh", demog = "race", id_type, full_cov_7_prior)

# Time in housing
exit_any_mcaid_los_hh <- demog_num_sum(covariate_nodeath_hh, full_demog = T, demog = "los",
                                       id_type, full_cov_7_prior)

# Size and composition
exit_any_mcaid_demogs_hh <- hh_demogs_sum(covariate_nodeath_hh, full_demog = T, 
                                          level = "hh", id_type, full_cov_7_prior)

# Program type
exit_any_mcaid_prog_hh <- demog_pct_sum(covariate_nodeath_hh, full_demog = T, 
                                        level = "hh", demog = "program_type", id_type, full_cov_7_prior)

# Voucher type
exit_any_mcaid_vouch_hh <- demog_pct_sum(covariate_nodeath_hh, full_demog = T, 
                                         level = "hh", demog = "voucher", id_type, full_cov_7_prior)

# Prior homelessness
exit_any_mcaid_homeless_hh <- demog_pct_sum(covariate_nodeath_hh, full_demog = T, level = "hh", 
                                         demog = "homeless_grp", id_type, full_cov_7_prior)
exit_any_mcaid_crisis_hh <- demog_pct_sum(covariate_nodeath_hh, full_demog = T, level = "hh", 
                                 demog = "crisis_any_prior", id_type, full_cov_7_prior)



# Combine for R markdown
exit_any_mcaid_prior <- bind_rows(exit_any_mcaid_age_hh, exit_any_mcaid_gender_hh, 
                                  exit_any_mcaid_race_hh, exit_any_mcaid_los_hh,
                                  exit_any_mcaid_demogs_hh, exit_any_mcaid_prog_hh,
                                  exit_any_mcaid_vouch_hh, 
                                  exit_any_mcaid_homeless_hh, exit_any_mcaid_crisis_hh) %>% 
  filter(group != "n" & 
           str_detect(group, "Average # (ED|hosp)", negate = T) &
           str_detect(group, "Did not experience", negate = T))

rm(exit_any_mcaid_age_hh, exit_any_mcaid_gender_hh, exit_any_mcaid_race_hh, exit_any_mcaid_los_hh,
   exit_any_mcaid_demogs_hh, exit_any_mcaid_prog_hh, exit_any_mcaid_vouch_hh, 
   exit_any_mcaid_homeless_hh, exit_any_mcaid_crisis_hh)



# REGRESSION MODEL FOR EXIT VS NOT ----
## Not including Medicaid factors ----
model_data <- covariate_nodeath_hh %>%
  filter(full_demog == T) %>%
  mutate(across(c("gender_me", "major_prog", "prog_type_use", "vouch_type_use"), ~ as_factor(.))) %>%
  # Relevel factors as they are made
  mutate(race_eth_me = fct_relevel(race_eth_me, c("White")),
    age_grp = fct_relevel(as_factor(case_when(
      age_at_exit < 25 ~ "<25",
      data.table::between(age_at_exit, 25, 44.99, NAbounds = NA) ~ "25-44",
      data.table::between(age_at_exit, 45, 61.99, NAbounds = NA) ~ "45-61",
      age_at_exit >= 62 ~ "62+",
      is.na(age_at_exit) ~ NA_character_)),
      "<25"),
    prog_type_use = fct_relevel(prog_type_use, "TBV"),
    los = fct_relevel(
      as_factor(case_when(housing_time_at_exit < 3 ~ "<3",
                          between(housing_time_at_exit, 3, 5.999, NAbounds = NA) ~ "3-5.99",
                          between(housing_time_at_exit, 6, 9.999, NAbounds = NA) ~ "6-9.99",
                          housing_time_at_exit >= 10 ~ "10+")),
      "<3", "3-5.99"),
    ed_any_prior = case_when(ed_cnt_prior >= 1 ~ 1L,
                       ed_cnt_prior == 0 ~ 0L),
    hosp_any_prior = case_when(hosp_cnt_prior >= 1 ~ 1L,
                         hosp_cnt_prior == 0 ~ 0L)
    )


anyexit <- glm(exit ~ gender_me + race_eth_me + age_grp + los + 
                 prog_type_use + hh_size + single_caregiver + hh_disability + 
                 crisis_any_prior + recent_homeless_grp, 
               data = model_data, family = "binomial")

anyexit_summ <- summary(anyexit)
anyexit_output <- cbind(OR = exp(coef(anyexit)),
                        exp(confint(anyexit)),
                        p = anyexit_summ$coefficients[,4]) %>% 
  as.data.frame() %>%
  rownames_to_column(var = "group") %>%
  mutate(ci = paste0(number(`2.5 %`, accuracy = 0.01), "–", 
                     number(`97.5 %`, accuracy = 0.01)))


### Model checking ----
# Using DHARMa
# Calculate residuals
anyexit_simulation_output <- simulateResiduals(fittedModel = anyexit, plot = F)

# Look at residuals for each covariate
covar = c("gender_me", "race_eth_me", "age_grp", "los", "prog_type_use", 
          "hh_size", "single_caregiver", "hh_disability", 
          "crisis_any_prior", "recent_homeless_grp")

# Need to delay between each run or only the last graph shows
# Numeric values produce 2 rounds of plots
lapply(covar, function(x) {
  message(x)
  plot(anyexit_simulation_output, form = model_data[[x]], quantreg = T)
  Sys.sleep(3)})


## Including Medicaid factors ----
model_data_mcaid <- model_data %>% 
  filter(full_cov_7_prior == T & age_at_exit < 62) %>%
  mutate(age_grp = fct_drop(age_grp))

anyexit_mcaid <- glm(exit ~ gender_me + race_eth_me + age_grp + los + 
                       prog_type_use + hh_size + single_caregiver + hh_disability + 
                       ed_any_prior + hosp_any_prior + ccw_flag + 
                       crisis_ed_any_prior + recent_homeless_grp, 
               data = model_data_mcaid, family = "binomial")

anyexit_mcaid_summ <- summary(anyexit_mcaid)
anyexit_mcaid_output <- cbind(OR = exp(coef(anyexit_mcaid)), 
                              exp(confint(anyexit_mcaid)),
                              p = anyexit_mcaid_summ$coefficients[,4]) %>% 
  as.data.frame() %>%
  rownames_to_column(var = "group") %>%
  mutate(ci = paste0(number(`2.5 %`, accuracy = 0.01), "–", 
                     number(`97.5 %`, accuracy = 0.01)))


### Model checking ----
# Using DHARMa
# Calculate residuals
anyexit_mcaid_simulation_output <- simulateResiduals(fittedModel = anyexit_mcaid, plot = F)

# Look at residuals for each covariate
covar = c("gender_me", "race_eth_me", "age_grp", "los", "major_prog", "hh_size", 
          "single_caregiver", "hh_disability", "kc_opp_index_score", 
          "ed_cnt_prior", "hosp_cnt_prior", "ccw_flag", "crisis_ed_any_prior", 
          "recent_homeless_grp")

# Need to delay between each run or only the last graph shows
# Numeric values produce 2 rounds of plots
lapply(covar, function(x) {
  message(x)
  plot(anyexit_mcaid_simulation_output, form = model_data_mcaid[[x]], quantreg = T)
  Sys.sleep(3)})



# FACTORS ASSOCIATED WITH EXIT TYPE -----
## Demogs ----
# Age
exit_type_age_hh <- age_sum(covariate_exits_hh, full_demog = T, exit_category)
exit_type_age_ind <- age_sum(covariate_exits, full_demog = T, exit_category)

# Gender
exit_type_gender_hh <- demog_pct_sum(covariate_exits_hh, full_demog = T, level = "hh", demog = "gender", exit_category)
exit_type_gender_ind <- demog_pct_sum(covariate_exits, full_demog = T, level = "ind", demog = "gender", exit_category)

# Race/eth
exit_type_race_hh <- demog_pct_sum(covariate_exits_hh, full_demog = T, level = "hh", demog = "race", exit_category)
exit_type_race_ind <- demog_pct_sum(covariate_exits, full_demog = T, level = "ind", demog = "race", exit_category)

# Combine for R markdown
exit_type_ind_hh <- bind_rows(exit_type_age_hh, exit_type_gender_hh, exit_type_race_hh) %>%
  filter(!group == "n")
exit_type_ind_ind <- bind_rows(exit_type_age_ind, exit_type_gender_ind, exit_type_race_ind) %>%
  filter(!group == "n")

rm(exit_type_age_hh, exit_type_gender_hh, exit_type_race_hh,
   exit_type_age_ind, exit_type_gender_ind, exit_type_race_ind)


## HH demogs ----
# You would expect the 'individual' and '_hh' results to be the same but they are not.
# It looks like there are ~224 rows where no id in a household lines up with a hh_id
# These are dropped for the HH-level analyses

# Time in housing (this is based on HH data)
exit_type_hh_los_hh <- demog_num_sum(covariate_exits_hh, demog = "los", full_demog = T, level = "hh", exit_category)
exit_type_hh_los_ind <- demog_num_sum(covariate_exits, demog = "los", full_demog = T, level = "ind", exit_category)

# Size and composition
exit_type_hh_demogs_hh <- hh_demogs_sum(covariate_exits_hh, full_demog = T, level = "hh", exit_category)
exit_type_hh_demogs_ind <- hh_demogs_sum(covariate_exits, full_demog = T, level = "ind", exit_category)

# Program type
exit_type_hh_prog_hh <- demog_pct_sum(covariate_exits_hh, full_demog = T, level = "hh", demog = "program_type", exit_category)
exit_type_hh_prog_ind <- demog_pct_sum(covariate_exits, full_demog = T, level = "ind", demog = "program_type", exit_category)

# Voucher type
exit_type_hh_vouch_hh <- demog_pct_sum(covariate_exits_hh, full_demog = T, level = "hh", demog = "voucher", exit_category)
exit_type_hh_vouch_ind <- demog_pct_sum(covariate_exits, full_demog = T, level = "ind", demog = "voucher", exit_category)


# Combine for R markdown
exit_type_hh_hh <- bind_rows(exit_type_hh_los_hh, exit_type_hh_demogs_hh, 
                             exit_type_hh_prog_hh, exit_type_hh_vouch_hh) %>%
  filter(!group == "n")
exit_type_hh_ind <- bind_rows(exit_type_hh_los_ind, exit_type_hh_demogs_ind, 
                             exit_type_hh_prog_ind, exit_type_hh_vouch_ind) %>%
  filter(!group == "n")
rm(exit_type_hh_los_hh, exit_type_hh_demogs_hh, exit_type_hh_prog_hh, exit_type_hh_vouch_hh,
   exit_type_hh_los_ind, exit_type_hh_demogs_ind, exit_type_hh_prog_ind, exit_type_hh_vouch_ind)


## Medicaid outcomes ----
exit_type_mcaid_11_prior_hh <- mcaid_outcomes_sum(covariate_exits_hh_mcaid, full_demog = T, 
                                               time = "prior", cov_time = "11_mth", show_num = T, exit_category)
exit_type_mcaid_7_prior_hh <- mcaid_outcomes_sum(covariate_exits_hh_mcaid, full_demog = T, 
                                              time = "prior", cov_time = "7_mth", show_num = T, exit_category)

# Restrict individual to covered prior and after because the individual table
# is used in a different part of the final report
exit_type_mcaid_11_prior_ind <- mcaid_outcomes_sum(covariate_exits_mcaid, full_demog = T, 
                                                  time = "both", cov_time = "11_mth", show_num = T, exit_category)
exit_type_mcaid_7_prior_ind <- mcaid_outcomes_sum(covariate_exits_mcaid, full_demog = T, 
                                                 time = "both", cov_time = "7_mth", show_num = T, exit_category)


## BH and homelessness ----
exit_type_homeless_hh <- demog_pct_sum(covariate_exits_hh, full_demog = T, level = "hh", 
                                   demog = "homeless_grp", exit_category)
exit_type_crisis_hh <- demog_pct_sum(covariate_exits_hh, full_demog = T, level = "hh", 
                                 demog = "crisis_any_prior", exit_category)

exit_type_homeless_ind <- demog_pct_sum(covariate_exits, full_demog = T, level = "ind", 
                                       demog = "homeless_grp", exit_category)
exit_type_crisis_ind <- demog_pct_sum(covariate_exits, full_demog = T, level = "ind", 
                                     demog = "crisis_any_prior", exit_category)



# REGRESSION MODEL FOR EXIT TYPE ----
## Not including Medicaid factors ----
model_data_exits <- covariate_exits_hh %>%
  filter(full_demog == T) %>%
  mutate(across(c("gender_me", "major_prog", "prog_type_use", "vouch_type_use"), ~ as_factor(.))) %>%
  # Relevel factors as they are made
  mutate(race_eth_me = fct_relevel(race_eth_me, c("White")),
         age_grp = fct_relevel(
           as_factor(case_when(age_at_exit < 25 ~ "<25",
                               data.table::between(age_at_exit, 25, 44.99, NAbounds = NA) ~ "25-44",
                        data.table::between(age_at_exit, 45, 61.99, NAbounds = NA) ~ "45-61",
                        age_at_exit >= 62 ~ "62+",
                        is.na(age_at_exit) ~ NA_character_)),
    "<25"),
    los = fct_relevel(
      as_factor(case_when(housing_time_at_exit < 3 ~ "<3",
                          between(housing_time_at_exit, 3, 5.999, NAbounds = NA) ~ "3-5.99",
                          between(housing_time_at_exit, 6, 9.999, NAbounds = NA) ~ "6-9.99",
                          housing_time_at_exit >= 10 ~ "10+")),
      "<3", "3-5.99"),
    prog_type_use = fct_relevel(prog_type_use, "TBV"),
    ed_any_prior = case_when(ed_cnt_prior >= 1 ~ 1L,
                             ed_cnt_prior == 0 ~ 0L),
    hosp_any_prior = case_when(hosp_cnt_prior >= 1 ~ 1L,
                               hosp_cnt_prior == 0 ~ 0L),
    exit_pos = case_when(exit_category == "Neutral" ~ 0L,
                         exit_category == "Positive" ~ 1L),
    exit_neg = case_when(exit_category == "Neutral" ~ 0L,
                         exit_category == "Negative" ~ 1L),
    exit_category = factor(exit_category, levels = c("Negative", "Neutral", "Positive"))
  )

model_data_exits$exit_category2 <- relevel(model_data_exits$exit_category, ref = "Neutral")

### Multinomial approach ----
exit_type <- multinom(exit_category2 ~ gender_me + race_eth_me + age_grp + los + 
                        prog_type_use + hh_size + single_caregiver + hh_disability + 
                        crisis_any_prior + recent_homeless_grp, 
                      data = model_data_exits)

summary(exit_type)

exit_type_results <- broom::tidy(exit_type, exponentiate = T, conf.int = T) %>%
  select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
  pivot_wider(names_from = "y.level", 
              names_glue = "{y.level}_{.value}",
              values_from = c("estimate", "conf.low", "conf.high", "p.value"),
              names_vary = "slowest") %>%
  mutate(Negative_ci = paste0(number(Negative_conf.low, accuracy = 0.01), 
                              "–", number(Negative_conf.high, accuracy = 0.01)),
         Positive_ci = paste0(number(Positive_conf.low, accuracy = 0.01), 
                              "–", number(Positive_conf.high, accuracy = 0.01))) %>%
  select(group = term, Negative_estimate, Negative_ci, Negative_p.value,
         Positive_estimate, Positive_ci, Positive_p.value)


### Model checking ----
# Using DHARMa not currently supported:
# https://github.com/florianhartig/DHARMa/issues/174


### Separated binomial approach ----
exit_type_neg <- glm(exit_neg ~ gender_me + race_eth_me + age_grp + los + 
                       prog_type_use + hh_size + single_caregiver + hh_disability, 
                     data = model_data_exits[!is.na(model_data_exits$exit_neg),], 
                     family = "binomial")

summary(exit_type_neg)
exit_type_neg_results <- cbind(OR = exp(coef(exit_type_neg)),
                                     exp(confint(exit_type_neg)),
                                     p = as.numeric(sprintf("%.4f", summary(exit_type_neg)[["coefficients"]][,4])))


exit_type_pos <- glm(exit_pos ~ gender_me + race_eth_me + age_grp + los + 
                       prog_type_use + hh_size + single_caregiver + hh_disability, 
                     data = model_data_exits[!is.na(model_data_exits$exit_pos),], 
                     family = "binomial")

summary(exit_type_pos)
exit_type_pos_results <- cbind(OR = exp(coef(exit_type_pos)),
                                     exp(confint(exit_type_pos)),
                                     p = as.numeric(sprintf("%.4f", summary(exit_type_pos)[["coefficients"]][,4])))


## Including Medicaid factors ----
model_data_exits_mcaid <- model_data_exits %>% 
  filter(full_cov_7_prior == T) %>%
  # Also drop groups with  very small counts
  filter(race_eth_me != "Unknown") %>%
  filter(age_grp != "62+") %>%
  # Need to relevel age_grp
  mutate(age_grp = fct_drop(age_grp))


### Multinomial approach ----
exit_type_mcaid <- multinom(exit_category2 ~ gender_me + race_eth_me + age_grp + los + 
                              prog_type_use + hh_size + single_caregiver + hh_disability + 
                              ed_any_prior + hosp_any_prior + ccw_flag + 
                              crisis_ed_any_prior + recent_homeless_grp, 
                            data = model_data_exits_mcaid)

summary(exit_type_mcaid)

exit_type_mcaid_results <- broom::tidy(exit_type_mcaid, exponentiate = T, conf.int = T) %>%
  select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
  pivot_wider(names_from = "y.level", 
              names_glue = "{y.level}_{.value}",
              values_from = c("estimate", "conf.low", "conf.high", "p.value"),
              names_vary = "slowest") %>%
  mutate(Negative_ci = paste0(number(Negative_conf.low, accuracy = 0.01), 
                              "–", number(Negative_conf.high, accuracy = 0.01)),
         Positive_ci = paste0(number(Positive_conf.low, accuracy = 0.01), 
                              "–", number(Positive_conf.high, accuracy = 0.01))) %>%
  select(group = term, Negative_estimate, Negative_ci, Negative_p.value,
         Positive_estimate, Positive_ci, Positive_p.value)


### Model checking ----
# Using DHARMa not currently supported:
# https://github.com/florianhartig/DHARMa/issues/174



### Separated binomial approach ----
exit_type_mcaid_neg <- glm(exit_neg ~ gender_me + race_eth_me + age_grp + los + 
                       prog_type_use + hh_size + single_caregiver + hh_disability, 
                     data = model_data_exits_mcaid[!is.na(model_data_exits_mcaid$exit_neg),], 
                     family = "binomial")

summary(exit_type_mcaid_neg)
exit_type_mcaid_neg_results <- cbind(OR = exp(coef(exit_type_mcaid_neg)),
                                     exp(confint(exit_type_mcaid_neg)),
                                     p = as.numeric(sprintf("%.4f", summary(exit_type_mcaid_neg)[["coefficients"]][,4])))


exit_type_mcaid_pos <- glm(exit_pos ~ gender_me + race_eth_me + age_grp + los + 
                       prog_type_use + hh_size + single_caregiver + hh_disability, 
                     data = model_data_exits_mcaid[!is.na(model_data_exits_mcaid$exit_pos),], 
                     family = "binomial")

summary(exit_type_mcaid_pos)
exit_type_mcaid_pos_results <- cbind(OR = exp(coef(exit_type_mcaid_pos)),
                                     exp(confint(exit_type_mcaid_pos)),
                                     p = as.numeric(sprintf("%.4f", summary(exit_type_mcaid_pos)[["coefficients"]][,4])))



# DEMOGS BY MEDICAID COVERAGE AMONG THOSE EXITING ----
### Demogs (prior) ----
# Age
exit_mcaid_7prior_age <- age_sum(covariate_exits, full_demog = T, full_cov_7_prior)
exit_mcaid_7prior_age_hh <- age_sum(covariate_exits_hh, full_demog = T, full_cov_7_prior)
# Gender
exit_mcaid_7prior_gender <- demog_pct_sum(covariate_exits, full_demog = T, 
                                          level = "ind", demog = "gender", full_cov_7_prior)
exit_mcaid_7prior_gender_hh <- demog_pct_sum(covariate_exits_hh, full_demog = T, 
                                          level = "hh", demog = "gender", full_cov_7_prior)
# Race/eth
exit_mcaid_7prior_race <- demog_pct_sum(covariate_exits, full_demog = T, 
                                        level = "ind", demog = "race", full_cov_7_prior)
exit_mcaid_7prior_race_hh <- demog_pct_sum(covariate_exits_hh, full_demog = T, 
                                        level = "hh", demog = "race", full_cov_7_prior)

# Combine for Rmarkdown
exit_mcaid_7prior_demogs <- bind_rows(exit_mcaid_7prior_age, exit_mcaid_7prior_gender, exit_mcaid_7prior_race) %>%
  rename("no_7mth_cov_prior" = "FALSE", "had_7mth_cov_prior" = "TRUE") %>%
  filter(!group == "n")
rm(exit_mcaid_7prior_age, exit_mcaid_7prior_gender, exit_mcaid_7prior_race)

exit_mcaid_7prior_demogs_hh <- bind_rows(exit_mcaid_7prior_age_hh, exit_mcaid_7prior_gender_hh, 
                                         exit_mcaid_7prior_race_hh) %>%
  rename("no_7mth_cov_prior" = "FALSE", "had_7mth_cov_prior" = "TRUE") %>%
  filter(!group == "n")
rm(exit_mcaid_7prior_age_hh, exit_mcaid_7prior_gender_hh, exit_mcaid_7prior_race_hh)


### Demogs (after) ----
# Age
exit_mcaid_7after_age <- age_sum(covariate_exits, full_demog = T, full_cov_7_after)
exit_mcaid_7after_age_hh <- age_sum(covariate_exits_hh, full_demog = T, full_cov_7_after)
# Gender
exit_mcaid_7after_gender <- demog_pct_sum(covariate_exits, full_demog = T, 
                                          level = "ind", demog = "gender", full_cov_7_after)
exit_mcaid_7after_gender_hh <- demog_pct_sum(covariate_exits_hh, full_demog = T, 
                                          level = "hh", demog = "gender", full_cov_7_after)
# Race/eth
exit_mcaid_7after_race <- demog_pct_sum(covariate_exits, full_demog = T, 
                                        level = "ind", demog = "race", full_cov_7_after)
exit_mcaid_7after_race_hh <- demog_pct_sum(covariate_exits_hh, full_demog = T, 
                                        level = "hh", demog = "race", full_cov_7_after)

# Combine for Rmarkdown
exit_mcaid_7after_demogs <- bind_rows(exit_mcaid_7after_age, exit_mcaid_7after_gender, exit_mcaid_7after_race) %>%
  rename("no_7mth_cov_after" = "FALSE", "had_7mth_cov_after" = "TRUE") %>%
  filter(!group == "n")
rm(exit_mcaid_7after_age, exit_mcaid_7after_gender, exit_mcaid_7after_race)

exit_mcaid_7after_demogs_hh <- bind_rows(exit_mcaid_7after_age_hh, exit_mcaid_7after_gender_hh, 
                                         exit_mcaid_7after_race_hh) %>%
  rename("no_7mth_cov_after" = "FALSE", "had_7mth_cov_after" = "TRUE") %>%
  filter(!group == "n")
rm(exit_mcaid_7after_age_hh, exit_mcaid_7after_gender_hh, exit_mcaid_7after_race_hh)


### HH demogs (prior) ----
# Time in housing (this is based on HH data)
exit_mcaid_7prior_hh_los <- demog_num_sum(covariate_exits_hh, full_demog = T, 
                                          demog = "los", full_cov_7_prior)
# Size and composition
exit_mcaid_7prior_hh_demogs <- hh_demogs_sum(covariate_exits_hh, full_demog = T, 
                                             level = "hh", full_cov_7_prior)
# Program type
exit_mcaid_7prior_hh_prog <- demog_pct_sum(covariate_exits_hh, full_demog = T, 
                                           level = "hh", demog = "program", full_cov_7_prior)
# Voucher type
exit_mcaid_7prior_hh_vouch <- demog_pct_sum(covariate_exits_hh, full_demog = T, 
                                            level = "hh", demog = "voucher", full_cov_7_prior)

# Combine for Rmarkdown
exit_mcaid_7prior_hh <- bind_rows(exit_mcaid_7prior_hh_los, exit_mcaid_7prior_hh_demogs, 
                               exit_mcaid_7prior_hh_prog, exit_mcaid_7prior_hh_vouch) %>%
  rename("no_7mth_cov_prior" = "FALSE", "had_7mth_cov_prior" = "TRUE") %>%
  filter(!group == "n")
rm(exit_mcaid_7prior_hh_los, exit_mcaid_7prior_hh_demogs, 
   exit_mcaid_7prior_hh_prog, exit_mcaid_7prior_hh_vouch)


### HH demogs (after) ----
# Time in housing (this is based on HH data)
exit_mcaid_7after_hh_los <- demog_num_sum(covariate_exits_hh, full_demog = T, 
                                          demog = "los", full_cov_7_after)
# Size and composition
exit_mcaid_7after_hh_demogs <- hh_demogs_sum(covariate_exits_hh, full_demog = T, 
                                             level = "hh", full_cov_7_after)
# Program type
exit_mcaid_7after_hh_prog <- demog_pct_sum(covariate_exits_hh, full_demog = T, 
                                           level = "hh", demog = "program", full_cov_7_after)
# Voucher type
exit_mcaid_7after_hh_vouch <- demog_pct_sum(covariate_exits_hh, full_demog = T, 
                                            level = "hh", demog = "voucher", full_cov_7_after)

# Combine for Rmarkdown
exit_mcaid_7after_hh <- bind_rows(exit_mcaid_7after_hh_los, exit_mcaid_7after_hh_demogs, 
                                  exit_mcaid_7after_hh_prog, exit_mcaid_7after_hh_vouch) %>%
  rename("no_7mth_cov_after" = "FALSE", "had_7mth_cov_after" = "TRUE") %>%
  filter(!group == "n")
rm(exit_mcaid_7after_hh_los, exit_mcaid_7after_hh_demogs, 
   exit_mcaid_7after_hh_prog, exit_mcaid_7after_hh_vouch)



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


# Function for consistently ordering tables
table_sorter <- function(tbl) {
  
  if ("order" %in% names(tbl)) {
    output <- tbl
  } else {
    output <- tbl %>% mutate(order = 1L)
  }
  
  if ("OR" %in% names(output)) {
    output <- output %>%
      mutate(group_order = case_when(OR == "ref" ~ 1L))
  } else {
    output <- output %>% mutate(group_order = NA_integer_)
  }
  
  output <- output %>%
    mutate(cat_order = case_when(category == "Age" ~ 1L,
                                 category == "Gender" ~ 2L,
                                 category == "Race/ethnicity" ~ 3L,
                                 category == "Time in housing" ~ 4L,
                                 category == "Household characteristics" ~ 5L,
                                 category == "Program type" ~ 6L,
                                 category == "Health and homelessness events" ~ 7L),
           group_order = case_when(group_order == 1L ~ 1L,
                                   group %in% c("recent_homeless_grp", "los<3", "<3", 
                                                "hh_size", "Experienced recent homelessness") ~ 1L,
                                   group %in% c("crisis_any_prior", "los3-5.99", 
                                                "3-5.99", "single_caregiver") ~ 2L,
                                   str_detect(group, "Experienced .*crisis.*exc") ~ 2L,
                                   group %in% c("crisis_ed_any_prior", "los6-9.99", 
                                                "6-9.99", "hh_disability") ~ 3L,
                                   str_detect(group, "Experienced .*crisis.*inc") ~ 3L,
                                   group %in% c("ed_cnt_prior", "los10+", "10+") ~ 4L,
                                   str_detect(group, "Average .*ED") ~ 4L,
                                   group == "ed_any_prior" ~ 5L,
                                   str_detect(group, "Experienced .*ED") ~ 5L,
                                   group %in% c("hosp_cnt_prior") ~ 6L,
                                   str_detect(group, "Average .*hospital") ~ 6L,
                                   str_detect(group, "Experienced .*hospital") ~ 7L,
                                   group %in% c("ccw_cnt", "ccw_flag") ~ 8L,
                                   str_detect(group, "chronic") ~ 8L)) %>%
    arrange(cat_order, order, group_order, group) %>%
    select(-ends_with("order"))
  
  output
}

# Function for tidying regression output
table_regression <- function(tbl, type = c("any_exit", "exit_type"), p_value = F) {
  output <- tbl %>%
    mutate(across(c(starts_with("OR"), ends_with("estimate")), 
                  ~ as.character(number(., accuracy = 0.01))),
           across(any_of(c(matches("^p$"), contains("value"))), ~ 
                    case_when(. < 0.001 ~ "<0.001",
                              . < 0.01 ~ "<0.01",
                              . < 0.05 ~ "<0.05",
                              TRUE ~ as.character(round(., 3)))),
           order = 2L)

  if (type == "any_exit") {
    output <- output %>%
      bind_rows(., data.frame(group = c("gender_meFemale", "race_eth_meWhite",
                                        "age_grp<25", "los<3", "prog_type_useTBV"),
                              OR = rep("ref", 5), 
                              ci = rep(NA_character_, 5), 
                              p = rep(NA_character_, 5),
                              order = rep(1L, 5)))
  } else if (type == "exit_type") {
    output <- output %>%
      bind_rows(., data.frame(group = c("gender_meFemale", "race_eth_meWhite",
                                        "age_grp<25", "los<3", "prog_type_useTBV"),
                              Negative_estimate = rep("ref", 5), 
                              Negative_ci = rep(NA_character_, 5), 
                              Negative_p.value = rep(NA_character_, 5),
                              Positive_estimate = rep("ref", 5), 
                              Positive_ci = rep(NA_character_, 5), 
                              Positive_p.value = rep(NA_character_, 5),
                              order = rep(1L, 5)))
  }
  output <- output %>%
    mutate(category = case_when(str_detect(group, "age_") ~ "Age",
                                str_detect(group, "gender_") ~ "Gender",
                                str_detect(group, "race_") ~ "Race/ethnicity",
                                str_detect(group, "^los") ~ "Time in housing",
                                group %in% c("hh_size", "single_caregiver", "hh_disability") ~ 
                                  "Household characteristics",
                                str_detect(group, "major_prog|prog_type_use") ~ "Program type",
                                group %in% c("crisis_any_prior", "crisis_ed_any_prior", 
                                             "recent_homeless_grp", 
                                             "ed_cnt_prior", "ed_any_prior",
                                             "hosp_cnt_prior", "hosp_any_prior",
                                             "ccw_flag") ~ 
                                  "Health and homelessness events",
                                group == "kc_opp_index_score" ~ "Neighborhood opportunity"),
           group = case_when(group == "hh_size" ~ "Household size",
                             group == "single_caregiver" ~ "Single caregiver",
                             group == "hh_disability" ~ "HoH disability",
                             group == "kc_opp_index_score" ~ "Neighborhood opportunity",
                             group == "recent_homeless_grp" ~ "Experienced recent homelessness",
                             group == "crisis_any_prior" ~ paste0("Experienced 1+ behavioral health crisis event in year prior to exit ",
                                                            "(excl. ED visits)"),
                             group == "crisis_ed_any_prior" ~ 
                               paste0("Experienced 1+ behavioral health crisis event in year prior to exit ",
                                      "(incl. ED visits)"),
                             group == "ed_any_prior" ~ "Experienced 1+ ED visit in year prior to exit",
                             group == "hosp_any_prior" ~ "Experienced 1+ hospitalization in year prior to exit",
                             group == "ccw_flag" ~ "2+ chronic conditions",
                             TRUE ~ str_remove(group, "age_grp|gender_me|los|major_prog|prog_type_use|race_eth_me"))) %>%
    filter(group != "(Intercept)")
  
  if (type == "any_exit") {
    output <- output %>%
      select(category, group, OR:p, order)
    if (p_value == F) {
      output <- output %>%
        mutate(OR = case_when(p =="<0.05" ~ paste0(OR, "*"),
                              p =="<0.01" ~ paste0(OR, "**"),
                              p =="<0.001" ~ paste0(OR, "***"),
                              TRUE ~ as.character(OR))) %>%
        select(-p)
    }
  } else if (type == "exit_type") {
    output <- output %>%
      select(category, group, Negative_estimate:Positive_p.value, order)
    if (p_value == F) {
      output <- output %>%
        mutate(Negative_estimate = case_when(Negative_p.value =="<0.05" ~ paste0(Negative_estimate, "*"),
                                             Negative_p.value =="<0.01" ~ paste0(Negative_estimate, "**"),
                                             Negative_p.value =="<0.001" ~ paste0(Negative_estimate, "***"),
                                             TRUE ~ as.character(Negative_estimate)),
               Positive_estimate = case_when(Positive_p.value =="<0.05" ~ paste0(Positive_estimate, "*"),
                                             Positive_p.value =="<0.01" ~ paste0(Positive_estimate, "**"),
                                             Positive_p.value =="<0.001" ~ paste0(Positive_estimate, "***"),
                                             TRUE ~ as.character(Positive_estimate))) %>%
        select(-Negative_p.value, -Positive_p.value)
    }
  }
  
  output <- table_sorter(output)
  
  output
}


## Table 1: demographics by exit and exit type ----
# Set up n for col names
n_remain_exit <- covariate_nodeath_hh %>% filter(full_demog == T) %>% count(id_type) %>% deframe()
n_exit_type <- covariate_exits_hh %>% filter(full_demog == T) %>% count(exit_category) %>% deframe()

n_mcaid_exit <- exit_any_mcaid_7_prior_hh %>% filter(group == "n") %>%
  select(-category, -group)
n_mcaid_type <- exit_type_mcaid_7_prior_hh %>% filter(group == "n") %>%
  select(-category, -group)


# Make table
table_1_demogs <- bind_rows(exit_any_ind_hh, exit_any_hh_hh, 
                            exit_any_homeless_hh, exit_any_crisis_hh, 
                            exit_any_mcaid_7_prior_hh) %>%
  left_join(., bind_rows(exit_type_ind_hh, exit_type_hh_hh,
                         exit_type_homeless_hh, exit_type_crisis_hh,
                         exit_type_mcaid_7_prior_hh) %>%
              select(category, group, Neutral, Positive, Negative),
            by = c("category", "group")) %>%
  filter(category != "Voucher type") %>%
  filter(!group %in% c("n", "Range (years)", "Child (aged <18)")) %>% 
  filter(str_detect(group, "Did not experience", negate = T)) %>%
  filter(str_detect(group, "well-child", negate = T)) %>%
  rename("Remained" = "id_control", "Exited" = "id_exit") %>%
  mutate(category = str_replace_all(category, "HoH time", "Time"),
         group = str_replace_all(group, " time in housing \\(years\\)", " time (years)"))

table_1_demogs <- table_sorter(table_1_demogs)

table_1_demogs <- table_1_demogs %>%
  gt(groupname_col = "category", rowname_col = "group") %>%
  tab_footnote(footnote = "AI/AN = American Indian/Alaskan Native, NH/PI = Native Hawaiian/Pacific Islander", 
               locations = cells_row_groups(groups = "Race/ethnicity")) %>%
  tab_footnote(footnote = "PBV = Project-based voucher, PH = Public housing, TBV = Tenant-based voucher", 
               locations = cells_row_groups(groups = "Program type")) %>%
  tab_footnote(footnote = md(glue("Health event data available for those aged <62 enrolled in Medicaid (",
                               "Remained N={n_mcaid_exit[[1]]}, ",
                               "Exited N={n_mcaid_exit[[2]]}, ",
                               "Negative N={n_mcaid_type[[1]]}, ",
                               "Neutral N={n_mcaid_type[[2]]}, ",
                               "Positive N={n_mcaid_type[[3]]}",
                               ")")),
               locations = cells_stub(rows = group %in%
                                        c("Experienced 1+ behavioral health crisis events in year prior to exit (inc. ED visits)",
                                          "Average # ED visits in year prior to exit",
                                          "Experienced 1+ ED visits in year prior to exit",
                                          "Average # hospitalizations in year prior to exit (per 100 people)",
                                          "Experienced 1+ hospitalizations in year prior to exit",
                                          "Average # of chronic conditions",
                                          "2+ chronic conditions"))) %>%
  cols_label(category = md("Category"),
             group = md("Group"),
             Remained = md(paste0("Remained (N=", number(n_remain_exit[1], big.mark = ","), ")")),
             Exited = md(paste0("Exited (N=", number(n_remain_exit[2], big.mark = ","), ")")),
             Negative = md(paste0("Negative exit (N=", number(n_exit_type[1], big.mark = ","), ")")),
             Neutral = md(paste0("Neutral exit (N=", number(n_exit_type[2], big.mark = ","), ")")),
             Positive = md(paste0("Positive exit (N=", number(n_exit_type[3], big.mark = ","), ")")))

table_1_demogs <- table_formatter(table_1_demogs)

# Save output
gtsave(table_1_demogs, filename = "demog_manuscript_table1.png",
       path = file.path(here::here(), "analyses/exit_factors"))


## Table 1a: demographics by exit and exit type (individual level) ----
# Set up n for col names
n_remain_exit <- covariate_nodeath %>% filter(full_demog == T) %>% count(id_type) %>% deframe()
n_exit_type <- covariate_exits %>% filter(full_demog == T) %>% count(exit_category) %>% deframe()

n_mcaid_exit <- exit_any_mcaid_7_prior_ind %>% filter(group == "n") %>%
  select(-category, -group)
n_mcaid_type <- exit_type_mcaid_7_prior_ind %>% filter(group == "n") %>%
  select(-category, -group)


# Make table
table_1a_demogs <- bind_rows(exit_any_ind_ind, exit_any_hh_ind, 
                            exit_any_homeless_ind, exit_any_crisis_ind, 
                            exit_any_mcaid_7_prior_ind) %>%
  left_join(., bind_rows(exit_type_ind_ind, exit_type_hh_ind,
                         exit_type_homeless_ind, exit_type_crisis_ind,
                         exit_type_mcaid_7_prior_ind) %>%
              select(category, group, Neutral, Positive, Negative),
            by = c("category", "group")) %>%
  filter(category != "Voucher type") %>%
  filter(!group %in% c("n", "Range (years)", "Child (aged <18)")) %>% 
  filter(str_detect(group, "Did not experience", negate = T)) %>%
  filter(str_detect(group, "well-child", negate = T)) %>%
  rename("Remained" = "id_control", "Exited" = "id_exit") %>%
  mutate(category = str_replace_all(category, "HoH time", "Time"),
         group = str_replace_all(group, " time in housing \\(years\\)", " time (years)"))

table_1a_demogs <- table_sorter(table_1a_demogs)

table_1a_demogs <- table_1a_demogs %>%
  gt(groupname_col = "category", rowname_col = "group") %>%
  tab_footnote(footnote = "AI/AN = American Indian/Alaskan Native, NH/PI = Native Hawaiian/Pacific Islander", 
               locations = cells_row_groups(groups = "Race/ethnicity")) %>%
  tab_footnote(footnote = "PBV = Project-based voucher, PH = Public housing, TBV = Tenant-based voucher", 
               locations = cells_row_groups(groups = "Program type")) %>%
  tab_footnote(footnote = md(glue("Health event data available for those aged <62 enrolled in Medicaid (",
                                  "Remained N={n_mcaid_exit[[1]]}, ",
                                  "Exited N={n_mcaid_exit[[2]]}, ",
                                  "Negative N={n_mcaid_type[[1]]}, ",
                                  "Neutral N={n_mcaid_type[[2]]}, ",
                                  "Positive N={n_mcaid_type[[3]]}",
                                  ")")),
               locations = cells_stub(rows = group %in%
                                        c("Experienced 1+ behavioral health crisis events in year prior to exit (inc. ED visits)",
                                          "Average # ED visits in year prior to exit",
                                          "Experienced 1+ ED visits in year prior to exit",
                                          "Average # hospitalizations in year prior to exit (per 100 people)",
                                          "Experienced 1+ hospitalizations in year prior to exit",
                                          "Average # of chronic conditions",
                                          "2+ chronic conditions"))) %>%
  cols_label(category = md("Category"),
             group = md("Group"),
             Remained = md(paste0("Remained (N=", number(n_remain_exit[1], big.mark = ","), ")")),
             Exited = md(paste0("Exited (N=", number(n_remain_exit[2], big.mark = ","), ")")),
             Negative = md(paste0("Negative exit (N=", number(n_exit_type[1], big.mark = ","), ")")),
             Neutral = md(paste0("Neutral exit (N=", number(n_exit_type[2], big.mark = ","), ")")),
             Positive = md(paste0("Positive exit (N=", number(n_exit_type[3], big.mark = ","), ")")))

table_1a_demogs <- table_formatter(table_1a_demogs)

# Save output
gtsave(table_1a_demogs, filename = "demog_manuscript_table1.png",
       path = file.path(here::here(), "analyses/exit_factors"))



## Table 2: demographics by exit and Medicaid coverage ----
# Set up n for col names
n_remain_exit_mcaid <- covariate_nodeath_hh %>% 
  filter(full_demog == T) %>% 
  count(id_type, full_cov_7_prior)

# Make table
table_2_mcaid_demogs <- exit_any_mcaid_prior %>%
  filter(category != "Voucher type") %>%
  filter(!group %in% c("n", "Range (years)", "Child (aged <18)")) %>% 
  mutate(category = str_replace_all(category, "HoH time", "Time"),
         group = str_replace_all(group, " time in housing \\(years\\)", " time (years)"))

table_2_mcaid_demogs <- table_sorter(table_2_mcaid_demogs)

table_2_mcaid_demogs <- table_2_mcaid_demogs %>%
  gt(groupname_col = "category", rowname_col = "group") %>%
  tab_footnote(footnote = "AI/AN = American Indian/Alaskan Native, NH/PI = Native Hawaiian/Pacific Islander", 
               locations = cells_row_groups(groups = "Race/ethnicity")) %>%
  tab_footnote(footnote = "PBV = Project-based voucher, PH = Public housing, TBV = Tenant-based voucher", 
               locations = cells_row_groups(groups = "Program type")) %>%
  sub_missing() %>%
  cols_label(category = md("Category"),
             group = md("Group"),
             id_control_FALSE = md(paste0("Remained, no Medicaid (N=", 
                                           number(n_remain_exit_mcaid$n[1], big.mark = ","), ")")),
             id_control_TRUE = md(paste0("Remained, Medicaid (N=", 
                                        number(n_remain_exit_mcaid$n[2], big.mark = ","), ")")),
             id_exit_FALSE = md(paste0("Exited, no Medicaid (N=", 
                                         number(n_remain_exit_mcaid$n[3], big.mark = ","), ")")),
             id_exit_TRUE = md(paste0("Exited, Medicaid (N=", 
                                      number(n_remain_exit_mcaid$n[4], big.mark = ","), ")")))

table_2_mcaid_demogs <- table_formatter(table_2_mcaid_demogs)

# Save output
gtsave(table_2_mcaid_demogs, filename = "demog_manuscript_table2.png",
       path = file.path(here::here(), "analyses/exit_factors"))


## Table 3: regression for exit vs. not -----
# Pull out numbers for footnote
model_mcaid_n <- model_data_mcaid %>% count(id_type) %>% deframe()

table_3_exit_regression <- bind_rows(select(anyexit_output, group, OR, ci, p), 
                                     select(anyexit_mcaid_output, group, OR, ci, p) %>%
                                       filter(group %in% c("ed_any_prior", "hosp_any_prior",
                                                           "ccw_flag", "crisis_ed_any_prior")))

# Run through clean up
table_3_exit_regression <- table_regression(table_3_exit_regression, type = "any_exit")

# Turn into gt table
table_3_exit_regression <- table_3_exit_regression %>%
  gt(groupname_col = "category", rowname_col = "group") %>%
  tab_footnote(footnote = "* = p<0.05, ** = p<0.01, *** = p<0.001",
               locations = cells_column_labels(columns = "OR")) %>%
  tab_footnote(footnote = "AI/AN = American Indian/Alaskan Native, NH/PI = Native Hawaiian/Pacific Islander", 
               locations = cells_row_groups(groups = "Race/ethnicity")) %>%
  tab_footnote(footnote = "PBV = Project-based voucher, PH = Public housing, TBV = Tenant-based voucher", 
               locations = cells_row_groups(groups = "Program type")) %>%
  tab_footnote(footnote = paste0("Health event data only available for those aged <62 enrolled in Medicaid ",
                                 "(N = ", number(model_mcaid_n[[1]], big.mark = ','), " for controls, ", 
                                 number(model_mcaid_n[[2]], big.mark = ','), " for exits)"),
               locations = cells_stub(rows = group %in%
                                        c("Experienced 1+ behavioral health crisis event in year prior to exit (incl. ED visits)",
                                          "Experienced 1+ ED visit in year prior to exit",
                                          "Experienced 1+ hospitalization in year prior to exit",
                                          "2+ chronic conditions"))) %>%
  sub_missing() %>%
  cols_label(category = md("Category"),
             group = md("Group"),
             OR = md("Odds ratio"),
             ci = md("95% CI"))

# Add standard formatting
table_3_exit_regression <- table_formatter(table_3_exit_regression)

# Save output
gtsave(table_3_exit_regression, filename = "demog_manuscript_table3.png",
       path = file.path(here::here(), "analyses/exit_factors"))


### Table 3a: just the Medicaid output ----
# Most of the Medicaid model ORs are stripped out above.
# This produces a table of just the Medicaid model results
# Run through clean up
table_3a_exit_regression <- table_regression(select(anyexit_mcaid_output, group, OR, ci, p), type = "any_exit")

# Turn into gt table
table_3a_exit_regression <- table_3a_exit_regression %>%
  gt(groupname_col = "category", rowname_col = "group") %>%
  tab_footnote(footnote = paste0("Health event data available for those aged <62 enrolled in Medicaid ",
                                 "(N = ", number(model_mcaid_n[[1]], big.mark = ','), " for controls, ", 
                                 number(model_mcaid_n[[2]], big.mark = ','), " for exits)")) %>%
  tab_footnote(footnote = "* = p<0.05, ** = p<0.01, *** = p<0.001",
               locations = cells_column_labels(columns = "OR")) %>%
  tab_footnote(footnote = "AI/AN = American Indian/Alaskan Native, NH/PI = Native Hawaiian/Pacific Islander", 
               locations = cells_row_groups(groups = "Race/ethnicity")) %>%
  tab_footnote(footnote = "PBV = Project-based voucher, PH = Public housing, TBV = Tenant-based voucher", 
               locations = cells_row_groups(groups = "Program type")) %>%
  sub_missing() %>%
  cols_label(category = md("Category"),
             group = md("Group"),
             OR = md("Odds ratio"),
             ci = md("95% CI"))

# Add standard formatting
table_3a_exit_regression <- table_formatter(table_3a_exit_regression)

# Save output
gtsave(table_3a_exit_regression, filename = "demog_manuscript_table3a.png",
       path = file.path(here::here(), "analyses/exit_factors"))




## Table 4: regression for exit type -----
# Set up n for col names
n_type_all_exits <- model_data_exits %>% count(exit_category2)
n_type_all_exits_mcaid <- model_data_exits_mcaid %>% count(exit_category2)

# Make table
table_4_exit_regression <- bind_rows(exit_type_results,
                                     filter(exit_type_mcaid_results, 
                                            group %in% c("ed_any_prior", "hosp_any_prior",
                                                           "ccw_flag", "crisis_ed_any_prior")))

# Run through clean up
table_4_exit_regression <- table_regression(table_4_exit_regression, type = "exit_type")

# Turn into gt table
table_4_exit_regression <- table_4_exit_regression %>%
  gt(groupname_col = "category", rowname_col = "group") %>%
  tab_footnote(footnote = "* = p<0.05, ** = p<0.01, *** = p<0.001",
               locations = cells_column_labels(columns = c("Negative_estimate", "Positive_estimate"))) %>%
  tab_footnote(footnote = "AI/AN = American Indian/Alaskan Native, NH/PI = Native Hawaiian/Pacific Islander", 
               locations = cells_row_groups(groups = "Race/ethnicity")) %>%
  tab_footnote(footnote = "HCV = Housing Choice Voucher, PH = Public housing", 
               locations = cells_row_groups(groups = "Program type")) %>%
  tab_footnote(footnote = paste0("Health event data only available for those aged <62 enrolled in Medicaid ",
                                 "(N = ", number(sum(n_type_all_exits_mcaid$n[1]), big.mark = ","), "/",
                                 number(sum(n_type_all_exits_mcaid$n[2]), big.mark = ","), "/",
                                 number(sum(n_type_all_exits_mcaid$n[3]), big.mark = ","), 
                                 " for neutral/negative/positive exits"),
               locations = cells_stub(rows = group %in%
                                        c("Experienced 1+ behavioral health crisis event in year prior to exit (incl. ED visits)",
                                          "Experienced 1+ ED visit in year prior to exit",
                                          "Experienced 1+ hospitalization in year prior to exit",
                                          "2+ chronic conditions"))) %>%
  sub_missing() %>%
  tab_spanner(label = md(paste0("Negative/positive exits vs. neutral exits <br>", 
                                "(neutral N=", number(n_type_all_exits$n[1], big.mark = ","), ")")),
              columns = everything(),
              level = 2) %>%
  tab_spanner(label = md(paste0("Negative exits (N=", 
                                number(n_type_all_exits$n[2], big.mark = ","), ")")),
              columns = starts_with("Negative"),
              level = 1) %>%
  tab_spanner(label = md(paste0("Positive exits (N=", 
                                number(n_type_all_exits$n[3], big.mark = ","), ")")),
              columns = starts_with("Positive"),
              level = 1) %>%
  cols_label(category = md("Category"),
             group = md("Group"),
             Negative_estimate = md("Odds ratio"),
             Negative_ci = md("95% CI"),
             Positive_estimate = md("Odds ratio"),
             Positive_ci = md("95% CI"))

# Add standard formatting
table_4_exit_regression <- table_formatter(table_4_exit_regression)

# Save output
gtsave(table_4_exit_regression, filename = "demog_manuscript_table4.png",
       path = file.path(here::here(), "analyses/exit_factors"))


### Table 4a: Just the Medicaid data ----
# Run through clean up
table_4a_exit_regression <- table_regression(exit_type_mcaid_results, type = "exit_type")

# Turn into gt table
table_4a_exit_regression <- table_4a_exit_regression %>%
  gt(groupname_col = "category", rowname_col = "group") %>%
  tab_footnote(footnote = "* = p<0.05, ** = p<0.01, *** = p<0.001",
               locations = cells_column_labels(columns = c("Negative_estimate", "Positive_estimate"))) %>%
  tab_footnote(footnote = "AI/AN = American Indian/Alaskan Native, NH/PI = Native Hawaiian/Pacific Islander", 
               locations = cells_row_groups(groups = "Race/ethnicity")) %>%
  tab_footnote(footnote = "HCV = Housing Choice Voucher, PH = Public housing", 
               locations = cells_row_groups(groups = "Program type")) %>%
  sub_missing() %>%
  tab_spanner(label = md(paste0("Negative/positive exits vs. neutral exits <br>", 
                                "(neutral N=", number(n_type_all_exits_mcaid$n[1], big.mark = ","), ")")),
              columns = everything(),
              level = 2) %>%
  tab_spanner(label = md(paste0("Negative exits (N=", 
                                number(n_type_all_exits_mcaid$n[2], big.mark = ","), ")")),
              columns = starts_with("Negative"),
              level = 1) %>%
  tab_spanner(label = md(paste0("Positive exits (N=", 
                                number(n_type_all_exits_mcaid$n[3], big.mark = ","), ")")),
              columns = starts_with("Positive"),
              level = 1) %>%
  cols_label(category = md("Category"),
             group = md("Group"),
             Negative_estimate = md("Odds ratio"),
             Negative_ci = md("95% CI"),
             Positive_estimate = md("Odds ratio"),
             Positive_ci = md("95% CI"))

# Add standard formatting
table_4a_exit_regression <- table_formatter(table_4a_exit_regression)

# Save output
gtsave(table_4a_exit_regression, filename = "demog_manuscript_table4a.png",
       path = file.path(here::here(), "analyses/exit_factors"))
