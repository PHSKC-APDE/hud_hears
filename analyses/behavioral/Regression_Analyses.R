## HUD HEARS Behavioral Health Exit Type Regression Code
#Code updated on July 18, 2023
#Author: Megan Suter



#Code Purpose: Run regression models for exit type and BH analysis
#NOTE: behavioral health conditions and Medicaid ED visits are based on Medicaid
# data. Those included in this sample have 7 months or greater of full Medicaid coverage
# before and after date of exit



#Run after file "Outcomes_code_cleaned.R"


# Data Prep ----
#Set  unknown race==missing
all_pop <- all_pop %>% 
  # Remove people with missing variables since they aren't in the model
  filter(include_demog == T) %>%
  # Change exit category to a factor variable
         mutate(across(c("gender_me", "prog_type_use", "exit_category"), ~ as_factor(.)),
         race_eth_me = fct_relevel(race_eth_me, c("White")))

# Set up numeric IDs for households
# Needed for geeglm to work properly
hh_ids <- all_pop %>% filter(!is.na(hh_id_kc_pha)) %>%
  distinct(hh_id_kc_pha) %>%
  arrange(hh_id_kc_pha) %>%
  mutate(id_hh = row_number())


all_pop <- all_pop %>%
  left_join(., hh_ids, by = "hh_id_kc_pha")


# MCAID coverage preliminary models that include subset with medicaid coverage 7/12 months before and after and age restriction
mcaid_subset7mo <- all_pop %>% filter(include_cov_age == T)

#Make subset with only under 62
# U62all_pop <- all_pop %>% filter(age_at_exit <62)


# Summary table of outcomes ----
## Functions to make and format tables
summarizer <- function(df,
                       outcome = c("all", "mcaid"),
                       ...) {
    # Set things up to select in pivot_ functions
  # There is probably a better way to do this but it works
  col_names <- df %>% dplyr::select(...) %>% colnames()
  
  outcome <- match.arg(outcome)
  
  if (outcome == "all") {
    cat_text <- "Crisis events (excl. Medicaid ED visits)"
    output <- df %>% 
      distinct(id_hudhears, exit_date, ..., crisis_num, crisis_any) %>%
      mutate(cnt = crisis_num, any = crisis_any)
  } else if (outcome == "mcaid") {
    cat_text <- "Crisis events (inc. Medicaid ED visits)"
    output <- df %>% 
      distinct(id_hudhears, exit_date, ..., crisis_num_mcaid, crisis_any_mcaid) %>%
      mutate(cnt = crisis_num_mcaid, any = crisis_any_mcaid)
  }
  
  output <- output %>%
    group_by(...) %>%
    summarise(n = number(n(), big.mark = ","), 
              any = scales::percent(mean(any, na.rm = T), accuracy = 0.1),
              visit_mean = round(mean(cnt * 100, na.rm = T), 1), 
              vist_med = median(cnt, na.rm = T),
              visit_range = paste0(min(cnt, na.rm = T), "-", max(cnt, na.rm = T))) %>%
    pivot_longer(cols = !col_names, values_transform = list(value = as.character)) %>%
    pivot_wider(id_cols = "name", names_from = col_names) %>%
    mutate(category = cat_text, .before = "name",
           name = case_when(name == "any" ~ "Proportion with 1+ crisis event",
                             name == "visit_mean" ~ "Mean number crisis events (per 100)",
                             name == "vist_med" ~ "Median number events",
                             name == "visit_range" ~ "Range of crisis event numbers",
                             TRUE ~ name)) %>%
    rename("group" = "name")
  
  output
}

## Make table ----
descriptive <- bind_rows(summarizer(all_pop, outcome = "all", exit_category),
                         summarizer(mcaid_subset7mo, outcome = "mcaid", exit_category))

#Note: continued below



# REGRESSION MODEL: EXCLUDING MCAID ----
## Crude ----
any_crude <- geepack::geeglm(crisis_any ~ exit_category, 
                            data = all_pop,
                            id = id_hh,
                            family = "binomial")

summary(any_crude)
broom::tidy(any_crude, conf.int = TRUE, exponentiate = T)

## Adjusted ----
# Multiple categories model (use this)
any_adj <- geepack::geeglm(crisis_any ~ exit_category + gender_me + age_at_exit + race_eth_me  + 
                                  hh_size + single_caregiver + housing_time_at_exit + prog_type_use + 
                                  hh_disability + crisis_any_before, 
                               data = all_pop, 
                               id = id_hh,
                               family = "binomial")

summary(any_adj)
broom::tidy(any_adj, conf.int = TRUE, exponentiate = T) %>% as.data.frame()


# Poisson model (not being used but just for interest's sake)
any_adj_pois <- geepack::geeglm(crisis_num ~ exit_category + gender_me + age_at_exit + race_eth_me  + 
                                  hh_size + single_caregiver + housing_time_at_exit + prog_type_use + 
                                  hh_disability + crisis_any_before, 
                               data = all_pop, 
                               id = id_hh,
                               family = "poisson")

summary(any_adj_pois)
broom::tidy(any_adj_pois, conf.int = TRUE, exponentiate = T) %>% as.data.frame()


# ## Stratified by prior crisis (not using these at this time) ----
# # No prior BH crises
# any_stat_no_prior <- geepack::geeglm(crisis_any ~ exit_category + gender_me + age_at_exit + race_eth_me  + 
#                                        hh_size + single_caregiver + housing_time_at_exit + prog_type_use + 
#                                        hh_disability,
#                                      data = all_pop[all_pop$crisis_any_before == 0, ], 
#                                      id = id_hh,
#                                      family = "binomial")
# 
# summary(any_stat_no_prior)
# broom::tidy(any_stat_no_prior, conf.int = TRUE, exponentiate = T) %>% as.data.frame()
# 
# # Prior BH crises
# any_stat_prior <- geepack::geeglm(crisis_any ~ exit_category + gender_me + age_at_exit + race_eth_me  + 
#                                        hh_size + single_caregiver + housing_time_at_exit + prog_type_use + 
#                                        hh_disability,
#                                      data = all_pop[all_pop$crisis_any_before == 1, ], 
#                                      id = id_hh,
#                                      family = "binomial")
# 
# summary(any_stat_prior)
# broom::tidy(any_stat_prior, conf.int = TRUE, exponentiate = T) %>% as.data.frame()
# 


# REGRESSION MODEL: INCLUDING MCAID ----
## Crude ----
mcaid_crude <- geepack::geeglm(crisis_any_mcaid ~ exit_category, 
                             data = mcaid_subset7mo,
                             id = id_hh,
                             family = "binomial")

summary(mcaid_crude)
broom::tidy(mcaid_crude, conf.int = TRUE, exponentiate = T)

## Adjusted ----
# Multiple categories model (use this)
mcaid_adj <- geepack::geeglm(crisis_any_mcaid ~ exit_category + gender_me + age_at_exit + race_eth_me  + 
                             hh_size + single_caregiver + housing_time_at_exit + prog_type_use + 
                             hh_disability +  crisis_any_mcaid_before, 
                           data = mcaid_subset7mo, 
                           id = id_hh,
                           family = "binomial")

summary(mcaid_adj)
broom::tidy(mcaid_adj, conf.int = TRUE, exponentiate = T) %>% as.data.frame()


# Poisson model (not being used but just for interest's sake)
mcaid_adj_pois <- geepack::geeglm(crisis_num_mcaid ~ exit_category + gender_me + age_at_exit + race_eth_me  + 
                                  hh_size + single_caregiver + housing_time_at_exit + prog_type_use + 
                                  hh_disability + crisis_any_mcaid_before, 
                                data = mcaid_subset7mo, 
                                id = id_hh,
                                family = "poisson")

summary(mcaid_adj_pois)
broom::tidy(mcaid_adj_pois, conf.int = TRUE, exponentiate = T) %>% as.data.frame()



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


table_regression <- function(tbl, type = c("all", "mcaid")) {
  # Do some basic setup
  output <- tbl %>%
    rename(group = term) %>%
    mutate(estimate = as.character(number(estimate, accuracy = 0.01)),
           p.value = case_when(p.value < 0.001 ~ "<0.001",
                               p.value < 0.01 ~ "0.01",
                               p.value < 0.05 ~ "<0.05",
                               TRUE ~ as.character(round(p.value, 3))),
           ci = paste0(number(conf.low, accuracy = 0.01), "–", 
                       number(conf.high, accuracy = 0.01))) %>%
    select(group, estimate, ci, p.value)
  
  if (type == "all") {
    output <- output %>%
      rename(estimate_all = estimate,
             ci_all = ci,
             p_all = p.value)
  } else if (type == "mcaid") {
    output <- output %>%
      mutate(group = ifelse(group == "crisis_any_mcaid_before", 
                            "crisis_any_before",
                            group)) %>%
      rename(estimate_mcaid = estimate,
             ci_mcaid = ci,
             p_mcaid = p.value)
  }
  
  output
}


# TABLE 1: DESCRIPTIVE STATS (CURRENTLY ONLY OUTCOMES) ----
# Turn into a gt table and make pretty


descriptive <- bind_rows(summarizer(all_pop, outcome = "all", exit_category),
                         summarizer(mcaid_subset7mo, outcome = "mcaid", exit_category))



descriptive <- descriptive %>% select(category, group, Positive, Neutral, Negative) %>%
gt(groupname_col = "category", rowname_col = "group") %>%
  tab_spanner(label = md("Crisis Events"), columns = ends_with("excl. Medicaid ED visits)")) %>%
  tab_spanner(label = md("Crisis Events (Medicaid Subpopulation)"), columns = ends_with("inc. Medicaid ED visits)")) %>%
  tab_footnote(footnote = "Includes behavioral health-related Emergency Department (ED) visits not captured in the full analysis", 
                  locations =  cells_row_groups(groups ="Crisis events (Medicaid subpopulation)"))

descriptive <- table_formatter(descriptive)

# Save output
gtsave(descriptive, filename = "bh_manuscript_table1.png",
       path = file.path(here::here(), "analyses/behavioral"))



# TABLE 2: REGRESSION OUTPUT ----
# Do some basic setup
any_model <- broom::tidy(any_adj, conf.int = TRUE, exponentiate = T) %>% as.data.frame()
any_model <- table_regression(any_model, type = "all")

mcaid_model <- broom::tidy(mcaid_adj, conf.int = TRUE, exponentiate = T) %>% as.data.frame()
mcaid_model <- table_regression(mcaid_model, type = "mcaid")


table2 <- left_join(any_model, mcaid_model, by = "group") %>%
  mutate(order = 2L,
         category = case_when(str_detect(group, "exit_category") ~ "Exit category",
                              str_detect(group, "age_") ~ "Age",
                              str_detect(group, "gender_") ~ "Gender",
                              str_detect(group, "race_") ~ "Race/ethnicity",
                              str_detect(group, "^los|housing_time") ~ "Time in housing",
                              group %in% c("hh_size", "single_caregiver", "hh_disability")
                                           ~ "Household characteristics",
                              str_detect(group, "prog_type_use") ~ "Program type",
                              str_detect(group, "before") ~ "Existing behavioral health"))

# Make and bind the reference rows
ref_rows <- data.frame(category = c("Exit category", "Gender", "Race/ethnicity", "Program type"),
                       group = c("exit_categoryNeutral", "gender_meFemale", 
                                 "race_eth_meWhite", "prog_type_useHCV"),
                       estimate_all = rep("ref", 4), 
                       estimate_mcaid = rep("ref", 4),
                       order = rep(1L, 4))


table2 <- bind_rows(table2, ref_rows) %>%
  mutate(cat_order = case_when(category == "Exit category" ~ 1L,
                               category == "Age" ~ 2L,
                               category == "Gender" ~ 3L,
                               category == "Race/ethnicity" ~ 4L,
                               category == "Time in housing" ~ 5L,
                               category == "Household characteristics" ~ 6L,
                               category == "Program type" ~ 7L,
                               category == "Existing behavioral health" ~ 8L),
         group_order = case_when(group %in% c("hh_size") ~ 1L,
                                 group %in% c("single_caregiver") ~ 2L,
                                 group %in% c("hh_disability") ~ 3L)) %>%
  arrange(cat_order, order, group_order, group) %>%
  filter(group != "(Intercept)") %>%
  select(-ends_with("order")) %>%
  mutate(estimate_all = case_when(p_all =="<0.05" ~ paste0(estimate_all, "*"),
                                 p_all =="<0.01" ~ paste0(estimate_all, "**"),
                                 p_all =="<0.001" ~ paste0(estimate_all, "***"),
                                 TRUE ~ as.character(estimate_all))) %>%
  mutate(estimate_mcaid = case_when(p_mcaid =="<0.05" ~ paste0(estimate_mcaid, "*"),
                                  p_mcaid =="<0.01" ~ paste0(estimate_mcaid, "**"),
                                  p_mcaid =="<0.001" ~ paste0(estimate_mcaid, "***"),
                                  TRUE ~ as.character(estimate_mcaid))) %>%
  mutate(group = case_when(group == "housing_time_at_exit" ~ "Years in housing",
                           group == "hh_size" ~ "Household size",
                           group == "single_caregiver" ~ "Single caregiver",
                           group == "hh_disability" ~ "HoH disability",
                           group == "age_at_exit" ~ "Age at exit (years)",
                           str_detect(group, "before") ~ "Prior crisis events",
                           TRUE ~ str_remove(group, "age_grp|gender_me|los|prog_type_use|race_eth_me|exit_category")))

# Turn into gt table
table2 <- table2 %>%
  gt(groupname_col = "category", rowname_col = "group") %>%
  tab_spanner(label = md("All exits"), columns = ends_with("_all")) %>%
  tab_spanner(label = md("Medicaid subset"), columns = ends_with("_mcaid")) %>%
  cols_label(category = md("Category"),
             group = md("Group"),
             estimate_all = md("Odds ratio"),
             ci_all = md("95% CI"),
             p_all = md("p-value"),
             estimate_mcaid = md("Odds ratio"),
             ci_mcaid = md("95% CI"),
             p_mcaid = md("p-value")) %>%
  tab_footnote(footnote = "* = p<0.05, ** = p<0.01, *** = p<0.001",
               locations = cells_column_labels(columns = starts_with("estimate"))) %>%
  tab_footnote(footnote = "Multiple genders = both genders reported at different time points", 
               locations =  cells_row_groups(groups = "Gender")) %>%
  tab_footnote(footnote = "AI/AN = American Indian/Alaskan Native, NH/PI = Native Hawaiian/Pacific Islander", 
               locations = cells_row_groups(groups = "Race/ethnicity")) %>%
  tab_footnote(footnote = "HoH = Head of household", 
               locations = cells_stub(rows = str_detect(group, "HoH"))) %>%
  tab_footnote(footnote = "HCV = Housing Choice Voucher, PH = Public housing", 
               locations = cells_row_groups(groups = "Program type")) %>%
  sub_missing()


table_2 <- table_formatter(table2)

# Save output
gtsave(table_2, filename = "bh_manuscript_table2.png",
       path = file.path(here::here(), "analyses/behavioral"))


##Table S2: Poisson Regression table
#Repeat the same thing, but adjust the inputs
# Do some basic setup
any_model <- broom::tidy(any_adj_pois, conf.int = TRUE, exponentiate = T) %>% as.data.frame()
any_model <- table_regression(any_model, type = "all")

mcaid_model <- broom::tidy(mcaid_adj_pois, conf.int = TRUE, exponentiate = T) %>% as.data.frame()
mcaid_model <- table_regression(mcaid_model, type = "mcaid")


tableS2 <- left_join(any_model, mcaid_model, by = "group") %>%
  mutate(order = 2L,
         category = case_when(str_detect(group, "exit_category") ~ "Exit category",
                              str_detect(group, "age_") ~ "Age",
                              str_detect(group, "gender_") ~ "Gender",
                              str_detect(group, "race_") ~ "Race/ethnicity",
                              str_detect(group, "^los|housing_time") ~ "Time in housing",
                              group %in% c("hh_size", "single_caregiver", "hh_disability")
                              ~ "Household characteristics",
                              str_detect(group, "prog_type_use") ~ "Program type",
                              str_detect(group, "before") ~ "Existing behavioral health"))

# Make and bind the reference rows
ref_rows <- data.frame(category = c("Exit category", "Gender", "Race/ethnicity", "Program type"),
                       group = c("exit_categoryNeutral", "gender_meFemale", 
                                 "race_eth_meWhite", "prog_type_useHCV"),
                       estimate_all = rep("ref", 4), 
                       estimate_mcaid = rep("ref", 4),
                       order = rep(1L, 4))


tableS2 <- bind_rows(tableS2, ref_rows) %>%
  mutate(cat_order = case_when(category == "Exit category" ~ 1L,
                               category == "Age" ~ 2L,
                               category == "Gender" ~ 3L,
                               category == "Race/ethnicity" ~ 4L,
                               category == "Time in housing" ~ 5L,
                               category == "Household characteristics" ~ 6L,
                               category == "Program type" ~ 7L,
                               category == "Existing behavioral health" ~ 8L),
         group_order = case_when(group %in% c("hh_size") ~ 1L,
                                 group %in% c("single_caregiver") ~ 2L,
                                 group %in% c("hh_disability") ~ 3L)) %>%
  arrange(cat_order, order, group_order, group) %>%
  filter(group != "(Intercept)") %>%
  select(-ends_with("order")) %>%
  mutate(estimate_all = case_when(p_all =="<0.05" ~ paste0(estimate_all, "*"),
                                  p_all =="<0.01" ~ paste0(estimate_all, "**"),
                                  p_all =="<0.001" ~ paste0(estimate_all, "***"),
                                  TRUE ~ as.character(estimate_all))) %>%
  mutate(estimate_mcaid = case_when(p_mcaid =="<0.05" ~ paste0(estimate_mcaid, "*"),
                                    p_mcaid =="<0.01" ~ paste0(estimate_mcaid, "**"),
                                    p_mcaid =="<0.001" ~ paste0(estimate_mcaid, "***"),
                                    TRUE ~ as.character(estimate_mcaid))) %>%
  mutate(group = case_when(group == "housing_time_at_exit" ~ "Years in housing",
                           group == "hh_size" ~ "Household size",
                           group == "single_caregiver" ~ "Single caregiver",
                           group == "hh_disability" ~ "HoH disability",
                           group == "age_at_exit" ~ "Age at exit (years)",
                           str_detect(group, "before") ~ "Prior crisis events",
                           TRUE ~ str_remove(group, "age_grp|gender_me|los|prog_type_use|race_eth_me|exit_category")))

# Turn into gt table
tableS2 <- tableS2 %>%
  gt(groupname_col = "category", rowname_col = "group") %>%
  tab_spanner(label = md("All exits"), columns = ends_with("_all")) %>%
  tab_spanner(label = md("Medicaid subset"), columns = ends_with("_mcaid")) %>%
  cols_label(category = md("Category"),
             group = md("Group"),
             estimate_all = md("Incidence rate ratio"),
             ci_all = md("95% CI"),
             p_all = md("p-value"),
             estimate_mcaid = md("Incidence rate ratio"),
             ci_mcaid = md("95% CI"),
             p_mcaid = md("p-value")) %>%
  tab_footnote(footnote = "* = p<0.05, ** = p<0.01, *** = p<0.001",
               locations = cells_column_labels(columns = starts_with("estimate"))) %>%
  tab_footnote(footnote = "Multiple genders = both genders reported at different time points", 
               locations =  cells_row_groups(groups = "Gender")) %>%
  tab_footnote(footnote = "AI/AN = American Indian/Alaskan Native, NH/PI = Native Hawaiian/Pacific Islander", 
               locations = cells_row_groups(groups = "Race/ethnicity")) %>%
  tab_footnote(footnote = "HoH = Head of household", 
               locations = cells_stub(rows = str_detect(group, "HoH"))) %>%
  tab_footnote(footnote = "HCV = Housing Choice Voucher, PH = Public housing", 
               locations = cells_row_groups(groups = "Program type")) %>%
  sub_missing()


table_S2 <- table_formatter(tableS2)

# Save output
gtsave(table_S2, filename = "bh_manuscript_tableS2.png",
       path = file.path(here::here(), "analyses/behavioral"))


## Added on 7/14/23 
##Table 1: demographics for all participants by exit and exit type ----

## Demogs for all ----
# Age
exit_any_age <- age_sum(all_pop, full_demog = T, sd = T, id_type)

# Gender
exit_any_gender <- demog_pct_sum(all_pop, full_demog = T, suppress = T, level = "ind", 
                                 demog = "gender", id_type)

# Race/eth
exit_any_race <- demog_pct_sum(all_pop, full_demog = T, suppress = T, level = "ind", 
                               demog = "race", id_type)

# Time in housing (this is based on HH data)
exit_any_hh_los <- demog_num_sum(all_pop, full_demog = T, level = "hh", demog = "los", sd = T, id_type)

# HH size and composition
exit_any_hh_demogs <- hh_demogs_sum(all_pop, full_demog = T, level = "hh", sd = T, id_type)

# Program type
exit_any_hh_prog <- demog_pct_sum(all_pop, full_demog = T, suppress = T, level = "hh", demog = "program_type", id_type)


# Combine for R markdown
exit_any_demogs <- bind_rows(exit_any_age, exit_any_gender, exit_any_race, exit_any_hh_los,
                             exit_any_hh_demogs)

rm(exit_any_age, exit_any_gender, exit_any_race, exit_any_hh_los,
   exit_any_hh_demogs, exit_any_hh_prog)


## Demogs by exit ----
# Age
exit_type_age <- age_sum(all_pop, full_demog = T, sd = T, exit_category)

# Gender
exit_type_gender <- demog_pct_sum(all_pop, full_demog = T, suppress = T, level = "ind", 
                                  demog = "gender", exit_category)

# Race/eth
exit_type_race <- demog_pct_sum(all_pop, full_demog = T, suppress = T, level = "ind", 
                                demog = "race", exit_category)

# Time in housing (this is based on HH data)
exit_type_hh_los <- demog_num_sum(all_pop, full_demog = T, level = "hh", demog = "los", sd = T, exit_category)

# HH size and composition
exit_type_hh_demogs <- hh_demogs_sum(all_pop, full_demog = T, level = "hh", sd = T, exit_category)

# Program type
exit_type_hh_prog <- demog_pct_sum(all_pop, full_demog = T, suppress = T, level = "hh", demog = "program_type", exit_category)




# Combine for R markdown
exit_type_demogs <- bind_rows(exit_type_age, exit_type_gender, exit_type_race, exit_type_hh_los,
                              exit_type_hh_demogs, exit_type_hh_prog)

rm(exit_type_age, exit_type_gender, exit_type_race, exit_type_hh_los,
   exit_type_hh_demogs, exit_type_hh_prog)


# Make descriptive table
# Make table
table_1_demogs <- exit_type_demogs %>%
  # Remove unwanted groups
  filter(!group %in% c("n", "Range (years)", "Child (aged <18)", "Senior (aged 62+)")) %>% 
  # Do some renaming
  mutate(category = str_replace_all(category, "HoH time", "Time"),
         group = str_replace_all(group, " time in housing \\(years\\)", " time (years)")) %>%
  distinct() %>%
  # Set up table
  gt(groupname_col = "category", rowname_col = "group") %>%
  tab_footnote(footnote = "AI/AN = American Indian/Alaskan Native, NH/PI = Native Hawaiian/Pacific Islander", 
               locations = cells_row_groups(groups = "Race/ethnicity")) %>%
  tab_footnote(footnote = md(glue("At household level (",
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
             Negative = md(paste0("Negative exit (N=", number(n_exit_type[1], big.mark = ","), ")")),
             Neutral = md(paste0("Neutral exit (N=", number(n_exit_type[2], big.mark = ","), ")")),
             Positive = md(paste0("Positive exit (N=", number(n_exit_type[3], big.mark = ","), ")")))

table_1_demogs <- table_formatter(table_1_demogs)

# Save output
gtsave(table_1_demogs, filename = "health_manuscript_table1.png",
       path = file.path(here::here(), "analyses/health"))
gtsave(table_1_demogs, filename = "health_manuscript_table1.html",
       path = file.path(here::here(), "analyses/health"))

