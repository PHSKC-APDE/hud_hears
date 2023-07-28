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




##summarize exit reasons by type----
reasons_by_exit_type <- all_pop %>% group_by(exit_category, exit_reason_clean) %>% summarize(n=n())
neutral_reasons <- reasons_by_exit_type %>% filter(exit_category %in% "Neutral")
positive_reasons <- reasons_by_exit_type %>% filter(exit_category %in% "Positive")
negative_reasons <- reasons_by_exit_type %>% filter(exit_category %in% "Negative")

#take top 10 for all, then by exit type
reasons_top10 <-reasons_by_exit_type %>% ungroup() %>% 
  mutate(Percent =(n()/sum(n())*100))%>%
  rename("Exit Reason"="exit_reason_clean") %>%
  rename("Exit Category"= "exit_category") %>%
  filter(rank(desc(n))<=10) %>% 
  arrange(rank(desc(n)))

neutral_top10 <-neutral_reasons %>% ungroup() %>% filter(rank(desc(n))<=10) %>% 
  arrange(rank(desc(n)))

positive_top10 <-positive_reasons %>% ungroup() %>% filter(rank(desc(n))<=10) %>%
  arrange(rank(desc(n)))

negative_top10 <-negative_reasons %>% ungroup() %>% filter(rank(desc(n))<=10) %>% 
  arrange(rank(desc(n)))


#export reasons and top 10 reasons tables (for all exits)

kable(reasons_top10, format="html") %>% kable_classic() %>% save_kable("C:/Users/n-mesuter/OneDrive - King County/Documents/GitHub/hud_hears/analyses/behavioral/top10_exitreasons.html")
kable(reasons_top10, format="html") %>% kable_classic() %>% save_kable("C:/Users/n-mesuter/OneDrive - King County/Documents/GitHub/hud_hears/analyses/behavioral/top10_exitreasons.png")

kable(reasons_by_exit_type, format="html") %>% kable_classic() %>% save_kable("C:/Users/n-mesuter/OneDrive - King County/Documents/GitHub/hud_hears/analyses/behavioral/reasons_by_exit_type.html")
kable(reasons_by_exit_type, format="html") %>% kable_classic() %>% save_kable("C:/Users/n-mesuter/OneDrive - King County/Documents/GitHub/hud_hears/analyses/behavioral/reasons_by_exit_type.png")


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

## Make table of outcomes by pop subset and exit category----
descriptive <- bind_rows(summarizer(all_pop, outcome = "all", exit_category),
                         summarizer(mcaid_subset7mo, outcome = "mcaid", exit_category))

#Note: table is formatted below



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

####Sensitivity analyses----

#Repeat this model, with all population but exclusion 62 and up
all_pop_U62 <-all_pop %>% filter(age_at_exit <62)

any_adj_U62 <- geepack::geeglm(crisis_any ~ exit_category + gender_me + age_at_exit + race_eth_me  + 
                             hh_size + single_caregiver + housing_time_at_exit + prog_type_use + 
                             hh_disability + crisis_any_before, 
                           data = all_pop_U62, 
                           id = id_hh,
                           family = "binomial")

summary(any_adj_U62)
broom::tidy(any_adj_U62, conf.int = TRUE, exponentiate = T) %>% as.data.frame()


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

#add adjustment variable of existing BH condition (this was not included in paper)
mcaid_adj_2 <- geepack::geeglm(crisis_any_mcaid ~ exit_category + gender_me + age_at_exit + race_eth_me  + 
                               hh_size + single_caregiver + housing_time_at_exit + prog_type_use + 
                               hh_disability +  crisis_any_mcaid_before + any_cond,
                             data = mcaid_subset7mo, 
                             id = id_hh,
                             family = "binomial")

summary(mcaid_adj_2)
broom::tidy(mcaid_adj_2, conf.int = TRUE, exponentiate = T) %>% as.data.frame()


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


# TABLE 2
#DESCRIPTIVE STATS for outcomes ----
# Turn into a gt table and make pretty


descriptive <- bind_rows(summarizer(all_pop, outcome = "all", exit_category),
                         summarizer(mcaid_subset7mo, outcome = "mcaid", exit_category))
#Change variable names
descriptive <-descriptive %>% mutate(category = case_when(category == "Crisis events (excl. Medicaid ED visits)" ~ "Crisis events",
                                                       category == "Crisis events (inc. Medicaid ED visits)" ~ "Crisis events (Medicaid subpopulation)"))

descriptive <- descriptive %>% select(category, group, Positive, Neutral, Negative) %>%
gt(groupname_col = "category", rowname_col = "group") %>%
  tab_footnote(footnote = "Includes behavioral health-related Emergency Department (ED) visits not captured in the full analysis", 
                  locations =  cells_row_groups(groups ="Crisis events (Medicaid subpopulation)"))

descriptive <- table_formatter(descriptive)

# Save output 
gtsave(descriptive, filename = "bh_manuscript_table2.png",
       path = file.path(here::here(), "analyses/behavioral"))
gtsave(descriptive, filename = "bh_manuscript_table2.html",
       path = file.path(here::here(), "analyses/behavioral"))





# TABLE 2: REGRESSION OUTPUT ----
# Do some basic setup
any_model <- broom::tidy(any_adj, conf.int = TRUE, exponentiate = T) %>% as.data.frame()
any_model <- table_regression(any_model, type = "all")

mcaid_model <- broom::tidy(mcaid_adj, conf.int = TRUE, exponentiate = T) %>% as.data.frame()
mcaid_model <- table_regression(mcaid_model, type = "mcaid")


table3 <- left_join(any_model, mcaid_model, by = "group") %>%
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


table3 <- bind_rows(table3, ref_rows) %>%
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
table3 <- table3 %>%
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
  tab_footnote(footnote = "HCV = Housing Choice Voucher, PH = Public housing, TBV=Tenant Based Voucher", 
               locations = cells_row_groups(groups = "Program type")) %>%
  sub_missing()


table_3 <- table_formatter(table3)

# Save output
gtsave(table_3, filename = "bh_manuscript_table3.png",
       path = file.path(here::here(), "analyses/behavioral"))
gtsave(table_3, filename = "bh_manuscript_table3.html",
       path = file.path(here::here(), "analyses/behavioral"))


##Table S3: Poisson Regression table
#Repeat the same thing, but adjust the inputs
# Do some basic setup
any_model <- broom::tidy(any_adj_pois, conf.int = TRUE, exponentiate = T) %>% as.data.frame()
any_model <- table_regression(any_model, type = "all")

mcaid_model <- broom::tidy(mcaid_adj_pois, conf.int = TRUE, exponentiate = T) %>% as.data.frame()
mcaid_model <- table_regression(mcaid_model, type = "mcaid")


tableS3 <- left_join(any_model, mcaid_model, by = "group") %>%
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


tableS3 <- bind_rows(tableS3, ref_rows) %>%
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
tableS3 <- tableS3 %>%
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
  tab_footnote(footnote = "HCV = Housing Choice Voucher, PH = Public housing, TBV=Tenant Based Voucher", 
               locations = cells_row_groups(groups = "Program type")) %>%
  sub_missing()


table_S3 <- table_formatter(tableS3)

# Save output
gtsave(table_S3, filename = "bh_manuscript_tableS3.png",
       path = file.path(here::here(), "analyses/behavioral"))
gtsave(table_S3, filename = "bh_manuscript_tableS3.html",
       path = file.path(here::here(), "analyses/behavioral"))


# Table s4----
#Repeating primary analysis with exclusion of 62 and up
# Do some basic setup
any_model_U62 <- broom::tidy(any_adj_U62, conf.int = TRUE, exponentiate = T) %>% as.data.frame()
any_model_U62 <- table_regression(any_model_U62, type = "all")

tableS4 <- any_model_U62 %>% mutate(order = 2L,
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
ref_rows_U62 <- data.frame(category = c("Exit category", "Gender", "Race/ethnicity", "Program type"),
                       group = c("exit_categoryNeutral", "gender_meFemale", 
                                 "race_eth_meWhite", "prog_type_useHCV"),
                       estimate_all = rep("ref", 4), 
                       order = rep(1L, 4))


tableS4 <- bind_rows(tableS4, ref_rows_U62) %>%
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
  mutate(group = case_when(group == "housing_time_at_exit" ~ "Years in housing",
                           group == "hh_size" ~ "Household size",
                           group == "single_caregiver" ~ "Single caregiver",
                           group == "hh_disability" ~ "HoH disability",
                           group == "age_at_exit" ~ "Age at exit (years)",
                           str_detect(group, "before") ~ "Prior crisis events",
                           TRUE ~ str_remove(group, "age_grp|gender_me|los|prog_type_use|race_eth_me|exit_category")))


# Turn into gt table
tableS4 <- tableS4 %>%
  gt(groupname_col = "category", rowname_col = "group") %>%
  tab_spanner(label = md("All Exits (Under 62 years"), columns = ends_with("_all")) %>%
  cols_label(category = md("Category"),
             group = md("Group"),
             estimate_all = md("Incidence rate ratio"),
             ci_all = md("95% CI"),
             p_all = md("p-value")) %>% 
  tab_footnote(footnote = "* = p<0.05, ** = p<0.01, *** = p<0.001",
               locations = cells_column_labels(columns = starts_with("estimate"))) %>%
  tab_footnote(footnote = "Multiple genders = both genders reported at different time points", 
               locations =  cells_row_groups(groups = "Gender")) %>%
  tab_footnote(footnote = "AI/AN = American Indian/Alaskan Native, NH/PI = Native Hawaiian/Pacific Islander", 
               locations = cells_row_groups(groups = "Race/ethnicity")) %>%
  tab_footnote(footnote = "HoH = Head of household", 
               locations = cells_stub(rows = str_detect(group, "HoH"))) %>%
  tab_footnote(footnote = "HCV = Housing Choice Voucher, PH = Public housing, TBV=Tenant Based Voucher", 
               locations = cells_row_groups(groups = "Program type")) %>%
  sub_missing()


table_S4 <- table_formatter(tableS4)

# Save output
gtsave(table_S4, filename = "bh_manuscript_tableS4.png",
       path = file.path(here::here(), "analyses/behavioral"))
gtsave(table_S4, filename = "bh_manuscript_tableS4.html",
       path = file.path(here::here(), "analyses/behavioral"))




## Added on 7/25/23 
##Table 1: demographics for all participants by exit and exit type ----

#Note: am using a different package for this table, vs. other tables
#### Prep vars ----
setDT(all_pop)
all_pop[, Senior := (fcase(age_at_exit >= 62, TRUE, 
                           default = FALSE))]
all_pop[, Child := (fcase(age_at_exit <18, TRUE, 
                          default = FALSE))]
# all_pop[gender_me == 'Multiple', gender_me := 'Another gender']
all_pop[, hh_disability := as.logical(hh_disability)]
all_pop[, single_caregiver := as.logical(single_caregiver)]
all_pop[race_eth_me == 'Unknown', race_eth_me := NA]
all_pop[, crisis_any_before := as.logical(crisis_any_before)]


#### Build Arsenal Table ----
# Configure Arsenal arguments ----
library(arsenal)
my_controls <- tableby.control(
  test = T,
  total = T,
  numeric.test = "kwt", cat.test = "chisq",
  numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
  # numeric.stats = c("meansd", "median", "Nmiss2"),
  digits = 0,
  cat.stats = c("countpct", "Nmiss2"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (IQR)",
    range = "Range",
    Nmiss2 = "Missing"
  )
)

# create table ----
table1 <- as.data.table(summary(
  arsenal::tableby(exit_category ~ 
                     age_at_exit +
                     Senior +
                     Child +
                     gender_me +
                     race_eth_me +
                     crisis_any_before +
                     housing_time_at_exit +
                     hh_size +
                     single_caregiver +
                     hh_disability +
                     prog_type_use, 
                   data = all_pop, # at exit!
                   control = my_controls)
))

# strip attributes ----
attr(table1, 'align') <- NULL
attr(table1, 'ylabel') <- NULL
attr(table1, 'control.list') <- NULL
table1 <- setDT(copy(as.data.frame(table1)))

# format big numbers ----
for(ii in 2:5){
  orig.colname <- names(table1)[ii]
  setnames(table1, orig.colname, 'tempy')
  orig.colname <- paste0(gsub("\\(N=\\d+\\)", "(N=", orig.colname), 
                         format(as.integer(gsub(".*\\((N=)(\\d+)\\).*", "\\2", orig.colname)), big.mark = ','), 
                         ")")
  table1[, tempy := unlist(gsub("\\(|\\)", "", tempy))] # drop parenthesis
  table1[, tempy := gsub(", ", ",_", tempy)] # temporarily swap out space 
  table1[, tempy := gsub(" - ", "_-_", tempy)] # temporarily swap out space i
  table1[, c('tempy1', 'tempy2') := tstrsplit(tempy, split = ' ', fixed=TRUE)]
  table1[, tempy1 := as.integer(tempy1)]
  table1[tempy1 > 999, tempy1.big := format(tempy1, big.mark = ',')]
  table1[, tempy1.big := fcase(tempy1 > 999, format(tempy1, big.mark = ','), 
                               tempy1 <= 999, as.character(tempy1))]
  table1[, tempy2 := suppressWarnings(as.integer(tempy2))]
  table1[, tempy2.big := fcase(tempy2 > 999, format(tempy2, big.mark = ','), 
                               tempy2 <= 999, as.character(tempy2))]
  table1[!is.na(tempy1.big) & is.na(tempy2.big) & grepl(" ", tempy), tempy2.big := gsub("^.* ", "", tempy)] # to keep 2nd half as is when only first part is a big number
  table1[, tempybig := gsub(" \\(NA\\)", "", paste0(tempy1.big, " (", tempy2.big, ")"))]
  table1[tempybig == 'NA', tempybig := NA]
  table1[is.na(tempybig) & !is.na(tempy), tempybig := tempy]
  table1[, tempy := tempybig]
  table1[, tempy := gsub(",_", ", ", tempy)]
  table1[, tempy := gsub("_-_", " - ", tempy)]
  setnames(table1, 'tempy', orig.colname)
  table1[, grep('tempy', names(table1)) := NULL]
}

# tidy table ----  
setnames(table1, names(table1)[1], 'col1')
table1[, col1 := gsub("race_eth_me", "Race/ethnicity", col1)]
table1[, col1 := gsub("gender_me", "Gender", col1)]
table1[, col1 := gsub("age_at_exit", "Age", col1)]
table1[, col1 := gsub("hh_disability", "Head of household disability", col1)]
table1[, col1 := gsub("hh_gt_1_worker", "Household ≥ 2 wage earners", col1)]
table1[, col1 := gsub("single_caregiver", "Single caregiver", col1)]
table1[, col1 := gsub("housing_time_at_exit", "Years of housing assistance", col1)]
table1[, col1 := gsub("prog_type_use", "Program type", col1)]
table1[, col1 := gsub("hh_size", "Household size", col1)]
table1[, col1 := gsub("Senior", "Senior (aged 62+)", col1)]
table1[, col1 := gsub("Child", "Child (aged < 18)", col1)]
table1[, col1 := gsub("crisis_any_before", "Prior crisis event)", col1)]


table1[, col1 := gsub("&nbsp;|\\*\\*", "", col1)]
table1[ !is.na(`p value`) & `p value` != "", variable := col1]
table1[, variable := variable[1], by= .(cumsum(!is.na(variable)) ) ] # fill downward

table1[variable %in% c('Age', 'Years of housing assistance'), col1 := gsub("\\(", "(years) (", col1)]
table1[variable %in% c('Age', 'Years of housing assistance'), col1 := gsub("Range", "Range (years)", col1)]

# more tidying ----
# when binary true/false, collapse it down to one row
tf.vars <- table1[col1 == 'TRUE']$variable # identify true / false variables
table1 <- table1[col1 != "FALSE"]
table1[col1 == "TRUE", col1 := variable]
for(tf in tf.vars){ # collapse the header and the TRUE row down to one row
  table1[col1 == tf, dup := 1:.N, col1]
  table1[dup == 2 & col1 == tf, "p value" := table1[dup==1 & col1 == tf]$`p value`]
  table1 <- table1[!(dup == 1 & col1 == tf)]
  table1[, dup := NULL]
}

#adds missing column, but that is not applicable here as the data have already been filtered to include only with non-missing covariates
table1 <- table1[!(col1 == "Missing" & get(names(table1)[2]) == 0 & get(names(table1)[3]) == 0 & get(names(table1)[4]) == 0)] # drop if missing always zero

table1[col1 != variable, col1 := paste0("   ", col1)] # add indent for categories within a variable

table1 <- table1[, 1:6]

setnames(table1, c("p value"), c("P-value"))

#Remove p-value column and rename col 1
table1 <- table1 %>% select (-c("P-value")) %>% rename("Demographic Variable"="col1")

#export as html and png
library(knitr)
library(kableExtra)
kable(table1, format="html") %>% kable_classic() %>% save_kable("C:/Users/n-mesuter/OneDrive - King County/Documents/GitHub/hud_hears/analyses/behavioral/table1.html")
kable(table1, format="html") %>% kable_classic() %>% save_kable("C:/Users/n-mesuter/OneDrive - King County/Documents/GitHub/hud_hears/analyses/behavioral/table1.png")

##Repeat this for Medicaid only----
#### Prep vars ----
setDT(mcaid_subset7mo)
mcaid_subset7mo[, Child := (fcase(age_at_exit <18, TRUE, 
                          default = FALSE))]
# mcaid_subset7mo[gender_me == 'Multiple', gender_me := 'Another gender']
mcaid_subset7mo[, hh_disability := as.logical(hh_disability)]
mcaid_subset7mo[, single_caregiver := as.logical(single_caregiver)]
mcaid_subset7mo[, crisis_any_mcaid_before := as.logical(crisis_any_mcaid_before)]
mcaid_subset7mo[race_eth_me == 'Unknown', race_eth_me := NA]

#### Build Arsenal Table ----
# Configure Arsenal arguments ----
library(arsenal)
my_controls <- tableby.control(
  test = T,
  total = T,
  numeric.test = "kwt", cat.test = "chisq",
  numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
  # numeric.stats = c("meansd", "median", "Nmiss2"),
  digits = 0,
  cat.stats = c("countpct", "Nmiss2"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (IQR)",
    range = "Range",
    Nmiss2 = "Missing"
  )
)

# create table ----
table1a <- as.data.table(summary(
  arsenal::tableby(exit_category ~ 
                     age_at_exit +
                     Child +
                     gender_me +
                     race_eth_me +
                     crisis_any_mcaid_before+
                     housing_time_at_exit +
                     hh_size +
                     single_caregiver +
                     hh_disability +
                     prog_type_use,
                   data = mcaid_subset7mo, # at exit!
                   control = my_controls)
))

# strip attributes ----
attr(table1a, 'align') <- NULL
attr(table1a, 'ylabel') <- NULL
attr(table1a, 'control.list') <- NULL
table1a <- setDT(copy(as.data.frame(table1a)))

# format big numbers ----
for(ii in 2:5){
  orig.colname <- names(table1a)[ii]
  setnames(table1a, orig.colname, 'tempy')
  orig.colname <- paste0(gsub("\\(N=\\d+\\)", "(N=", orig.colname), 
                         format(as.integer(gsub(".*\\((N=)(\\d+)\\).*", "\\2", orig.colname)), big.mark = ','), 
                         ")")
  table1a[, tempy := unlist(gsub("\\(|\\)", "", tempy))] # drop parenthesis
  table1a[, tempy := gsub(", ", ",_", tempy)] # temporarily swap out space 
  table1a[, tempy := gsub(" - ", "_-_", tempy)] # temporarily swap out space i
  table1a[, c('tempy1', 'tempy2') := tstrsplit(tempy, split = ' ', fixed=TRUE)]
  table1a[, tempy1 := as.integer(tempy1)]
  table1a[tempy1 > 999, tempy1.big := format(tempy1, big.mark = ',')]
  table1a[, tempy1.big := fcase(tempy1 > 999, format(tempy1, big.mark = ','), 
                               tempy1 <= 999, as.character(tempy1))]
  table1a[, tempy2 := suppressWarnings(as.integer(tempy2))]
  table1a[, tempy2.big := fcase(tempy2 > 999, format(tempy2, big.mark = ','), 
                               tempy2 <= 999, as.character(tempy2))]
  table1a[!is.na(tempy1.big) & is.na(tempy2.big) & grepl(" ", tempy), tempy2.big := gsub("^.* ", "", tempy)] # to keep 2nd half as is when only first part is a big number
  table1a[, tempybig := gsub(" \\(NA\\)", "", paste0(tempy1.big, " (", tempy2.big, ")"))]
  table1a[tempybig == 'NA', tempybig := NA]
  table1a[is.na(tempybig) & !is.na(tempy), tempybig := tempy]
  table1a[, tempy := tempybig]
  table1a[, tempy := gsub(",_", ", ", tempy)]
  table1a[, tempy := gsub("_-_", " - ", tempy)]
  setnames(table1a, 'tempy', orig.colname)
  table1a[, grep('tempy', names(table1a)) := NULL]
}

# tidy table ----  
setnames(table1a, names(table1a)[1], 'col1')
table1a[, col1 := gsub("race_eth_me", "Race/ethnicity", col1)]
table1a[, col1 := gsub("gender_me", "Gender", col1)]
table1a[, col1 := gsub("age_at_exit", "Age", col1)]
table1a[, col1 := gsub("hh_disability", "Head of household disability", col1)]
table1a[, col1 := gsub("hh_gt_1_worker", "Household ≥ 2 wage earners", col1)]
table1a[, col1 := gsub("single_caregiver", "Single caregiver", col1)]
table1a[, col1 := gsub("housing_time_at_exit", "Years of housing assistance", col1)]
table1a[, col1 := gsub("prog_type_use", "Program type", col1)]
table1a[, col1 := gsub("hh_size", "Household size", col1)]
table1a[, col1 := gsub("Child", "Child (aged < 18)", col1)]
table1a[, col1 := gsub("Child", "Child (aged < 18)", col1)]
table1a[, col1 := gsub("crisis_any_mcaid_before", "Prior crisis event", col1)]



table1a[, col1 := gsub("&nbsp;|\\*\\*", "", col1)]
table1a[ !is.na(`p value`) & `p value` != "", variable := col1]
table1a[, variable := variable[1], by= .(cumsum(!is.na(variable)) ) ] # fill downward

table1a[variable %in% c('Age', 'Years of housing assistance'), col1 := gsub("\\(", "(years) (", col1)]
table1a[variable %in% c('Age', 'Years of housing assistance'), col1 := gsub("Range", "Range (years)", col1)]

# more tidying ----
# when binary true/false, collapse it down to one row
tf.vars <- table1a[col1 == 'TRUE']$variable # identify true / false variables
table1a <- table1a[col1 != "FALSE"]
table1a[col1 == "TRUE", col1 := variable]
for(tf in tf.vars){ # collapse the header and the TRUE row down to one row
  table1a[col1 == tf, dup := 1:.N, col1]
  table1a[dup == 2 & col1 == tf, "p value" := table1a[dup==1 & col1 == tf]$`p value`]
  table1a <- table1a[!(dup == 1 & col1 == tf)]
  table1a[, dup := NULL]
}

#adds missing column, but that is not applicable here as the data have already been filtered to include only with non-missing covariates
table1a <- table1a[!(col1 == "Missing" & get(names(table1a)[2]) == 0 & get(names(table1a)[3]) == 0 & get(names(table1a)[4]) == 0)] # drop if missing always zero

table1a[col1 != variable, col1 := paste0("   ", col1)] # add indent for categories within a variable

table1a <- table1a[, 1:6]

setnames(table1a, c("p value"), c("P-value"))

#Remove p-value column and rename col 1
table1a <- table1a %>% select (-c("P-value")) %>% rename("Demographic Variable"="col1")

#export as html and png
# library(knitr)
# library(kableExtra)
kable(table1a, format="html") %>% kable_classic() %>% save_kable("C:/Users/n-mesuter/OneDrive - King County/Documents/GitHub/hud_hears/analyses/behavioral/table1a.html")
kable(table1a, format="html") %>% kable_classic() %>% save_kable("C:/Users/n-mesuter/OneDrive - King County/Documents/GitHub/hud_hears/analyses/behavioral/table1a.png")

