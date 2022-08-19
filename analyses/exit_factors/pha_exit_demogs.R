## Script name: pha_exit_demogs
##
## Purpose of script: Demographics of people exiting housing
##
## Author: Alastair Matheson, Public Health - Seattle & King County
## Date Created: 2021-08-19
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
               knitr, kableExtra, rmarkdown, DiagrammeR)

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
# Combined exit/timevar data
exit_timevar <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.stage_pha_exit_timevar
                           WHERE chooser = chooser_max") %>%
  mutate(portfolio_final = ifelse(str_detect(portfolio_final, "LAKE CITY"),
                                  "LAKE CITY COURT", portfolio_final))

# Demographic table
pha_demo <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.final_demo")

# Covariate table
covariate <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.control_match_covariate")
covariate_hh <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.control_match_covariate_hh")


# Calyear table
pha_calyear <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.final_calyear") %>%
  mutate(portfolio_final = ifelse(str_detect(portfolio_final, "LAKE CITY"),
                                  "LAKE CITY COURT", portfolio_final))

# Action codes from raw 50058 data
exits_58 <- dbGetQuery(db_hhsaw,
                       "SELECT c.id_hudhears, b.id_kc_pha, a.agency, a.act_type, a.act_date FROM
                       (SELECT DISTINCT agency, act_type, act_date, id_hash 
                         FROM pha.stage_kcha WHERE act_type = 6) a
                       LEFT JOIN
                       (SELECT DISTINCT id_hash, id_kc_pha FROM pha.final_identities) b
                       ON a.id_hash = b.id_hash
                       LEFT JOIN
                       (SELECT DISTINCT id_hudhears, id_kc_pha FROM amatheson.hudhears_xwalk_ids
                        WHERE id_kc_pha IS NOT NULL) c
                        ON b.id_kc_pha = c.id_kc_pha
                       UNION
                       SELECT z.id_hudhears, y.id_kc_pha, x.agency, x.act_type, x.act_date FROM
                       (SELECT DISTINCT agency, act_type, act_date, id_hash 
                         FROM pha.stage_sha WHERE act_type = 6) x
                       LEFT JOIN
                       (SELECT id_hash, id_kc_pha FROM pha.final_identities) y
                       ON x.id_hash = y.id_hash
                       LEFT JOIN
                       (SELECT DISTINCT id_hudhears, id_kc_pha FROM amatheson.hudhears_xwalk_ids
                        WHERE id_kc_pha IS NOT NULL) z
                        ON y.id_kc_pha = z.id_kc_pha"
                       )


# EXIT NUMBERS/CONSORT DIAGRAM ----
# Check how many exit events are captured in the timevar table
exit_timevar %>%
  filter(!is.na(act_date)) %>%
  distinct(hh_id_kc_pha, act_date) %>%
  summarise(cnt = n())

# See how many individuals have an exit and how many are true exits
exit_timevar %>%
  filter(!is.na(act_date)) %>%
  summarise(cnt = n_distinct(id_hudhears))

exit_timevar %>%
  filter(!is.na(act_date)) %>%
  distinct(id_hudhears, true_exit) %>%
  count(true_exit)


# See how many individuals have a true exit in the study period
exit_timevar %>%
  filter(true_exit == 1 & !is.na(exit_order_study)) %>%
  summarise(cnt = n_distinct(id_hudhears))

# See how many true exits in the study period
exit_timevar %>%
  filter(true_exit == 1 & !is.na(exit_order_study)) %>%
  distinct(id_hudhears, act_date) %>%
  summarise(cnt = n())


## CONSORT diagram ----
# Set up DF to track counts
consort_df <- exit_timevar %>% filter(!is.na(act_date))

### Make diagram ---- 
# Using approach here: https://stackoverflow.com/questions/61745574/diagrammer-arrow-problems
# Set up function to toggle Medicaid and household on/off
consort_maker <- function(df = consort_df, mcaid_prior = F, 
                          mcaid_after = F, household = F,
                          exit_type = F) {
  exits_tot <- nrow(df)
  
  ### Set up values for each node ----
  # Date range
  consort_period_excl_sha <- df %>% filter(agency == "SHA" & 
                                                     (act_date < "2012-01-01" | act_date > "2018-12-31")) %>% nrow()
  consort_period_excl_kcha <- df %>% filter(agency == "KCHA" & 
                                                      (act_date < "2016-01-01" | act_date > "2018-12-31")) %>% nrow()
  
  df <- df %>% filter((agency == "SHA" & act_date >= "2012-01-01" & act_date <= "2018-12-31") |
                                        (agency == "KCHA" & act_date >= "2016-01-01" & act_date <= "2018-12-31"))
  
  exits_period <- nrow(df)
  
  # Household level or not
  if (household == T) {
    df <- df %>% filter(id_kc_pha == hh_id_kc_pha)
    covariate_use <- covariate_hh
  } else {
    covariate_use <- covariate
  }
  hholds <- nrow(df)
  one_per_hh <- exits_period - hholds
  
  
  # True/false exits
  df <- df %>% filter(true_exit == 1)
  exits_true <- nrow(df)
  exits_false <- hholds - exits_true
  
  # One exit per person
  df <- df %>% filter(exit_order_study == exit_order_max_study & exit_type_keep == 1)
  exits_per_person <- nrow(df)
  exits_multi <- exits_true - nrow(df)
  
  # Add in covariates
  df <-  left_join(df, 
                   distinct(covariate_use, id_hudhears, exit_date, exit_category, exit_death, age_at_exit,
                            gender_me, race_eth_me, single_caregiver, hh_size, hh_disability,
                            housing_time_at_exit, kc_opp_index_score, 
                            full_cov_11_prior, full_cov_7_prior, full_cov_11_after, full_cov_7_after),
                   by = c("id_hudhears", "act_date" = "exit_date", "exit_category"))
  
  # Remove deaths or missing reasons
  exits_death_removed <- nrow(df %>% filter(exit_death == 1L))
  exits_missing <- nrow(df %>% filter(is.na(exit_death)))
  df <- df %>% filter(exit_death == 0 & !is.na(exit_death))
  exits_death <- nrow(df)
  
  # Remove missing covariates
  missing_age <- nrow(df %>% filter(is.na(age_at_exit)))
  missing_gender <- nrow(df %>% filter(is.na(gender_me)))
  missing_race <- nrow(df %>% filter(is.na(race_eth_me) | race_eth_me == "Unknown"))
  missing_agency <- nrow(df %>% filter(is.na(agency)))
  missing_caregiver <- nrow(df %>% filter(is.na(single_caregiver)))
  missing_hhsize <- nrow(df %>% filter(is.na(hh_size)))
  missing_disability <- nrow(df %>% filter(is.na(hh_disability)))
  missing_los <- nrow(df %>% filter(is.na(housing_time_at_exit)))
  missing_prog <- nrow(df %>% filter(is.na(major_prog)))
  missing_opp_index <- nrow(df %>% filter(is.na(kc_opp_index_score)))
  
  df <- df %>%
    filter(!(is.na(age_at_exit) | is.na(gender_me) | is.na(race_eth_me) | race_eth_me == "Unknown" |
               is.na(agency) | is.na(single_caregiver) | is.na(hh_size) | is.na(hh_disability) | 
               is.na(housing_time_at_exit) | is.na(major_prog) | is.na(kc_opp_index_score)))
  
  exits_demogs <- nrow(df)
  exits_demogs_removed <- exits_death - exits_demogs
  
  # Split into exit type
  
  
  # Medicaid coverage
  mcaid_7_prior <- nrow(df %>% filter(full_cov_7_prior == 0))
  mcaid_7_after <- nrow(df %>% filter(full_cov_7_after == 0))
  mcaid_7_prior_after <- nrow(df %>% filter(full_cov_7_prior == 0 | full_cov_7_after == 0))
  
  if (mcaid_prior == T & mcaid_after == F) {
    df <- df %>% filter(full_cov_7_prior == 1)
  } else if (mcaid_prior == F & mcaid_after == T) {
    df <- df %>% filter(full_cov_7_after == 1)
  } else {
    df <- df %>% filter(full_cov_7_prior == 1 & full_cov_7_after == 1)  
  }
  
  # Also remove those aged 62+ since most are dual
  if (mcaid_prior == T | mcaid_after == T) {
    mcaid_age_62 <- nrow(df %>% filter(age_at_exit >= 62))
    df <- df %>% filter(age_at_exit < 62)
  }
  
  exits_mcaid <- nrow(df)
  
  
  ### Set up columns ----
  # Column 1: main boxes and side box placeholders (blank text)
  a1 <- glue("a1 [label = 'Exits total: {format(exits_tot, big.mark = ',', trim = T)}'];")
  a2 <- glue("a2 [label = 'Exits in study period: {format(exits_period, big.mark = ',', trim = T)}'];")
  if (household == T) {
    a2h <- glue("a2h [label = 'Households: {format(hholds, big.mark = ',', trim = T)}'];")
  } else {
    a2h <- "a2h [shape = point, label = '', width = 0, height = 0]"
  }
  a3 <- glue("a3 [label = 'True exits: {format(exits_true, big.mark = ',', trim = T)}'];")
  a4 <- glue("a4 [label = 'One exit per person: {format(exits_per_person, big.mark = ',', trim = T)}'];")
  a5 <- glue("a5 [label = 'Non-death exits: {format(exits_death, big.mark = ',', trim = T)}'];")
  a6 <- glue("a6 [label = 'Complete demographics (primary analysis): \n{format(exits_demogs, big.mark = ',', trim = T)}'];")
  if (mcaid_prior == T | mcaid_after == T) {
    a7 <- glue("a7 [label = 'Non-dual, full Medicaid coverage (secondary analysis): \n{format(exits_mcaid, big.mark = ',', trim = T)}'];") 
  } else {
    # For reasons unknown to me, the code fails if there is not another fake box here
    a7 <- "a7 [shape = point, label = '', width = 0, height = 0]"
  }
  
  # Column 2: side boxes
  b1 <- glue("b1 [label = 'Exits outside study period \n",
             "KCHA (<2016, >2018): {format(consort_period_excl_kcha, big.mark = ',', trim = T)} \n",
             "SHA (<2012, >2018): {format(consort_period_excl_sha, big.mark = ',', trim = T)}'];")
  if (household == T) {
    b1h <- glue("b1h [label = 'Non-heads of household (n = {format(one_per_hh, big.mark = ',', trim = T)})'];")
  } else {
    b1h <- ""
  }
  b2 <- glue("b2 [label = 'False exits (n = {format(exits_false, big.mark = ',', trim = T)})'];")
  b3 <- glue("b3 [label = 'Multiple exits per person (n = {format(exits_multi, big.mark = ',', trim = T)})'];")
  b4 <- glue("b4 [label = 'Exits due to death (n = {format(exits_death_removed, big.mark = ',', trim = T)}) or \n",
             "missing exit reasons (n = {format(exits_missing, big.mark = ',', trim = T)})'];")
  b5 <- glue("b5 [label = 'Missing demographics (n = {format(exits_demogs_removed, big.mark = ',', trim = T)}): \n",
             " - Opportunity index: {format(missing_opp_index, big.mark = ',', trim = T)} \n",
             " - Household demographics: {format(missing_hhsize, big.mark = ',', trim = T)} \n",
             " - Age: {format(missing_age, big.mark = ',', trim = T)}'];")
  if (mcaid_prior == T & mcaid_after == T) {
    b6 <- glue("b6 [label = '<7 months full Medicaid coverage \n", 
               "prior to or after exit \n", 
               "(n = {format(mcaid_7_prior, big.mark = ',', trim = T)}/", 
               "{format(mcaid_7_after, big.mark = ',', trim = T)}, respectively) \n",
               "Aged 62+ (n = {format(mcaid_age_62, big.mark = ',', trim = T)}) '];")
  } else if (mcaid_prior == T & mcaid_after == F) {
    b6 <- glue("b6 [label = '<7 months full Medicaid coverage \n", 
               "prior to exit \n", 
               "(n = {format(mcaid_7_prior, big.mark = ',', trim = T)}) \n",
               "Aged 62+ (n = {format(mcaid_age_62, big.mark = ',', trim = T)}) '];")
  } else if (mcaid_prior == F & mcaid_after == T) {
    b6 <- glue("b6 [label = '<7 months full Medicaid \n", 
               "coverage after exit \n", 
               "(n = {format(mcaid_7_after, big.mark = ',', trim = T)}) \n",
               "Aged 62+ (n = {format(mcaid_age_62, big.mark = ',', trim = T)}) '];")
  } else {
    b6 <- ""
  }
  
  # Edges/connections
  lines1 <- glue("a1 -> x1 [arrowhead='none'] \n",
                 "x1 -> b1 \n",
                 "x1 -> a2")
  if (household == T) {
    lines1h <- glue("a2 -> x1h [arrowhead='none'] \n",
                   "x1h -> b1h \n",
                   "x1h -> a2h")
    lines2 <- glue("a2h -> x2 [arrowhead='none'] \n",
                   "x2 -> b2 \n",
                   "x2 -> a3")
  } else {
    lines1h <- ""
    lines2 <- glue("a2 -> x2 [arrowhead='none'] \n",
                   "x2 -> b2 \n",
                   "x2 -> a3")
  }
  lines3 <- glue("a3 -> x3 [arrowhead='none'] \n",
                 "x3 -> b3 \n",
                 "x3 -> a4")
  lines4 <- glue ("a4 -> x4 [arrowhead='none'] \n",
                  "x4 -> b4 \n",
                  "x4 -> a5")
  lines5 <- glue("a5 -> x5 [arrowhead='none'] \n",
                 "x5 -> b5 \n",
                 "x5 -> a6")
  if (mcaid_prior == T | mcaid_after == T) {
    lines6 <- glue("a6 -> x6 [arrowhead='none'] \n",
                   "x6 -> b6 \n",
                   "x6 -> a7")
  } else {
    lines6 <- ""
  }

  
  # Set up extra blanks and ranks if needed
  if (household == T) {
    x1h <- "; x1h"
    rank1h <- glue("subgraph {{ \n
                    rank = same; x1h; b1h; \n
                  }}")
  } else {
    x1h <- ""
    rank1h <- ""
  }
  
  if (mcaid_prior == T | mcaid_after == T) {
    x6 <- "; x6"
    rank6 <- glue("subgraph {{ \n
                    rank = same; x6; b6; \n
                  }}")
  } else {
    x6 <- ""
    rank6 <- ""
  }
  
  
  # Bring into one place
  graph_txt <- 
  glue("digraph prisma {{
    graph [splines = ortho, nodesep = 1, dpi = 72]
    
    # set up main nodes
    node [shape=box, fontsize = 12, fontname = 'Helvetica', width = 2];
    {a1} {a2h} {a2} {a3} {a4} {a5} {a6} {a7}
    
    # set up side nodes
    node [shape=box, fontsize = 10, fontname = 'Helvetica', width = 2];
    {b1} {b1h} {b2} {b3} {b4} {b5} {b6}
    
    # create filler nodes without box around 
    node [shape = point, width = 0, height = 0]
    x1 {x1h}; x2; x3; x4; x5 {x6}
    
    # Edge definitions
    {lines1}
    {lines1h}
    {lines2}
    {lines3}
    {lines4}
    {lines5}
    {lines6}
       
    # Make subgraph definition so arrow is horzontal
    subgraph {{
      rank = same; x1; b1;
    }}
    {rank1h}
    subgraph {{
      rank = same; x2; b2;
    }}
    subgraph {{
      rank = same; x3; b3;
    }}
    subgraph {{
      rank = same; x4; b4;
    }}
    subgraph {{
      rank = same; x5; b5;
    }}
    {rank6}
  
  }}
       ")
  
  DiagrammeR::grViz(graph_txt)
}

# Run Medicaid and non-Medicaid versions
mcaid_graph <- consort_maker(df = consort_df, mcaid_prior = T, mcaid_after = T, household = F)
mcaid_prior_graph <- consort_maker(df = consort_df, mcaid_prior = T, mcaid_after = F, household = F)
nonmcaid_graph <- consort_maker(df = consort_df, mcaid_prior = F, mcaid_after = F, household = F)

# Repeat but for households
mcaid_graph_hh <- consort_maker(df = consort_df, mcaid_prior = T, mcaid_after = T, household = T)
mcaid_prior_graph_hh <- consort_maker(df = consort_df, mcaid_prior = T, mcaid_after = F, household = T)
nonmcaid_graph_hh <- consort_maker(df = consort_df, mcaid_prior = F, mcaid_after = F, household = T)

# Clunky workarounds to get export to work
# https://github.com/rich-iannone/DiagrammeR/issues/340
# https://stackoverflow.com/questions/65669640/save-diagrammer-object-to-png-on-disc
mcaid_graph %>% DiagrammeRsvg::export_svg() %>% charToRaw() %>% rsvg::rsvg() %>%
  png::writePNG(file.path(here::here(), "analyses/consort_diag_mcaid.png"))
mcaid_prior_graph %>% DiagrammeRsvg::export_svg() %>% charToRaw() %>% rsvg::rsvg() %>%
  png::writePNG(file.path(here::here(), "analyses/consort_diag_mcaid_prior.png"))
nonmcaid_graph %>% DiagrammeRsvg::export_svg() %>% charToRaw() %>% rsvg::rsvg() %>%
  png::writePNG(file.path(here::here(), "analyses/consort_diag_nonmcaid.png"))

mcaid_graph_hh %>% DiagrammeRsvg::export_svg() %>% charToRaw() %>% rsvg::rsvg() %>%
  png::writePNG(file.path(here::here(), "analyses/consort_diag_mcaid_hh.png"))
mcaid_prior_graph_hh %>% DiagrammeRsvg::export_svg() %>% charToRaw() %>% rsvg::rsvg() %>%
  png::writePNG(file.path(here::here(), "analyses/consort_diag_mcaid_prior_hh.png"))
nonmcaid_graph_hh %>% DiagrammeRsvg::export_svg() %>% charToRaw() %>% rsvg::rsvg() %>%
  png::writePNG(file.path(here::here(), "analyses/consort_diag_nonmcaid_hh.png"))



# EXITS TYPES ----
exit_timevar %>%
  filter(!is.na(act_date) & agency == "KCHA" & act_date >= "2016-01-01" & act_date <= "2018-12-31") %>%
  filter(true_exit == 1) %>%
  distinct(agency, hh_id_kc_pha, act_date, exit_reason_clean, exit_category) %>%
  count(agency, exit_reason_clean, exit_category) %>%
  arrange(agency, -n) %>%
  head(10)

exit_timevar %>%
  filter(!is.na(act_date) & agency == "SHA" & act_date >= "2012-01-01" & act_date <= "2018-12-31") %>%
  filter(true_exit == 1) %>%
  distinct(agency, hh_id_kc_pha, act_date, exit_reason_clean, exit_category) %>%
  count(agency, exit_reason_clean, exit_category) %>%
  arrange(agency, -n) %>%
  head(10)
  


# EXITS NOT CAPTURED IN THE BOTH EXIT AND 50058 DATA ----
# People with info in the 58 data but not the exit data
only_58 <- exits_58 %>%
  mutate(exit_year = year(act_date)) %>%
  anti_join(., distinct(exit_timevar, id_hudhears, act_date, exit_year), 
            by = c("id_hudhears", "exit_year"))
  
only_58 %>% filter((agency == "SHA" & between(exit_year, 2012, 2018)) | 
                      (agency == "KCHA" & between(exit_year, 2016, 2018))) %>% 
  distinct(agency, exit_year, id_hudhears) %>% count(agency, exit_year)

only_58 %>% filter((agency == "SHA" & between(exit_year, 2012, 2018)) | 
                     (agency == "KCHA" & between(exit_year, 2016, 2018))) %>%  
  distinct(agency, exit_year, id_hudhears) %>% count(agency)

# People with info in the exit_timevar data but not the 58 data
# NB. There were a few hundred people whose IDs didn't match who are not included in the timevar data

# Explanations seen for these situations:
# 1) Exit reason is expiry of voucher and household retained housing right away
# 2) Changes in household composition (exit act_dates were applied to all individuals
#    in that household even if the person had left the household by the time of exit)
# 3) Identity matching didn't work completely for the timevar table but the different
#    id_kc_pha values were linked via a household ID (related to #2)

only_exit <- exit_timevar %>%
  filter(!is.na(act_date)) %>%
  distinct(id_hudhears, agency, act_date, exit_year) %>%
  anti_join(., mutate(exits_58, exit_year = year(act_date)), by = c("id_hudhears", "exit_year"))

only_exit %>% 
  filter((agency == "SHA" & between(exit_year, 2012, 2018)) | 
           (agency == "KCHA" & between(exit_year, 2016, 2018))) %>% 
  count(agency)
only_exit %>% 
  filter((agency == "SHA" & between(exit_year, 2012, 2018)) | 
           (agency == "KCHA" & between(exit_year, 2016, 2018))) %>% 
  count(agency, exit_year)


# EXITS BY YEAR ----
## Any exit vs not ----
exit_count <- function(year, hh = F, drop_death = F) {
  message("Working on ", year)
  exits <- setDT(exit_timevar)
  exits <- exits[exit_year == year | (year >= year(from_date) & year <= year(to_date))]
  
  if (drop_death == T) {
    exits <- exits[!str_detect(tolower(exit_timevar$exit_reason_clean), "decease")]
  }
  
  if (hh == F) {
    exits[, exited := max(exit_year == year & true_exit == 1, na.rm = T), by = "id_hudhears"]
    exits[, exited := ifelse(is.na(exited) | is.infinite(exited) | exited == 0, "Did not exit", "Exited")]
    exits <- unique(exits[, .(agency, id_hudhears, exited)])
  } else {
    exits[, exited := max(exit_year == year & true_exit == 1, na.rm = T), by = "hh_id_kc_pha"]
    exits[, exited := ifelse(is.na(exited) | is.infinite(exited) | exited == 0, "Did not exit", "Exited")]
    exits <- unique(exits[, .(agency, hh_id_kc_pha, exited)])
  }
  
  exits <- exits %>%
    group_by(agency) %>%
    count(exited) %>%
    mutate(year_tot = sum(n)) %>%
    ungroup() %>%
    mutate(suppressed = between(year_tot, 1, 9),
           n_supp = ifelse(suppressed, NA, n),
           year_tot_label = ifelse(suppressed, "<10", as.character(scales::comma(year_tot))),
           year_tot_supp = ifelse(suppressed, NA, year_tot),
           pct = round(n / year_tot * 100, 1),
           pct_supp = ifelse(suppressed, NA, pct),) %>%
    mutate(exit_year = year)
  
  exits
}


any_exit <- bind_rows(lapply(c(2012:2020), exit_count)) %>%
  filter(agency == "SHA" | (agency == "KCHA" & exit_year >= 2016))
any_exit_hh <- bind_rows(lapply(c(2012:2020), exit_count, hh = T)) %>%
  filter(agency == "SHA" | (agency == "KCHA" & exit_year >= 2016))


### Stacked percent bar graph of any exit/no exit ----
# Ind level
exit_year_bar <- any_exit %>%
  ggplot(aes(fill = as.character(exited), y = n_supp, x = exit_year)) +
  geom_bar(position = "fill", stat="identity", colour = "black") +
  geom_text(position = position_fill(vjust = 0.5), size = 2, show.legend = F,
            aes(group = exited, label = paste0(pct_supp, "%"),
                color = exited)) +
  scale_color_manual(values = c("white", "black")) +
  geom_text(aes(x = exit_year, y = 1.02, label = year_tot_label), vjust = 0, size = 2) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum(axis_text_size = 10, grid = F, base_family = "Calibri") +
  labs(x = "Year",
       y = "Percent of people",
       caption = "NB. KCHA exit data is incomplete prior to October 2015.",
       fill = "Exit vs. not") +
  facet_wrap(~ agency, nrow = 2)

ggsave(filename = file.path(here::here(), "analyses/exit_factors/exits_by_year_ind.png"),
       device = "png", plot = exit_year_bar,
       width = 6.5, height = 6, units = "in")



# HH level
any_exit_hh %>%
  ggplot(aes(fill = as.character(exited), y = n_supp, x = exit_year)) +
  geom_bar(position = "fill", stat="identity", colour = "black") +
  geom_text(position = position_fill(vjust = 0.5), size = 3, show.legend = F,
            aes(group = exited, label = paste0(pct_supp, "%"),
                color = exited)) +
  scale_color_manual(values = c("white", "black")) +
  geom_text(aes(x = exit_year, y = 1.02, label = year_tot_label), vjust = 0, size = 3) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum(axis_text_size = 10, grid = F, base_family = "Calibri") +
  labs(title = "Proportion of households exiting by year",
       x = "Year",
       y = "Percent of households",
       caption = "NB. KCHA exit data is incomplete prior to October 2015",
       fill = "Exit vs. not") +
  facet_grid(~ agency)


## Exit types ----
exit_any <- function(hh = F, drop_death = F, drop_kcha = T) {
  if (hh == F) {
    id_var <- "id_hudhears"
  } else {
    id_var <- "hh_id_kc_pha"
  }
  
  output <-  exit_timevar %>% 
    # Only keep people who had no activity after an exit
    filter(true_exit == 1) 
  
  if (drop_kcha == T) {
    # Only keep exits 2016 onward for KCHA since data before then are incomplete
    output <- output %>%
      filter(agency == "SHA" | (agency == "KCHA" & exit_year >= 2016))
  }
  
  if (drop_death == T) {
    output <- output %>% filter(str_detect(tolower(exit_reason_clean), "decease", negate = T))
  }
  
  output <- output %>%
    select(id_var, agency, act_date, exit_year, exit_category) %>%
    distinct() %>%
    group_by(agency, exit_year) %>%
    count(exit_category) %>%
    mutate(year_tot = sum(n)) %>%
    ungroup() %>%
    mutate(suppressed = between(year_tot, 1, 9),
           n_supp = ifelse(suppressed, NA, n),
           year_tot_label = ifelse(suppressed, "<10", as.character(scales::comma(year_tot))),
           year_tot_supp = ifelse(suppressed, NA, year_tot),
           pct = round(n / year_tot * 100, 1),
           pct_supp = ifelse(suppressed, NA, pct),)
  
  output
}

exit_year <- exit_any(hh = F, drop_death = F)
exit_year_hh <- exit_any(hh = T, drop_death = F)
exit_year_nodeath <- exit_any(hh = F, drop_death = T)
exit_year_hh_nodeath <- exit_any(hh = T, drop_death = T)

# Trick from scales::show_col
# (https://stackoverflow.com/questions/62800823/set-a-color-to-show-clear-the-numbers)
hcl <- farver::decode_colour(viridisLite::viridis(length(unique(exit_year$exit_category))), "rgb", "hcl")
label_col <- ifelse(hcl[, "l"] > 50, "black", "white")

# # Line graph showing numbers over time
ggplot(exit_year, aes(group = exit_category, y = n_supp, x = exit_year, color = exit_category)) +
  geom_line(size = 1) +
  scale_color_viridis(discrete = T) +
  theme_ipsum(base_family = "Calibri") +
  labs(title = "Number of exits from KCHA and SHA",
       x = "Year",
       y = "Number of exits",
       caption = "NB. KCHA exit data is incomplete prior to October 2015") +
  facet_wrap(~agency, scales = "free_x")


# # Stacked percent bar graph
exit_type_bar <- exit_year %>%
  filter(!is.na(exit_category)) %>%
  ggplot(aes(fill = exit_category, y = n_supp, x = exit_year)) +
  geom_bar(position = "fill", stat="identity", colour = "black") +
  geom_text(position = position_fill(vjust = 0.5), size = 2.5, show.legend = F,
            aes(group = exit_category, label = paste0(pct_supp, "%"),
                color = exit_category)) +
  scale_color_manual(values = label_col) +
  geom_text(aes(x = exit_year, y = 1.02, label = year_tot_label), vjust = 0, size = 2.5) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum(axis_text_size = 10, grid = F, base_family = "Calibri") +
  labs(x = "Year",
       y = "Percent of exits",
       #title = "Proportion of exit types by year",
       caption = "NB. KCHA exit data is incomplete prior to October 2015",
       fill = "Exit category") +
  facet_wrap(~agency, nrow = 2)

ggsave(filename = file.path(here::here(), "analyses/exit_factors/exits_by_type_ind.png"),
       device = "png", plot = exit_type_bar,
       width = 6.5, height = 6, units = "in")


exit_type_no_dth_bar <- exit_year_nodeath %>%
  filter(!is.na(exit_category)) %>%
  ggplot(aes(fill = exit_category, y = n_supp, x = exit_year)) +
  geom_bar(position = "fill", stat="identity", colour = "black") +
  geom_text(position = position_fill(vjust = 0.5), size = 2.5, show.legend = F,
            aes(group = exit_category, label = paste0(pct_supp, "%"),
                color = exit_category)) +
  scale_color_manual(values = label_col) +
  geom_text(aes(x = exit_year, y = 1.02, label = year_tot_label), vjust = 0, size = 2.5) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum(axis_text_size = 10, grid = F, base_family = "Calibri") +
  labs(x = "Year",
       y = "Percent of exits",
       #title = "Proportion of exit types by year (excluding deaths)",
       caption = "NB. KCHA exit data is incomplete prior to October 2015",
       fill = "Exit category") +
  facet_wrap(~agency, nrow = 2)

ggsave(filename = file.path(here::here(), "analyses/exit_factors/exits_by_type_no_dth_ind.png"),
       device = "png", plot = exit_type_no_dth_bar,
       width = 6.5, height = 6, units = "in")


## Set up key values for markdown outpuyt ----
kcha_max_pos <- exit_year %>% 
  filter(agency == "KCHA" & exit_category == "Positive") %>%
  filter(pct == max(pct))
sha_max_pos <- exit_year %>% 
  filter(agency == "SHA" & exit_category == "Positive") %>%
  filter(pct == max(pct))

kcha_max_pos_hh <- exit_year_hh %>% 
  filter(agency == "KCHA" & exit_category == "Positive") %>%
  filter(pct == max(pct))
sha_max_pos_hh <- exit_year_hh %>% 
  filter(agency == "SHA" & exit_category == "Positive") %>%
  filter(pct == max(pct))


# DEMOGS OF PEOPLE EXITING ----
exit_demogs <- exit_timevar %>%
  distinct(agency, id_hudhears, id_kc_pha, from_date, to_date, act_date, exit_year, exit_reason_clean, exit_category, true_exit,
           major_prog, disability, portfolio_final) %>%
  left_join(., pha_demo, by = "id_kc_pha") %>%
  # Recode Latino so that it trumps in race_eth_me (currently most Latino/a are found under multiple)
  # Consider changing how the pha_demo table sets this up
  mutate(race_eth_me = ifelse(race_latino == 1, "Latino", race_eth_me))

exit_demogs_hh <- exit_timevar %>%
  select(-id_hudhears, -id_kc_pha) %>%
  distinct() %>%
  left_join(., pha_demo, by = c("hh_id_kc_pha" = "id_kc_pha")) %>%
  mutate(exit_year = year(act_date)) %>%
  mutate(race_eth_me = ifelse(race_latino == 1, "Latino", race_eth_me))


# Look at demogs of people who exit vs don't in a given year
exit_demogs_year <- function(year_run, hh = F, drop_death = F, drop_kcha = T) {
  demog_list <- c("major_prog", "race_eth_me", "gender_recent", "agegrp", "agegrp_expanded", 
                  "time_housing", "disability", "portfolio_final")
  
  if (hh == F) {
    exits <- setDT(mutate(exit_demogs, id_var = id_kc_pha))
  } else {
    exits <- setDT(mutate(exit_demogs_hh, id_var = hh_id_kc_pha))
  }

  if (drop_kcha == T & year_run < 2016) {
    # Only keep 2016 onward for KCHA since data before then are incomplete
    exits <- exits[agency == "SHA"]
  }

  exits <- exits[exit_year == year_run | (year_run >= year(from_date) & year_run <= year(to_date))]
  
  if (hh == F) {
    exits[, exited := max(exit_year == year_run & true_exit == 1, na.rm = T), by = .(id_kc_pha)]
  } else {
    exits[, exited := max(exit_year == year_run & true_exit == 1, na.rm = T), by = .(hh_id_kc_pha)]
  }
  exits[is.infinite(exited), exited := NA]
  
  if (hh == F) {
    id_var <- rlang::sym("id_kc_pha")
    exits <- exits %>%
      select(id_var, id_kc_pha, agency, exited, act_date, true_exit, exit_reason_clean, exit_category, 
             race_eth_me, gender_recent, major_prog, disability, portfolio_final) %>%
      distinct() %>%
      left_join(., filter(pha_calyear, .data[["id_kc_pha"]] == id_kc_pha & year == year_run) %>%
                  select(agency, id_kc_pha, agegrp, agegrp_expanded, time_housing),
                by = c("agency", "id_kc_pha"))
  } else {
    id_var <- rlang::sym("hh_id_kc_pha")
    exits <- exits %>%
      select(id_var, hh_id_kc_pha, agency, exited, act_date, true_exit, exit_reason_clean, exit_category, 
             race_eth_me, gender_recent, major_prog, disability, portfolio_final) %>%
      distinct() %>%
      left_join(., filter(pha_calyear, hh_id_kc_pha == id_kc_pha & year == year_run) %>%
                  select(agency, hh_id_kc_pha, agegrp, agegrp_expanded, time_housing),
                by = c("agency", "hh_id_kc_pha"))
  }
  
  exit_sum <- bind_rows(lapply(demog_list, function(x) {
    if (drop_death == T) {
      # exits <- exits %>% filter(str_detect(tolower(exit_reason_clean), "decease", negate = T))
      output <- exits[str_detect(tolower(exit_reason_clean), "decease", negate = T)]
    } else {
      output <- exits
    }
    
    output %>%
      filter(exited == 1 & !is.na(exited) & !is.infinite(exited) & 
               # Need these additional filters to ensure only the rows from that year
               # are retained (sometimes a person had exits across multiple years or 
               # from/to dates that fell in the same year)
               !is.na(act_date) & true_exit == 1 & year(act_date) == year_run) %>%
      distinct(agency, {{id_var}}, act_date, exit_category, across(all_of(x))) %>%
      group_by(agency, across(all_of(x))) %>%
      count(exit_category) %>%
      mutate(year_tot = sum(n)) %>%
      ungroup() %>%
      mutate(suppressed = between(year_tot, 1, 9),
             n_supp = ifelse(suppressed, NA, n),
             year_tot_label = ifelse(suppressed, "<10", as.character(scales::comma(year_tot))),
             year_tot_supp = ifelse(suppressed, NA, year_tot),
             pct = round(n / year_tot * 100, 1),
             pct_supp = ifelse(suppressed, NA, pct),
             category = rlang::as_name(x)) %>%
      rename(group = x) %>%
      mutate(group = as.character(group))
  }))
  

  non_exit_sum <- bind_rows(lapply(demog_list, function(y) {
    exits %>%
      filter(exited == 0 | is.na(exited) | is.infinite(exited)) %>%
      distinct(agency, {{id_var}}, act_date, exit_category, across(all_of(y))) %>%
      group_by(agency, across(all_of(y))) %>%
      summarise(n = n_distinct({{id_var}})) %>%
      mutate(year_tot = sum(n)) %>%
      ungroup() %>%
      mutate(category = rlang::as_name(y)) %>%
      rename(group = y) %>%
      mutate(group = as.character(group))
  }))
  
  output <- bind_rows(mutate(exit_sum, exit = 1L), 
                      mutate(non_exit_sum, exit = 0L)) %>%
    mutate(suppressed = between(year_tot, 1, 9),
           n_supp = ifelse(suppressed, NA, n),
           year_tot_label = ifelse(suppressed, "<10", 
                                   as.character(scales::comma(year_tot, accuracy = 1))),
           year_tot_supp = ifelse(suppressed, NA, year_tot),
           pct = round(n / year_tot * 100, 1),
           pct_supp = ifelse(suppressed, NA, pct),
           exit_year = year_run) %>%
    select(exit_year, agency, category, group, exit, exit_category, n, n_supp,
           year_tot, year_tot_supp, year_tot_label, pct, pct_supp, suppressed) %>%
    arrange(exit_year, agency, category, group, exit, exit_category)
  return(output)
}


# Run function for various combinations of options
exit_demogs_sum <- bind_rows(lapply(c(2012:2020), exit_demogs_year, hh = F, drop_death = F))
exit_demogs_sum_hh <- bind_rows(lapply(c(2012:2020), exit_demogs_year, hh = T, drop_death = F))
exit_demogs_sum_nodeath <- bind_rows(lapply(c(2012:2020), exit_demogs_year, hh = F, drop_death = T))
exit_demogs_sum_hh_nodeath <- bind_rows(lapply(c(2012:2020), exit_demogs_year, hh = T, drop_death = T))

# Duplicate group values to make highlighted gpplots
exit_demogs_sum <- exit_demogs_sum %>% mutate(group2 = group)
exit_demogs_sum_hh <- exit_demogs_sum_hh %>% mutate(group2 = group)
exit_demogs_sum_nodeath <- exit_demogs_sum_nodeath %>% mutate(group2 = group)
exit_demogs_sum_hh_nodeath <- exit_demogs_sum_hh_nodeath %>% mutate(group2 = group)


## Make a higher level summary ----
demog_collapse <- function(df) {
  output <- df %>%
    group_by(exit_year, agency, category, group, group2, exit) %>%
    summarise(n = sum(n)) %>%
    group_by(exit_year, agency, category, group, group2) %>%
    mutate(year_tot = sum(n),
           year_tot_label = ifelse(between(year_tot, 1, 9), "<10", as.character(year_tot)),
           year_tot_supp = ifelse(between(year_tot, 1, 9), 0, year_tot)) %>%
    ungroup() %>%
    mutate(pct = round(n / year_tot * 100, 1))
  output
}

# Count of all exits by group
exit_demogs_sum_collapse <- demog_collapse(exit_demogs_sum)
exit_demogs_sum_hh_collapse <- demog_collapse(exit_demogs_sum_hh)
exit_demogs_sum_nodeath_collapse <- demog_collapse(exit_demogs_sum_nodeath)
exit_demogs_sum_hh_nodeath_collapse <- demog_collapse(exit_demogs_sum_hh_nodeath)

# # Line graph showing numbers over time by age group
# exit_demogs_sum_collapse %>%
#   filter(category == "agegrp_expanded" & !is.na(group) & exit == 1) %>%
#   ggplot(aes(y = n_supp, x = exit_year)) +
#   geom_line(data = exit_demogs_sum_collapse %>% select(-group) %>%
#               filter(category == "agegrp_expanded" & !is.na(group2) & exit == 1),
#             aes(group = group2), color = "grey", size = 0.5, alpha = 0.6) +
#   geom_line(color = "#69b3a2", size = 1.2) +
#   scale_color_viridis(discrete = T) +
#   theme_ipsum(grid = F) +
#   labs(title = "Number of exits from KCHA and SHA by age",
#        x = "Year",
#        y = "Number of exits",
#        caption = "NB. KCHA exit data is incomplete prior to October 2015") +
#   facet_grid(group ~ agency)

# Stacked percent bar graph of exit type by age group
exit_demogs_sum %>%
  filter(category == "agegrp" & !is.na(group) & exit == 1) %>%
  ggplot(aes(fill = exit_category, y = n_supp, x = exit_year)) +
  geom_bar(position = "fill", stat = "identity", colour = "black") +
  geom_text(position = position_fill(vjust = 0.5), size = 3, show.legend = F,
            aes(group = exit_category, label = paste0(pct_supp, "%"),
                color = exit_category)) +
  scale_color_manual(values = label_col) +
  geom_text(aes(x = exit_year, y = 1.03, label = year_tot_label), vjust = 0, size = 2.8) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.25)) +
  coord_cartesian(ylim = c(0, 1.1)) +
  theme_ipsum(axis_text_size = 10) +
  labs(title = "Proportion of exit types by year and age group",
       x = "Year",
       y = "Percent of exits",
       caption = "NB. KCHA exit data is incomplete prior to October 2015",
       fill = "Exit category") +
  facet_grid(group ~ agency, scales = "free_x")


exit_demogs_sum_nodeath %>%
  filter(category == "agegrp" & !is.na(group) & exit == 1) %>%
  ggplot(aes(fill = exit_category, y = n_supp, x = exit_year)) +
  geom_bar(position = "fill", stat = "identity", colour = "black") +
  geom_text(position = position_fill(vjust = 0.5), size = 3, show.legend = F,
            aes(group = exit_category, label = paste0(pct_supp, "%"),
                color = exit_category)) +
  scale_color_manual(values = label_col) +
  geom_text(aes(x = exit_year, y = 1.03, label = year_tot_label), vjust = 0, size = 3) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.25)) +
  coord_cartesian(ylim = c(0, 1.1)) +
  theme_ipsum(axis_text_size = 10) +
  labs(title = "Proportion of exit types by year and age group (excluding deaths)",
       x = "Year",
       y = "Percent of exits",
       caption = "NB. KCHA exit data is incomplete prior to October 2015",
       fill = "Exit category") +
  facet_grid(group ~ agency, scales = "free_x")


exit_demogs_sum_nodeath %>%
  filter(category == "major_prog" & !is.na(group) & exit == 1) %>%
  ggplot(aes(fill = exit_category, y = n_supp, x = exit_year)) +
  geom_bar(position = "fill", stat = "identity", colour = "black") +
  geom_text(position = position_fill(vjust = 0.5), size = 3, show.legend = F,
            aes(group = exit_category, label = paste0(pct_supp, "%"),
                color = exit_category)) +
  scale_color_manual(values = label_col) +
  geom_text(aes(x = exit_year, y = 1.03, label = year_tot_label), vjust = 0, size = 3) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 1.05)) +
  theme_ipsum(axis_text_size = 10) +
  labs(title = "Proportion of exit types by year and program type (excluding deaths)",
       x = "Year",
       y = "Percent of exits",
       caption = "NB. KCHA exit data is incomplete prior to October 2015",
       fill = "Exit category") +
  facet_grid(group ~ agency, scales = "free_x")


# Stacked bar chart for portfolios
exit_demogs_sum %>%
  filter(agency == "SHA") %>%
  filter(category == "portfolio_final" & !is.na(group) & group != "Unknown" & exit == 1) %>%
  ggplot(aes(fill = exit_category, y = n_supp, x = exit_year)) + 
  geom_bar(position = "fill", stat="identity", colour = "black") +
  geom_text(position = position_fill(vjust = 0.5), size = 2, show.legend = F,
            aes(group = exit_category, label = paste0(pct_supp, "%"),
                color = exit_category)) +
  scale_color_manual(values = label_col) +
  geom_text(aes(x = exit_year, y = 1.03, label = year_tot_label), vjust = 0, size = 2) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum(axis_text_size = 10, grid = F) +
  labs(title = "Proportion of exit types by year and portfolio (SHA)",
       x = "Year",
       y = "Percent of exits",
       caption = "NB. KCHA exit data is incomplete prior to October 2015",
       fill = "Exit category") +
  facet_wrap(~ group)


# REMOVE DEATHS AND RERUN ----



# MAKE MARKDOWN DOC ----
render(file.path(here::here(), "analyses/pha_exit_demogs.Rmd"), "html_document")


