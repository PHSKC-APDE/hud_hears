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
# Combined exit/timevar data
exit_timevar <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.stage_pha_exit_timevar") %>%
  mutate(portfolio_final = ifelse(str_detect(portfolio_final, "LAKE CITY"),
                                  "LAKE CITY COURT", portfolio_final))

# Demographic table
pha_demo <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.final_demo")

# Calyear table
pha_calyear <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.final_calyear") %>%
  mutate(portfolio_final = ifelse(str_detect(portfolio_final, "LAKE CITY"),
                                  "LAKE CITY COURT", portfolio_final))

# Action codes from raw 50058 data
exits_58 <- dbGetQuery(db_hhsaw,
                       "SELECT b.id_kc_pha, a.agency, a.act_type, a.act_date FROM
                       (SELECT DISTINCT agency, act_type, act_date, id_hash 
                         FROM pha.stage_kcha WHERE act_type = 6) a
                       LEFT JOIN
                       (SELECT id_hash, id_kc_pha FROM pha.final_identities) b
                       ON a.id_hash = b.id_hash
                       UNION
                       SELECT d.id_kc_pha, c.agency, c.act_type, c.act_date FROM
                       (SELECT DISTINCT agency, act_type, act_date, id_hash 
                         FROM pha.stage_sha WHERE act_type = 6) c
                       LEFT JOIN
                       (SELECT id_hash, id_kc_pha FROM pha.final_identities) d
                       ON c.id_hash = d.id_hash"
                       )


# EXITS NOT CAPTURED IN THE BOTH EXIT AND 50058 DATA ----
# People with info in the 58 data but not the exit data
only_58 <- exits_58 %>%
  mutate(exit_year = year(act_date)) %>%
  anti_join(., distinct(exit_timevar, id_kc_pha, act_date, exit_year), 
            by = c("id_kc_pha", "exit_year"))
  
only_58 %>% distinct(agency, exit_year, id_kc_pha) %>% count(agency, exit_year)

# People with info in the exit_timevar data but not the 58 data
# NB. There were a few hundred people whose IDs didn't match who are not included in the timevar data
# Exclude anyone with a truncated date as those exits are already baked into the to_date

# Explanations seen for these situations:
# 1) Exit reason is expiry of voucher and household retained housing right away
# 2) Changes in household composition (exit act_dates were applied to all individuals
#    in that household even if the person had left the household by the time of exit)
# 3) Identity matching didn't work completely for the timevar table but the different
#    id_kc_pha values were linked via a household ID (related to #2)

only_exit <- exit_timevar %>%
  filter(!is.na(act_date)) %>%
  group_by(id_kc_pha) %>%
  mutate(truncate_max = max(truncate_date)) %>%
  ungroup() %>%
  filter(truncate_max == 0) %>%
  distinct(id_kc_pha, agency, act_date, exit_year) %>%
  anti_join(., mutate(exits_58, exit_year = year(act_date)), by = c("id_kc_pha", "exit_year"))

only_exit %>% count(agency, exit_year)


# EXITS BY YEAR ----
## Any exit vs not ----
exit_count <- function(year, hh = F, drop_death = F) {
  message("Working on ", year)
  exits <- setDT(exit_timevar)
  exits <- exits[exit_year == year | (year >= year(from_date) & year <= year(to_date))]
  
  if (drop_death == T) {
    exits <- exits[!str_detect(tolower(exit_timevar$exit_reason), "decease")]
  }
  
  if (hh == F) {
    exits[, exited := max(exit_year == year & true_exit == 1), by = "id_kc_pha"]
    exits[, exited := ifelse(exited == 1, "Exited", "Did not exit")]
    exits <- unique(exits[, .(agency, id_kc_pha, exited)])
  } else {
    exits[, exited := max(exit_year == year & true_exit == 1), by = "hh_id_kc_pha"]
    exits[, exited := ifelse(exited == 1, "Exited", "Did not exit")]
    exits <- unique(exits[, .(agency, hh_id_kc_pha, exited)])
  }
  
  exits <- exits %>%
    group_by(agency) %>%
    count(exited) %>%
    mutate(year_tot = sum(n)) %>%
    ungroup() %>%
    mutate(suppressed = between(year_tot, 1, 9),
           n_supp = ifelse(suppressed, NA, n),
           year_tot_label = ifelse(suppressed, "<10", as.character(year_tot)),
           year_tot_supp = ifelse(suppressed, NA, year_tot),
           pct = round(n / year_tot * 100, 1),
           pct_supp = ifelse(suppressed, NA, pct),) %>%
    mutate(exit_year = year)
}

any_exit <- bind_rows(lapply(c(2012:2020), exit_count))
any_exit_hh <- bind_rows(lapply(c(2012:2020), exit_count, hh = T))


# Stacked percent bar graph of any exit/no exit
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
  theme_ipsum(axis_text_size = 10, grid = F) +
  labs(title = "Proportion of households exiting by year",
       x = "Year",
       y = "Percent of households",
       caption = "NB. KCHA exit data is incomplete prior to October 2015",
       fill = "Exit vs. not") +
  facet_grid(~ agency)


## Exit types ----
exit_any <- function(hh = F, drop_death = F) {
  if (hh == F) {
    id_var <- "id_kc_pha"
  } else {
    id_var <- "hh_id_kc_pha"
  }
  
  output <-  exit_timevar %>% 
    # Only keep people who had no activity after an exit
    filter(true_exit == 1) 
  
  if (drop_death == T) {
    output <- output %>% filter(str_detect(tolower(exit_reason), "decease", negate = T))
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
           year_tot_label = ifelse(suppressed, "<10", as.character(year_tot)),
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
  theme_ipsum() +
  labs(title = "Number of exits from KCHA and SHA",
       x = "Year",
       y = "Number of exits",
       caption = "NB. KCHA exit data is incomplete prior to October 2015") +
  facet_wrap(~agency)


# # Stacked percent bar graph
ggplot(exit_year, aes(fill = exit_category, y = n_supp, x = exit_year)) +
  geom_bar(position = "fill", stat="identity", colour = "black") +
  geom_text(position = position_fill(vjust = 0.5), size = 3, show.legend = F,
            aes(group = exit_category, label = paste0(pct_supp, "%"),
                color = exit_category)) +
  scale_color_manual(values = label_col) +
  geom_text(aes(x = exit_year, y = 1.02, label = year_tot_label), vjust = 0, size = 3) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum() +
  labs(title = "Proportion of exit types by year",
       x = "Year",
       y = "Percent of exits",
       caption = "NB. KCHA exit data is incomplete prior to October 2015",
       fill = "Exit category") +
  facet_wrap(~agency)


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
  distinct(agency, id_kc_pha, from_date, to_date, act_date, exit_year, exit_reason, exit_category, true_exit,
           major_prog, disability, portfolio_final) %>%
  left_join(., pha_demo, by = "id_kc_pha") %>%
  # Recode Latino so that it trumps in race_eth_me (currently most Latino/a are found under multiple)
  # Consider changing how the pha_demo table sets this up
  mutate(race_eth_me = ifelse(race_latino == 1, "Latino", race_eth_me))

exit_demogs_hh <- exit_timevar %>%
  select(-id_kc_pha) %>%
  distinct() %>%
  left_join(., pha_demo, by = c("hh_id_kc_pha" = "id_kc_pha")) %>%
  mutate(exit_year = year(act_date)) %>%
  mutate(race_eth_me = ifelse(race_latino == 1, "Latino", race_eth_me))


# Look at demogs of people who exit vs don't in a given year
exit_demogs_year <- function(year_run, hh = F, drop_death = F) {
  demog_list <- c("major_prog", "race_eth_me", "gender_recent", "agegrp_expanded", 
                  "time_housing", "disability", "portfolio_final")
  
  if (hh == F) {
    id_var <- rlang::sym("id_kc_pha")
    exits <- exit_demogs
  } else {
    id_var <- rlang::sym("hh_id_kc_pha")
    exits <- exit_demogs_hh
  }
  
  if (drop_death == T) {
    exits <- exits %>% filter(str_detect(tolower(exit_reason), "decease", negate = T))
  }
  
  exits <- exits %>%
    filter(exit_year == year_run | (year_run >= year(from_date) & year_run <= year(to_date))) %>%
    group_by({{id_var}}) %>%
    # group_by(id_kc_pha) %>%
    mutate(exited = max(exit_year == year_run & true_exit == 1)) %>%
    ungroup() %>%
    select(id_var, agency, exited, act_date, exit_category, race_eth_me, gender_recent,
           major_prog, disability, portfolio_final) %>%
    distinct() %>%
    left_join(., filter(pha_calyear, .data[[id_var]] == id_kc_pha & year == year_run) %>%
                select(id_var, agegrp_expanded, time_housing),
              by = quo_name(id_var))
  
  
  exit_sum <- bind_rows(lapply(demog_list, function(x) {
    exits %>%
      filter(exited == 1 & !is.na(exited)) %>%
      distinct(agency, {{id_var}}, act_date, exit_category, across(all_of(x))) %>%
      group_by(agency, across(all_of(x))) %>%
      count(exit_category) %>%
      mutate(year_tot = sum(n)) %>%
      ungroup() %>%
      mutate(suppressed = between(year_tot, 1, 9),
             n_supp = ifelse(suppressed, NA, n),
             year_tot_label = ifelse(suppressed, "<10", as.character(year_tot)),
             year_tot_supp = ifelse(suppressed, NA, year_tot),
             pct = round(n / year_tot * 100, 1),
             pct_supp = ifelse(suppressed, NA, pct),
             category = rlang::as_name(x)) %>%
      rename(group = x) %>%
      mutate(group = as.character(group))
  }))
  

  non_exit_sum <- bind_rows(lapply(demog_list, function(x) {
    exits %>%
      filter(exited == 0 | is.na(exited)) %>%
      distinct(agency, {{id_var}}, act_date, exit_category, across(all_of(x))) %>%
      group_by(agency, across(all_of(x))) %>%
      summarise(n = n_distinct({{id_var}})) %>%
      mutate(year_tot = sum(n)) %>%
      ungroup() %>%
      mutate(suppressed = between(year_tot, 1, 9),
             n_supp = ifelse(suppressed, NA, n),
             year_tot_label = ifelse(suppressed, "<10", as.character(year_tot)),
             year_tot_supp = ifelse(suppressed, NA, year_tot),
             pct = round(n / year_tot * 100, 1),
             pct_supp = ifelse(suppressed, NA, pct),
             category = rlang::as_name(x)) %>%
      rename(group = x) %>%
      mutate(group = as.character(group))
  }))
  
  output <- bind_rows(mutate(exit_sum, exit = 1L), 
                      mutate(non_exit_sum, exit = 0L)) %>%
    mutate(exit_year = year_run) %>%
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
  filter(category == "agegrp_expanded" & !is.na(group) & exit == 1) %>%
  ggplot(aes(fill = exit_category, y = n_supp, x = exit_year)) +
  geom_bar(position = "fill", stat = "identity", colour = "black") +
  geom_text(position = position_fill(vjust = 0.5), size = 2, show.legend = F,
            aes(group = exit_category, label = paste0(pct_supp, "%"),
                color = exit_category)) +
  scale_color_manual(values = label_col) +
  geom_text(aes(x = exit_year, y = 1.02, label = year_tot_label), vjust = 0, size = 2) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum(axis_text_size = 10) +
  labs(title = "Proportion of exit types by year and age group",
       x = "Year",
       y = "Percent of exits",
       caption = "NB. KCHA exit data is incomplete prior to October 2015",
       fill = "Exit category") +
  facet_grid(group ~ agency)


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
  geom_text(aes(x = exit_year, y = 1.02, label = year_tot_label), vjust = 0, size = 2) +
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
render(file.path(here::here(), "analyses/pha_exit_demogs.Rmd"), "word_document")
