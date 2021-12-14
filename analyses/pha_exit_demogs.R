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
exit_timevar <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.stage_pha_exit_timevar
                           WHERE chooser = chooser_max") %>%
  mutate(portfolio_final = ifelse(str_detect(portfolio_final, "LAKE CITY"),
                                  "LAKE CITY COURT", portfolio_final))

# Demographic table
pha_demo <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.final_demo")

# Covariate table
covariate <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.control_match_covariate")


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



# EXIT NUMBERS ----
# Check how many exit events are captured in the timevar table
exit_timevar %>%
  filter(!is.na(act_date)) %>%
  distinct(hh_id_kc_pha, act_date) %>%
  summarise(cnt = n())

# See how many individuals have an exit
exit_timevar %>%
  filter(!is.na(act_date)) %>%
  distinct(id_hudhears, true_exit, act_date) %>%
  group_by(true_exit) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  mutate(tot = sum(cnt),
         pct = round(cnt/tot*100, 1))


# See how many individuals have an exit in the study period
exit_timevar %>%
  filter(!is.na(act_date) & 
           ((agency == "SHA" & act_date >= "2012-01-01" & act_date <= "2018-12-31") |
              (agency == "KCHA" & act_date >= "2016-01-01" & act_date <= "2018-12-31"))
         ) %>%
  distinct(id_hudhears, true_exit, act_date) %>%
  group_by(true_exit) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  mutate(tot = sum(cnt),
         pct = round(cnt/tot*100, 1))



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

### NEED TO UPDATE TIMEVAR TABLE SO HH_ID_HUDHEARS IS INCLUDED -----

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


# Stacked percent bar graph of any exit/no exit
any_exit %>%
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
  labs(title = "Proportion of people exiting by year",
       x = "Year",
       y = "Percent of people",
       caption = "NB. KCHA exit data is incomplete prior to October 2015.",
       fill = "Exit vs. not") +
  facet_grid(~ agency, scales = "free_x")

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
  theme_ipsum() +
  labs(title = "Number of exits from KCHA and SHA",
       x = "Year",
       y = "Number of exits",
       caption = "NB. KCHA exit data is incomplete prior to October 2015") +
  facet_wrap(~agency, scales = "free_x")


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
  facet_wrap(~agency, scales = "free_x")

ggplot(exit_year_nodeath, aes(fill = exit_category, y = n_supp, x = exit_year)) +
  geom_bar(position = "fill", stat="identity", colour = "black") +
  geom_text(position = position_fill(vjust = 0.5), size = 3, show.legend = F,
            aes(group = exit_category, label = paste0(pct_supp, "%"),
                color = exit_category)) +
  scale_color_manual(values = label_col) +
  geom_text(aes(x = exit_year, y = 1.02, label = year_tot_label), vjust = 0, size = 3) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum() +
  labs(title = "Proportion of exit types by year (excluding deaths)",
       x = "Year",
       y = "Percent of exits",
       caption = "NB. KCHA exit data is incomplete prior to October 2015",
       fill = "Exit category") +
  facet_wrap(~agency, scales = "free_x")



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
  geom_text(position = position_fill(vjust = 0.5), size = 2, show.legend = F,
            aes(group = exit_category, label = paste0(pct_supp, "%"),
                color = exit_category)) +
  scale_color_manual(values = label_col) +
  geom_text(aes(x = exit_year, y = 1.03, label = year_tot_label), vjust = 0, size = 2) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 1.05)) +
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
  geom_text(position = position_fill(vjust = 0.5), size = 2, show.legend = F,
            aes(group = exit_category, label = paste0(pct_supp, "%"),
                color = exit_category)) +
  scale_color_manual(values = label_col) +
  geom_text(aes(x = exit_year, y = 1.03, label = year_tot_label), vjust = 0, size = 2) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 1.05)) +
  theme_ipsum(axis_text_size = 10) +
  labs(title = "Proportion of exit types by year and age group (excluding deaths)",
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


