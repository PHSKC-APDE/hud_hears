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
exit_timevar <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.stage_pha_exit_timevar")

# Demographic table
pha_demo <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.final_demo")

# Calyear table
pha_calyear <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.final_calyear")

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
  mutate(year = year(act_date)) %>%
  anti_join(., distinct(exit_timevar, id_kc_pha, act_date) %>% mutate(year = year(act_date)), 
            by = c("id_kc_pha", "year"))
  
only_58 %>% count(year)

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
  distinct(id_kc_pha, act_date) %>%
  mutate(year = year(act_date)) %>%
  anti_join(., mutate(exits_58, year = year(act_date)), by = c("id_kc_pha", "year"))

only_exit %>% count(year)


# EXITS BY YEAR ----
## Any exit vs not ----
exit_count <- function(year) {
  exits <- exit_demogs %>%
    filter(exit_year == year | (year >= year(from_date) & year <= year(to_date))) %>%
    group_by(id_kc_pha) %>%
    # Only people who had no activity after an exit are truly exited
    mutate(exited = max(exit_year == year & activity_mismatch == 0),
           exited = replace_na(exited, 0)) %>%
    ungroup() %>%
    distinct(agency, id_kc_pha, exited) %>%
    group_by(agency) %>%
    count(exited) %>%
    mutate(year_tot = sum(n)) %>%
    ungroup() %>%
    mutate(pct = round(n / year_tot * 100, 1)) %>%
    mutate(year = year)
}



## Exit types ----
exit_year <- exit_timevar %>% 
  # Only keep people who had no activity after an exit
  filter(!is.na(act_date) & activity_mismatch == 0) %>%
  distinct(id_kc_pha, agency, act_date, exit_category) %>%
  mutate(year = year(act_date)) %>%
  group_by(agency, year) %>%
  count(exit_category) %>%
  mutate(year_tot = sum(n)) %>%
  ungroup() %>%
  mutate(pct = round(n / year_tot * 100, 1))

# Trick from scales::show_col
# (https://stackoverflow.com/questions/62800823/set-a-color-to-show-clear-the-numbers)
hcl <- farver::decode_colour(viridisLite::viridis(length(unique(exit_year$exit_category))), "rgb", "hcl")
label_col <- ifelse(hcl[, "l"] > 50, "black", "white")

# # Line graph showing numbers over time
# ggplot(exit_year, aes(group = exit_category, y = n, x = year, color = exit_category)) + 
#   geom_line(size = 1) +
#   scale_color_viridis(discrete = T) +
#   theme_ipsum() + 
#   labs(title = "Number of exits from KCHA and SHA",
#        x = "Year",
#        y = "Number of exits",
#        caption = "NB. KCHA exit data is incomplete prior to October 2015") +
#   facet_wrap(~agency)



# Stacked percent bar graph of any exit/no exit
exit_demogs_sum_collapse %>%
  filter(category == "agegrp_expanded" & !is.na(group)) %>%
  ggplot(aes(fill = as.character(exit), y = n, x = year)) +
  geom_bar(position = "fill", stat="identity", colour = "black") +
  geom_text(position = position_fill(vjust = 0.5), size = 2, show.legend = F,
            aes(group = exit, label = paste0(pct, "%"),
                color = exit)) +
  scale_color_manual(values = label_col) +
  geom_text(aes(x = year, y = 1.02, label = year_tot), vjust = 0, size = 2) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum(axis_text_size = 10) +
  labs(title = "Proportion of age group exiting by year",
       x = "Year",
       y = "Percent of people",
       caption = "NB. KCHA exit data is incomplete prior to October 2015",
       fill = "Exit vs. not") +
  facet_grid(group ~ agency)

# # Stacked percent bar graph
# ggplot(exit_year, aes(fill = exit_category, y = n, x = year)) + 
#   geom_bar(position = "fill", stat="identity", colour = "black") +
#   geom_text(position = position_fill(vjust = 0.5), size = 2.5, show.legend = F,
#             aes(group = exit_category, label = paste0(pct, "%"),
#                 color = exit_category)) +
#   scale_color_manual(values = label_col) +
#   geom_text(aes(x = year, y = 1.02, label = year_tot), vjust = 0, size = 3) +
#   scale_fill_viridis(discrete = T) +
#   scale_y_continuous(labels = scales::percent) +
#   theme_ipsum() +
#   labs(title = "Proportion of exit types by year",
#        x = "Year",
#        y = "Percent of exits",
#        caption = "NB. KCHA exit data is incomplete prior to October 2015",
#        fill = "Exit category") +
#   facet_wrap(~agency)


## Set up key values for markdown outpuyt ----
kcha_max_pos <- exit_year %>% 
  filter(agency == "KCHA" & exit_category == "Positive") %>%
  filter(pct == max(pct))
sha_max_pos <- exit_year %>% 
  filter(agency == "SHA" & exit_category == "Positive") %>%
  filter(pct == max(pct))



# DEMOGS OF PEOPLE EXITING ----
exit_demogs <- left_join(exit_timevar, pha_demo, by = "id_kc_pha") %>%
  mutate(exit_year = year(act_date))

# Look at demogs of people who exit vs don't in a given year
exit_demogs_year <- function(year) {
  exits <- exit_demogs %>%
    filter(exit_year == year | (year >= year(from_date) & year <= year(to_date))) %>%
    group_by(id_kc_pha) %>%
    mutate(exited = max(exit_year == year & activity_mismatch == 0)) %>%
    ungroup() %>%
    mutate(year = year) %>%
    select(-disability) %>%
    left_join(., select(pha_calyear, id_kc_pha, year, time_housing, agegrp_expanded, disability), 
              by = c("id_kc_pha", "year"))
  
  demog_list <- c("race_eth_me", "gender_recent", "agegrp_expanded", "time_housing", "disability")
  
  exit_sum <- bind_rows(lapply(demog_list, function(x) {
    exits %>%
      filter(exited == 1 & !is.na(exited)) %>%
      distinct(agency, id_kc_pha, act_date, exit_category, across(all_of(x))) %>%
      group_by(agency, across(all_of(x))) %>%
      count(exit_category) %>%
      mutate(year_tot = sum(n)) %>%
      ungroup() %>%
      mutate(pct = round(n / year_tot * 100, 1),
             category = rlang::as_name(x)) %>%
      rename(group = x) %>%
      mutate(group = as.character(group))
  }))
  
  non_exit_sum <- bind_rows(lapply(demog_list, function(x) {
    exits %>%
      filter(exited == 0 | is.na(exited)) %>%
      distinct(agency, id_kc_pha, act_date, exit_category, across(all_of(x))) %>%
      group_by(agency, across(all_of(x))) %>%
      summarise(n = n_distinct(id_kc_pha)) %>%
      mutate(year_tot = sum(n)) %>%
      ungroup() %>%
      mutate(pct = round(n / year_tot * 100, 1),
             category = rlang::as_name(x)) %>%
      rename(group = x) %>%
      mutate(group = as.character(group))
  }))
  
  output <- bind_rows(mutate(exit_sum, exit = 1L), 
                      mutate(non_exit_sum, exit = 0L)) %>%
    mutate(year = year) %>%
    select(year, agency, category, group, exit, exit_category, n, year_tot, pct) %>%
    arrange(year, agency, category, group, exit, exit_category)
  return(output)
}
  
exit_demogs_sum <- bind_rows(lapply(c(2012:2020), exit_demogs_year))

# Duplicate group values to make highlighted gpplots
exit_demogs_sum <- exit_demogs_sum %>% mutate(group2 = group)


## Make a higher level summaries ----
# Count of all exits vs not
exit_sum_collapse <- exit_demogs_sum %>%
  group_by(year, agency, category, group, group2, exit) %>%
  summarise(n = sum(n)) %>%
  group_by(year, agency, category, group, group2) %>%
  mutate(year_tot = sum(n)) %>%
  ungroup() %>%
  mutate(pct = round(n / year_tot * 100, 1))

# Count of all exits by group
exit_demogs_sum_collapse <- exit_demogs_sum %>%
  group_by(year, agency, category, group, group2, exit) %>%
  summarise(n = sum(n)) %>%
  group_by(year, agency, category, group, group2) %>%
  mutate(year_tot = sum(n)) %>%
  ungroup() %>%
  mutate(pct = round(n / year_tot * 100, 1))


# # Line graph showing numbers over time by age group
# exit_demogs_sum_collapse %>%
#   filter(category == "agegrp_expanded" & !is.na(group) & exit == 1) %>%
#   ggplot(aes(y = n, x = year)) + 
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

# # Stacked percent bar graph of exit type by age group
# exit_demogs_sum %>%
#   filter(category == "agegrp_expanded" & !is.na(group) & exit == 1) %>%
# ggplot(aes(fill = exit_category, y = n, x = year)) + 
#   geom_bar(position = "fill", stat="identity", colour = "black") +
#   geom_text(position = position_fill(vjust = 0.5), size = 2, show.legend = F,
#             aes(group = exit_category, label = paste0(pct, "%"),
#                 color = exit_category)) +
#   scale_color_manual(values = label_col) +
#   geom_text(aes(x = year, y = 1.02, label = year_tot), vjust = 0, size = 2) +
#   scale_fill_viridis(discrete = T) +
#   scale_y_continuous(labels = scales::percent) +
#   theme_ipsum(axis_text_size = 10) +
#   labs(title = "Proportion of exit types by year and age group",
#        x = "Year",
#        y = "Percent of exits",
#        caption = "NB. KCHA exit data is incomplete prior to October 2015",
#        fill = "Exit category") +
#   facet_grid(group ~ agency)



# MAKE MARKDOWN DOC ----
render(file.path(here::here(), "analyses/pha_exit_demogs.Rmd"), "html_document")
