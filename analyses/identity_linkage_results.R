## Script name: identity_linkage_results
##
## Purpose of script: Look at the HUD HEARS study ID matching results
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

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table)

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
xwalk <- dbGetQuery(db_hhsaw, 
                    "SELECT DISTINCT id_hudhears,  source_system, id_kcmaster, id_kc_pha,
                    id_mcaid, kcid, id_hmis, id_hchn, customerkey
                    FROM amatheson.hudhears_xwalk_ids")

# Need PHA ID data to filter out people who were only on the waitlist
pha_demo <- odbc::dbGetQuery(db_hhsaw, "SELECT * FROM pha.final_demo")

# All PHA IDs
pha_ids <- odbc::dbGetQuery(db_hhsaw, "SELECT DISTINCT id_kc_pha, id_hash FROM pha.final_identities")

# Raw exit data
pha_exit <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.stage_pha_exit") %>%
  mutate(hh_female = case_when(hh_gender == "Female" ~ 1L,
                               hh_gender == "Male" ~ 0L,
                               TRUE ~ NA_integer_),
         id_hash = as.character(toupper(openssl::sha256(paste(str_replace_na(ssn, ''),
                                                              str_replace_na(lname, ''),
                                                              str_replace_na(fname, ''),
                                                              str_replace_na(mname, ''),
                                                              str_replace_na(dob, ''),
                                                              '', # No gender
                                                              sep = "|")))),
         hh_id_hash = as.character(toupper(openssl::sha256(paste(str_replace_na(hh_ssn, ''),
                                                                 str_replace_na(hh_lname, ''),
                                                                 str_replace_na(hh_fname, ''),
                                                                 '', # No HH mname
                                                                 str_replace_na(hh_dob, ''),
                                                                 str_replace_na(hh_female, ''),
                                                                 sep = "|"))))
  )

# Exit timevar
# Combined exit/timevar data
exit_timevar <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.stage_pha_exit_timevar") %>%
  mutate(portfolio_final = ifelse(str_detect(portfolio_final, "LAKE CITY"),
                                  "LAKE CITY COURT", portfolio_final))



# SUMMARIZE EXIT/50058 LINKAGE ----
# Number exit events
pha_exit %>%
  distinct(pha_source, id_hash, hh_id_hash, act_date) %>%
  group_by(pha_source) %>%
  summarise(cnt = n())

# Number IDs
pha_exit %>%
  distinct(pha_source, id_hash, hh_id_hash) %>%
  group_by(pha_source) %>%
  summarise(cnt = n())

# Number that joined
exit_timevar %>% filter(!is.na(act_date)) %>%
  distinct(hh_id_kc_pha, act_date) %>% summarise(cnt = n())



# SUMMARIZE HUD HEARS LINKAGES FOR PHA IDS ----
source_count <- xwalk %>% 
  filter(!is.na(id_kc_pha)) %>%
  inner_join(., distinct(pha_demo, id_kc_pha), by = "id_kc_pha") %>%
  distinct(id_hudhears) %>%
  left_join(., xwalk, by = "id_hudhears") %>%
  group_by(id_hudhears) %>%
  mutate(sources = n_distinct(source_system) - 1) %>%
  ungroup()


# Reshape to find most common matches
source_count_summ <- source_count %>%
  distinct(id_hudhears, source_system, sources) %>%
  mutate(value = 1L) %>%
  pivot_wider(., id_cols = c("id_hudhears", "sources"), names_from = source_system, values_fill = 0L)

source_count_summ %>% group_by(BHDATA, MEDICAID, esd, HMIS_CLIENT, HCHN) %>%
  summarise(count = n()) %>% ungroup() %>%
  arrange(-count) %>%
  head()

# Demogs of who did/didn't match
# There are slightly more (176,290 vs 175,916) id_kc_pha values than id_hudhears values
# Need to pick which demogs to use
# Randomly select one
pha_match_demogs <- pha_demo %>%
  mutate(race_eth_me = ifelse(race_latino == 1, "Latino", race_eth_me)) %>%
  left_join(., select(source_count, id_hudhears, sources, id_kc_pha), by = "id_kc_pha") %>%
  filter(!is.na(id_hudhears))
set.seed(98104)
pha_match_demogs <- pha_match_demogs %>%
  mutate(any_match = sources >= 1,
         chooser = runif(nrow(pha_match_demogs), 0, 1)) %>%
  group_by(id_hudhears) %>%
  mutate(chooser_max = max(chooser),
         rows = n()) %>%
  ungroup() %>%
  filter(chooser == chooser_max)


pha_match_demogs %>% 
  mutate(age_2019 = floor(interval(start = dob, end = "2019-12-31") / years(1)), 
         # Remove spurious ages
         age_2019 = ifelse(age_2019 > 125, NA_integer_, age_2019),
         senior = case_when(age_2019 >= 62 ~ 1L, age_2019 < 62 ~ 0L),
         child = case_when(age_2019 < 18 ~ 1L, age_2019 >= 18 ~ 0L)) %>%
  filter(!is.na(age_2019)) %>%
  group_by(any_match) %>%
  summarise(age_mean = mean(age_2019),
            age_med = median(age_2019),
            age_min = min(age_2019),
            age_max = max(age_2019),
            senior = mean(senior),
            child = mean(child))


pha_match_demogs %>% 
  count(any_match, gender_me) %>%
  group_by(any_match) %>%
  mutate(tot = sum(n), pct = round(n/tot*100,1))

pha_match_demogs %>% 
  count(any_match, race_eth_me) %>%
  group_by(any_match) %>%
  mutate(tot = sum(n), pct = round(n/tot*100,1))


