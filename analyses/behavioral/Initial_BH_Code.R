##01/21/2022
# #Initial Code--Behavioral Analysis
# Making some descriptive tables!
# 
# # #Steps
# # 1) Connect to HHSAW SQL server
# # 2) Read in appropriate data tables
#     a) Table that includes full Medicaid coverage for 7 and 11 months with
#        exits and matched non-exits
# # 3) Modify Alastair's code to make demographic comparisons
#     a) Descriptives by exit type
#     b) Differences in Medicaid coverage by exit type
#     c) Differences in Medicaid continuation after coverage

#Connect to HHSAW Server
library(DBI)
db_hhsaw <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = "tcp:kcitazrhpasqlprp16.azds.kingcounty.gov,1433",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryPassword")

#Load necessary packages
pacman::p_load(data.table, DBI, glue, dplyr, tidyr, lubridate)

# #Load data table 
# #Load some data in to test
# #Load some data in to test
# ##Relevant tables to work with
# control_match_covariate <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[control_match_covariate]"))
# 
# control_match_long <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[control_match_long]"))
# 
# control_match <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[control_match]"))
# 
# ##Left join this table with Medicaid ID table
# control_match_id_mcaid <- dbGetQuery(db_hhsaw,
#                                      "SELECT a.*, b.id_mcaid
#                                      FROM
# (SELECT * FROM hudhears.control_match_long) a
# LEFT JOIN
# (SELECT DISTINCT id_hudhears, id_mcaid
# FROM claims.hudhears_id_xwalk WHERE id_mcaid is not null) b
# ON a.id_hudhears = b.id_hudhears") %>%
#   mutate(exit_1yr_prior = exit_date - years(1) + days(1),
#          exit_1yr_after = exit_date + years(1) - days(1))
# 
# 
# ## Observations off by 300
# # Do antijoin to see why this is happening
# 
# mis_match<- anti_join(control_match_id_mcaid, control_match_covariate, by=c("id_hudhears"))
# mis_match2<- as.data.frame(mis_match)
# 
# 
# 
# 
# control_match_covariate %>% select(id_hudhears, starts_with("full_")) %>% head() %>% mutate(chk = full_cov_7_prior * full_cov_7_after)


##Join covariate table BH events-includes, crisis, ITAs, and outpatient BH events (all non-Medicaid)
covariate <- dbGetQuery(db_hhsaw, "SELECT * FROM hudhears.control_match_covariate WHERE id_type = 'id_exit'")

exits_bh <- dbGetQuery(db_hhsaw,
                       "SELECT a.id_hudhears, a.gender_me, a.exit_date, DATEADD(year, 1, a.exit_date) AS yr_later, b.crisis_date AS event_date, b.source
                       FROM
                       (SELECT id_hudhears, exit_date, gender_me 
                         FROM hudhears.control_match_covariate 
                         WHERE id_type = 'id_exit' AND exit_death = 0 AND exit_date <= '2018-12-31') a
                       LEFT JOIN
                       (SELECT DISTINCT id_hudhears, program, description, crisis_date, 'crisis' AS source
                         FROM hudhears.bh_crisis_events) b
                       ON a.id_hudhears = b.id_hudhears
                       AND b.crisis_date >= a.exit_date AND b.crisis_date <= DATEADD(year, 1, a.exit_date)
                       --WHERE b.crisis_date IS NOT NULL
                       
                       UNION
                       
                       SELECT x.id_hudhears, x.gender_me, x.exit_date, DATEADD(year, 1, x.exit_date) AS yr_later, y.call_date AS event_date, y.source
                       FROM
                       (SELECT id_hudhears, exit_date, gender_me 
                         FROM hudhears.control_match_covariate 
                         WHERE id_type = 'id_exit' AND exit_death = 0 AND exit_date <= '2018-12-31') x
                       LEFT JOIN
                       (SELECT DISTINCT id_hudhears, call_date, 'ita' AS source FROM hudhears.bh_ita_events) y
                       ON x.id_hudhears = y.id_hudhears
                       AND y.call_date >= x.exit_date AND y.call_date <= DATEADD(year, 1, x.exit_date)
                       --WHERE y.call_date IS NOT NULL)

                       UNION
SELECT z.id_hudhears, z.gender_me, z.exit_date, DATEADD(year, -1, z.exit_date) AS yr_before, d.event_date AS event_date, d.source
FROM
(SELECT id_hudhears, exit_date, gender_me 
  FROM hudhears.control_match_covariate 
  WHERE id_type = 'id_exit' AND exit_death = 0) z
LEFT JOIN
(SELECT DISTINCT id_hudhears, event_date, 'outpt' AS source FROM hudhears.bh_outpatient_events) d
ON z.id_hudhears = d.id_hudhears
AND d.event_date <= z.exit_date AND d.event_date >= DATEADD(year, -1, z.exit_date)
WHERE d.event_date IS NOT NULL
                       ")
 #Get exit type variable from relevant table
#Note: only getting that one variable
pha_stage <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.stage_pha_exit_timevar WHERE act_date IS NOT NULL")




exits_bh_type <- left_join(exits_bh, distinct(pha_stage, id_hudhears, act_date, exit_category), by=c("id_hudhears", "exit_date"="act_date"))




exits_bh_sum <- exits_bh %>% group_by(id_hudhears, exit_date, exit_category) %>% summarise(event_count = n_distinct(event_date, na.rm = T)) %>% ungroup() 
exits_bh %>% filter(source %in% c("crisis", "ita"))%>%summarize(events=n_distinct (id_hudhears))



