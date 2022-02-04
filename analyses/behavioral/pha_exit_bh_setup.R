## Script name: pha_exit_bh_setup
##
## Purpose of script: Create tables used for behavioral health analyses in the HUD HEARS Study 
##
## Author: Alastair Matheson, Public Health - Seattle & King County
## Date Created: 2022-02-02
## Email: alastair.matheson@kingcounty.gov
##
## Notes:
##   
##


# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table)

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


# CRISIS EVENT TABLE ----
# Remove table if it exists
try(DBI::dbExecute(db_hhsaw, "DROP TABLE hudhears.bh_crisis_events"), silent = T)

DBI::dbExecute(db_hhsaw,
               "SELECT x.id_hudhears, MIN(x.first_event) as first_event, MAX(x.last_event) as last_event 
               INTO hudhears.bh_crisis_events
               FROM
               (SELECT a.id_hudhears, b.program, c.description, b.start_date, d.first_service, d.last_service, 
                 coalesce(first_service, start_date) as first_event,
                 coalesce(last_service, start_date) as last_event 
                 FROM
                 (SELECT DISTINCT id_hudhears, kcid FROM amatheson.hudhears_xwalk_ids
                   WHERE kcid IS NOT NULL) a
                 INNER JOIN
                 (SELECT kcid, auth_no, program, start_date FROM bhrd.au_master
                   WHERE program IN ('13', '15', '40', '74', '75', '76', '79', '80', '120', '160', '176', 'HL')) b
                 ON a.kcid = b.kcid
                 LEFT JOIN
                 (SELECT program, description FROM bhrd.sp_program) c
                 ON b.program = c.program
                 LEFT JOIN 
                 (SELECT auth_no, MIN(event_date) AS first_service, MAX(event_date) AS last_service 
                   FROM bhrd.ea_cpt_service 
                   --where source_id NOT IN(3, 6, 7, 8)
                   GROUP BY auth_no) d
                 ON b.auth_no = d.auth_no
               ) x
               GROUP BY x.id_hudhears")


# ITA TABLE ----
# Remove table if it exists
try(DBI::dbExecute(db_hhsaw, "DROP TABLE hudhears.bh_ita_events"), silent = T)

DBI::dbExecute(db_hhsaw,
               "SELECT a.id_hudhears, b.call_date, b.ITA_INVEST, b.ITA_DETAINED, 
               b.ITA_NEW_DETAINED, b.ITA_REVOKED_DETAINED, b.ITA_DETAINED_MH, b.ITA_DETAINED_SUD,
               b.ITA_REF_VOLUNTARY, b.ITA_REF_OUTPATIENT
               INTO hudhears.bh_ita_events
               FROM
               (SELECT DISTINCT id_hudhears, kcid FROM amatheson.hudhears_xwalk_ids
                 WHERE kcid IS NOT NULL) a
               INNER JOIN
               (SELECT DISTINCT
                 CAST(cc.call_stamp as date) as call_date,
                 cc.[call_stamp],
                 cc.[intake_no],
                 cc.[kcid],
                 CASE WHEN disp.[investigation] = 'Y' then 1 else 0 END AS [ITA_INVEST], -- Total Number of ITA Investigations 
                 CASE when disp.[detained] = 'Y' THEN 1 else 0 END AS [ITA_DETAINED], -- Total number of ITA investigations resulting in detention or revocation.
                 CASE WHEN [detained] = 'Y' AND cc.[disposition] <> 'FR' THEN 1 ELSE 0 END AS [ITA_NEW_DETAINED],
                 CASE WHEN [detained] = 'Y' AND cc.[disposition] = 'FR' THEN 1 ELSE 0 END AS [ITA_REVOKED_DETAINED],
                 CASE WHEN [disposition_type] ='M' and [detained] = 'Y' then 1 else 0 END as [ITA_DETAINED_MH],
                 CASE WHEN [disposition_type] ='S' and [detained] = 'Y' then 1 else 0 END as [ITA_DETAINED_SUD],
                 CASE when disp.voluntary = 'Y' then 1 else 0 END AS [ITA_REF_VOLUNTARY],
                 CASE when disp.discharge_referral = 'Y' then 1 else 0 END AS [ITA_REF_OUTPATIENT]
                 FROM
                 (SELECT TOP 10000 * FROM [bhrd].[cc_intake]) cc
                 LEFT JOIN 
                 (SELECT * FROM [bhrd].[ref_ita_disposition]) disp
                 ON cc.disposition = disp.disposition
                 WHERE cc.[call_stamp] IS NOT NULL
                 AND cc.[disposition] IS NOT NULL	
                 AND cc.call_stamp >= '2012-01-01' 
                 AND cc.call_stamp  <= '2019-12-31'
                 AND disp.version = '2021') b
               ON a.kcid = b.kcid")


