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
               "SELECT DISTINCT a.id_hudhears, b.program, b.auth_no, c.description, b.start_date, d.event_date,
               coalesce(d.event_date, b.start_date) as crisis_date
               INTO hudhears.bh_crisis_events
                 FROM
                 (SELECT DISTINCT id_hudhears, kcid FROM amatheson.hudhears_xwalk_ids
                   WHERE kcid IS NOT NULL) a
                 INNER JOIN
                 (SELECT kcid, auth_no, program, start_date FROM bhrd.au_master
                   WHERE program IN ('13', '15', '40', '74', '75', '76', '79', '80', 
                   '120', '125', '160', '176', 'CTU', 'HL')
                      AND status_code in ('TM','AA')) b
                 ON a.kcid = b.kcid
                 LEFT JOIN
                 (SELECT program, description FROM bhrd.sp_program) c
                 ON b.program = c.program
                 LEFT JOIN 
                 (SELECT DISTINCT auth_no, event_date FROM bhrd.ea_cpt_service 
                   WHERE source_id NOT IN(3, 6, 7, 8, 10)) d
                 ON b.auth_no = d.auth_no")


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
                 -- Total Number of ITA Investigations
                 CASE WHEN disp.[investigation] = 'Y' then 1 else 0 END AS [ITA_INVEST],  
                 -- Total number of ITA investigations resulting in detention or revocation.
                 CASE when disp.[detained] = 'Y' THEN 1 else 0 END AS [ITA_DETAINED], 
                 CASE WHEN [detained] = 'Y' AND cc.[disposition] <> 'FR' THEN 1 ELSE 0 END AS [ITA_NEW_DETAINED],
                 CASE WHEN [detained] = 'Y' AND cc.[disposition] = 'FR' THEN 1 ELSE 0 END AS [ITA_REVOKED_DETAINED],
                 CASE WHEN [disposition_type] ='M' and [detained] = 'Y' then 1 else 0 END as [ITA_DETAINED_MH],
                 CASE WHEN [disposition_type] ='S' and [detained] = 'Y' then 1 else 0 END as [ITA_DETAINED_SUD],
                 CASE when disp.voluntary = 'Y' then 1 else 0 END AS [ITA_REF_VOLUNTARY],
                 CASE when disp.discharge_referral = 'Y' then 1 else 0 END AS [ITA_REF_OUTPATIENT]
                 FROM
                 (SELECT  * FROM [bhrd].[cc_intake]) cc
                 LEFT JOIN 
                 (SELECT * FROM [bhrd].[ref_ita_disposition]) disp
                 ON cc.disposition = disp.disposition
                 WHERE cc.[call_stamp] IS NOT NULL
                 AND cc.[disposition] IS NOT NULL	
                 AND cc.call_stamp >= '2012-01-01' 
                 AND cc.call_stamp  <= '2019-12-31'
                 AND disp.version = '2021') b
               ON a.kcid = b.kcid
               WHERE b.ITA_INVEST = 1 OR b.ITA_DETAINED = 1")


# OUTPATIENT EVENTS ----
# Remove table if it exists
try(DBI::dbExecute(db_hhsaw, "DROP TABLE hudhears.bh_outpatient_events"), silent = T)

DBI::dbExecute(db_hhsaw,
               "SELECT DISTINCT id.id_hudhears, au.auth_no, au.agency_id, au.program, sp.description,
               ea.event_date, 1 AS outpatient_event
               INTO hudhears.bh_outpatient_events
               FROM
               (SELECT DISTINCT id_hudhears, kcid FROM amatheson.hudhears_xwalk_ids
                 WHERE kcid IS NOT NULL) id
               INNER JOIN
               bhrd.au_master au
               ON id.kcid = au.kcid
               INNER JOIN
               bhrd.ea_cpt_service ea
               ON au.auth_no = ea.auth_no
               LEFT JOIN
               bhrd.sp_program sp
               ON au.program = sp.program 
               WHERE ea.event_date BETWEEN au.start_date AND 
                IsNull(au.expire_date, DATEADD(day, sp.maximum_length, au.start_date)) -- open authorizations at the time of the event
               AND sp.action_code in ('A')
               AND au.status_code in ('TM','AA')
               AND sp.loc in ('OP') -- outpatient services
               -- Keep only main benefit outpatient programs
               AND sp.program in ('158', '372', '373', '400', '401', '500', '501')
               AND ea.source_id NOT IN(3, 6, 7, 8, 10) -- exclude system generated services because they do not come from providers")
