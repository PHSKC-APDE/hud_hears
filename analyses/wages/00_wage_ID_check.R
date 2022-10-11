# Header ----
# Author: Danny Colombara
# Date October 11, 2022
# R version 4.2.1
# Purpose: Check how much data we have for our HUD HEARS <> wage linkage

# Set up ----
library(DBI)
library(data.table)
library(rads)

# functions ----
devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/apde/main/R/create_db_connection.R")

cleanssn <- function(DTx, ssnx = NULL){
  # ensure nine digits long ----
  DTx[, paste0(ssnx) := as.character(get(ssnx))]
  DTx[, paste0(ssnx) := gsub('\\D+','', get(ssnx))] # only keep numbers phone number strings
  DTx[nchar(get(ssnx)) > 9, paste0(ssnx) := NA] # drop if SSN is > 9 digits
  DTx[nchar(get(ssnx)) < 9, paste0(ssnx) := gsub(" ", "0", sprintf("%9i", as.numeric(get(ssnx))))] # add preceeding zeros to make 9 digits
  
  return(DTx)
  
}

# connect ----
hhsaw20 = create_db_connection( # this is dev
  server = "hhsaw", 
  prod = F, 
  interactive = F
)

hhsaw16 = create_db_connection( # this is prod
  server = "hhsaw", 
  prod = T, 
  interactive = F
)

dw_inthealth <- create_db_connection(
  server = "inthealth", 
  interactive = F, 
  prod = F)


# ESD wage SSN ----
wagesize <- setDT(DBI::dbGetQuery(conn = dw_inthealth, "SELECT count(*) FROM [stg_esd].[raw_KCWage2]")) 
rawwage <- setDT(DBI::dbGetQuery(conn = dw_inthealth, "SELECT distinct(SSN) FROM [stg_esd].[raw_KCWage2]")) 
setnames(rawwage, "SSN", "ssn")
nrow(rawwage) # 765,593 unique SSN in KCWage2

cleanssn(rawwage, "ssn") # properly format SSN


# Xwalk SSN ----
xwalk <- setDT(DBI::dbGetQuery(conn = hhsaw16, "SELECT * FROM dcolombara.hudhears_esd_xwalk"))
cleanssn(xwalk, "ssn")
length(unique(xwalk$ssn))


# HUD HEARS age ----
hh <- setDT(DBI::dbGetQuery(conn = hhsaw16, 
                                      "SELECT id_hudhears,
                                        age = age_at_exit
                                        FROM [hudhears].[control_match_covariate]")) 

hh <- merge(hh, xwalk, 'id_hudhears', all = F)
hh <- hh[, id_hudhears := NULL]
setorder(hh, ssn, -age) # oldest age per SSN first
hh[, dup := 1:.N, ssn]
hh <- hh[dup == 1] # keep oldest age per SSN
hh[, dup := NULL]

identical(length(unique(hh$ssn)), nrow(hh)) # confirm SSN are unique
identical(length(unique(hh$ssn)), length(unique(xwalk$ssn))) # confirm didn't lose any xwalk SSN
nrow(hh) # unique SSN with age data HUDHEARS

hh.working.age <- hh[age %in% 18:65]
nrow(hh.working.age) # of HUDHEARS persons with SSN (sent to ESD) who are 18 to 65

# Intersection of working age HUDHEARS SSN and SSN for which we have wages ----
hh.wage <- merge(hh.working.age, rawwage, 'ssn', all = F)
nrow(hh.wage) # working age HUD HEARS pop with wage data

# summary ----
message(paste0(length(unique(xwalk$ssn)), ": Number of SSN provided to ESD in xwalk file"))

message(paste0(wagesize[1,1], ": number of rows in KCwages2"))
message(paste0(nrow(rawwage), ": unique SSN in KCWages2"))

message(paste0(nrow(hh), ": number of HUD HEARS people with SSN in xwalk"))
message(paste0(nrow(hh.working.age), ": number of HUD HEARS people 18-65 with SSN in xwalk"))
message(paste0(nrow(hh.wage), ": number of HUD HEARS 18-65 LINKED TO WAGE data"))
message(paste0(rads::round2(100*nrow(hh.wage)/nrow(hh.working.age), 1), "%: number of HUD HEARS 18-65 LINKED TO WAGE data"))

