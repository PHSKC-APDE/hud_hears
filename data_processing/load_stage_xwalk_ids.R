# HEADER ----
# Author: Alastair Matheson, 2021-09-20
#
# Purpose: Create master ID linkage file for the HUD HEARS project, identifying people across Mcaid, Mcare, and PHA with a NEW HUD HEARS study ID
#


# OVERVIEW ----
# 1) Prepare Medicaid/BHRD/HMIS identifiers
# 2) Prepare PHA identifiers
# 3) Prepare ESD identifiers (to come)
# 4) Prepare Medicare identifiers (to come)
# 5) Link IDs
# 6) Retain history of linkages


# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, RecordLinkage, lubridate)


start.time <- Sys.time()

# FUNCTIONS ... general data cleaning / prep procedures ----
## Data prep ... clean names ----
prep_names <- function(dt){
  # Set up columns to work on
  cols <- c("lname", "fname", "mname", "lname_alt", "lname_suff")
  
  # Set up suffixes to remove
  suffix <- c(" SR", " JR", "-JR", "JR", "JR I", "JR II", "JR III", " II", " III", " IV")
  
  # Remove any extraneous spaces at the beginning or end of a name
  for(v in intersect(cols, names(dt))) {
    set(dt, i = NULL, j = v, value = str_squish(dt[[v]]))
  }
  
  dt <- unique(dt)
  
  # Clean up names and remove extraneous characters
  for(v in intersect(cols, names(dt))) {
    set(dt, i = NULL, j = v, value = toupper(dt[[v]]))
    set(dt, i = NULL, j = v, value = str_replace_all(dt[[v]], "_|-", " "))
    set(dt, i = NULL, j = v, value = str_replace_all(dt[[v]], "NULL|\\.|\"|\\\\|'|`|[0-9]|\\*", ""))
    set(dt, i = NULL, j = v, value = str_replace_all(dt[[v]], "^NONE$|^DONT KNOW$", ""))
    set(dt, i = NULL, j = v, value = str_replace_all(dt[[v]], "^REFUSED$|^CONSENT$|XXREFUSEDXX", ""))
    set(dt, i = NULL, j = v, value = str_replace_all(dt[[v]], paste(suffix, "$", collapse="|", sep = ""), ""))
    set(dt, i = NULL, j = v, value = str_squish(dt[[v]]))
    set(dt, i = NULL, j = v, value = ifelse(dt[[v]] == "", NA_character_, dt[[v]]))
    set(dt, i = NULL, j = v, value = toupper(dt[[v]]))
  }
  
  # Clean up where middle initial seems to be in first name field
  # NOTE: There are many rows with a middle initial in the fname field AND 
  # the mname field (and the initials are not always the same).
  # In those situations, keep existing middle initial and drop from fname if
  # the initial is the same
  dt[, f_init := str_sub(fname, -2, -1)]
  dt[, mname := case_when(str_detect(f_init, "[:space:][A-Z]") & is.na(mname) ~ str_sub(fname, -1),
                         TRUE ~ mname)]
  dt[, fname := case_when(str_detect(f_init, "[:space:][A-Z]") & str_sub(fname, -1) == mname ~ 
                            str_sub(fname, 1, -3),
                          TRUE ~ fname)]
  # when middle initial is not a letter, replace it with NA
  dt[, mname := str_remove_all(mname, "(\\d|[:punct:])")]
  # Remove any first name unknown values
  dt[fname == "FNU", fname := NA_character_]
  # Flag baby, institutional, and other names for cleaning
  dt[, drop_name := case_when(fname %in% c("UNBORN", "BABY", "MIRACLE", "CHILD") & 
                               lname %in% c("UNBORN", "BABY", "CHILD") ~ 1L,
                              fname == "XX" & str_detect(lname, "^TWO|^THREE|^FOUR") ~ 1L,
                              lname == "YWCA" ~ 1L,
                              lname == "ELDER" & fname == "PLACE" ~ 1L,
                              lname == "ELDER PLACE" ~ 1L,
                              str_detect(lname, "LIVE IN") ~ 1L,
                              str_detect(fname, "LIVE IN") ~ 1L,
                              TRUE ~ 0L)]
  # Clean baby and other names
  for(v in intersect(cols, names(dt))) {
    set(dt, i = dt[drop_name == 1, which = T], j = v, value = NA_character_)
  }
  dt[fname %in% c("UNBORN", "BABY"), fname := NA_character_]
  dt[, f_init := NULL]
  dt[, drop_name := NULL]
  dt <- dt[!(lname == "DUFUS" & fname == "IAM")]
 
  
  # Do one more clean up to convert blanks to NA
  for(v in intersect(cols, names(dt))) {
    set(dt, i = NULL, j = v, value = ifelse(dt[[v]] == "", NA_character_, dt[[v]]))
  }
  
  return(dt)
}

## Data prep ... clean dob ----
prep_dob <- function(dt){
  # Extract date components
  dt[, dob_y := as.integer(lubridate::year(dob))] # extract year
  dt[, dob_m := as.integer(lubridate::month(dob))] # extract month
  dt[, dob_d := as.integer(lubridate::day(dob))] # extract day
  return(dt)
}

## Data prep .... clean sex ----
prep_sex <- function(dt){
  # Change sex to numeric for improved strcmp function
  dt[gender == "Multiple", gender := 0L]
  dt[gender %in% c("Male", "M"), gender := 1L]
  dt[gender %in% c("Female", "F"), gender := 2L]
  dt[gender %in% c("Unknown", "NULL"), gender := NA_integer_]
  dt[, gender := as.integer(gender)]
  
  return(dt)
}

## Data prep .... clean SSN ----
prep_ssn <- function(dt){
  # Strip any non-numeric values
  dt[, ssn := str_remove_all(ssn, "\\D+")]
  #
  dt[, ssn := case_when(ssn %in% c("010010101", "011111111", "011223333", 
                                   "111111111", "112234455", "111119999", "123121234", "123123123", "123456789", 
                                   "222111212", "222332222",
                                   "333333333", "444444444", 
                                   "555112222", "555115555", "555555555", "555555566",
                                   "699999999",  
                                   "888888888", "898989898", "898888899") ~ NA_character_,
                        as.numeric(ssn) < 1000000 | as.numeric(ssn) >= 900000000 ~ NA_character_,
                        between(as.numeric(ssn), 666000000, 666999999) ~ NA_character_,
                        str_sub(ssn, -4, -1) == "0000" | str_sub(ssn, 1, 3) == "999" |
                          str_detect(ssn, "XXX|A00-0|A000") ~ NA_character_,
                        TRUE ~ ssn)]
  # Try and restore any dropped leading zeros
  dt[, ssn := str_pad(round(as.numeric(ssn), digits = 0), width = 9, side = "left", pad = "0")]
  
  return(dt)
}


## Consolidate cluster IDs across identities ----
# Adaptation of Carolina's code
# From here: https://github.com/DCHS-PME/PMEtools/blob/master/R/idm_dedup.R
# pairs_input = Output from a RecordLinkage getPairs function
# df = The data frame that was fed into the matching process. 
#      Must have rowid and id_hash fields
# iteration = What match cycle this is (affects cluster ID suffix)

match_process <- function(pairs_input, df, iteration) {
  ### Attach ids for each individual pairwise link found ----
  pairs <- pairs_input %>%
    distinct(id1, id2) %>%
    left_join(df, ., by = c(rowid = "id1"))
  pairs <- setDF(pairs)
  
  ### Roll up pair combinations ----
  # self-join to consolidate all pair combinations for clusters with > 2 identities linked 
  # roll up cluster id correctly with coalesce
  # formula for how many other_pair2 records should exist for n number of matching records: 
  #   (n*(n-1)/2) + 1 - e.g. 3 carolina johnsons will have 4  records (3*2/2+1)
  remaining_dupes <- sum(!is.na(pairs$id2))
  
  # while loop self-joining pairs until no more open pairs remain
  recursion_level <- 0
  recursion_df <- pairs %>% rename(id2_recur0 = id2)
  while (remaining_dupes > 0) {
    recursion_level <- recursion_level + 1
    print(paste0(remaining_dupes, " remaining duplicated rows. Starting recursion iteration ", recursion_level))
    recursion_df <- pairs %>%
      self_join_dups(base_df = ., iterate_df = recursion_df, iteration = recursion_level)
    remaining_dupes <- sum(!is.na(recursion_df[ , paste0("id2_recur", recursion_level)]))
  }
  
  # identify full list of id columns to coalesce after recursion
  recurcols <- tidyselect::vars_select(names(recursion_df), matches("_recur\\d")) %>%
    sort(decreasing = T)
  coalesce_cols <- c(recurcols, "rowid")
  coalesce_cols <- rlang::syms(coalesce_cols)
  
  # coalesce recursive id columns in sequence to generate single common cluster ID
  pairsend <- recursion_df %>%
    mutate(clusterid = coalesce(!!!coalesce_cols)) %>%
    rename(id2 = id2_recur0) %>%
    select(-contains("_recur")) %>%
    distinct()
  
  # identify any unclosed cluster groups (open triangle problem), resulting in duplicated cluster
  double_dups <- setDT(pairsend %>% select(rowid, clusterid))
  double_dups <- unique(double_dups)
  double_dups <- double_dups[, if(.N > 1) .SD, by = "rowid"]
  
  if (nrow(double_dups) > 0) {
    double_dups[, row_min := min(rowid), by = "clusterid"]
    # See if there are still any open triangles
    double_dups[, rows_per_id := uniqueN(row_min), by = "rowid"]
    
    if (max(double_dups$rows_per_id) == 2) {
      message("There are open relationships with 2 dimensions")
      double_dups[, back_join_id := min(row_min), by = "rowid"]
      double_dups[, row_min := NULL]
    } else if (max(double_dups$rows_per_id) == 3) {
      message("There are open relationships with 3 dimensions")
      # If the min of a clusterid points to a row that has a lower min, point to that lower min instead
      rowid_min <- double_dups[, rowid_min := min(row_min), by = "rowid"]
      rowid_min <- unique(rowid_min[, .(rowid, rowid_min)])
      # Join back to data to keep min
      double_dups <- merge(double_dups, rowid_min, by.x = "row_min", by.y = "rowid", all.x = T)
      double_dups[, back_join_id := min(rowid_min.y), by = "rowid"]
      double_dups[, rowid_min.x := NULL]
      double_dups[, rowid_min.y := NULL]
    } else if (max(double_dups$rows_per_id) >= 3) {
      stop("Need to check the the logic for double_dups works when rows_per_id > 3")
    } else {
      setnames(double_dups, "row_min", "back_join_id")
    }
    double_dups[, rowid := NULL]
    double_dups[, rows_per_id := NULL]
    double_dups <- unique(double_dups)
    double_dups <- setDF(double_dups)
  }
  
  # collapse duplicate partial clusters to one cluster
  # error checking to make sure that correct total clusters are maintained
  if (nrow(double_dups) > 0) {
    message("Collapsing partial clusters")
    pairsend <- left_join(pairsend, double_dups, by = c("clusterid" = "clusterid")) %>%
      mutate(clusterid2 = coalesce(back_join_id, clusterid))
    
    message("There are ", sum(pairsend$clusterid != pairsend$clusterid2), 
            " mismatched clusterid/clusterid2 combos and at least ",
            nrow(double_dups)*2, " expected")
    
    pairsend <- pairsend %>%
      mutate(clusterid = clusterid2) %>%
      select(-clusterid2, -back_join_id)
  }
  
  ### Add identifiers/unique ids for paired records ----
  # overwrite the original pairs with the consolidated & informed dataframe
  pairs_final <- df %>%
    rename_all(~ paste0(., "_b")) %>%
    left_join(pairsend, ., by = c(id2 = "rowid_b"))
  
  ### Take the union of all unique ids with their cluster ids ----
  # (swinging links from _b cols to unioned rows, and taking distinct)
  # create cluster index
  cluster_index <- select(pairs_final, clusterid, id_hash = id_hash_b) %>%
    drop_na() %>%
    bind_rows(select(pairs_final, clusterid, id_hash)) %>%
    distinct()
  
  ### Check that each personal id only in one cluster ----
  n_pi_split <- setDT(pairs_final %>% select(id_hash, clusterid))
  n_pi_split <- unique(n_pi_split)
  n_pi_split <- n_pi_split[, if(.N > 1) .SD, by = "id_hash"]
  
  if (nrow(n_pi_split)) {
    stop(glue::glue("Deduplication processing error: {nrow(n_pi_split)} ",
                    "clients sorted into more than one cluster. ", 
                    "This is an internal failure in the function and will require debugging. ", 
                    "Talk to package maintainer)"))
  }
  
  ### Report results ----
  n_orig_ids <- df %>% select(id_hash) %>% n_distinct()
  n_cluster_ids <- n_distinct(cluster_index$clusterid)
  
  message("Number of unique clients prior to deduplication: ", n_orig_ids, 
          ". Number of deduplicated clients: ", n_cluster_ids)
  
  
  ### Attach cluster IDS back to base file ----
  output <- left_join(df, 
                      # Set up iteration name
                      rename(cluster_index, 
                             !!quo_name(paste0("clusterid_", iteration)) := clusterid), 
                      by = "id_hash")
  output
}


## Helper functions specifically for client deduplication
#' Function for joining duplicated records to base pair, used in recursive deduplication
#' @param base_df The starting dataframe with initial duplicated pair ids
#' @param iterate_df The df with iterated rowid joins - what is continually updated during recursive pair closing
#' @param iteration Numeric counter indicating which recursion iteration the self-joining loop is on. Used for column name suffixes
self_join_dups <- function(base_df, iterate_df, iteration) {
  joinby <- paste0("rowid_recur", iteration)
  names(joinby) <- paste0("id2_recur", iteration-1)
  
  base_df %>%
    select(rowid, id2) %>%
    rename_all(~paste0(., "_recur", iteration)) %>%
    left_join(iterate_df, ., by = joinby)
}


# This function creates a vector of unique IDs of any length
# id_n = how many unique IDs you want generated
# id_length = how long do you want the ID to get (too short and you'll be stuck in a loop)
id_nodups <- function(id_n, id_length, seed = 202198104) {
  set.seed(seed)
  id_list <- stringi::stri_rand_strings(n = id_n, length = id_length, pattern = "[a-z0-9]")
  
  # If any IDs were duplicated (very unlikely), overwrite them with new IDs
  iteration <- 1
  while(any(duplicated(id_list)) & iteration <= 50) {
    id_list[which(duplicated(id_list))] <- stringi::stri_rand_strings(n = sum(duplicated(id_list), na.rm = TRUE),
                                                                      length = id_length,
                                                                      pattern = "[a-z0-9]")
    iteration <<- iteration + 1
  }
  
  if (iteration == 50) {
    stop("After 50 iterations there are still duplicate IDs. ",
         "Either decrease id_n or increase id_length")
  } else {
    return(id_list)
  }
}


# PREP IDH DATA ----
## (1) Load IDH data from SQL ----  
db_hhsaw <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = "tcp:kcitazrhpasqlprp16.azds.kingcounty.gov,1433",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryPassword")


idh <- setDT(odbc::dbGetQuery(
  db_hhsaw, "SELECT DISTINCT KCMASTER_ID, SOURCE_SYSTEM, MEDICAID_ID, KCID, 
  PERSONAL_ID, HCHN_client_san, FULL_NAME, SSN, DOB, GENDER, ADDRESS, CITY, ZIP,
  FIRST_NAME_ORIG, MIDDLE_NAME_ORIG, LAST_NAME_ORIG, NAME_SUFFIX_ORIG,
  ALIAS_NAME_ORIG, SSN_ORIG, DOB_ORIG, GENDER_ORIG, ADDR1_ORIG, ADDR2_ORIG, CITY_ORIG,
  ZIP_ORIG, STATE_ORIG, AD_MATCH_CODE 
  FROM dwhealth.IM_HISTORY_TABLE
  WHERE SOURCE_SYSTEM IN ('BHDATA', 'HMIS_CLIENT', 'HCHN', 'MEDICAID')"))

setnames(idh, names(idh), 
         c("id_kcmaster", "source_system", "id_mcaid", "kcid", "id_hmis", "id_hchn", 
           "full_name", "ssn", "dob", "gender", "geo_add1_clean", "geo_city_clean", "geo_zip_clean", 
           "fname", "mname", "lname", "lname_suff", "lname_alt", "ssn_raw", "dob_raw", "gender_raw", 
           "geo_add1_raw", "geo_add2_raw", "geo_city_raw", "geo_zip_raw", "geo_state_raw",
           "geo_add_score"))


## (2) Tidy IDH data ----
# Names
idh <- prep_names(idh)
idh <- prep_names(idh) # run second time because some people have suffixes like "blah JR IV", so first removes IV and second removes JR

# Gender
idh <- prep_sex(idh)

# DOB
idh[, dob := as.Date(dob, format = "%m/%d/%Y")]
idh <- prep_dob(idh)

# SSN
idh <- prep_ssn(idh)

# id_hash
idh[, id_hash := as.character(toupper(openssl::sha256(paste(str_replace_na(ssn, ''),
                                                            str_replace_na(id_kcmaster, ''),
                                                            str_replace_na(id_mcaid, ''),
                                                            str_replace_na(kcid, ''),
                                                            str_replace_na(id_hmis, ''),
                                                            str_replace_na(id_hchn, ''),
                                                            str_replace_na(lname, ''),
                                                            str_replace_na(fname, ''),
                                                            str_replace_na(mname, ''),
                                                            str_replace_na(lname_alt, ''),
                                                            str_replace_na(dob_y, ''),
                                                            str_replace_na(dob_m, ''),
                                                            str_replace_na(dob_d, ''),
                                                            str_replace_na(gender, ''),
                                                            sep = "|"))))]
idh <- unique(idh)

## (3) Compare to existing IDs ----
# Placeholder until a table of IDs exists and a history table is set up


# Prep MCARE DATA ----

## NOT RUNNING UNTIL WE RECEIVE MCARE DATA AGAIN ----
# WILL THEN NEED TO OVERHAUL THE PROCESSES BELOW
# 
# ## NOTE TO PREVENT FUTURE INSANITY ----
# # There are id_mcare in names and ssn files that never appear in our MBSS, so it possible for people to match with Mcaid or PHA and not
# # appear in the elig_demo file
# 
# ## (1) Load data from SQL ----  
# db_claims <- dbConnect(odbc(), "PHClaims51")   
# 
# mcare.elig <- setDT(odbc::dbGetQuery(db_claims, "SELECT DISTINCT id_mcare, dob, gender, gender_female, gender_male FROM final.mcare_elig_demo"))
# 
# mcare.names <- setDT(odbc::dbGetQuery(db_claims, "SELECT DISTINCT bene_id AS id_mcare, bene_srnm_name AS lname, 
#                                               bene_gvn_name AS fname, bene_mdl_name AS mname FROM stage.mcare_xwalk_edb_user_view"))
# 
# mcare.ssn <- setDT(odbc::dbGetQuery(db_claims, "SELECT DISTINCT bene_id AS id_mcare, ssn FROM stage.mcare_xwalk_bene_ssn"))
# 
# ## (2) Tidy individual data files before merging ----
# # Keep only unique rows of identifiers within a file
# if(nrow(mcare.elig) - length(unique(mcare.elig$id_mcare)) != 0){
#   stop('non-unique id_mcare in mcare.elig')
# } # confirm all ids are unique in elig data
# 
# mcare.names <- unique(mcare.names)
# if(nrow(mcare.names) - length(unique(mcare.names$id_mcare)) != 0){
#   stop('non-unique id_mcare in mcare.names')
# } # confirm all ids are unique in names data
# 
# if(nrow(mcare.ssn) - length(unique(mcare.ssn$id_mcare)) >0){
#   stop('non-unique id_mcare in mcare.ssn')
# } # confirm all id and ssn are unique
# 
# 
# ## (3) Merge Mcare identifiers together ----
# # for all of WA state, want the most complete dataset possible, regardless of whether missing SSN or any other bit of information
# mcare.dt <- merge(mcare.ssn, mcare.names, by = "id_mcare", all.x=T, all.y = T)  
# if(nrow(mcare.dt) - length(unique(mcare.dt$id_mcare)) != 0){
#   stop('non-unique id_mcare!')
# }
# 
# mcare.dt <- merge(mcare.dt, mcare.elig, by = "id_mcare",  all.x=T, all.y = T)
# if(nrow(mcare.dt) - length(unique(mcare.dt$id_mcare)) != 0){
#   stop('non-unique id_mcare!')
# }
# 
# ## (4) Run cleaning functions on Medicare data ----
# mcare.dt <- prep_names(mcare.dt)
# mcare.dt <- prep_names(mcare.dt) # run second time because some people have suffixes like "blah JR IV", so first removes IV and second removes JR
# mcare.dt <- prep_dob(mcare.dt)
# mcare.dt <- prep_sex(mcare.dt)
# 
# # without ssn, dob, and last name, there is no hope of matching
# # But keep in so all id_mcares are present
# # mcare.dt <- mcare.dt[!(is.na(ssn) & is.na(dob_y) & is.na(lname) ), ] 
# 
# ## (5) Deduplicate when all information is the same (name, SSN, dob, & gender) except id_mcare ----
# # Keep all for now
# 
# ## Identify the duplicates
# mcare.dt[, dup := .N, by = c("lname", "fname", "mname", "ssn", "dob_y", "dob_m", "dob_d", "gender")]
# # mcare.dups <- mcare.dt[dup != 1 & !is.na(lname), ]
# # mcare.nondup <- mcare.dt[!id_mcare %in% mcare.dups$id_mcare ]
# # 
# # # choose the one to keep by the most recent enrollment year for each potential duplicate (from MBSF)
# # mbsf <- setDT(odbc::dbGetQuery(db_claims, "SELECT DISTINCT [bene_id] AS id_mcare, [bene_enrollmt_ref_yr] AS year FROM [PHClaims].[stage].[mcare_mbsf]"))
# # mbsf <- mbsf[id_mcare %in% mcare.dups$id_mcare] # limit to ids that identify a duplicate in mcare.dups
# # mbsf <- unique(mbsf[, .(maxyear = max(year)), by = "id_mcare"])
# # 
# # # merge MBSF max date back onto potential duplicates
# # mcare.dups <- merge(mcare.dups, mbsf, by = "id_mcare")
# # 
# # # keep the most recent year for each set of duplicates
# # mcare.dups[, group := .GRP, by = .(ssn, lname, fname, mname, dob_y, dob_m, dob_d, gender)]
# # mcare.dups <- mcare.dups[mcare.dups[, .I[which.max(maxyear)], by = 'group'][,V1], ] 
# # 
# # # combine non-duplicate and deduplicated data
# # mcare.dt <- rbind(mcare.nondup, mcare.dups, fill = T)[, c("dup", "maxyear", "group") := NULL]
# # rm(mcare.dups, mcare.nondup, mbsf)
# 
# 



# PREP PHA DATA ----
## (1) Load data from SQL ----
db_hhsaw <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = "tcp:kcitazrhpasqlprp16.azds.kingcounty.gov,1433",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryPassword")

pha <- odbc::dbGetQuery(db_hhsaw, "SELECT * FROM pha.final_identities")


## (2) Tidy PHA data ----
# Align fields with earlier prep
pha <- setDT(pha %>%
               mutate(gender = case_when(female == 1 ~ 2L,
                                         female == 0 ~ 1L)))

# Names
pha <- prep_names(pha)

# DOB
pha <- prep_dob(pha)

# SSN
pha <- prep_ssn(pha)

# Set up columns to match other data sets (only those used for matching)
pha[, lname_alt := NA_character_]


# id_hash
pha[, id_hash := as.character(toupper(openssl::sha256(paste(str_replace_na(ssn, ''),
                                                            str_replace_na(pha_id, ''),
                                                            str_replace_na(lname, ''),
                                                            str_replace_na(fname, ''),
                                                            str_replace_na(mname, ''),
                                                            str_replace_na(dob_y, ''),
                                                            str_replace_na(dob_m, ''),
                                                            str_replace_na(dob_d, ''),
                                                            str_replace_na(gender, ''),
                                                            sep = "|"))))]


# Add source
pha[, source_system := "pha"]

# Keep only distinct values
pha <- unique(pha)

## (3) Compare to existing IDs ----
# Placeholder for now until there is a table of existing IDs and history



# PREP ESD DATA ----
## (1) Load data from SQL ----
db_hhsaw_dev <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = "tcp:kcitazrhpasqldev20.database.windows.net,1433",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryPassword")

esd <- setDT(odbc::dbGetQuery(db_hhsaw_dev, "SELECT * FROM esd.stage_elig_id"))


## (2) Tidy PHA data ----
# Rename fields
setnames(esd, names(esd), c("customerkey", "ssn", "fname", "mname", "lname", "dob", "gender"))

# Names
esd <- prep_names(esd)

# Gender
esd <- prep_sex(esd)

# DOB
esd <- prep_dob(esd)

# SSN
esd <- prep_ssn(esd)


# id_hash
esd[, id_hash := as.character(toupper(openssl::sha256(paste(str_replace_na(ssn, ''),
                                                            str_replace_na(customerkey, ''),
                                                            str_replace_na(lname, ''),
                                                            str_replace_na(fname, ''),
                                                            str_replace_na(mname, ''),
                                                            str_replace_na(dob_y, ''),
                                                            str_replace_na(dob_m, ''),
                                                            str_replace_na(dob_d, ''),
                                                            str_replace_na(gender, ''),
                                                            sep = "|"))))]


# Add source
esd[, source_system := "esd"]

# Keep only distinct values
esd <- unique(esd)

## (3) Compare to existing IDs ----
# Placeholder for now until there is a table of existing IDs and history



# COMBINE ALL SOURCES INTO ONE PLACE ----
input <- bind_rows(idh,
                   # mcare.dt,
                   pha,
                   esd) %>%
  select(-starts_with("geo_"), -full_name, -ssn_raw, -dob_raw, -gender_raw, 
         -female, -dob, -last_run, -pha_id) %>%
  distinct() %>%
  # Add phonics and set up a rowid for self-joining later
  mutate(lname_phon = RecordLinkage::soundex(lname),
         lname_alt_phon = RecordLinkage::soundex(lname_alt),
         fname_phon = RecordLinkage::soundex(fname),
         rowid = row_number())



# FIRST PASS: BLOCK ON SSN ----
## Run deduplication ----
# Blocking on SSN or PHA ID and string compare names
st <- Sys.time()
match_01 <- RecordLinkage::compare.dedup(
  input, 
  blockfld = "ssn", 
  strcmp = c("lname", "fname", "mname", "dob_y", "dob_m", "dob_d", "gender"), 
  exclude = c("source_system", "id_kcmaster", "id_mcaid", "kcid", "id_hmis",
              "id_hchn", "lname_suff", "lname_alt",
              # "id_mcare", 
              "id_kc_pha", "lname_phon", "fname_phon", "lname_alt_phon", "rowid", "id_hash"))
message("Pairwise comparisons complete. Total run time: ", round(Sys.time() - st, 2), " ", units(Sys.time()-st))

summary(match_01)


## Add weights and extract pairs ----
# Using EpiLink approach
match_01 <- epiWeights(match_01)
classify_01 <- epiClassify(match_01, threshold.upper = 0.6)
summary(classify_01)
pairs_01 <- getPairs(classify_01, single.rows = TRUE) %>%
  mutate(across(contains("dob_"), ~ str_squish(.)),
         across(contains("dob_"), ~ as.numeric(.)))

## Review output and select cutoff point(s) ----
# pairs_01 %>%
#   filter(Weight >= 0.4) %>%
#   filter(id_kcmaster.1 != id_kcmaster.2 | is.na(id_kcmaster.1) | is.na(id_kcmaster.2)) %>%
#   filter(id_kc_pha.1 != id_kc_pha.2 | is.na(id_kc_pha.1) | is.na(id_kc_pha.2)) %>%
#   filter(source_system.1 == "esd" | source_system.2 == "esd") %>%
#   filter(!((dob_m.1 == "1" & dob_d.1 == "1") | (dob_m.2 == "1" & dob_d.2 == "1"))) %>%
#   # filter(!(dob_m.1 == "1" & dob_d.1 == "1" & dob_m.2 == "1" & dob_d.2 == "1")) %>%
#   # filter(dob_m.1 == "1" & dob_d.1 == "1" & dob_m.2 == "1" & dob_d.2 == "1") %>%
#   # filter(dob_y.1 != dob_y.2) %>%
#   filter(dob_y.1 == dob_y.2 | abs(dob_y.1 - dob_y.2) == 100) %>%
#   # filter(dob_m.1 != dob_m.2 | dob_d.1 != dob_d.2) %>%
#   # filter(dob_m.1 == dob_d.2 & dob_d.1 == dob_m.2) %>%
#   # filter(dob_m.1 != dob_d.1) %>%
#   # filter(is.na(lname.1) | is.na(lname.2)) %>%
#   filter(lname.1 == fname.2 & fname.1 == lname.2) %>%
#   # filter(gender.1 == gender.2) %>%
#   # filter(source_system.1 != source_system.2) %>%
#   # filter(is.na(gender.1) | is.na(gender.2) | is.na(dob_y.1) | is.na(dob_y.2)) %>%
#   # filter(fname.1 == fname.2) %>%
#   select(id1, ssn.1, lname.1, fname.1, mname.1, dob_y.1,
#          dob_m.1, dob_d.1, gender.1, source_system.1, id_kcmaster.1, id_mcaid.1, id_kc_pha.1,
#          id2, ssn.2, lname.2, fname.2, mname.2, dob_y.2,
#          dob_m.2, dob_d.2, gender.2, source_system.2, id_kcmaster.2, id_mcaid.2, id_kc_pha.2,
#          Weight) %>%
#   tail()

# Make truncated version
pairs_01_trunc <- pairs_01 %>%
  # Avoid matching all the PHA or IDH IDs again
  filter(id_kcmaster.1 != id_kcmaster.2 | is.na(id_kcmaster.1) | is.na(id_kcmaster.2)) %>%
  filter(id_kc_pha.1 != id_kc_pha.2 | is.na(id_kc_pha.1) | is.na(id_kc_pha.2)) %>%
  filter(
    # SECTION FOR NON-JAN 1 BIRTH DATES
    (
      !((dob_m.1 == "1" & dob_d.1 == "1") | (dob_m.2 == "1" & dob_d.2 == "1")) &
        (
          # Can take quite a low score when SSN matches, names are transposed, and YOB is the same or off by 100
          (Weight >= 0.4 & (dob_y.1 == dob_y.2 | abs(dob_y.1 - dob_y.2) == 100) & 
             lname.1 == fname.2 & fname.1 == lname.2) |
            # Higher score when SSN matches, names are transposed, and YOB is different
            (Weight >= 0.5 & dob_y.1 != dob_y.2 & lname.1 == fname.2 & fname.1 == lname.2) |
            # Same month and day of birth but different year, no name checks
            (Weight >= 0.65 & dob_y.1 != dob_y.2 & dob_m.1 == dob_m.2 & dob_d.1 == dob_d.2) |
            # Transposed month and day of birth but no name checks
            (Weight >= 0.63 & dob_y.1 == dob_y.2 & dob_m.1 == dob_d.2 & dob_d.1 == dob_m.2) |
            # Mismatched gender but same YOB
            (Weight >= 0.7 & dob_y.1 == dob_y.2 & gender.1 != gender.2) |
            # Higher threshold if mismatched gender and YOB
            (Weight >= 0.82 & dob_y.1 != dob_y.2 & gender.1 != gender.2) | 
            # Catch everything else
            (Weight >= 0.74 & gender.1 == gender.2)
        )
    ) |
      # SECTION FOR WHEN THERE IS A JAN 1 BIRTH DATE INVOLVED
      (Weight >= 0.75 & dob_m.1 == "1" & dob_d.1 == "1" & dob_m.2 == "1" & dob_d.2 == "1") |
      (Weight >= 0.77 & (dob_m.1 == "1" & dob_d.1 == "1") | (dob_m.2 == "1" & dob_d.2 == "1")) |
      # SECTION FOR MISSING GENDER AND/OR DOB
      (
        (is.na(gender.1) | is.na(gender.2) | is.na(dob_y.1) | is.na(dob_y.2)) &
          (
            # First names match
            (Weight > 0.55 & fname.1 == fname.2) |
              # Higher threshold first names don't match
              (Weight > 0.64 & fname.1 != fname.2)
          )
      ) |
      # SECTION FOR CATCHING OTHER MATCHES
      id_hash.1 == id_hash.2
  )

## Collapse IDs ----
match_01_dedup <- match_process(pairs_input = pairs_01_trunc, df = input, iteration = 1)

## Error check ----
match_01_chk <- setDT(match_01_dedup %>% distinct(id_hash, clusterid_1))
match_01_chk[, cnt := .N, by = "id_hash"]

if (max(match_01_chk$cnt) > 1) {
  stop("Some id_hash values are associated with multiple clusterid_1 values. ",
       "Check what went wrong.")
}
rm(match_01_chk)


# SECOND PASS: BLOCK ON PHONETIC LNAME, FNAME AND DOB ----
## Run deduplication ----
st <- Sys.time()
match_02 <- RecordLinkage::compare.dedup(
  input, 
  blockfld = c("lname_phon", "fname_phon", "dob_y", "dob_m", "dob_d"), 
  strcmp = c("ssn", "lname", "fname", "mname", "gender"), 
  exclude = c("source_system", "id_kcmaster", "id_mcaid", "kcid", "id_hmis",
              "id_hchn", "lname_suff", "lname_alt", "lname_alt_phon",
              #"id_mcare", 
              "id_kc_pha", "rowid", "id_hash"))
message("Pairwise comparisons complete. Total run time: ", round(Sys.time() - st, 2), " ", units(Sys.time()-st))

summary(match_02)

## Add weights and extract pairs ----
# Using EpiLink approach
match_02 <- epiWeights(match_02)
classify_02 <- epiClassify(match_02, threshold.upper = 0.6)
summary(classify_02)
pairs_02 <- getPairs(classify_02, single.rows = TRUE) %>%
  mutate(across(contains("dob_"), ~ str_squish(.)),
         across(contains("dob_"), ~ as.numeric(.)))


## Review output and select cutoff point(s) ----
pairs_02 %>%
  filter(id_kcmaster.1 != id_kcmaster.2 | is.na(id_kcmaster.1) | is.na(id_kcmaster.2)) %>%
  filter(id_kc_pha.1 != id_kc_pha.2 | is.na(id_kc_pha.1) | is.na(id_kc_pha.2)) %>%
  # select(-contains("id_hash")) %>%
  filter(Weight >= 0.78) %>%
  # filter(ssn.1 != ssn.2) %>%
  # filter(!is.na(ssn.1) & !is.na(ssn.2)) %>%
  filter(is.na(ssn.1) | is.na(ssn.2)) %>%
  # filter(!(dob_m.1 == "1" & dob_d.1 == "1")) %>%
  filter(dob_m.1 == "1" & dob_d.1 == "1") %>%
  # filter(id_mcaid.1 != id_mcaid.2) %>%
  # filter(fname.1 == fname.2) %>%
  filter(!(source_system.1 == "MEDICAID" & source_system.2 == "MEDICAID" & id_mcaid.1 != id_mcaid.2)) %>%
  select(id1, ssn.1, lname.1, fname.1, mname.1, dob_y.1,
         dob_m.1, dob_d.1, gender.1, source_system.1, id_kcmaster.1, id_mcaid.1, id_kc_pha.1,
         id2, ssn.2, lname.2, fname.2, mname.2, dob_y.2,
         dob_m.2, dob_d.2, gender.2, source_system.2, id_kcmaster.2, id_mcaid.2, id_kc_pha.2,
         Weight) %>%
  tail()


# Make truncated data frame
pairs_02_trunc <- pairs_02 %>%
  # Avoid matching all the PHA or IDH IDs again
  filter(id_kcmaster.1 != id_kcmaster.2 | is.na(id_kcmaster.1) | is.na(id_kcmaster.2)) %>%
  filter(id_kc_pha.1 != id_kc_pha.2 | is.na(id_kc_pha.1) | is.na(id_kc_pha.2)) %>%
  # Don't include mismatching id_mcaids (spot checking a few indicated most (but not all) are different people)
  filter(!(source_system.1 == "MEDICAID" & source_system.2 == "MEDICAID" & id_mcaid.1 != id_mcaid.2)) %>%
  filter(
    # Matching SSN all have high weights and look good
    ssn.1 == ssn.2 |
      # id_mcaid matches are also good to include
      id_mcaid.1 == id_mcaid.2 |
      # SECTION WHERE SSNs DO NOT MATCH
      (Weight >= 0.85 & ssn.1 != ssn.2 & !(dob_m.1 == "1" & dob_d.1 == "1")) |
      (Weight >= 0.955 & ssn.1 != ssn.2 & dob_m.1 == "1" & dob_d.1 == "1") |
      # SECTION WHERE AN SSN IS MISSING
      (Weight >= 0.75 & (is.na(ssn.1) | is.na(ssn.2)) & !(dob_m.1 == "1" & dob_d.1 == "1")) |
      (Weight >= 0.82 & (is.na(ssn.1) | is.na(ssn.2)) & dob_m.1 == "1" & dob_d.1 == "1")
  )


## Collapse IDs ----
match_02_dedup <- match_process(pairs_input = pairs_02_trunc, df = input, iteration = 2) %>%
  mutate(clusterid_2 = clusterid_2 + max(match_01_dedup$clusterid_1))

## Error check ----
match_02_chk <- setDT(match_02_dedup %>% distinct(id_hash, clusterid_2))
match_02_chk[, cnt := .N, by = "id_hash"]

if (max(match_02_chk$cnt) > 1) {
  stop("Some id_hash values are associated with multiple clusterid_2 values. ",
       "Check what went wrong.")
}

rm(match_02_chk)



# THIRD PASS: BLOCK ON ID_KCMASTER ----
# Makes sure existing linkages within IDH data end up in the same ID
# Can just add a cluster ID to id_kcmaster
match_03_dedup <- input %>% 
  filter(!is.na(id_kcmaster)) %>%
  distinct(id_kcmaster) %>%
  mutate(clusterid_3 = row_number() + max(match_02_dedup$clusterid_2)) %>%
  inner_join(., input, by = "id_kcmaster") %>% 
  select(id_kcmaster, source_system:rowid, clusterid_3)



# FOURTH PASS: BLOCK ON ID_KC_PHA ----
# Makes sure existing linkages within PHA data end up in the same ID
# Can just add a cluster ID to id_kc_pha
match_04_dedup <- input %>% 
  filter(!is.na(id_kc_pha)) %>%
  distinct(id_kc_pha) %>%
  mutate(clusterid_4 = row_number() + max(match_03_dedup$clusterid_3)) %>%
  inner_join(., input, by = "id_kc_pha") %>% 
  select(id_kc_pha, source_system:rowid, clusterid_4)




# BRING MATCHING ROUNDS TOGETHER ----
# Use clusterid_1 as the starting point, find where one clusterid_2 value
# is associated with multiple clusterid_1 values, then take the min of the latter.
# Repeat using clusterid_2 until there is a 1:1 match between clusterid_1 and _2
# For now, need to do this iterative for passes 3 and 4. Eventually come up with a better
# scalable solution

final_dedup <- function(df1, df2, id1, id2) {
  ## Make joint data
  ids_join <- setDT(full_join(select(df1, id_hash, id1) %>%
                                rename(clusterid_1 = id1), 
                              select(df2, id_hash, id2) %>%
                                rename(clusterid_2 = id2),
                              by = "id_hash"))
  
  # Count how much consolidation is required
  ids_join[, clusterid_1_cnt := uniqueN(clusterid_1), by = "clusterid_2"]
  ids_join[, clusterid_2_cnt := uniqueN(clusterid_2), by = "clusterid_1"]
  remaining_dupes <- ids_join %>% 
    count(clusterid_1_cnt, clusterid_2_cnt) %>%
    filter(clusterid_1_cnt != clusterid_2_cnt) %>%
    summarise(dups = sum(n))
  remaining_dupes <- remaining_dupes$dups[1]
  
  if (remaining_dupes > 0) {
    # Keep deduplicating until there are no more open triangles
    recursion_level <- 0
    recursion_dt <- copy(ids_join)
    setnames(recursion_dt, old = c("clusterid_1", "clusterid_2"), new = c("id_1_recur0", "id_2_recur0"))
    
    while (remaining_dupes > 0) {
      recursion_level <- recursion_level + 1
      message(remaining_dupes, " remaining duplicated rows. Starting recursion iteration ", recursion_level)
      
      recursion_dt <- final_dedup_collapse(iterate_dt = recursion_dt, iteration = recursion_level)
      
      # Check how many duplicates remain
      remaining_dupes <- recursion_dt %>% count(clusterid_1_cnt, clusterid_2_cnt) %>%
        filter(clusterid_1_cnt != clusterid_2_cnt) %>%
        summarise(dups = sum(n))
      remaining_dupes <- remaining_dupes$dups[1]
    }
    
    # Get final ID to use
    ids_join <- recursion_dt[, cluster_final := get(paste0("id_1_recur", recursion_level))]
  } else {
    message("No duplicates to resolve")
    ids_join[, cluster_final := clusterid_1]
  }
  
  # Keep only relevant columns
  ids_join <- ids_join[, .(id_hash, cluster_final)]
  
  ids_join
}

final_dedup_collapse <- function(iterate_dt, iteration) {
  # Set up all the new variables needed
  # Try and make this dynamic later
  id_1_old <- paste0("id_1_recur", iteration-1)
  id_1_min <- paste0("id_1_recur", iteration, "_min")
  id_1_new <- paste0("id_1_recur", iteration)
  id_1_cnt <- paste0("id_1_recur", iteration, "_cnt")
  
  id_2_old <- paste0("id_2_recur", iteration-1)
  id_2_min <- paste0("id_2_recur", iteration, "_min")
  id_2_new <- paste0("id_2_recur", iteration)
  id_2_cnt <- paste0("id_2_recur", iteration, "_cnt")
  
  # Any rows with blank IDs those that didn't match at all. Just bring over non-NA
  iterate_dt[is.na(iterate_dt[[id_1_old]]), (id_1_old) := get(id_2_old)]
  iterate_dt[is.na(iterate_dt[[id_2_old]]), (id_2_old) := get(id_1_old)]
  # First find the existing min value for IDs 1 and 2
  iterate_dt[, (id_1_min) := min(get(id_1_old), na.rm = T), by = id_2_old]
  iterate_dt[, (id_2_min) := min(get(id_2_old), na.rm = T), by = id_1_old]
  # Then set the new ID 1 and 2 based on the min
  iterate_dt[, (id_1_new) := min(get(id_1_min), na.rm = T), by = id_2_min]
  iterate_dt[, (id_2_new) := min(get(id_2_min), na.rm = T), by = id_1_min]
  # Set up a count of remaining duplicates
  iterate_dt[, clusterid_1_cnt := uniqueN(get(id_1_new)), by = id_2_new]
  iterate_dt[, clusterid_2_cnt := uniqueN(get(id_2_new)), by = id_1_new]
  print(head(iterate_dt))
  iterate_dt
}



## Make joint data with pass 1 plus other passes ----
combine_1_2 <- final_dedup(df1 = match_01_dedup, id1 = "clusterid_1",
                           df2 = match_02_dedup, id2 = "clusterid_2")

combine_1_3 <- final_dedup(df1 = match_01_dedup, id1 = "clusterid_1",
                           df2 = match_03_dedup, id2 = "clusterid_3")

combine_1_4 <- final_dedup(df1 = match_01_dedup, id1 = "clusterid_1",
                           df2 = match_04_dedup, id2 = "clusterid_4")

## Make joint data with pass 2 plus other passes ----
combine_2_3 <- final_dedup(df1 = match_02_dedup, id1 = "clusterid_2",
                           df2 = match_03_dedup, id2 = "clusterid_3")

combine_2_4 <- final_dedup(df1 = match_02_dedup, id1 = "clusterid_2",
                           df2 = match_04_dedup, id2 = "clusterid_4")

## Make joint data with pass 2 plus other passes ----
combine_3_4 <- final_dedup(df1 = match_03_dedup, id1 = "clusterid_3",
                           df2 = match_04_dedup, id2 = "clusterid_4")


## Now bring the combinations together ----
combine_1_2_3 <- final_dedup(df1 = combine_1_2, id1 = "cluster_final",
                             df2 = combine_1_3, id2 = "cluster_final")

combine_1_2_4 <- final_dedup(df1 = combine_1_2, id1 = "cluster_final",
                             df2 = combine_1_4, id2 = "cluster_final")

combine_all_1 <- final_dedup(df1 = combine_1_2_3, id1 = "cluster_final",
                             df2 = combine_1_2_4, id2 = "cluster_final")

combine_all_2 <- final_dedup(df1 = combine_2_3, id1 = "cluster_final",
                             df2 = combine_2_4, id2 = "cluster_final")

combine_all_1_2 <- final_dedup(df1 = combine_all_1, id1 = "cluster_final",
                               df2 = combine_all_2, id2 = "cluster_final")

combine_all <- final_dedup(df1 = combine_all_1_2, id1 = "cluster_final",
                           df2 = combine_3_4, id2 = "cluster_final")



## Error check ----
combine_all_chk <- unique(combine_all[, c("id_hash", "cluster_final")])
combine_all_chk[, cnt_id := .N, by = "id_hash"]
combine_all_chk[, cnt_hash := .N, by = "cluster_final"]
# cnt_id should = 1 and cnt_hash should be >= 1
combine_all_chk %>% count(cnt_id, cnt_hash)
if (max(combine_all_chk$cnt_id) > 1) {
  stop("There is more than one cluster ID for a given id_has. Investigate why.")
}

# Check cnt_hash by source system
left_join(select(input, id_hash, source_system), combine_all_chk, by = "id_hash") %>%
  count(source_system, cnt_id, cnt_hash) %>%
  as.data.frame()


## Now make an alpha-numeric ID that will be stored in a table ----
# NB. This will need to be reworked when there is an existing table with APDE IDs
#  Likely make twice as many IDs as needed then weed out the ones already in
#    the master list, before trimming to the actual number needed.

ids_final <- id_nodups(id_n = n_distinct(combine_all$cluster_final),
                       id_length = 10)
ids_final <- combine_all %>%
  distinct(cluster_final) %>%
  arrange(cluster_final) %>%
  bind_cols(., id_hudhears = ids_final)

names_final <- input %>%
  select(source_system, id_kcmaster, id_mcaid, kcid, id_hmis, id_hchn, customerkey, id_kc_pha, 
         #id_mcare, 
         ssn, fname, mname, lname, lname_suff, lname_alt, 
         dob_y, dob_m, dob_d, gender, id_hash) %>%
  left_join(., select(combine_all, id_hash, cluster_final), by = "id_hash") %>%
  left_join(., ids_final, by = "cluster_final") %>%
  # select(id_hudhears, id_mcaid, 
  #        #id_mcare, 
  #        id_kc_pha, id_hash) %>%
  distinct() %>%
  mutate(last_run = Sys.time())


# Look at sources by ID
names_final_cnt <- setDT(unique(names_final[, c("id_hudhears", "source_system")]))
names_final_cnt[, cnt_source := .N, by = "id_hudhears"]
names_final_cnt %>% group_by(source_system, cnt_source) %>%
  summarise(n = n_distinct(id_hudhears)) %>%
  group_by(source_system) %>% mutate(tot = sum(n)) %>%
  ungroup() %>%
  mutate(pct = round(n/tot*100, 1)) %>%
  as.data.frame()

# QA FINAL DATA ----
### REVIEW POINT ----
# Number of id_hashes compared to the number of id_hudhearss
message("There are ", n_distinct(names_final$id_hash), " IDs and ", 
        n_distinct(names_final$id_hudhears), " id_hudhears IDs")

db_hhsaw <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = "tcp:kcitazrhpasqlprp16.azds.kingcounty.gov,1433",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryPassword")

mcaid_elig_demo <- setDT(odbc::dbGetQuery(db_hhsaw, "SELECT DISTINCT id_mcaid FROM claims.final_mcaid_elig_demo")) # go back to elig_demo because it has an ALMOST complete list of all Mcaid ID

# mcare.only <- setDT(odbc::dbGetQuery(db_claims, "SELECT DISTINCT id_mcare FROM final.mcare_elig_demo")) # go back to elig_demo because it has an ALMOST complete list of all Mcare ID


## Confirm that every ID is accounted for ----
# Check Mcaid
extra_mcaid <- setdiff(names_final[!is.na(id_mcaid)]$id_mcaid, mcaid_elig_demo[!is.na(id_mcaid)]$id_mcaid)
missing_mcaid <- setdiff(mcaid_elig_demo[!is.na(id_mcaid)]$id_mcaid, names_final[!is.na(id_mcaid)]$id_mcaid)
length(extra_mcaid) # Might be > zero if there are id_mcaid in stage.mcaid_elig that were not in the elig_demo
length(missing_mcaid) # should be zero

# # Check Mcare
# extra.mcare <- setdiff(names_final[!is.na(id_mcare)]$id_mcare, mcare.only[!is.na(id_mcare)]$id_mcare) 
# missing.mcare <- setdiff(mcare.only[!is.na(id_mcare)]$id_mcare, names_final[!is.na(id_mcare)]$id_mcare) 
# length(extra.mcare) # Expect there will be extra b/c there are Mcare ids in SSN and Names files that are not in MBSF
# length(missing.mcare) # should be zero in length

## Confirm that there are no duplicates in the final names_final linkage ----      
# Mcaid - id_hudhears per id_mcaid
id_dup_chk <- copy(names_final)
id_dup_chk[!is.na(id_mcaid), dup_mcaid := uniqueN(id_hudhears, na.rm = T), by = "id_mcaid"]
id_dup_chk %>% count(dup_mcaid)
if (nrow(id_dup_chk[!is.na(id_mcaid) & dup_mcaid > 1]) == 0) {
  message("There were no Medicaid IDs allocated to >1 APDE IDs (as expected)")
} else {
  stop("Some Medicaid IDs have been assigned to multiple APDE IDs. Investigate why.")
}

# Mcaid - id_mcaid per id_hudhears
id_dup_chk[!is.na(id_mcaid), dup_apde_mcaid := uniqueN(id_mcaid, na.rm = T), by = "id_hudhears"]
id_dup_chk %>% count(dup_apde_mcaid)
id_dup_chk[dup_apde_mcaid > 1]

# PHA
id_dup_chk[!is.na(id_kc_pha), dup_pha := uniqueN(id_hudhears, na.rm = T), by = "id_kc_pha"]
id_dup_chk %>% count(dup_pha)
if (nrow(id_dup_chk[!is.na(id_kc_pha) & dup_pha > 1]) == 0) {
  message("There were no PHA IDs allocated to >1 APDE IDs (as expected)")
} else {
  stop("Some PHA IDs have been assigned to multiple APDE IDs. Investigate why.")
}

# PHA - id_kc_pha per id_hudhears
id_dup_chk[!is.na(id_kc_pha), dup_apde_pha := uniqueN(id_kc_pha, na.rm = T), by = "id_hudhears"]
id_dup_chk %>% count(dup_apde_pha)
id_dup_chk[dup_apde_pha > 1]


## Check there aren't a crazy high number of rows per ID ----
id_dup_chk[, id_cnt := .N, by = "id_hudhears"]
id_dup_chk %>% count(id_cnt)


## Check that some mcaid and PHA IDs matched ----
id_dup_chk[, id_mcaid_present := ifelse(!is.na(dup_apde_mcaid), 1L, 0L)]
id_dup_chk[, id_pha_present := ifelse(!is.na(dup_apde_pha), 1L, 0L)]
id_dup_chk[, id_mcaid_pha := max(id_mcaid_present) + max(id_pha_present), by = "id_hudhears"]
id_dup_chk %>% distinct(id_hudhears, id_mcaid_pha) %>% count(id_mcaid_pha)


# LOAD TO SQL ----
## identify the column types to be created in SQL ----
# Only consider ones that might not load as desired
sql_columns <- c("id_hudhears" = "char(10)", 
                 #"id_mcare" = "CHAR(15) collate SQL_Latin1_General_Cp1_CS_AS", 
                 "id_mcaid" = "char(11)", 
                 "id_kc_pha" = "char(10)", 
                 "id_hash" = "char(64)",
                 "last_run" = "datetime")  

# ensure column order in R is the same as that in SQL
setcolorder(names_final, c("id_hudhears", "source_system", "id_kcmaster", "id_kc_pha", "id_mcaid", 
            "kcid", "id_hmis", "id_hchn", "customerkey", "ssn", "fname", "mname", "lname", 
            "lname_suff", "lname_alt", "dob_y", "dob_m", "dob_d", "gender", "id_hash", "last_run"))

## Write table to SQL ----
# Split into smaller tables to avoid SQL connection issues
start <- 1L
max_rows <- 100000L
cycles <- ceiling(nrow(names_final)/max_rows)

lapply(seq(start, cycles), function(i) {
  start_row <- ifelse(i == 1, 1L, max_rows * (i-1) + 1)
  end_row <- min(nrow(names_final), max_rows * i)
  
  message("Loading cycle ", i, " of ", cycles, " (rows: ", start_row, "-", end_row, ")")
  if (i == 1) {
    dbWriteTable(db_hhsaw,
                 DBI::Id(schema = "amatheson", table = "hudhears_xwalk_ids"),
                 value = as.data.frame(names_final[start_row:end_row]),
                 overwrite = T, append = F,
                 field.types = sql_columns)
  } else {
    dbWriteTable(db_hhsaw,
                 DBI::Id(schema = "amatheson", table = "hudhears_xwalk_ids"),
                 value = as.data.frame(names_final[start_row:end_row]),
                 overwrite = F, append = T)
  }
})


## Confirm that all rows were loaded to sql ----
stage.count <- as.numeric(odbc::dbGetQuery(db_hhsaw, 
                                           "SELECT COUNT (*) FROM amatheson.hudhears_xwalk_ids"))
if(stage.count != nrow(names_final))
  stop("Mismatching row count, error writing or reading data")      

# close database connections    
dbDisconnect(db_claims)  
dbDisconnect(db_apde51)  


# CLEAN UP ----
## Remove data
rm(list = ls(pattern = "match"))
rm(list = ls(pattern = "pairs"))
rm(list = ls(pattern = "classify"))
rm(list = ls(pattern = "ids_"))
rm(list = ls(pattern = "combine_"))
rm(mcaid, mcare.dt, pha)
rm(input)


## The end! ----      
run.time <- Sys.time() - start.time  
print(run.time)

Sys.time() - start.time
