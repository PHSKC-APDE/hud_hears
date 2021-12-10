## Script name: HUD HEARS PROCESSING - PHA
##
## Purpose of script: Combine PHA 50058 and PHA exit data
##
## Author: Alastair Matheson, Public Health - Seattle & King County
## Date Created: 2021-08-13
## Email: alastair.matheson@kingcounty.gov
##
## Notes:
##   
##

# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, RecordLinkage, lubridate, sqldf)

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


# FUNCTIONS ----
## Adaptation of Carolina's code ----
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
  # In case the input was a data table, convert to data frame
  # (otherwise the recursion breaks after 1 round)
  pairs <- data.table::setDF(pairs)
  
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
  double_dups <- pairsend %>%
    select(rowid, clusterid) %>%
    distinct() %>%
    group_by(rowid) %>%
    filter(n() > 1) %>%
    group_by(clusterid) %>%
    mutate(row_min = min(rowid)) %>%
    ungroup() %>%
    rename(back_join_id = row_min) %>%
    select(-rowid) %>%
    distinct()
  
  
  # collapse duplicate partial clusters to one cluster
  # error checking to make sure that correct total clusters are maintained
  if (nrow(double_dups) > 0) {
    pairsend <- left_join(pairsend, double_dups, by = c(clusterid = "clusterid")) %>%
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
  n_pi_split <- pairs_final %>% 
    group_by(id_hash) %>% 
    filter(n_distinct(clusterid) > 1) %>%
    n_distinct()
  
  if (n_pi_split) {
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
id_nodups <- function(id_n, id_length, seed = 98104) {
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


# BRING IN DATA ----
## Exit definitions ----
exit_def <- read.csv(file.path(here::here(), "data_processing", "PHA_exit_definitions.csv"))

## PHA time-varying data ----
pha_timevar <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.final_timevar")

## PHA identities ----
names_existing <- dbGetQuery(db_hhsaw, "SELECT * FROM pha.final_identities")


## Exit data ----
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


# LINK DATA ----
# Link SHA on individuals and KCHA on heads of household
## Final pool of identities ----
names_list <- bind_rows(select(pha_exit, pha_source, ssn, lname, fname, mname, dob, id_hash) %>%
                          filter(pha_source == "sha_exit") %>% distinct(),
                        select(pha_exit, pha_source, hh_lname, hh_fname, hh_dob, hh_female, hh_id_hash) %>%
                          filter(pha_source == "kcha_exit") %>% distinct() %>%
                          rename(lname = hh_lname, fname = hh_fname,
                                 dob = hh_dob, female = hh_female),
                        select(names_existing, ssn, lname, fname, mname, dob, female,
                               id_hash, id_kc_pha) %>%
                          distinct() %>%
                          mutate(pha_source = "existing_ids")
) %>%
  # Make a hash of variables that are used for identity matching
  mutate(id_hash = case_when(is.na(id_hash) ~ hh_id_hash,
                             TRUE ~ id_hash)) %>%
  select(-hh_id_hash) %>%
  distinct()



## Add in variables for matching ----
names_list <- names_list %>%
  mutate(lname_phon = RecordLinkage::soundex(lname),
         fname_phon = RecordLinkage::soundex(fname),
         dob_y = lubridate::year(dob),
         dob_m = lubridate::month(dob),
         dob_d = as.numeric(lubridate::day(dob)),
         rowid = row_number())


# FIRST PASS: BLOCK ON SSN OR PHA ID ----
## Run deduplication ----
# Blocking on SSN or PHA ID and string compare names
# NB. Only applies to SHA since KCHA has no SSN info
#     SHA does not have gender info
st <- Sys.time()
match_01 <- RecordLinkage::compare.dedup(
  names_list, 
  blockfld = "ssn", 
  strcmp = c("lname", "fname", "mname", "dob_y", "dob_m", "dob_d", "female"), 
  exclude = c("lname_phon", "fname_phon", "dob", "rowid", "id_hash", "id_kc_pha", "pha_source"))
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
#   # Only keep matches with waitlist data in it
#   filter(!(pha_source.1 == "existing_ids" & pha_source.2 == "existing_ids")) %>%
#   # NON-JAN 1 SECTION
#   # filter(!((dob_m.1 == "1" & dob_d.1 == "1") | (dob_m.2 == "1" & dob_d.2 == "1"))) %>%
#   # filter(Weight >= 0.7 & dob_y.1 != dob_y.2 & dob_m.1 == dob_m.2 & dob_d.1 == dob_d.2) %>%
#   # filter((Weight >= 0.4 & (dob_y.1 == dob_y.2 | abs(dob_y.1 - dob_y.2) == 100) &
#   #           lname.1 == fname.2 & fname.1 == lname.2)) %>%
#   # filter(Weight >= 0.5 & dob_y.1 != dob_y.2 & lname.1 == fname.2 & fname.1 == lname.2) %>%
#   # filter(Weight >= 0.65 & dob_y.1 == dob_y.2 & (dob_m.1 != dob_m.2 | dob_d.1 != dob_d.2)) %>%
#   # filter(Weight >= 0.63 & dob_y.1 == dob_y.2 & dob_m.1 == dob_d.2 & dob_d.1 == dob_m.2) %>%
#   # filter(Weight >= 0.6 & dob_y.1 == dob_y.2 & dob_m.1 == dob_m.2 & dob_d.1 == dob_d.2) %>%
#   # filter(Weight >= 0.74) %>%
#   # JAN 1 SECTION
# filter((dob_m.1 == "1" & dob_d.1 == "1") | (dob_m.2 == "1" & dob_d.2 == "1")) %>%
#   # filter(Weight >= 0.65 & dob_y.1 == dob_y.2) %>%
#   filter(Weight >= 0.65 & dob_y.1 != dob_y.2) %>%
#   # MISSING DOB
#   # filter((is.na(dob_y.1) | is.na(dob_y.2))) %>%
#   # filter(Weight > 0.55 & fname.1 == fname.2) %>%
#   # filter(Weight > 0.55 & fname.1 != fname.2) %>%
#   select(id1, ssn.1, lname.1, fname.1, mname.1, dob.1, female.1, id_kc_pha.1, pha_source.1,
#          id2, ssn.2, lname.2, fname.2, mname.2, dob.2, female.2, id_kc_pha.2, pha_source.2,
#          Weight) %>%
#   tail()


pairs_01_trunc <- pairs_01 %>%
  # Only keep matches with waitlist data in it
  filter(!(pha_source.1 == "existing_ids" & pha_source.2 == "existing_ids")) %>%
  filter(
    # SECTION FOR NON-JAN 1 BIRTH DATES
    (!((dob_m.1 == "1" & dob_d.1 == "1") | (dob_m.2 == "1" & dob_d.2 == "1")) &
       (
         # Can take quite a low score when SSN matches, names are transposed, and YOB is the same or off by 100
         (Weight >= 0.4 & (dob_y.1 == dob_y.2 | abs(dob_y.1 - dob_y.2) == 100) & 
            lname.1 == fname.2 & fname.1 == lname.2) |
           # Higher score when SSN matches, names are transposed, and YOB is different
           (Weight >= 0.65 & dob_y.1 != dob_y.2 & lname.1 == fname.2 & fname.1 == lname.2) |
           # Same month and day of birth but different year, no name checks
           (Weight >= 0.7 & dob_y.1 != dob_y.2 & dob_m.1 == dob_m.2 & dob_d.1 == dob_d.2) |
           # Transposed month and day of birth but no name checks
           (Weight >= 0.63 & dob_y.1 == dob_y.2 & dob_m.1 == dob_d.2 & dob_d.1 == dob_m.2) |
           # Catch everything else
           (Weight >= 0.74)
       )
    ) |
      # SECTION FOR WHEN THERE IS A JAN 1 BIRTH DATE INVOLVED
      (Weight >= 0.7 & dob_m.1 == "1" & dob_d.1 == "1" & dob_m.2 == "1" & dob_d.2 == "1") |
      (Weight >= 0.7 & (dob_m.1 == "1" & dob_d.1 == "1") | (dob_m.2 == "1" & dob_d.2 == "1")) |
      # SECTION FOR DOB
      (Weight > 0.58 & (is.na(dob_y.1) | is.na(dob_y.2)))
  )


## Collapse IDs ----
match_01_dedup <- match_process(pairs_input = pairs_01_trunc, 
                                df = names_list, iteration = 1)


## Error check ----
match_01_chk <- setDT(match_01_dedup %>% distinct(id_hash, clusterid_1))
match_01_chk[, cnt := .N, by = "id_hash"]

if (max(match_01_chk$cnt) > 1) {
  stop("Some id_hash values are associated with multiple clusterid_1 values. ",
       "Check what went wrong.")
}


# SECOND PASS: BLOCK ON PHONETIC LNAME, FNAME AND DOB ----
## Run deduplication ----
st <- Sys.time()
match_02 <- RecordLinkage::compare.dedup(
  names_list, 
  blockfld = c("lname_phon", "fname_phon", "dob_y", "dob_m", "dob_d"), 
  strcmp = c("ssn", "lname", "fname", "mname", "female"), 
  exclude = c("dob", "rowid", "id_hash", "id_kc_pha", "pha_source"))
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
# pairs_02 %>% filter(Weight <= 0.8 & ssn_id.1 != ssn_id.2) %>% select(-contains("id_hash")) %>% head()
# 
# pairs_02 %>% 
#   # Only keep matches with waitlist data in it
#   filter(!(pha_source.1 == "existing_ids" & pha_source.2 == "existing_ids")) %>%
#   # select(-contains("id_hash")) %>% 
#   filter(Weight >= 0.69) %>% 
#   # SECTION WHERE SSNs DO NOT MATCH
#   # filter(ssn.1 != ssn.2 & !is.na(ssn.1) & !is.na(ssn.2)) %>%
#   # SECTION WHERE AN SSN IS MISSING
#   filter(is.na(ssn.1) | is.na(ssn.2)) %>%
#   filter(!(dob_m.1 == "1" & dob_d.1 == "1")) %>%
#   # filter(dob_m.1 == "1" & dob_d.1 == "1") %>%
#   select(id1, ssn.1, lname.1, fname.1, mname.1, dob.1, female.1, id_kc_pha.1, pha_source.1,
#          id2, ssn.2, lname.2, fname.2, mname.2, dob.2, female.2, id_kc_pha.2, pha_source.2,
#          Weight) %>%
#   tail()


pairs_02_trunc <- pairs_02 %>%
  # Only keep matches with waitlist data in it
  filter(!(pha_source.1 == "existing_ids" & pha_source.2 == "existing_ids")) %>%
  filter(
    # Matching SSN all have high weights and look good
    ssn.1 == ssn.2 |
      # SECTION WHERE SSNs DO NOT MATCH
      (Weight >= 0.88 & ssn.1 != ssn.2 & !(dob_m.1 == "1" & dob_d.1 == "1")) |
      (Weight >= 0.90 & ssn.1 != ssn.2 & dob_m.1 == "1" & dob_d.1 == "1") |
      # SECTION WHERE AN SSN IS MISSING
      ((is.na(ssn.1) | is.na(ssn.2)) &
         (
           (Weight >= 0.69 & !(dob_m.1 == "1" & dob_d.1 == "1")) |
             (Weight >= 0.85 & dob_m.1 == "1" & dob_d.1 == "1")
         )
      )
  )


## Collapse IDs ----
match_02_dedup <- match_process(pairs_input = pairs_02_trunc, df = names_list, iteration = 2) %>%
  mutate(clusterid_2 = clusterid_2 + max(match_01_dedup$clusterid_1))

## Error check ----
match_02_chk <- setDT(match_02_dedup %>% distinct(id_hash, clusterid_2))
match_02_chk[, cnt := .N, by = "id_hash"]

if (max(match_02_chk$cnt) > 1) {
  stop("Some id_hash values are associated with multiple clusterid_2 values. ",
       "Check what went wrong.")
}


# BRING MATCHING ROUNDS TOGETHER ----
# Use clusterid_1 as the starting point, find where one clusterid_2 value
# is associated with multiple clusterid_1 values, then take the min of the latter.
# Repeat using clusterid_2 until there is a 1:1 match between clusterid_1 and _2

final_dedup <- function(iterate_dt, iteration) {
  id_1_old <- paste0("id_1_recur", iteration-1)
  id_1_min <- paste0("id_1_recur", iteration, "_min")
  id_1_new <- paste0("id_1_recur", iteration)
  id_1_cnt <- paste0("id_1_recur", iteration, "_cnt")
  
  id_2_old <- paste0("id_2_recur", iteration-1)
  id_2_min <- paste0("id_2_recur", iteration, "_min")
  id_2_new <- paste0("id_2_recur", iteration)
  id_2_cnt <- paste0("id_2_recur", iteration, "_cnt")
  
  # First find the existing min value for IDs 1 and 2
  iterate_dt[, (id_1_min) := min(get(id_1_old)), by = id_2_old]
  iterate_dt[, (id_2_min) := min(get(id_2_old)), by = id_1_old]
  # Then set the new ID 1 and 2 based on the min
  iterate_dt[, (id_1_new) := min(get(id_1_min)), by = id_2_min]
  iterate_dt[, (id_2_new) := min(get(id_2_min)), by = id_1_min]
  
  # Set up a count of remaining duplicates
  iterate_dt[, clusterid_1_cnt := uniqueN(get(id_1_new)), by = id_2_new]
  iterate_dt[, clusterid_2_cnt := uniqueN(get(id_2_new)), by = id_1_new]
}

# Make joint data
ids_dedup <- setDT(full_join(select(match_01_dedup, id_hash, clusterid_1), 
                             select(match_02_dedup, id_hash, clusterid_2),
                             by = "id_hash"))

# Count how much consolidation is required
ids_dedup[, clusterid_1_cnt := uniqueN(clusterid_1), by = "clusterid_2"]
ids_dedup[, clusterid_2_cnt := uniqueN(clusterid_2), by = "clusterid_1"]
remaining_dupes <- ids_dedup %>% count(clusterid_1_cnt, clusterid_2_cnt) %>%
  filter(clusterid_1_cnt != clusterid_2_cnt) %>%
  summarise(dups = sum(n))
remaining_dupes <- remaining_dupes$dups[1]

if (remaining_dupes > 0) {
  # Keep deduplicating until there are no more open triangles
  recursion_level <- 0
  recursion_dt <- copy(ids_dedup)
  setnames(recursion_dt, old = c("clusterid_1", "clusterid_2"), new = c("id_1_recur0", "id_2_recur0"))
  
  while (remaining_dupes > 0) {
    recursion_level <- recursion_level + 1
    message(remaining_dupes, " remaining duplicated rows. Starting recursion iteration ", recursion_level)
    
    recursion_dt <- final_dedup(iterate_dt = recursion_dt, iteration = recursion_level)
    
    # Check how many duplicates remain
    remaining_dupes <- recursion_dt %>% count(clusterid_1_cnt, clusterid_2_cnt) %>%
      filter(clusterid_1_cnt != clusterid_2_cnt) %>%
      summarise(dups = sum(n))
    remaining_dupes <- remaining_dupes$dups[1]
  }
  
  # Get final ID to use
  ids_dedup <- recursion_dt[, cluster_final := get(paste0("id_1_recur", recursion_level))]
} else {
  ids_dedup[, cluster_final := clusterid_1]
}

# Keep only relevant columns
ids_dedup <- ids_dedup[, .(id_hash, cluster_final)]


## Error check ----
ids_dedup_chk <- unique(ids_dedup[, c("id_hash", "cluster_final")])
ids_dedup_chk[, cnt_id := .N, by = "id_hash"]
ids_dedup_chk[, cnt_hash := .N, by = "cluster_final"]
# cnt_id should = 1 and cnt_hash should be >= 1
ids_dedup_chk %>% count(cnt_id, cnt_hash)
if (max(ids_dedup_chk$cnt_id) > 1) {
  stop("There is more than one cluster ID for a given id_has. Investigate why.")
}



## Make an alpha-numeric ID that will be stored in a table ----
new_ids_needed <- n_distinct(ids_dedup$cluster_final)
if (exists("names_existing")) {
  # Pull in existing IDs to make sure they are not repeated
  ids_existing <- unique(names_existing$id_kc_pha)
  
  # If using the same seed as was used to create other IDs, can just tack on
  # the number of new IDs needed
  ids_final <- id_nodups(id_n = length(ids_existing) + new_ids_needed,
                         id_length = 10)
  
  ids_final_dedup <- ids_final[!ids_final %in% ids_existing]
  
  # Make sure there are enough new IDs
  if (length(ids_final_dedup) < new_ids_needed) {
    # Run again but make it longer
    ids_final <- id_nodups(id_n = length(ids_existing) + new_ids_needed * 3,
                           id_length = 10)
    
    ids_final_dedup <- ids_final[!ids_final %in% ids_existing]
    rm(ids_final_dedup)
  }
  # Check again
  if (length(ids_final_dedup) < new_ids_needed) {
    stop("Could not generate enough new IDs")
  }
  
  # Trim to correct size
  ids_final <- ids_final_dedup[1:new_ids_needed]
} else {
  ids_final <- id_nodups(id_n = new_ids_needed, id_length = 10)
}

# Join to cluster IDs
ids_final <- ids_dedup %>%
  distinct(cluster_final) %>%
  arrange(cluster_final) %>%
  bind_cols(., id_kc_pha = ids_final)

# Join to make final list of names and IDs
names_final <- names_list %>%
  select(ssn, lname, fname, mname, dob, female, id_hash) %>%
  left_join(., select(ids_dedup, id_hash, cluster_final), by = "id_hash") %>%
  left_join(., ids_final, by = "cluster_final") %>%
  select(-cluster_final) %>%
  distinct() %>%
  mutate(last_run = Sys.time())


## Consolidate IDs ----
# Easiest to assign a number to each ID so taking the min works to consolidate groups
# Quicker to use data table to set these up
names_existing <- setDT(names_existing)
names_existing_join <- copy(names_existing)
setnames(names_existing_join, "id_kc_pha", "id_kc_pha_old")
names_existing_join[, `:=` (hash_cnt_old = .N, id_num_old = .GRP), by = "id_kc_pha_old"]

names_final <- setDT(rename(names_final, id_kc_pha_new = id_kc_pha))
names_final[, `:=` (hash_cnt_new = .N, id_num_new = .GRP), by = "id_kc_pha_new"]
names_final[, id_num_new := id_num_new + length(ids_existing)]

# Bring old and new together
names_dedup <- merge(names_existing_join[, .(id_hash, id_kc_pha_old, hash_cnt_old, id_num_old)],
                     names_final[, .(id_hash, id_kc_pha_new, hash_cnt_new, id_num_new)],
                     by = "id_hash", all = T)

# Check for unclosed groups in either direction
names_dedup[!is.na(id_kc_pha_old), id_min_new := min(id_num_new, na.rm = T), by = "id_kc_pha_old"]
names_dedup[!is.na(id_kc_pha_new), id_min_old := min(id_num_old, na.rm = T), by = "id_kc_pha_new"]

# Replace infinite values created above
names_dedup[is.infinite(id_min_old), id_min_old := NA]
names_dedup[is.infinite(id_min_new), id_min_new := NA]

# Any rows with blank id_min_new and id_min_old are those that didn't match at all. Just bring over id_num_new
names_dedup[is.na(id_min_new) & is.na(id_min_old), id_min_new := id_num_new]

# Consolidate so old number is preferentially kept
names_dedup[, id_num_final := coalesce(id_min_old, id_min_new)]

# Make a reshaped list of IDs and ID numbers to get final ID
id_num_list <- bind_rows(distinct(names_dedup, id_kc_pha_old, id_num_old) %>% 
                           filter(!is.na(id_kc_pha_old)) %>%
                           rename(id_kc_pha_final = id_kc_pha_old,
                                  id_num_final = id_num_old),
                         distinct(names_dedup, id_kc_pha_new, id_num_new) %>% 
                           rename(id_kc_pha_final = id_kc_pha_new, 
                                  id_num_final = id_num_new))

# Check the number of IDs matches what is expected
if (nrow(id_num_list) != length(ids_existing) + new_ids_needed) {
  stop("Mismatched number of rows in id_num_list (too many or too few)")
}

# Join to get the final ID
names_dedup <- left_join(select(names_dedup, id_hash, id_kc_pha_old, id_kc_pha_new, id_num_final),
                         id_num_list,
                         by = "id_num_final") %>%
  select(-id_num_final)


## Make final names table ----
# Set up last_run time
run_time <- Sys.time()
names_final <- left_join(names_final, select(names_dedup, id_hash, id_kc_pha_final),
                         by = "id_hash") %>%
  select(ssn, lname, fname, mname, dob, female, id_hash, id_kc_pha_final) %>%
  rename(id_kc_pha = id_kc_pha_final) %>%
  mutate(last_run = run_time)


## Check how many people from exit data matched ----
names_final <- names_final %>%
  group_by(id_kc_pha) %>%
  mutate(id_cnt = n()) %>%
  ungroup() %>%
  left_join(., names_list %>% distinct(id_hash, pha_source), by = "id_hash")

names_final %>% filter(pha_source != "existing_ids") %>% count(pha_source, id_cnt)


# TRY MATCHING ON ADDRESS ----
# First link existing IDs with timevar table
timevar_add <- pha_timevar %>% 
  distinct(id_kc_pha, geo_hash_clean) %>%
  left_join(., select(names_existing, -last_run), 
            by = c("id_kc_pha"))

# Then find the people who didn't match to anyone else and join to their exit address
exit_add <- left_join(filter(names_final, pha_source == "sha_exit" & id_cnt == 1),
                      select(pha_exit, id_hash, geo_hash_clean),
                      by = "id_hash") %>%
  select(-last_run)

# Bring it all together and see if an address led to too many IDs merging
names_add <- inner_join(timevar_add, exit_add, by = c("geo_hash_clean", "lname", "fname")) %>% 
  group_by(id_kc_pha.y) %>% 
  mutate(id_cnt_new = n_distinct(id_kc_pha.x)) %>% 
  ungroup()

names_add %>% count(id_cnt_new)

# Just keep the 1:1 updates
names_add <- names_add %>% 
  filter(id_cnt_new == 1) %>%
  select(id_hash.y, id_kc_pha.x) %>%
  rename(id_hash = id_hash.y, id_kc_pha_update = id_kc_pha.x) %>%
  distinct()

# Bring back to the names table and update IDs
names_final <- left_join(names_final, names_add, by = "id_hash") %>%
  mutate(id_kc_pha = ifelse(!is.na(id_kc_pha_update), id_kc_pha_update, id_kc_pha)) %>%
  select(-id_kc_pha_update) %>%
  group_by(id_kc_pha) %>%
  mutate(id_cnt = n()) %>%
  ungroup()

names_final %>% filter(pha_source != "existing_ids") %>% count(pha_source, id_cnt)


# TRY MATCHING ON HH_ID (KCHA) ----
# Bring in relevant fields from the stage KCHA table and join to hh_id
kcha_stage <- dbGetQuery(db_hhsaw, 
                         "SELECT DISTINCT hh_id, id_hash 
                         FROM pha.stage_kcha
                         WHERE hh_id IS NOT NULL")

kcha_hh_ids <- inner_join(kcha_stage, names_existing, by = "id_hash")


# Find the KCHA people who didn't match to anyone else and join to their hh_id
exit_hh_id <- left_join(filter(names_final, pha_source == "kcha_exit" & id_cnt == 1),
                        select(pha_exit, hh_id_hash, hh_id),
                        by = c("id_hash" = "hh_id_hash")) %>%
  select(-last_run)


# Bring it all together and see if an hh_id led to too many IDs merging
names_hh_id <- inner_join(kcha_hh_ids, exit_hh_id, by = c("hh_id", "lname", "fname")) %>% 
  group_by(id_kc_pha.y) %>% 
  mutate(id_cnt_new = n_distinct(id_kc_pha.x)) %>% 
  ungroup()

names_hh_id %>% count(id_cnt_new)

# Just keep the 1:1 updates
names_hh_id <- names_hh_id %>% 
  filter(id_cnt_new == 1) %>%
  select(id_hash.y, id_kc_pha.x) %>%
  rename(id_hash = id_hash.y, id_kc_pha_update = id_kc_pha.x) %>%
  distinct()

# Bring back to the names table and update IDs
names_final <- left_join(names_final, names_hh_id, by = "id_hash") %>%
  mutate(id_kc_pha = ifelse(!is.na(id_kc_pha_update), id_kc_pha_update, id_kc_pha)) %>%
  select(-id_kc_pha_update) %>%
  group_by(id_kc_pha) %>%
  mutate(id_cnt = n()) %>%
  ungroup()

names_final %>% filter(pha_source != "existing_ids") %>% count(pha_source, id_cnt)


# PREPARE STAGE EXIT TIMEVAR TABLE ----
## Join names back to exit data ----
pha_exit_id <- left_join(pha_exit, select(names_final, id_hash, id_kc_pha, id_cnt),
                         by = "id_hash") %>%
  left_join(., select(names_final, id_hash, id_kc_pha, id_cnt), by = c("hh_id_hash" = "id_hash")) %>%
  mutate(id_kc_pha = coalesce(id_kc_pha.x, id_kc_pha.y),
         id_cnt = coalesce(id_cnt.x, id_cnt.y))


## Set up the max period for each person ----
pha_timevar <- pha_timevar %>%
  group_by(id_kc_pha, period) %>%
  mutate(max_in_period = max(to_date),
         max_in_period_flag = to_date == max_in_period) %>%
  group_by(id_kc_pha) %>%
  mutate(max_period = max(period, na.rm = T)) %>%
  ungroup()

## Join exit data to the timevar table ----
# Only people who matched IDs will be included
# Just include key elements for now, add remaining details later once exit data is processed
pha_timevar_exit <- pha_timevar %>%
  select(id_kc_pha, hh_id_long, hh_id_kc_pha, agency, from_date, to_date, 
         period, max_in_period, max_in_period_flag, max_period) %>%
  left_join(., distinct(pha_exit_id, id_kc_pha, act_date, exit_reason, exit_category, pha_source, id_cnt),
            by = "id_kc_pha")


## Apply exit information across entire household ----
hh_timevar_exit <- pha_timevar_exit %>%
  distinct(hh_id_long, hh_id_kc_pha, act_date, exit_reason, exit_category, pha_source) %>%
  filter(!is.na(act_date))

# Use sqldf for non-equi join
pha_timevar_exit <- sqldf("SELECT a.*, b.hh_id_kc_pha AS hh_id_kc_pha_y, 
              b.act_date AS act_date_y, b.exit_reason AS exit_reason_y,
              b.exit_category AS exit_category_y, b.pha_source AS pha_source_y
              FROM 
              (SELECT * FROM pha_timevar_exit) a
              LEFT JOIN
              (SELECT * FROM hh_timevar_exit) b
              ON a.hh_id_long = b.hh_id_long AND 
              a.from_date <= b.act_date AND a.to_date >= b.act_date
              ") %>%
  rename(act_date.x = act_date) %>%
  mutate(act_date = coalesce(act_date.x, as.Date(act_date_y, origin = "1970-01-01")),
         # Can't use coalesce for other fields because sometimes they are NA when there is an act_date
         exit_reason = ifelse(is.na(act_date.x), exit_reason_y, exit_reason),
         exit_category = ifelse(is.na(act_date.x), exit_category_y, exit_category),
         pha_source = ifelse(is.na(act_date.x), pha_source_y, pha_source)) %>%
  select(-ends_with(".x"), -ends_with("_y")) %>%
  distinct()


## Set up a complete set of from/to dates for each exit a person has ----
# This is needed to find 'true' exits versus those where a person had activity after an exit
# Also to apply exit dates to all of a person's from/to dates 
#  (some are NA because they were associated with multiple household IDs)
# Make a list of a person's exit dates and add a count (don't include NAs)
exit_list <- pha_timevar_exit %>% filter(!is.na(act_date)) %>% distinct(id_kc_pha, act_date) %>%
  group_by(id_kc_pha) %>% mutate(exit_cnt = n_distinct(act_date)) %>% ungroup()

# Repeat a person's from/to date for each act date
timevar_repeat <- pha_timevar_exit %>% 
  distinct(id_kc_pha, agency, from_date, to_date, 
           period, max_in_period, max_in_period_flag, max_period) %>%
  left_join(., exit_list, by = "id_kc_pha") %>%
  arrange(id_kc_pha, act_date, from_date, to_date, agency)


## Flag if a person has activity after an exit date ----
timevar_repeat <- setDT(timevar_repeat) # Switch to data table for faster group work

# Flag when an act_date falls in a from/to date range
timevar_repeat[, in_range := act_date >= from_date & act_date <= to_date]

# Find periods where the act_date isn't in the last from/to dates in that period
# Sometimes there is a single day covered by a from/to date right after the exit date
# Sometimes people stay in housing for years after the act_date
timevar_repeat[, activity_mismatch := max(in_range != max_in_period_flag, na.rm = T), by = c("id_kc_pha", "act_date", "period")]

# Find the time between the act_date and end of the period
timevar_repeat[, activity_gap_max := case_when(in_range == 0 | in_range == F ~ NA_real_,
                                               TRUE ~ interval(start = act_date, end = max_in_period) / ddays(1) + 1)]
# If agency is same in the next period, find the time between this period and the next one
# If agency changes we would want to count this as an exit
timevar_repeat[, period_gap := case_when(period == max_period ~ NA_real_,
                                         max_in_period_flag == 0 | max_in_period_flag == F ~ NA_real_,
                                         lead(id_kc_pha, 1) == id_kc_pha & act_date == lead(act_date, 1) & 
                                           agency == lead(agency, 1) ~ 
                                           interval(start = to_date, end = lead(from_date, 1)) / ddays(1) + 1,
                                         TRUE ~ NA_real_)]
# Apply the period gap to the entire period
timevar_repeat[, period_gap := max(period_gap, na.rm = T), by = .(id_kc_pha, period)]
timevar_repeat[is.infinite(period_gap), period_gap := NA_real_]


# 1) For people with multiple periods (i.e., non-contiguous from/to date ranges),
#     need at least 12 months between the end of a period and the next period if
#     the exit occurs in the earlier period.
#     Using 12 months because this is the follow up time for most study outcomes.
# 2) Assume people short gaps (<1 year) between exit date and max date for that period
#      were a true exit.
#    This is to catch the people whose act_date was not in the final from/to date range for that period
#      or there was a gap between act_date and move out (e.g., if a voucher expired 
#      but the person did not move out for a few months).
# 3) Also include exits that fall within 182 days after a person's final to_date
#    (e.g., some exits due to death have a later act_date than the final to_date)
timevar_repeat[, true_exit := case_when(is.na(activity_mismatch) | is.na(act_date) ~ NA_integer_,
                                        !is.na(period_gap) & period_gap <= 365 ~ 0L,
                                        activity_gap_max < 365 ~ 1L, 
                                        max_in_period == T & act_date > to_date & lead(id_kc_pha, 1) != id_kc_pha & 
                                          interval(start = to_date, end = act_date) / ddays(1) + 1 <= 182 ~ 1L,
                                        TRUE ~ 0L)]


# Remove rows that don't have an in_range exit (except for ones that occur later than 
# the final to_date)
timevar_repeat[in_range == F & !(max_in_period == T & max_period == period & act_date > to_date), 
               `:=` (act_date = NA_Date_, activity_mismatch = NA_integer_, true_exit = NA_integer_)]
timevar_repeat <- unique(timevar_repeat)

# Also get rid of duplicate rows where one has an act_date and one does not
timevar_repeat[, `:=` (row_cnt = .N, dates = uniqueN(act_date, na.rm = T)), by = .(id_kc_pha, from_date, to_date)]
timevar_repeat <- timevar_repeat[!(row_cnt > dates & dates != 0 & is.na(act_date))]


# See if the act date was ever in range of a person's activity intervals
timevar_repeat[, ever_in_range := max(in_range), by = c("id_kc_pha", "act_date")]

timevar_repeat %>% count(ever_in_range, true_exit)


## Add in remaining timevar and exit columns ----
# Only add in bare minimum, join to regular timevar table on id and from_date to get rest
timevar_exit_final <- timevar_repeat %>%
  left_join(., 
            distinct(pha_timevar, id_kc_pha, from_date, to_date, hh_id_long, hh_id_kc_pha, cov_time,
                     disability, major_prog, subsidy_type, prog_type, operator_type, vouch_type_final, geo_hash_clean,
                     geo_kc_area, portfolio_final),
            by = c("id_kc_pha", "from_date", "to_date")) %>%
  left_join(., 
            distinct(pha_timevar_exit, id_kc_pha, act_date, exit_reason, exit_category, pha_source) %>%
              mutate(exit_year = year(act_date)),
            by = c("id_kc_pha", "act_date"))


## Add possible corrected exit date ----
# Many exit dates do not match the period's final to_date, either because it was before
#  (e.g., an eviction exit date but the person didn't move out for a few months) or 
#  because it was after (e.g., a death was recorded in the exit data at a later date)
#  after a final.
# Set up a second, possible correct exit date based on the to_date.
timevar_exit_final <- timevar_exit_final %>%
  mutate(act_date_mismatch = case_when(is.na(act_date) ~ NA_integer_,
                                       act_date != max_in_period ~ 1L,
                                       act_date == max_in_period ~ 0L,
                                       TRUE ~ NA_integer_))


## Tidy up exit reasons ----
timevar_exit_final <- timevar_exit_final %>%
  rename(exit_category_pha = exit_category) %>%
  # Remove a non-breaking space that messes up the join
  mutate(exit_reason = str_replace_all(exit_reason, "\u00A0", " ")) %>%
  left_join(., select(exit_def, exit_reason, exit_reason_clean, exit_category), by = "exit_reason")


## Reorder columns ----
# Also replace Infinite values
timevar_exit_final <- timevar_exit_final %>%
  mutate(activity_mismatch = ifelse(is.infinite(activity_mismatch), NA, activity_mismatch)) %>%
  select(id_kc_pha, hh_id_long, hh_id_kc_pha, agency, from_date, to_date, period, period_gap, cov_time, 
         max_in_period, max_in_period_flag, in_range, ever_in_range, activity_mismatch, activity_gap_max, 
         exit_cnt, exit_year, act_date, act_date_mismatch, 
         true_exit, exit_reason, exit_reason_clean, exit_category_pha, exit_category, 
         pha_source, disability, major_prog, subsidy_type, prog_type, operator_type, vouch_type_final, 
         geo_hash_clean, geo_kc_area, portfolio_final) %>%
  distinct() %>%
  mutate(last_run = Sys.time())


# LOAD TO SQL ----
# Connect to HHSAW again (probably has timed out)
db_hhsaw <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = "tcp:kcitazrhpasqlprp16.azds.kingcounty.gov,1433",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryPassword")

# LOAD DATA TO SQL ----
# Split into smaller tables to avoid SQL db_hhsawection issues
start <- 1L
max_rows <- 50000L
cycles <- ceiling(nrow(timevar_exit_final)/max_rows)

lapply(seq(start, cycles), function(i) {
  start_row <- ifelse(i == 1, 1L, max_rows * (i-1) + 1)
  end_row <- min(nrow(timevar_exit_final), max_rows * i)
  
  message("Loading cycle ", i, " of ", cycles)
  if (i == 1) {
    dbWriteTable(db_hhsaw,
                 name = DBI::Id(schema = "pha", table = "stage_pha_exit_timevar"),
                 value = as.data.frame(timevar_exit_final[start_row:end_row, ]),
                 overwrite = T, append = F)
  } else {
    dbWriteTable(db_hhsaw,
                 name = DBI::Id(schema = "pha", table = "stage_pha_exit_timevar"),
                 value = as.data.frame(timevar_exit_final[start_row:end_row ,]),
                 overwrite = F, append = T)
  }
})

# CLEAN UP ----
rm(list = ls(pattern = "match"))
rm(list = ls(pattern = "classify"))
rm(list = ls(pattern = "pairs"))
rm(list = ls(pattern = "names"))
rm(list = ls(pattern = "pha"))
rm(list = ls(pattern = "id"))
rm(list = ls(pattern = "exit"))
rm(kcha_stage, timevar_add, timevar_repeat)
rm(recursion_dt, recursion_level, remaining_dupes)
rm(cycles, max_rows, start)
rm(run_time, st, final_dedup, self_join_dups)
