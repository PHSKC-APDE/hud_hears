# Header ----
# Author: Danny Colombara
# Date: May 9, 2022
# R version 4.1.3
# Purpose: Set up data for HUD HEARS wage analysis
# Notes: This analysis will use data labeled SOW1, which is the HUD HEARS project, 
#        rather than SOW, which is for the CDC evaluation. 
#        Wage data is for both 'GUIDE' (2017+) and 'UTAB' (2010-2017) becasue 
#        they are sourced from separate longitudinal wage databases 
#        (TAXIS (1984-2014) & NGTS (2014+)).
#
# Notes: Will need to set up / create the following:
#        outcome_1: wages
#        outcome_2: %AMI
#        predictor: exit_type
#        intermediate vars:
#          - exit_date
#          - wage_date
#          - hh_members
#        potential confounders:
#          - race/eth, 
#          - gender, 
#          - age, 
#          - disability, 
#          - stability (time in public housing), 
#          - agency, 
#          - program, 
#          - opportunity index
#          - exit quarter
#        id_individual
#        id_hh
#
# Remember! HHSAW20 is DEV and HHSAW16 is PROD 


# Set-up ----
    rm(list=ls())
    options("scipen" = 999) # turn off scientific notation
    pacman::p_load(lubridate, httr, rads, data.table, DBI, odbc)
    
    # output folder
      outputdir <- "C:/Users/dcolombara/King County/DPH Health And Housing - Documents/HUD HEARS Study/wage_analysis/output/"
    
    # dictionary from GitHub
      dict <- fread("https://raw.githubusercontent.com/PHSKC-APDE/esd_etl/main/ref/esd_blob_dictionary.csv")
    
    # easy SQL connections
      devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/apde/main/R/create_db_connection.R") 
    
# Load data ----
    # open DB connection to HHSAW ----
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
      
    # ESD wage data ----
      wage <- setDT(DBI::dbGetQuery(conn = hhsaw20, 
                                    "SELECT id_esd, yr, qtr, wages, hrs 
                                     FROM [esd].[stage_sow1_wages]
                                     WHERE wages IS NOT NULL"))
      
    # ESD address data ----
      esd_address <- setDT(DBI::dbGetQuery(conn = hhsaw20, 
                                           "SELECT id_esd, start_date, end_date, geo_hash_geocode
                                           FROM [esd].[final_sow1_address]"))

    # Address geocodes ----
      geocodes <- setDT(DBI::dbGetQuery(conn = hhsaw16, 
                                            "SELECT geo_hash_geocode, geo_countyfp10, geo_tractce10
                                             FROM ref.address_geocode
                                             WHERE geo_statefp10 = 53"))

    # HUDHEARS <> ESD crosswalk ----
      # from https://github.com/PHSKC-APDE/hud_hears/blob/main/data_processing/load_analytic_tables.R#L46, which in turn is from
      # from https://github.com/PHSKC-APDE/hud_hears/blob/main/data_processing/load_stage_xwalk_ids.R
      # this will have to be recreated once we have the ESD data refresh
      xwalk <- setDT(DBI::dbGetQuery(conn = hhsaw16, 
                               "SELECT id_hudhears, id_esd = customerkey 
                               from [esd].[hudhears_id_xwalk]"))
      
    # PHA exit data ----
      # housing vars considered but not used: prog_type, operator_type, vouch_type_final
      # there is also a `to_date` and `max_in_period` column which is usually, but not always
      # the same as exit_date (i.e., act_date). Following example in the capstone project of only
      # using exit_date(act_date).
      exits <- setDT(DBI::dbGetQuery(conn = hhsaw16, 
                              "SELECT id_hudhears, id_kc_pha, from_date, exit_date = act_date,
                                      exit_reason_clean, exit_category, agency, subsidy_type, major_prog
                              FROM [pha].[stage_pha_exit_timevar]
                              WHERE true_exit = 1 AND chooser = chooser_max AND
                              true_exit = 1 AND 
                              act_date IS NOT NULL AND
                              exit_type_keep = 1 AND 
                              exit_order_study = exit_order_max_study AND 
                              exit_order_study IS NOT NULL"))

    # PHA Demographic data ----
      demographics <- setDT(DBI::dbGetQuery(conn = hhsaw16, 
                                "SELECT id_hudhears,
                                        hh_id_kc_pha, exit_date, gender_me, race_eth_me,
                                        age_at_exit, housing_time_at_exit, hh_size, 
                                        hh_disability, n_disability, single_caregiver, 
                                        geo_tractce10, kc_opp_index_score
                                        FROM [hudhears].[control_match_covariate]
                                WHERE id_type = 'id_exit'")) 

    # Opportunity index ----
      message("!!!NOTE!!!
          Opportunity Index is only available for 4 counties identified by the Puget Sound Regional Council.
          King (33), Kitsap (35), Pierce (53), Snohomish (61)")
      message("Raj Chetty's Opportunity Atlast / Opportunity Insights is available by census tract for the whole country
              but does not provide a singluar index / composite indicator.")
      
      myurl <- "https://raw.githubusercontent.com/PHSKC-APDE/hud_hears/main/analyses/capstone/00_opportunity_index/kc_opp_indices_scaled.csv"
      opportunity <- data.table::fread(httr::content(
        httr::GET(url = myurl, httr::authenticate(Sys.getenv("GITHUB_PAT"), "")), 
        type = "text", encoding = "UTF-8"))
      
    # Area Median Income (AMI) ----
      myurl <- "https://raw.githubusercontent.com/PHSKC-APDE/hud_hears/main/analyses/wages/ref/hud_30percent_ami_2022.csv"
      ami <- data.table::fread(httr::content(
        httr::GET(url = myurl, httr::authenticate(Sys.getenv("GITHUB_PAT"), "")),
        type = "text", encoding = "UTF-8"))
      
# Tidy datasets ----
    # ESD wage data (wage) ----
      # create quarter as a date
      wage[, qtr_date := lubridate::yq(paste0(yr, ":Q", qtr))]
      wage[, c("yr", "qtr") := NULL]
      
      # end before 2020 due to COVID-19 pandemic
      wage <- wage[qtr_date < "2020-01-01"]
      wage[, qtr_date2 := qtr_date] # needed because data.table::foverlaps needs start and end to interval
      
      # collapse wages from multiple jobs into a single row per id and time period
      setorder(wage, id_esd, qtr_date)
      wage <- wage[, .(wage = sum(wages), hrs = sum(hrs)), .(id_esd, qtr_date, qtr_date2)]
      
      wage <- unique(wage)
    
    # ESD address data (esd_address) ----  
      # already cleaned when making the 'final' table
      # only useful if IDs are in the wage data
      esd_address <- esd_address[id_esd %in% wage$id_esd] 
      
    # Address geocodes (geocodes) ----
      geocodes <- unique(geocodes)
      
    # HUDHEARS <> ESD crosswalk (xwalk) ----  
      # de-duplicate in agnostic manner if there are duplicate hudhears or esd ids
      xwalk[, dup := 1:.N, id_hudhears]
      xwalk <- xwalk[dup == 1]
      xwalk[, dup := NULL]
      
      xwalk[, dup := 1:.N, id_esd]
      xwalk <- xwalk[dup == 1]
      xwalk[, dup := NULL]
      
      xwalk[, id_esd := as.character(id_esd)]
      
      xwalk <- unique(xwalk)
      
    # PHA exit data (exits) ----
      # baseline count of exits
        count01_baseline <- rbind(copy(exits)[, .N, agency], data.table(agency = "Either", N = nrow(exits)))
      
      # ??? limit dates to when have data for BOTH KCHA and SHA to avoid temporal biases in use of agency in model as a confounder
        exits <- exits[exit_date >= "2016-01-01"]
        
        count02_limit_dates <- rbind(copy(exits)[, .N, agency], data.table(agency = "Either", N = nrow(exits)))
        
      # Note to my future self that the max date in the exit data prepared by Alastair is 2018-12-31 (as of 2022-05-18)
        
      # identify the quarter in which the exits took places 
        exits[, exit_qtr := lubridate::yq(paste0(year(exit_date), ":Q", quarter(exit_date)))]
        
      # drop when reason for exits prohibits ability to identify future wages
        exits <- exits[exit_reason_clean != "Deceased"]
        
        count03_drop_deceased <- rbind(copy(exits)[, .N, agency], data.table(agency = "Either", N = nrow(exits)))
        
      # keep only positive and negative exits (for simpler binary comparison & because moving in with family / friends will confuse analysis)
        exits <- exits[exit_category != "Neutral"]
        
        count04_drop_neutral <- rbind(copy(exits)[, .N, agency], data.table(agency = "Either", N = nrow(exits)))
        
        exits <- unique(exits)
        
        count05_unique <- rbind(copy(exits)[, .N, agency], data.table(agency = "Either", N = nrow(exits)))
        
    # PHA Demographic data (demographics) ----
      demographics <- unique(demographics)
      demographics[, geo_countyfp10 := "033"] # all PHA clients must live in KC

    # Opportunity index (opportunity) ----
      opportunity[, geo_tractce10 := substrRight(as.character(GEOID10), 1, 6)]  
      opportunity <- opportunity[, .(geo_tractce10, opportunity_index = OPP_Z)]
      
      opportunity <- unique(opportunity)
      
    # Area Median Income (ami) ----
      # back calculate 100% AMI from 30% AMI that is in the table
      amivars <- grep("^l30", names(ami), value = T)
      ami[, gsub("l30", "ami", amivars) := lapply(.SD, function(X){X / 0.3}), .SDcols = amivars]
      
      # keep select columns
      amikeep <- c("fips2010", gsub("l30", "ami", amivars))
      ami <- ami[, ..amikeep]
      
      # trim fips code
      ami[, fips2010 := substr(fips2010, 1, 5)] # first 5 digits are State + County
      
      # reshape wide to long
      ami <- melt(ami, 
                  id.vars = c("fips2010"), 
                  measure.vars = grep("ami_", names(ami), value = T), 
                  variable.name = "hh_size", 
                  value.name = "ami")
      ami[, hh_size := as.integer(gsub("^ami_", "", hh_size))]
      
      ami <- unique(ami)
      
      setorder(ami, fips2010, hh_size)
      
      
# Merge main datasets ----
    # merge crosswalk onto exits to get id_esd ----
        combo <- merge(exits, xwalk, by = "id_hudhears", all.x = T, all.y = F)
        
        missing_id_esd = rads::round2(100*nrow(combo[is.na(id_esd)])/nrow(combo), 0)
        if(missing_id_esd > 5){warning(paste0(
          "After merging the xwalk table onto the exit data, ", missing_id_esd, "% of the rows are missing an `id_esd`.
          These rows will be dropped from the analysis."
        ))}
        
        combo <- combo[!is.na(id_esd)]
        
        count06_xwalk_esd <- rbind(copy(combo)[, .N, agency], data.table(agency = "Either", N = nrow(combo)))
        
    # merge demographics onto exit_xwalk ----
        combo <- merge(combo, demographics, by = c("id_hudhears", "exit_date"), all.x = TRUE, all.y = FALSE)
        
        missing_id_hh = rads::round2(100*nrow(combo[is.na(hh_id_kc_pha)])/nrow(combo), 0)
        if(missing_id_hh > 5){warning(paste0(
          "After merging the demographics table onto the exit data, ", missing_id_hh, "% of the rows are missing a household id (hh_id_kc_pha).
          These rows will be dropped from the analysis."
        ))}
        
        combo <- combo[!is.na(hh_id_kc_pha)]
        
        count07_add_demog <- rbind(copy(combo)[, .N, agency], data.table(agency = "Either", N = nrow(combo)))

    # merge geocodes onto ESD address data ----
        address <- merge(esd_address, geocodes, by = "geo_hash_geocode", all.x = F, all.y = F)
        address <- unique(address[, geo_hash_geocode := NULL]) # was only needed to link to county and tract geoids
        
    # merge ESD address info onto wage data accounting for specific time windows ----
        # use data.table::foverlaps to limit when wage date is in the address time window
        # addresses from ESD data
        setkey(wage, id_esd, qtr_date, qtr_date2) # keys define the foverlaps 'by' variable, with final two designating the interval 
        setkey(address, id_esd, start_date, end_date)
        wage_address = foverlaps(wage, address, mult = "last", nomatch = NA) # keeps all the wage data and just the address data that can be merged by id_esd and date
        
        wage_address <- wage_address[, .(id_esd, qtr_date, qtr_date2, wage, hrs, geo_countyfp10, geo_tractce10)]
        
    # merge PHA addresses onto wage data by date to get most complete list of addresses ----
        # rename geographies to distinguish from those in wage data
        combo_address <- combo[, .(id_esd, from_date, exit_date, combo_geo_tractce10 = geo_tractce10, combo_geo_countyfp10 = geo_countyfp10)] 
        
        # speed up merge below by limiting wage data to id_esds that are in the remaining pool of PHA ids
        wage_address <- wage_address[id_esd %in% unique(combo$id_esd)]
        
        # use data.table::foverlaps to limit when wage date is in the address time window
        setkey(wage_address, id_esd, qtr_date, qtr_date2)
        setkey(combo_address, id_esd, from_date, exit_date)
        wage_combo_address = foverlaps(wage_address, combo_address, nomatch = NA)
        
        # preferentially trust address data from PHA over that from ESD wages
        wage_combo_address[!is.na(combo_geo_tractce10), geo_tractce10 := combo_geo_tractce10]
        wage_combo_address[!is.na(combo_geo_countyfp10), geo_countyfp10 := combo_geo_countyfp10]
        
        # keep specific columns
        wage_combo_address <- wage_combo_address[, .(id_esd, qtr_date, wage, hrs, wage_geo_countyfp10 = geo_countyfp10, wage_geo_tractce10 = geo_tractce10)]
        
    # merge wage onto exit_xwalk_demographics ----
      # (1) create template of id_hudhears and all relevant quarters (+/- 4 quarters from exit) ----
        # wide
          template <- combo[, .(id_hudhears, exit_qtr)]
          for(qnum in 1:4){
            template[, "t0" := exit_qtr] # quarter of exit
            template[, paste0("t-", qnum) := exit_qtr - months(qnum*3)] # 4 quarters before exit quarter
            template[, paste0("t", qnum) := exit_qtr + months(qnum*3)] # 4 quarters after exit quarter
          }
        # reshape wide to long
          template <- melt(template, 
                         id.vars = c("id_hudhears", "exit_qtr"), 
                         measure.vars = grep("^t", names(template), value = T),
                         value.name = "qtr_date")
        # tidy and create quarter indicator
          template <- setorder(template[, .(id_hudhears, qtr_date)], id_hudhears, qtr_date)
          template[, qtr := -4:4, id_hudhears]

      # (2) merge template of all quarters of interest onto exit_xwalk_demographics ----
          # will have 8 additional rows per id, one for +/- 4 quarters from exit
            combo <- merge(combo, template, by = "id_hudhears", all = T) 
          
          # demographics are more or less constant, but address post exit is not, so set TRACT and COUNTY to NA after time zero
            combo[qtr %in% 1:4, c("geo_tractce10", "geo_countyfp10") := NA ]
      
      # (3) merge on wages based on id_esd and specific quarters of interest ----
          combo <- merge(combo, copy(wage_combo_address), by = c("id_esd", "qtr_date"), all.x = T, all.y = F)
        
      # (4) fill in tract and county from ESD wage data addresses when possible for any qtr 1+ ----
        combo[qtr > 0 & !is.na(wage_geo_tractce10), geo_tractce10 := wage_geo_tractce10]
        combo[qtr > 0 & !is.na(wage_geo_countyfp10), geo_countyfp10 := wage_geo_countyfp10]
        combo[, c("wage_geo_countyfp10", "wage_geo_tractce10") := NULL]

      # (5) drop person if there is NOT >= one wage data point before AND after exit ----  
        usable_id_esd <- intersect(
          unique(combo[!is.na(wage) & qtr %in% -4:0]$id_esd), 
          unique(combo[!is.na(wage) & qtr %in% 1:4]$id_esd))
        combo <- combo[id_esd %in% usable_id_esd]
        
        count08_add_wages <- rbind(unique(combo[, .(id_esd, agency)])[, .N, agency], data.table(agency = "Either", N = length(unique(combo$id_esd)) ))

# Tidy combined data.table ----
    # drop if have not been in public housing for at least one year ----
        combo <- combo[housing_time_at_exit >= 1]
        
        count09_min_1yr <- rbind(unique(combo[, .(id_esd, agency)])[, .N, agency], data.table(agency = "Either", N = length(unique(combo$id_esd)) ))
        
    # fill in missing wage, hrs, & address data when possible ----
      # wage data ----
        setorder(combo[is.na(wage), .N, qtr], qtr)[]
        setorder(combo, id_esd, qtr_date, qtr)
        combo[, wage  := nafill(wage, type = 'locf'), by = "id_esd" ] # fill geoid forward / downward
        combo[, wage  := nafill(wage, type = 'nocb'), by = "id_esd" ] # fill geoid backward / upward
        combo[, hrs  := nafill(hrs, type = 'locf'), by = "id_esd" ] # fill geoid forward / downward
        combo[, hrs  := nafill(hrs, type = 'nocb'), by = "id_esd" ] # fill geoid backward / upward
        
      # address data ----
          # check if have address for each quarter of exit (qtr == 0)
          setorder(combo[is.na(geo_tractce10), .N, qtr])[] # rarely missing baseline address and when missing, always missing, so leave as is.
          setorder(combo[is.na(geo_countyfp10), .N, qtr])[] # never missing bc know all PHA are in KC
        
        # fill forward and backward for address in qtr 1:4 only
          # nafill only works with numeric and want to avoid DT[, myvar  := myvar[1], by= .(ID , cumsum(!is.na(myvar)) ) ] 
          combo[, c("geo_tractce10", "geo_countyfp10") := lapply(.SD, as.numeric), .SDcols = c("geo_tractce10", "geo_countyfp10")]
          # use nafill
            combo[qtr %in% 1:4, geo_tractce10  := nafill(geo_tractce10, type = 'locf'), by = "id_esd" ] # fill geoid backward / upward
            combo[qtr %in% 1:4, geo_tractce10  := nafill(geo_tractce10, type = 'nocb'), by = "id_esd" ] # fill geoid backward / upward
            combo[qtr %in% 1:4, geo_countyfp10  := nafill(geo_countyfp10, type = 'locf'), by = "id_esd" ] # fill geoid backward / upward
            combo[qtr %in% 1:4, geo_countyfp10  := nafill(geo_countyfp10, type = 'nocb'), by = "id_esd" ] # fill geoid backward / upward
          # convert geographies back to properly formatted character values
            # combo[, c("geo_tractce10", "geo_countyfp10") := lapply(.SD, as.character), .SDcols = c("geo_tractce10", "geo_countyfp10")]
            combo[, geo_tractce10 := sprintf("%06i", geo_tractce10)][geo_tractce10 == "    NA", geo_tractce10 := NA]
            combo[, geo_countyfp10 := sprintf("%03i", geo_countyfp10)][geo_countyfp10 == " NA", geo_countyfp10 := NA]

# Merge on reference data ----
      # merge on area median income by county ----
          combo[!is.na(geo_countyfp10), fips2010 := paste0("53", geo_countyfp10)]
          combo <- merge(combo, ami, by = c("fips2010", "hh_size"), all.x = T, all.y = F)
          combo[, c("fips2010") := NULL]
          combo[, percent_ami := rads::round2(100*4*wage / ami, 1)] # multiply by four because wage is quarterly but AMI is annual
          message("Some KC households will not have an AMI because they have a HH size >8 and the reference sheet only applies to 
                  households with size 1:8")
          
      # merge opportunity index onto combo table ----      
          combo <- merge(combo, opportunity, by = c("geo_tractce10"), all.x =  T, all.y = F)  
          message("!!!NOTE!!!
          Opportunity Index is only available for 4 counties identified by the Puget Sound Regional Council.
          King (33), Kitsap (35), Pierce (53), Snohomish (61). /n
          Raj Chetty's Opportunity Atlast / Opportunity Insights is available by census tract for the whole country
          but does not provide a singluar index / composite indicator.")
          
          # ignore kc_opp_index_score bc copies opportunity_index, but only for quarters -4:0 and only for KC
          combo[, kc_opp_index_score := NULL]

# Select/order columns in final dataset ----
    setorder(combo, hh_id_kc_pha, id_kc_pha, qtr)
    combo <- combo[, .(hh_id_kc_pha, id_kc_pha, 
                      exit_category, exit_date, exit_qtr, qtr, qtr_date,
                      wage, hrs, ami, percent_ami, opportunity_index,
                      race_eth_me, gender_me, age_at_exit, hh_size, hh_disability, n_disability, 
                      single_caregiver, housing_time_at_exit, 
                      agency, major_prog, subsidy_type, exit_reason_clean)]

    count10_final <- rbind(unique(combo[, .(id_kc_pha, agency)])[, .N, agency], data.table(agency = "Either", N = length(unique(combo$id_kc_pha)) ))
    
# Write analytic table ----
    # open connection to prod----
      hhsaw16 = create_db_connection( # this is dev
        server = "hhsaw", 
        prod = T, 
        interactive = F
      )
    
    # write table to T ----
    DBI::dbWriteTable(conn = hhsaw16, 
                      name = DBI::Id(schema = "hudhears", table = "wage_analytic_table"), 
                      value = setDF(copy(combo)), 
                      append = F, 
                      overwrite = T)
      
# Get summary of all counts as data set is processed ----
    counts <- grep("^count", ls(), value = T)
    count.history <- data.table(source = NA_character_, agency = NA_character_, N = NA_integer_)
    for(cc in counts){
      tempy <- get(cc)
      tempy[, source := cc]
      count.history <- rbind(count.history, tempy)
    }
    
    # reshape long to wide
    count.history <- dcast(count.history, source ~ agency, value.var = "N")
    count.history[, "NA" := NULL]
    count.history <- count.history[!is.na(source)]
    
# Save & print summary of all counts as data is processed ----
    write.csv(count.history, paste0(outputdir, "count_history.csv"))
    print(count.history)
    
# The end! ----