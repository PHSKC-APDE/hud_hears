# Header ----
# Author: Danny Colombara
# Date: May 9, 2022
# R version 4.1.3
# Purpose: Set up data for HUD HEARS wage analysis
# Notes: This analysis will use data labeled SOW1, which is the HUD HEARS project, 
#        rather than SOW2, which is for the CDC evaluation. 
#        Wage data is for both 'GUIDE' (2017+) and 'UTAB' (2010-2017) because 
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
    pacman::p_load(lubridate, httr, rads, data.table, DBI, odbc, consort)
    
    # output folder
      outputdir <- "C:/Users/dcolombara/King County/DPH Health And Housing - Documents/HUD HEARS Study/wage_analysis/output/"
    
    # dictionary from GitHub
      dict <- data.table::fread(httr::content(
        httr::GET(url = "https://raw.githubusercontent.com/PHSKC-APDE/esd_etl/main/ref/esd_blob_dictionary.csv", 
                  httr::authenticate(Sys.getenv("GITHUB_PAT"), "")),
        type = "text", encoding = "UTF-8"))
      
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
      # limit both PHA data to 2016-01-01 <> 20218-12-31 to avoid temporal biases
      exits <- setDT(DBI::dbGetQuery(conn = hhsaw16, 
                              "SELECT id_hudhears, id_kc_pha, from_date, exit_date = act_date,
                                      exit_reason_clean, exit_category, agency, subsidy_type, major_prog
                              FROM [pha].[stage_pha_exit_timevar]
                              WHERE true_exit = 1 AND 
                              chooser = chooser_max AND
                              act_date IS NOT NULL AND
                              act_date >= '2016-01-01' AND act_date <= '2018-12-31' AND
                              exit_type_keep = 1 AND 
                              exit_order_study = exit_order_max_study AND 
                              exit_order_study IS NOT NULL"))

    # Get PHA data for counts for CONSORT diagram ----
        consort.dt <- setDT(DBI::dbGetQuery(conn = hhsaw16, 
                                       "SELECT id_hudhears, id_kc_pha, from_date, exit_date = act_date,
                                      exit_reason_clean, exit_category, agency, subsidy_type, major_prog, 
                                      true_exit, exit_type_keep, exit_order_study,
                                      exit_order_max_study
                              FROM [pha].[stage_pha_exit_timevar]
                                    WHERE chooser = chooser_max AND act_date IS NOT NULL"))    
        
        consort01_total_exits <- nrow(consort.dt)
        
        consort.dt[, outside_period := agency]
        consort.dt[(agency == "KCHA" & exit_date >= as.Date("2016-01-01") & exit_date <= as.Date("2018-12-31")) |
             (agency == "SHA" & exit_date >= as.Date("2016-01-01") & exit_date <= as.Date("2018-12-31")), 
           outside_period := NA]
        consort01_side_outside_period <- nrow(consort.dt[!is.na(outside_period)])
        consort01_side_KCHA_outside <- nrow(consort.dt[outside_period == "KCHA"])
        consort01_side_SHA_outside <- nrow(consort.dt[outside_period == "SHA"])
        consort02_outside_period <- nrow(consort.dt[is.na(outside_period)])
        
        consort.dt[true_exit == 0 & is.na(outside_period), false_exit := "False exit"]
        consort02_side_false_exit <- nrow(consort.dt[!is.na(false_exit)])
        consort03_false_exit <- nrow(consort.dt[is.na(outside_period) & is.na(false_exit)])
        
        
        consort.dt[, multiple_exits := "Multiple exits"]
        consort.dt[is.na(outside_period) & is.na(false_exit) & 
             exit_order_study == exit_order_max_study & exit_type_keep == 1, 
           multiple_exits := NA]
        consort03_side_multiple_exits <- nrow(consort.dt[is.na(outside_period) & is.na(false_exit) & !is.na(multiple_exits)])
        consort04_multiple_exits <- nrow(consort.dt[is.na(outside_period) & is.na(false_exit) & is.na(multiple_exits)])

        
        consort.dt[, exit_reasons := "Missing or neutral deaths"]
        consort.dt[is.na(outside_period) & is.na(false_exit) & is.na(multiple_exits) &
             exit_category %in% c("Positive", "Negative"),
           exit_reasons := NA]
        consort04_side_exit_reasons <- nrow(consort.dt[is.na(outside_period) & is.na(false_exit) & is.na(multiple_exits) & !is.na(exit_reasons)])
        consort05_exit_reasons <- nrow(consort.dt[is.na(outside_period) & is.na(false_exit) & is.na(multiple_exits) & is.na(exit_reasons)])

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
      # downloaded from Neighborhood Stabilization Program Data: https://www.huduser.gov/portal/datasets/NSP.html
      ami <- rbindlist(
        lapply(
          X = as.list(2014:2020), 
          FUN = function(X){
            tempami <- data.table::fread(
              httr::content(
                httr::GET(url = paste0("https://raw.githubusercontent.com/PHSKC-APDE/hud_hears/main/analyses/wages/ref/FY", X, "NSPLimits_50_120.csv"),
                          httr::authenticate(Sys.getenv("GITHUB_PAT"), "")), 
                type = "text", encoding = "UTF-8")
              )
            tempami[, amiyear := X]
            tempami[, grep("im120", names(tempami), value = T) := NULL]
            setnames(tempami, tolower(gsub(paste0("lim50_", X - 2000, "p"), "ami_", names(tempami))))
            tempami[, grep("^ami_", names(tempami), value = T) := lapply(.SD, function(X){X * 2}), .SDcols = grep("^ami_", names(tempami), value = T)] # calc 100% AMI
            tempami[, fips2010 := substr(fips2010, 1, 5)] # first 5 digits are State + County
            tempami <- tempami[state == 53] # Washington State only
            keepers <- c("amiyear", "fips2010", "name", grep("^ami", names(tempami), value = T))
            tempami <- tempami[, ..keepers]
          }
        )
      )
      
      # reshape wide to long
      ami <- melt(ami, 
                  id.vars = c("amiyear", "fips2010", "name"), 
                  measure.vars = grep("ami_", names(ami), value = T), 
                  variable.name = "hh_size", 
                  value.name = "ami")
      ami[, hh_size := as.integer(gsub("^ami_", "", hh_size))]
      
      ami <- unique(ami)
      
      setorder(ami, amiyear, fips2010, hh_size)
      
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
       # Note to my future self that the max date in the exit data prepared by Alastair is 2018-12-31 (as of 2022-05-18)
        
      # identify the quarter in which the exits took places 
        exits[, exit_qtr := lubridate::yq(paste0(year(exit_date), ":Q", quarter(exit_date)))]
        
      # drop when reason for exits prohibits ability to identify future wages
        exits <- exits[exit_reason_clean != "Deceased"]
        
      # keep only positive and negative exits (for simpler binary comparison & because moving in with family / friends will confuse analysis)
        exits <- exits[exit_category != "Neutral"]
        
        exits <- unique(exits)
        
        count05_unique <- rbind(copy(exits)[, .N, agency], data.table(agency = "Either", N = nrow(exits)))
        if(count05_unique[agency == 'Either']$N != consort05_exit_reasons){
          stop("The rows counts at this stage should match
               consort05_exit_reasons")
        }
        
        
    # PHA Demographic data (demographics) ----
      demographics <- unique(demographics)
      demographics[, geo_countyfp10 := "033"] # all PHA clients must live in KC

    # Opportunity index (opportunity) ----
      opportunity[, geo_tractce10 := substrRight(as.character(GEOID10), 1, 6)]  
      opportunity <- opportunity[, .(geo_tractce10, opportunity_index = OPP_Z)]
      
      opportunity <- unique(opportunity)
      
    # Area Median Income (ami) ----
      # already tidy
      
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
        
        consort06_match_id_esd <- nrow(combo)
        consort05_side_match_id_esd <- consort05_exit_reasons - nrow(combo)

    # merge demographics onto exit_xwalk ----
        combo <- merge(combo, demographics, by = c("id_hudhears", "exit_date"), all.x = TRUE, all.y = FALSE)
        
        missing_id_hh = rads::round2(100*nrow(combo[is.na(hh_id_kc_pha)])/nrow(combo), 0)
        if(missing_id_hh > 5){warning(paste0(
          "After merging the demographics table onto the exit data, ", missing_id_hh, "% of the rows are missing a household id (hh_id_kc_pha).
          These rows will be dropped from the analysis."
        ))}
        
        combo <- combo[!is.na(hh_id_kc_pha)]
        
        count07_add_demog <- rbind(copy(combo)[, .N, agency], data.table(agency = "Either", N = nrow(combo)))
        
        consort07_demographics <- nrow(combo)
        consort06_side_demographics <- consort06_match_id_esd - nrow(combo)

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
          for(qnum in 1:8){
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
          template[, qtr := -8:8, id_hudhears]

      # (2) merge template of all quarters of interest onto exit_xwalk_demographics ----
          # will have 8 additional rows per id, one for +/- 4 quarters from exit
            combo <- merge(combo, template, by = "id_hudhears", all = T) 
          
          # demographics are more or less constant, but address post exit is not, so set TRACT and COUNTY to NA after time zero
            combo[qtr >= 1, c("geo_tractce10", "geo_countyfp10") := NA ]
      
      # (3) merge on wages based on id_esd and specific quarters of interest ----
          combo <- merge(combo, copy(wage_combo_address), by = c("id_esd", "qtr_date"), all.x = T, all.y = F)
        
      # (4) fill in tract and county from ESD wage data addresses when possible for any qtr 1+ ----
        combo[qtr > 0 & !is.na(wage_geo_tractce10), geo_tractce10 := wage_geo_tractce10]
        combo[qtr > 0 & !is.na(wage_geo_countyfp10), geo_countyfp10 := wage_geo_countyfp10]
        combo[, c("wage_geo_countyfp10", "wage_geo_tractce10") := NULL]

      # (5) drop person if there is NOT >= one wage data point before AND after exit ----  
        usable_id_esd <- intersect(
          unique(combo[!is.na(wage) & qtr < 0]$id_esd), 
          unique(combo[!is.na(wage) & qtr > 0]$id_esd))
        combo <- combo[id_esd %in% usable_id_esd]
        
        count08_add_wages <- rbind(unique(combo[, .(id_esd, agency)])[, .N, agency], data.table(agency = "Either", N = length(unique(combo$id_esd)) ))

        consort08_wages <- length(unique(combo$id_esd))
        consort07_side_wages <- consort07_demographics - consort08_wages
        
# Tidy combined data.table ----
    # drop if have not been in public housing for at least one year ----
        combo <- combo[housing_time_at_exit >= 1]
        
        count09_min_1yr <- rbind(unique(combo[, .(id_esd, agency)])[, .N, agency], data.table(agency = "Either", N = length(unique(combo$id_esd)) ))
        
        consort09_min_1yr <- length(unique(combo$id_esd))
        consort09_side_min_1yr <- consort08_wages - consort09_min_1yr
        
    # fill in missing wage, hrs, & address data when possible ----
      # wage data ----
        # quarterly wage data
            setorder(combo[is.na(wage), .N, qtr], qtr)[]
            setorder(combo, id_esd, qtr_date, qtr)
            combo[, wage  := nafill(wage, type = 'locf'), by = "id_esd" ] # fill wage forward / downward
            combo[, wage  := nafill(wage, type = 'nocb'), by = "id_esd" ] # fill wage backward / upward
            combo[, hrs  := nafill(hrs, type = 'locf'), by = "id_esd" ] # fill wage forward / downward
            combo[, hrs  := nafill(hrs, type = 'nocb'), by = "id_esd" ] # fill wage backward / upward
        
        # hourly wage data
          combo[, wage_hourly := rads::round2(wage / hrs, 2)]
          # drop hourly wages that irrational
            combo[, year := year(qtr_date)]
            minwage <- fread(
              httr::content(
                httr::GET(url = "https://raw.githubusercontent.com/PHSKC-APDE/hud_hears/main/analyses/wages/ref/minimum_wage_history.csv", 
                                      httr::authenticate(Sys.getenv("GITHUB_TOKEN"), ""))
                )
              )
            minwage <- minwage[geography == "WA", .(year, minimumwage = as.numeric(gsub('\\$', '', wage)))]
            combo <- merge(combo, minwage, by = 'year', all.x = T, all.y = F)
            ids.w.problem.hourly.wages <- combo[is.nan(wage_hourly) | 
                                                 wage_hourly > 100 | # would not be in public housing if made $100/hr
                                                 wage_hourly < minimumwage # can't make less than WA minimum wage 
                                               ]$id_kc_pha 
            combo[id_kc_pha %in% ids.w.problem.hourly.wages, c("wage_hourly", "hrs") := NA]
            combo[, year := NULL]
        
      # address data ----
          # check if have address for each quarter of exit (qtr == 0)
          setorder(combo[is.na(geo_tractce10), .N, qtr])[] # rarely missing baseline address and when missing, always missing, so leave as is.
          setorder(combo[is.na(geo_countyfp10), .N, qtr])[] # never missing bc know all PHA are in KC
        
        # fill forward and backward for address in qtr 1:4 only
          # nafill only works with numeric and want to avoid DT[, myvar  := myvar[1], by= .(ID , cumsum(!is.na(myvar)) ) ] 
          combo[, c("geo_tractce10", "geo_countyfp10") := lapply(.SD, as.numeric), .SDcols = c("geo_tractce10", "geo_countyfp10")]
          # use nafill
            combo[qtr %in% 1:8, geo_tractce10  := nafill(geo_tractce10, type = 'locf'), by = "id_esd" ] # fill geoid backward / upward
            combo[qtr %in% 1:8, geo_tractce10  := nafill(geo_tractce10, type = 'nocb'), by = "id_esd" ] # fill geoid backward / upward
            combo[qtr %in% 1:8, geo_countyfp10  := nafill(geo_countyfp10, type = 'locf'), by = "id_esd" ] # fill geoid backward / upward
            combo[qtr %in% 1:8, geo_countyfp10  := nafill(geo_countyfp10, type = 'nocb'), by = "id_esd" ] # fill geoid backward / upward
          # convert geographies back to properly formatted character values
            # combo[, c("geo_tractce10", "geo_countyfp10") := lapply(.SD, as.character), .SDcols = c("geo_tractce10", "geo_countyfp10")]
            combo[, geo_tractce10 := sprintf("%06i", geo_tractce10)][geo_tractce10 == "    NA", geo_tractce10 := NA]
            combo[, geo_countyfp10 := sprintf("%03i", geo_countyfp10)][geo_countyfp10 == " NA", geo_countyfp10 := NA]

# Merge on reference data ----
      # merge on area median income by county ----
          combo[!is.na(geo_countyfp10), fips2010 := paste0("53", geo_countyfp10)]
          combo[, amiyear := year(qtr_date)]
          combo <- merge(combo, ami, by = c("amiyear", "fips2010", "hh_size"), all.x = T, all.y = F)
          combo[, c("fips2010", "amiyear") := NULL]
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

# Tidy exit definitions for analysis ----
    combo[, exit_category := factor(exit_category, levels = c("Positive", "Negative"))] # to force specific order in graph  
    combo[exit_category == "Negative", exit := 0]
    combo[exit_category == "Positive", exit := 1]
          
# Select/order columns in final dataset ----
    setorder(combo, hh_id_kc_pha, id_kc_pha, qtr)
    combo <- combo[, .(hh_id_kc_pha, id_kc_pha, 
                      exit, exit_category, exit_date, exit_year = year(exit_date), exit_qtr, qtr, qtr_date,
                      wage, hrs, wage_hourly, ami, percent_ami, opportunity_index,
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
    
    # write table to Azure 16 (production) SQL server ----
    table_config <- yaml::yaml.load(httr::GET(url = "https://raw.githubusercontent.com/PHSKC-APDE/hud_hears/main/analyses/wages/ref/wage_analytic_table.yaml", 
                                              httr::authenticate(Sys.getenv("GITHUB_TOKEN"), "")))
    DBI::dbWriteTable(conn = hhsaw16,
                      name = DBI::Id(schema = table_config$schema, table = table_config$table),
                      value = setDF(copy(combo)),
                      overwrite = T,
                      append = F, 
                      field.types = unlist(table_config$vars)) 

# Create consort diagram ----
    # nicely format all consort counts ----
      counts <- sort(grep("^consort[0-9][0-9]", ls(), value = T))
      for(uglynum in counts){
        assign(uglynum, format(get(uglynum), big.mark = ',', trim = T))
      }
      
    # create consort diagram ----
      consort.complete <- 
        add_box(txt = paste0("Total exits: ", consort01_total_exits)) |>
        add_side_box(txt = c(paste0("Exits outside study period: ", consort01_side_outside_period, 
                                    "\n\u2022 KCHA: ", consort01_side_KCHA_outside, 
                                    "\n\u2022 SHA: ", consort01_side_SHA_outside)))  |>
        add_box(txt = paste0("Exits in study period: ", consort02_outside_period))   |>
        add_side_box(txt = paste0("False exits: ", consort02_side_false_exit))   |>
        add_box(txt = paste0("True exits: ", consort03_false_exit))   |>
        add_side_box(txt = paste0("Multiple exits per person: ", consort03_side_multiple_exits))   |>
        add_box(txt = paste0("Single exit per person: ", consort04_multiple_exits))   |>
        add_side_box(txt = paste0("Neutral or missing exits: ", consort04_side_exit_reasons))   |>
        add_box(txt = paste0("Positive and negative exits reasons: ", consort05_exit_reasons))   |>
        add_side_box(txt = paste0("Could not link with ESD data: ", consort05_side_match_id_esd))   |>
        add_box(txt = paste0("Linked to ESD data: ", consort06_match_id_esd))   |>
        add_side_box(txt = paste0("Could not link with demographics: ", consort06_side_demographics))   |>
        add_box(txt = paste0("Linked to demographics: ", consort07_demographics))   |>
        add_side_box(txt = paste0("Could not link with wages: ", consort07_side_wages))   |>
        add_box(txt = paste0("Linked to wages: ", consort08_wages))   |>
        add_side_box(txt = paste0("Less than 1 year in public housing: ", consort09_side_min_1yr))   |>
        add_box(txt = paste0("At least one year in public housing: ", consort09_min_1yr))
        
      myplot <- plot(consort.complete)
      
    # save consort diagram ----
      ggplot2::ggsave(paste0(outputdir, "consort_diagram.pdf"),
             plot = consort.complete, 
             dpi=600, 
             width = 6.5, 
             height = 9, 
             units = "in") 
      ggplot2::ggsave(paste0(outputdir, "consort_diagram.png"),
             plot = consort.complete, 
             dpi=600, 
             width = 6.5, 
             height = 9, 
             units = "in") 
      
    
# The end! ----