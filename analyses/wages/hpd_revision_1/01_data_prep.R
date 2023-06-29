# Header ----
# Author: Danny Colombara
# Date: June 6, 2023
# R version 4.2.2
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
#          - exit quarter
#        id_individual
#        id_hh
#
#   Decided to drop opportunity index from analysis on 10/24/2022 due to reviewer
#   feedback on initial papers and consistency with those papers
#
# Remember! HHSAW20 is DEV and HHSAW16 is PROD 


# Set-up ----
    rm(list=ls())
    options("scipen" = 999) # turn off scientific notation
    pacman::p_load(lubridate, httr, rads, data.table, DBI, odbc, consort, Microsoft365R)
    
    # output folder
      # will export to SharePoint below
    
    # dictionary from GitHub
      dict <- data.table::fread(httr::content(
        httr::GET(url = "https://raw.githubusercontent.com/PHSKC-APDE/esd_etl/main/ref/esd_blob_dictionary.csv", 
                  httr::authenticate(Sys.getenv("GITHUB_PAT"), "")),
        type = "text", encoding = "UTF-8"))
      
    # easy SQL connections
      devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/apde/main/R/create_db_connection.R") 
      
    # connect to SharePoint
      site <- get_team("DPH Health And Housing")
      drv <- site$get_drive("Documents")
      
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
      wage <- setDT(DBI::dbGetQuery(conn = hhsaw20, "SELECT id_hudhears, yr, qtr, wages, hrs FROM [esd].[stage_sow1_wage3]"))
      
    # ESD address data ----
      # main address info
      esd_address <- setDT(DBI::dbGetQuery(conn = hhsaw20, "SELECT * FROM [esd].[stage_sow1_address3]"))
      
      # geocodes
        geocodes <- setDT(
          kcgeocode::fetch_addresses(ads = esd_address, 
                                     input_type = "raw", 
                                     geocode = T, 
                                     con = hhsaw16, 
                                     deduplicate = T))  
      # merge on geocode 
        esd_geo <- merge(esd_address, geocodes, by = c("geo_add1_raw", "geo_add2_raw", "geo_add3_raw", "geo_city_raw", "geo_state_raw", "geo_zip_raw"), all.x = T, all.y = F)
        esd_geo <- esd_geo[, .(id_hudhears, start_date, end_date, geo_id10_county, geo_id10_tract, geo_id10_state)]
        rm(geocodes); rm(esd_address)  
      
        
    # HUDHEARS <> ESD crosswalk ----
      # no longer needed because id_hudhears is already on wage and address tables
        
    # PHA exit data ----
      # housing vars considered but not used: prog_type, operator_type, vouch_type_final
      # there is also a `to_date` and `max_in_period` column which is usually, but not always
      # the same as exit_date (i.e., act_date). Following example in the capstone project of only
      # using exit_date(act_date).
      # limit both PHA data to 2016-01-01 <> 20218-12-31 to avoid temporal biases
      exits <- setDT(DBI::dbGetQuery(conn = hhsaw16, 
                              "SELECT id_hudhears, id_kc_pha, from_date, exit_date = act_date,
                                      exit_reason_clean, exit_category, agency, subsidy_type, major_prog, prog_type
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
                                       "SELECT id_hudhears, hh_id_kc_pha, id_kc_pha, from_date, exit_date = act_date,
                                      exit_reason_clean, exit_category, agency, subsidy_type, major_prog, 
                                      true_exit, exit_type_keep, exit_order_study,
                                      exit_order_max_study
                              FROM [pha].[stage_pha_exit_timevar]
                                    WHERE chooser = chooser_max AND act_date IS NOT NULL"))    
        birthdays <- setDT(DBI::dbGetQuery(conn = hhsaw16, 
                                           "SELECT id_kc_pha, dob FROM [pha].[final_demo]"))  
        consort.dt <- merge(consort.dt, birthdays, by = 'id_kc_pha', all.x = T, all.y = F)
        consort.dt[, age := rads::calc_age(dob, exit_date)]
        
        consort01_total_exits <- nrow(consort.dt)
        consort01_total_exits_hh <- length(unique((consort.dt$hh_id_kc_pha)))
        
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

        # combine consort #2 & #3
        consort02_side_false_exit <- consort02_side_false_exit + consort03_side_multiple_exits
        consort03_false_exit <- copy(consort04_multiple_exits)
        
        consort.dt[, exit_reasons := "Missing or neutral deaths"]
        consort.dt[is.na(outside_period) & is.na(false_exit) & is.na(multiple_exits) &
             exit_category %in% c("Positive", "Negative", 'Neutral') & (exit_reason_clean != "Deceased"),
           exit_reasons := NA]
        consort04_side_exit_reasons <- nrow(consort.dt[is.na(outside_period) & is.na(false_exit) & is.na(multiple_exits) & !is.na(exit_reasons)])
        consort05_exit_reasons <- nrow(consort.dt[is.na(outside_period) & is.na(false_exit) & is.na(multiple_exits) & is.na(exit_reasons)])

    # PHA Demographic data ----
      demographics <- setDT(DBI::dbGetQuery(conn = hhsaw16, 
                                "SELECT id_hudhears,
                                        hh_id_kc_pha, exit_date, gender_me, race_eth_me,
                                        age_at_exit, housing_time_at_exit, hh_size, 
                                        hh_disability, n_disability, single_caregiver, 
                                        geo_tractce10
                                        FROM [hudhears].[control_match_covariate]
                                WHERE id_type = 'id_exit'")) # Where condition limits to exits only, if not specified, can also get controls

    # Area Median Income (AMI) ----
      # Previously downloaded from Neighborhood Stabilization Program Data: 
      # https://www.huduser.gov/portal/datasets/NSP.html
      # BUT, their scale only went up to a household of 8 people. 
      # New data table received from Tyler 2023-06-26 goes up to 12 people, which covers all households in data
      # data is from 2018, but data is minimal and I noted in the paper that inflation is ignored, 
      # so use the 2018 AMI for 2016:2018
      ami <- fread(file.path(here::here(), "analyses/wages/ref/KCHA_2018_Area_Median_Income.csv"))[, c('notes', 'multiplier') := NULL]
      ami <- setDT(tidyr::crossing(data.table(amiyear = c(2015:2018)), ami))

    # MIT living wage ----
      living_wage <- fread(paste0(here::here(), '/analyses/wages/ref/MIT_Living_Wage_2023_06_06.csv'))
      living_wage <- living_wage[, .(adults, children, living_wage_2016, living_wage_2017, living_wage_2018)]
      living_wage[, mitlw_category := paste0('cat', formatC(.I, digits = 1, flag = '0', format = 'd'))]

# Tidy datasets ----
    # ESD wage data (wage) ----
      # create quarter as a date
      wage[, qtr_date := lubridate::yq(paste0(yr, ":Q", qtr))]
      wage[, c("yr", "qtr") := NULL]
      
      # end before 2020 due to COVID-19 pandemic
      wage <- wage[qtr_date < "2020-01-01"]
      wage[, qtr_date2 := qtr_date] # needed because data.table::foverlaps needs start and end to interval
      
      # collapse wages from multiple jobs into a single row per id and time period
      setorder(wage, id_hudhears, qtr_date)
      wage <- wage[, .(wage = sum(wages), hrs = sum(hrs)), .(id_hudhears, qtr_date, qtr_date2)]
      
      wage <- wage[!is.na(id_hudhears)]
      
      wage <- unique(wage)
      
    # ESD address data (esd_geo) ----  
      esd_geo <- unique(esd_geo[id_hudhears %in% wage$id_hudhears]) # only useful if IDs are in the wage data 

    # HUDHEARS <> ESD crosswalk (xwalk) ----  
      # NA because wage and address data already have id_hudhears
      
    # PHA exit data (exits) ----
       # Note to my future self that the max date in the exit data prepared by Alastair is 2018-12-31 (as of 2022-05-18)
        
      # identify the quarter in which the exits took places 
        exits[, exit_qtr := lubridate::yq(paste0(year(exit_date), ":Q", quarter(exit_date)))]
        
      # drop when reason for exits prohibits ability to identify future wages
        exits <- exits[exit_reason_clean != "Deceased"]
        
      # keep only positive and negative exits (for simpler binary comparison & because moving in with family / friends will confuse analysis)
        # exits <- exits[exit_category != "Neutral"] # now will process neutral exits as a separate group because of reviewer comment
        
        exits <- unique(exits)
        
        count05_unique <- rbind(copy(exits)[, .N, agency], data.table(agency = "Either", N = nrow(exits)))
        if(count05_unique[agency == 'Either']$N != consort05_exit_reasons){
          stop("The rows counts at this stage should match
               consort05_exit_reasons")
        }

    # PHA Demographic data (demographics) ----
      demographics <- unique(demographics)
      demographics[, geo_countyfp10 := "033"] # all PHA clients must live in KC

      # create combined race/gender indicator as per ESD request
      for(GG in c("Female", "Male")){
        for(RR in unique(demographics$race_eth_me)){
          demographics[gender_me == GG & race_eth_me == RR, race_gender := paste(RR, GG)]
        }
      }
      
      # create count of kids < 18 for eventual linkage to MIT Living Wage data
      demographics <- merge(demographics, 
                           unique(copy(demographics)[age_at_exit <= 17, kids := .N, hh_id_kc_pha][!is.na(kids), .(hh_id_kc_pha, kids)]),
                           by = 'hh_id_kc_pha', 
                           all.x = T, 
                           all.y = F)[is.na(kids), kids := 0]

      # create count of adult >= 18 for eventual linkage to MIT Living Wage data
      demographics <- merge(demographics, 
                           unique(copy(demographics)[age_at_exit > 17, adults := .N, hh_id_kc_pha][!is.na(adults), .(hh_id_kc_pha, adults)]),
                           by = 'hh_id_kc_pha', 
                           all.x = T, 
                           all.y = F)[is.na(adults), adults := 0]
      
      demographics[kids == 0 & adults == 0, c('kids', 'adults') := NA] # when both are zero, means age was missing so can't know
      
      
    # Area Median Income (ami) ----
      # already tidy
      
# Merge main datasets ----
      # merge demographics onto exits to get age for those who do not match
        combo <- merge(exits, demographics[, .(id_hudhears, tempage = age_at_exit)], by = "id_hudhears", all.x = T, all.y = F)
        
      # limit to working age individuals 
        consort05.5_side_age <- nrow(combo[tempage < 18 | tempage > 61 | is.na(tempage)])
        consort05.5_age <- nrow(combo[tempage %in% c(18:61) & !is.na(tempage)])
        combo <- combo[tempage %in% c(18:61) & !is.na(tempage)] # keep those who are working age
        
        count06_xwalk_esd <- rbind(copy(combo)[, .N, agency], data.table(agency = "Either", N = nrow(combo)))
        
        consort06_match_id_esd <- nrow(combo)

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

    # merge ESD address info onto ESD wage data accounting for specific time windows ----
        # use data.table::foverlaps to limit when wage date is in the address time window
        # addresses from ESD data
        setkey(wage, id_hudhears, qtr_date, qtr_date2) # keys define the foverlaps 'by' variable, with final two designating the interval 
        setkey(esd_geo, id_hudhears, start_date, end_date)
        wage_address = foverlaps(wage, esd_geo, mult = "all", nomatch = NA) # keeps all the wage data and just the address data that can be merged by id_esd and date
        
        wage_address <- wage_address[, .(id_hudhears, qtr_date, qtr_date2, wage, hrs, geo_id10_county, geo_id10_tract)] # order and keep columns of interest
        
            # fill address forward and backward 
            # nafill only works with numeric and want to avoid DT[, myvar  := myvar[1], by= .(ID , cumsum(!is.na(myvar)) ) ] 
            # so convert to numeric before using nafill
            wage_address[, c("geo_id10_tract", "geo_id10_county") := lapply(.SD, as.numeric), .SDcols = c("geo_id10_tract", "geo_id10_county")]
            # use nafill
            wage_address[, geo_id10_tract  := nafill(geo_id10_tract, type = 'locf'), by = "id_hudhears" ] # fill forward / downward
            wage_address[, geo_id10_tract  := nafill(geo_id10_tract, type = 'nocb'), by = "id_hudhears" ] # fill geoid backward / upward
            wage_address[, geo_id10_county  := nafill(geo_id10_county, type = 'locf'), by = "id_hudhears" ] # fill forward / downward
            wage_address[, geo_id10_county  := nafill(geo_id10_county, type = 'nocb'), by = "id_hudhears" ] # fill geoid backward / upward
            # convert geographies back to properly formatted character values
            wage_address[!is.na(geo_id10_tract), temp_geo_id10_tract := sprintf("%06g", geo_id10_tract)]
            wage_address[!is.na(geo_id10_county), temp_geo_id10_county := sprintf("%03g", geo_id10_county)]
            wage_address[, c("geo_id10_tract", "geo_id10_county") := NULL]
            setnames(wage_address, c("temp_geo_id10_tract", "temp_geo_id10_county"), c("geo_id10_tract", "geo_id10_county"))
        
        # speed up merge below by limiting wage data to id_esds that are in the remaining pool of PHA clients
        wage_address <- wage_address[id_hudhears %in% unique(combo$id_hudhears)]
        
    # merge PHA addresses onto ESD wage data by date to get most complete list of addresses ----
        # rename geographies in the housing data to distinguish from those in wage data
        combo_address <- combo[, .(id_hudhears, from_date, exit_date, combo_geo_tractce10 = geo_tractce10, combo_geo_countyfp10 = geo_countyfp10)] 
        
        # use data.table::foverlaps to limit when wage date is in the address time window
        setkey(wage_address, id_hudhears, qtr_date, qtr_date2)
        setkey(combo_address, id_hudhears, from_date, exit_date)
        wage_combo_address = foverlaps(wage_address, combo_address, nomatch = NA)
        
        # preferentially trust address data from PHA over that from ESD wages
        wage_combo_address[, geo_tractce10 := geo_id10_tract]
        wage_combo_address[!is.na(combo_geo_tractce10), geo_tractce10 := combo_geo_tractce10]
        wage_combo_address[, geo_countyfp10 := geo_id10_county ]
        wage_combo_address[!is.na(combo_geo_countyfp10), geo_countyfp10 := combo_geo_countyfp10]
        
        # keep specific columns
        wage_combo_address <- wage_combo_address[, .(id_hudhears, qtr_date, wage, hrs, wage_geo_countyfp10 = geo_countyfp10, wage_geo_tractce10 = geo_tractce10)]
        
    # merge wage onto exit_xwalk_demographics ----
      # (1) create template of id_hudhears and all relevant quarters (+/- 4 quarters from exit) ----
        # while want +/- 4 quarters from exit, will process +/- 8 quarters for QA of wage trends
        # in raw data and to fill in missing information when possible
        
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
          # will have additional rows per id, one for +/- 8 quarters from exit
            combo <- merge(combo, template, by = "id_hudhears", all = T) 
          
          # demographics are more or less constant, but address post exit is not, so set TRACT and COUNTY to NA after time zero
            combo[qtr >= 1, c("geo_tractce10", "geo_countyfp10") := NA ]
      
      # (3) merge on wages based on id_esd and specific quarters of interest ----
          combo <- merge(combo, copy(wage_combo_address), by = c("id_hudhears", "qtr_date"), all.x = T, all.y = F)
        
      # (4) fill in tract and county from ESD wage data addresses when possible for any qtr 1+ ----
        combo[qtr > 0 & !is.na(wage_geo_tractce10), geo_tractce10 := wage_geo_tractce10]
        combo[qtr > 0 & !is.na(wage_geo_countyfp10), geo_countyfp10 := wage_geo_countyfp10]
        combo[, c("wage_geo_countyfp10", "wage_geo_tractce10") := NULL]

      # (5) drop outlier wage values ----  
        combo[wage >= mean(wage, na.rm = T) + (3*sd(wage, na.rm = T)), wage := NA] # outlier if wage is > 3 z-scores from mean
        combo[wage <= mean(wage, na.rm = T) - (3*sd(wage, na.rm = T)), wage := NA] # outlier if wage is > 3 z-scores from mean
        
      # (6) drop person if there is NOT >= one wage data point before AND after exit ----  
        length(unique(combo[]$id_hudhears))
        
        usable_id_hudhears <- intersect(
          unique(combo[!is.na(wage) & qtr < 0]$id_hudhears), 
          unique(combo[!is.na(wage) & qtr > 0]$id_hudhears))
        
        combo <- combo[id_hudhears %in% usable_id_hudhears]
        
        count08_add_wages <- rbind(unique(combo[, .(id_hudhears, agency)])[, .N, agency], data.table(agency = "Either", N = length(unique(combo$id_hudhears)) ))

        consort08_wages <- length(unique(combo$id_hudhears))
        consort07_side_wages <- consort07_demographics - consort08_wages
        
# Tidy combined data.table ----
    # drop if have not been in public housing for at least one year ----
        combo <- combo[housing_time_at_exit >= 1]
        
        count09_min_1yr <- rbind(unique(combo[, .(id_hudhears, agency)])[, .N, agency], data.table(agency = "Either", N = length(unique(combo$id_hudhears)) ))
        
        consort09_min_1yr <- length(unique(combo$id_hudhears))
        consort09_side_min_1yr <- consort08_wages - consort09_min_1yr
        
    # Drop if age_at_exit is.na OR > 61 for any wage earner ----
        senior.households <- unique(combo[age_at_exit > 61]$hh_id_kc_pha)
        
        consort10_side_agelimit <- length(unique(combo[hh_id_kc_pha %in% senior.households | is.na(age_at_exit)]$id_hudhears))
        
        combo <- combo[!hh_id_kc_pha %in% senior.households]
        combo <- combo[!is.na(age_at_exit)]
        consort10_agelimit = length(unique(combo$id_hudhears))
        consort10_agelimit_hh <- length(unique((combo$hh_id_kc_pha)))
        
        
    # fill in missing wage, hrs, & address data when possible ----
      # wage data ----
        # quarterly wage data
            setorder(combo[is.na(wage), .N, qtr], qtr)[] # just to see distribution of where we have data
            setorder(combo, id_hudhears, qtr_date, qtr)
            combo[, wage  := nafill(wage, type = 'locf'), by = "id_hudhears" ] # fill wage forward / downward
            combo[, wage  := nafill(wage, type = 'nocb'), by = "id_hudhears" ] # fill wage backward / upward
            combo[, hrs  := nafill(hrs, type = 'locf'), by = "id_hudhears" ] # fill hours forward / downward
            combo[, hrs  := nafill(hrs, type = 'nocb'), by = "id_hudhears" ] # fill hours backward / upward
        
        # hourly wage data
          combo[, wage_hourly := rads::round2(wage / hrs, 2)]
          # drop hourly wages that irrational
            combo[is.infinite(wage_hourly), wage_hourly := NA]
            combo[, year := year(qtr_date)]
            minwage <- fread(
              httr::content(
                httr::GET(url = "https://raw.githubusercontent.com/PHSKC-APDE/hud_hears/main/analyses/wages/ref/minimum_wage_history.csv", 
                                      httr::authenticate(Sys.getenv("GITHUB_TOKEN"), ""))
                )
              )
            minwage <- minwage[geography == "WA", .(year, minimumwage = as.numeric(gsub('\\$', '', wage)))]
            combo <- merge(combo, minwage, by = 'year', all.x = T, all.y = F)
            ids.w.problem.hourly.wages <- combo[is.na(wage_hourly) |
                                                is.nan(wage_hourly) | 
                                                wage_hourly > 100 | # would not receive federal housing assistance if made $100/hr
                                                wage_hourly < minimumwage # can't make less than WA minimum wage 
                                               ]$id_kc_pha 
            combo[id_kc_pha %in% ids.w.problem.hourly.wages, c("wage_hourly", "hrs") := NA]
            combo[, year := NULL]
        
      # address data ----
          # check if have address for each quarter of exit (qtr == 0)
          setorder(combo[is.na(geo_tractce10), .N, qtr])[] # rarely missing baseline address and when missing, always missing, so leave as is.
          setorder(combo[is.na(geo_countyfp10), .N, qtr])[] # never missing bc know all PHA are in KC
        
        # fill forward and backward for address in qtr 1:8 only
          # nafill only works with numeric and want to avoid DT[, myvar  := myvar[1], by= .(ID , cumsum(!is.na(myvar)) ) ] 
          # so convert to numeric before using nafill
          combo[, c("geo_tractce10", "geo_countyfp10") := lapply(.SD, as.numeric), .SDcols = c("geo_tractce10", "geo_countyfp10")]
          # use nafill
            combo[qtr %in% 1:8, geo_tractce10  := nafill(geo_tractce10, type = 'locf'), by = "id_hudhears" ] # fill forward / downward
            combo[qtr %in% 1:8, geo_tractce10  := nafill(geo_tractce10, type = 'nocb'), by = "id_hudhears" ] # fill geoid backward / upward
            combo[qtr %in% 1:8, geo_countyfp10  := nafill(geo_countyfp10, type = 'locf'), by = "id_hudhears" ] # fill forward / downward
            combo[qtr %in% 1:8, geo_countyfp10  := nafill(geo_countyfp10, type = 'nocb'), by = "id_hudhears" ] # fill geoid backward / upward
          # convert geographies back to properly formatted character values
            combo[!is.na(geo_tractce10), temp_geo_tractce10 := sprintf("%06g", geo_tractce10)]
            combo[!is.na(geo_countyfp10), temp_geo_countyfp10 := sprintf("%03g", geo_countyfp10)]
            combo[, c("geo_tractce10", "geo_countyfp10") := NULL]
            setnames(combo, c("temp_geo_tractce10", "temp_geo_countyfp10"), c("geo_tractce10", "geo_countyfp10"))

# Merge on reference data ----
      # merge area median income by year onto combo table ----
          combo[, amiyear := year(qtr_date)]
          combo[, fips2010 := as.integer(geo_countyfp10)]
          combo[fips2010 == 33, fips2010 := 53033] 
          combo <- merge(combo, ami, by = c("amiyear", "hh_size", 'fips2010'), all.x = T, all.y = F)
          combo[, c("amiyear") := NULL]
          combo[, hhwage := sum(wage),  by = c("hh_id_kc_pha", "qtr")] # calculate quarterly wage for whole household
          combo[, hhhrs := sum(hrs),  by = c("hh_id_kc_pha", "qtr")] # calculate quarterly hours for whole household
          combo[, hhwage_hourly := rads::round2(hhwage / hhhrs, 2)]
          combo[, percent_ami := rads::round2(100*4*hhwage / ami, 1)] # multiply by four because wage is quarterly but AMI is annual
          
          # only keep percent_ami if always present across time
          message("Have a LOT of missing percent AMI because of missing address information 4 quarters after exit")
          ids.missing.percent_ami <- unique(combo[qtr %in% -4:4 & is.na(percent_ami)]$id_kc_pha)    
          combo[id_kc_pha %in% ids.missing.percent_ami, percent_ami := NA]
          combo[!is.na(percent_ami), .N, qtr]
          
      # merge on MIT Living wage data ----
          # identify the number of workers per household
          combo <- merge(combo, 
                         combo[qtr == 0, .(workers = .N), hh_id_kc_pha],
                         by = 'hh_id_kc_pha', 
                         all.x = T, 
                         all.y = F)      
          combo[, hh_gt_1_worker := 0]
          combo[workers > 1 & qtr %in% -4:4, hh_gt_1_worker := 1]
          
          # create categories for merging with MIT Living wage
          combo[, mitlw_category := fcase(adults == 1 & kids == 0, 'cat01', 
                                          adults == 1 & kids == 1, 'cat02', 
                                          adults == 1 & kids == 2, 'cat03', 
                                          adults == 1 & kids == 3, 'cat04', 
                                          adults == 2 & workers == 1 & kids == 0, 'cat05',
                                          adults == 2 & workers == 1 & kids == 1, 'cat06',
                                          adults == 2 & workers == 1 & kids == 2, 'cat07',
                                          adults == 2 & workers == 1 & kids == 3, 'cat08',
                                          adults == 2 & workers == 2 & kids == 0, 'cat09',
                                          adults == 2 & workers == 2 & kids == 1, 'cat10',
                                          adults == 2 & workers == 2 & kids == 2, 'cat11',
                                          adults == 2 & workers == 2 & kids == 3, 'cat12')]
          
          # merge on MIT living wage
          combo <- merge(combo, 
                         copy(living_wage)[, c('adults', 'children') := NULL], 
                         by = 'mitlw_category', 
                         all.x = T, 
                         all.y = F)
          
          # identify whether or not have a living wage (according to MIT metric) at quarters 0 (exit) & 4 (1 year post exit)
          # note that MIT living wage uses hourly wages that are PER WORKING adult, so use the average household wage rather than the sum of household wages
          combo[!is.na(mitlw_category) & qtr %in% c(0, 4), living_wage := 0]
          combo[!is.na(mitlw_category) & qtr %in% c(0, 4) & year(qtr_date) == 2016 & hhwage_hourly >= living_wage_2016, living_wage := 1]
          combo[!is.na(mitlw_category) & qtr %in% c(0, 4) & year(qtr_date) == 2017 & hhwage_hourly >= living_wage_2017, living_wage := 1]
          combo[!is.na(mitlw_category) & qtr %in% c(0, 4) & year(qtr_date) == 2018 & hhwage_hourly >= living_wage_2018, living_wage := 1]
          combo[, c('living_wage_2016', 'living_wage_2017', 'living_wage_2018') := NULL]
          
          setorder(combo[!is.na(living_wage), .N, .(living_wage, qtr)], qtr)[]
          
# Tidy exit definitions for analysis ----
    combo[, exit_category := factor(exit_category, levels = c("Positive", "Neutral", "Negative"))] # to force specific order in graph  
    combo[exit_category == "Negative", exit := -1]
    combo[exit_category == 'Neutral', exit := 0]
    combo[exit_category == "Positive", exit := 1]
          
# Select/order columns in final dataset ----
    setorder(combo, hh_id_kc_pha, id_kc_pha, qtr)
    combo <- combo[, .(hh_id_kc_pha, id_kc_pha, 
                      exit, exit_category, exit_date, exit_year = year(exit_date), exit_qtr, qtr, qtr_date,
                      wage, hrs, wage_hourly, ami, percent_ami, hhwage, hhhrs, hhwage_hourly, living_wage, 
                      race_eth_me, gender_me, race_gender, age_at_exit, hh_size, hh_disability, n_disability, 
                      single_caregiver, housing_time_at_exit, hh_gt_1_worker,
                      agency, prog_type, major_prog, subsidy_type, exit_reason_clean)]

    count10_final <- rbind(unique(combo[, .(id_kc_pha, agency)])[, .N, agency], data.table(agency = "Either", N = length(unique(combo$id_kc_pha)) ))

# Create PH/PBV/TBV flag ----    
    combo[prog_type %in% c("PBS8", "COLLABORATIVE HOUSING"), prog_type_use := "PBV"]
    combo[prog_type %in% c("PH", "SHA OWNED AND MANAGED"), prog_type_use := "PH"]
    combo[prog_type %in% c("PORT", "TBS8", "TENANT BASED"), prog_type_use := "TBV"]
    
# Create consort diagram ----
    # nicely format all consort counts ----
      counts <- sort(grep("^consort[0-9][0-9]", ls(), value = T))
      for(uglynum in counts){
        assign(uglynum, format(get(uglynum), big.mark = ',', trim = T))
      }
      
    # create consort diagram ----
      consort.complete <- 
        add_box(txt = paste0("Total exits: ", consort01_total_exits)) |>
        add_side_box(txt = c(paste0("Outside study period: ", consort01_side_outside_period)))  |>
        add_box(txt = paste0("Exits in study period: ", consort02_outside_period))   |>
        add_side_box(txt = paste0("False exits: ", consort02_side_false_exit))   |>
        add_box(txt = paste0("True exits: ", consort03_false_exit))   |>

        add_side_box(txt = paste0("Deceased or missing exit reasons: ", consort04_side_exit_reasons))   |>
        add_box(txt = paste0("Positive, negative or neutral exits reasons: ", consort05_exit_reasons))   |>
      
        add_side_box(txt = paste0("Not working age: ", consort05.5_side_age))   |>
        add_box(txt = paste0("Working age: ", consort05.5_age))   |>
      
        add_side_box(txt = paste0("Insufficient wage data: ", consort07_side_wages))   |>
        add_box(txt = paste0("Linked to wages: ", consort08_wages))   |>
        add_side_box(txt = paste0("< 1 year housing support: ", consort09_side_min_1yr))   |>
        add_box(txt = paste0(">= 1 year housing support: ", consort09_min_1yr)) |>
        add_side_box(txt = paste0("HH with wage earner over 61: ", consort10_side_agelimit)) |>
        add_box(txt = paste0("Final study population: ", consort10_agelimit)) 

      myplot <- plot(consort.complete)
      
    # save consort diagram ----
      tempy <- tempfile(fileext = ".pdf") 
      ggplot2::ggsave(filename = tempy,
                      plot = build_grid(consort.complete), 
                      dpi=600, 
                      width = 6.5, 
                      height = 7.2, 
                      units = "in") 
      
      drv$upload_file(src = tempy, 
                      dest = "HUD HEARS Study/wage_analysis/output/hpd_revision_1/appendix_fig_1_flow_diagram.pdf")  
      
      tempy <- tempfile(fileext = ".tiff") 
      ggplot2::ggsave(filename = tempy,
                      plot = build_grid(consort.complete), 
                      dpi=600, 
                      width = 6.5, 
                      height = 7.2, 
                      units = "in", 
                      compression = "lzw") 
      drv$upload_file(src = tempy, 
                      dest = "HUD HEARS Study/wage_analysis/output/hpd_revision_1/appendix_fig_1_flow_diagram.tiff") 
      
    # save consort data in CSV ----
      flowdata <- sort(ls(pattern = 'consort[0-9]'))
      flowdata.dt <- data.table()
      for(i in 1:length(flowdata)){
        temp <- data.table(name = flowdata[i], value = get(flowdata[i]))
        flowdata.dt <- rbind(flowdata.dt, temp)
      }
      
      tempy <- tempfile(fileext = ".xlsx") 
      openxlsx::write.xlsx(flowdata.dt, file = tempy, asTable = T, overwrite = T)    
      drv$upload_file(src = tempy, 
                      dest = "HUD HEARS Study/wage_analysis/output/hpd_revision_1/Table_consort.xlsx") 
      
# Write analytic table ----
    # open connection to prod----
      hhsaw16 = create_db_connection( # this is dev
        server = "hhsaw", 
        prod = T, 
        interactive = F
      )
    
    # write table to Azure 16 (production) SQL server ----
    table_config <- yaml::read_yaml(file.path(here::here(), "analyses/wages/ref/wage_analytic_table.yaml"))

    setcolorder(combo, names(table_config$vars))
      
    DBI::dbWriteTable(conn = hhsaw16,
                      name = DBI::Id(schema = table_config$schema, table = table_config$table),
                      value = setDF(unique(copy(combo))),
                      overwrite = T,
                      append = F, 
                      field.types = unlist(table_config$vars)) 

    
# The end! ----
      