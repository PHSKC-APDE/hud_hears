## Script name: create_opportunity_index.R
##
## Purpose of script: Create opportunity index score 
##      First, by standardizing each individual component in the 4-county area (by census tract)- to confirm the same methodology as PSRC 2018 
##      Second, by standardizing each individual component in King County (by census tract)
##
## Author: Taylor Keating
## Date Created: 1/21/2022
## Email:n-tkeating@kingcounty.gov
##
## Notes: Data comes from PSRC 2018 and uses Kirwan methodology for opportunity areas
##
##        Final scores by standardizing in 4-county area is within opp_4county object
##        Final scores by standardizing in King County is within opp_kc object
##   
##

# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, ggplot2, viridis, hrbrthemes,
               knitr, kableExtra, rmarkdown, readxl, readr)


# function that takes in a column and returns a vector with a standardized value for each element of the column
standardize<- function(column){
  value<- rep(NA, length(column))
  for(i in 1:length(column)){
    value[i]<- (column[i] - mean(column, na.rm=TRUE)) / sd(column, na.rm=TRUE)
  }
  return(value)
}

#-----------------------
##
# We'll obtain opportunity index scores by first standardizing within the full 4-county area (to confirm the same methodology as PSRC) 
#                                       and second standardizing in just King County
#   Standardize within each element of the 5 composite categories (EDU, ECON, HOUSE, TRANS, HEALTH)
#   Average each element to obtain a z-score for each of the 5 composite categories
#   Average the 5 composite z-scores to obtain an Opportunity index z-score
##


# Read in data from PSRC

edu_raw<- read_excel("PSRC_opp_mapping_all_indicators_raw.xlsx",
                     sheet="Index-EDU", col_types = "numeric", na="null") %>% select(-contains("Class"))
edu_raw<- read_excel("PSRC_opp_mapping_all_indicators_raw.xlsx",
                      sheet="Index-EDU", col_types = "numeric", na="null") %>% select(-contains("Class"))
econ_raw<-  read_excel("PSRC_opp_mapping_all_indicators_raw.xlsx",
                       sheet="Index-ECON", col_types= "numeric", na="null") %>% select(-contains("Class"))
house_raw<-  read_excel("PSRC_opp_mapping_all_indicators_raw.xlsx",
                        sheet="Index-HOUSE", col_types= "numeric", na="null") %>% select(-contains("Class"))
trans_raw<-  read_excel("PSRC_opp_mapping_all_indicators_raw.xlsx",
                        sheet="Index-TRANS", col_types = "numeric", na="null") %>% select(-contains("Class"))
health_raw<-  read_excel("PSRC_opp_mapping_all_indicators_raw.xlsx",
                         sheet="Index-HEALTH", col_types= "numeric", na="null") %>% select(-contains("Class"))
opp_raw<-  read_excel("PSRC_opp_mapping_all_indicators_raw.xlsx",
                      sheet="Index-ALL", col_types= "numeric", na="null") %>% select(-contains("Class"))



#####- Education Component
##    standardize 5 elements and then take average

##    Entire 4-county area
edu_4county<- edu_raw %>%
  mutate(ela_z= standardize(`01-ELA`),
         math_z= standardize(`02-Math`),
         pov_z= (standardize(`03-Pov`))*(-1), # note poverty standardization is multiplied by -1
         qual_z= standardize(`04-Qual`),
         grad_z= standardize(`05-Grad`))
edu_4county<- edu_4county %>%
  mutate(EDU_Z= apply(X=(edu_4county %>% select(ela_z, math_z, pov_z, qual_z, grad_z)), MARGIN=1, FUN=mean))

##   filter for King County and then standardize
edu_kc<- edu_raw %>% select(-contains("Z")) %>%
  filter(substr(as.character(GEOID10), 1, 5)=="53033") %>%
  mutate(ela_z= standardize(`01-ELA`),
         math_z= standardize(`02-Math`),
         pov_z= (standardize(`03-Pov`))*(-1), # note poverty standardization is multiplied by -1
         qual_z= standardize(`04-Qual`),
         grad_z= standardize(`05-Grad`))
edu_kc<- edu_kc %>%
  mutate(EDU_Z= apply(X=(edu_kc %>% select(ela_z, math_z, pov_z, qual_z, grad_z)), MARGIN=1, FUN=mean))
#####



#####- Economic Health Component
##    standardize elements (Jobs Auto, Jobs Transit, Growth, Unemployment)
##    create Jobs score from avg of Jobs Auto and Jobs Transit scores
##    average the 3 standardized elements (Jobs, Growth, Unemployment)

##    Entire 4-county area
econ_4county<- econ_raw %>% 
  mutate(jobs_auto_z= standardize(`06a-Jobs-Auto`),
         jobs_transit_z= standardize(`06b-Jobs-Transit`),
         growth_z= standardize(`07-Job-Growth`),
         unemploy_z= (standardize(`08-Unemploy`))*(-1)) # note unemployment standardization is multiplied by -1
econ_4county<- econ_4county %>%
  mutate(job_z= apply(X=(econ_4county %>% select(jobs_auto_z, jobs_transit_z)), MARGIN=1, FUN=mean))
econ_4county<- econ_4county %>%
  mutate(ECON_Z= apply(X=(econ_4county %>% select(job_z,growth_z,unemploy_z)), MARGIN=1, FUN=mean))

##   filter for King County and then standardize
econ_kc<- econ_raw %>% select(-contains("Z")) %>%
  filter(substr(as.character(GEOID10), 1, 5)=="53033") %>%
  mutate(jobs_auto_z= standardize(`06a-Jobs-Auto`),
         jobs_transit_z= standardize(`06b-Jobs-Transit`),
         growth_z= standardize(`07-Job-Growth`),
         unemploy_z= (standardize(`08-Unemploy`))*(-1)) # note unemployment standardization is multiplied by -1
econ_kc<- econ_kc %>%
  mutate(job_z= apply(X=(econ_kc %>% select(jobs_auto_z, jobs_transit_z)), MARGIN=1, FUN=mean))
econ_kc<- econ_kc %>%
  mutate(ECON_Z= apply(X=(econ_kc %>% select(job_z,growth_z,unemploy_z)), MARGIN=1, FUN=mean))
#####



#####- Housing and Neighborhood Quality Component
##    standardize 3 elements and then take average

##    Entire 4-county area
house_4county<- house_raw %>% 
  mutate(vacancy_z= standardize(`09-Vacancy`),
         poor_stock_z= (standardize(`10-Poor-Stock`))*(-1), # note poor stock standardization is multiplied by -1
         crime_z= standardize(`11-Crime`))
house_4county<- house_4county %>%
  mutate(HOUSE_Z= apply(X=(house_4county %>% select(vacancy_z,poor_stock_z,crime_z)), MARGIN=1, FUN=mean))

##   filter for King County and then standardize
house_kc<- house_raw %>% select(-contains("Z")) %>%
  filter(substr(as.character(GEOID10), 1, 5)=="53033") %>%
  mutate(vacancy_z= standardize(`09-Vacancy`),
         poor_stock_z= (standardize(`10-Poor-Stock`))*(-1), # note poor stock standardization is multiplied by -1
         crime_z= standardize(`11-Crime`))
house_kc<- house_kc %>%
  mutate(HOUSE_Z= apply(X=(house_kc %>% select(vacancy_z,poor_stock_z,crime_z)), MARGIN=1, FUN=mean))
#####



#####- Mobility and Transportation Component
##    standardize 4 elements and then take average

##    Entire 4-county area
trans_4county<- trans_raw %>% 
  mutate(drive_costs_z= (standardize(`12-Drive-Costs`))*(-1), # note drive costs standardization is multiplied by -1
         HCT_z= standardize(`13-HCT`),
         transit_fares_z= (standardize(`14-Transit-Fares`))*(-1), # note transit fares standardization is multiplied by -1
         walkers_z= standardize(`15-Walkers`))
trans_4county<- trans_4county %>%
  mutate(TRANS_Z= apply(X=(trans_4county %>% select(drive_costs_z,HCT_z,transit_fares_z,walkers_z)), MARGIN=1, FUN=mean))

##   filter for King County and then standardize
trans_kc<- trans_raw %>% select(-contains("Z")) %>% 
  filter(substr(as.character(GEOID10), 1, 5)=="53033") %>%
  mutate(drive_costs_z= (standardize(`12-Drive-Costs`))*(-1), # note drive costs standardization is multiplied by -1
         HCT_z= standardize(`13-HCT`),
         transit_fares_z= (standardize(`14-Transit-Fares`))*(-1), # note transit fares standardization is multiplied by -1
         walkers_z= standardize(`15-Walkers`))
trans_kc<- trans_kc %>%
  mutate(TRANS_Z= apply(X=(trans_kc %>% select(drive_costs_z,HCT_z,transit_fares_z,walkers_z)), MARGIN=1, FUN=mean))
#####



#####- Health and Environment Component
##    standardize 3 elements and then take average

##    Entire 4-county area
health_4county<- health_raw %>% 
  mutate(park_z= (standardize(`16-Park`))*(-1), # park standardization is multiplied by -1
         waste_z= (standardize(`17-Waste`))*(-1), # waste standardization is multiplied by -1
         desert_z= (standardize(`18-Desert`))*(-1)) # desert standardization is multiplied by -1
health_4county<- health_4county %>%
  mutate(HEALTH_Z= apply(X=(health_4county %>% select(park_z,waste_z,desert_z)), MARGIN=1, FUN=mean))

##   filter for King County and then standardize
health_kc<- health_raw %>% select(-contains("Z")) %>% 
  filter(substr(as.character(GEOID10), 1, 5)=="53033") %>%
  mutate(park_z= (standardize(`16-Park`))*(-1), # park standardization is multiplied by -1
         waste_z= (standardize(`17-Waste`))*(-1), # waste standardization is multiplied by -1
         desert_z= (standardize(`18-Desert`))*(-1)) # desert standardization is multiplied by -1
health_kc<- health_kc %>%
  mutate(HEALTH_Z= apply(X=(health_kc %>% select(park_z,waste_z,desert_z)), MARGIN=1, FUN=mean))
#####



#####- Opportunity Score
##  Join each of 5 composite scores in dataset by GEOID10
##  then take average of the 5 to obtain an opportunity score

##   Entire 4-county area
opp_4county<- opp_raw %>% 
  left_join( (edu_4county %>% select(GEOID10,EDU_Z)), by="GEOID10" ) %>%
  left_join( (econ_4county %>% select(GEOID10,ECON_Z)), by="GEOID10" ) %>%
  left_join( (house_4county %>% select(GEOID10,HOUSE_Z)), by="GEOID10" ) %>%
  left_join( (trans_4county %>% select(GEOID10,TRANS_Z)), by="GEOID10" ) %>%
  left_join( (health_4county %>% select(GEOID10,HEALTH_Z)), by="GEOID10" ) 
opp_4county<- opp_4county %>%
  mutate(OPP_Z= apply(X=(opp_4county %>% select(EDU_Z,ECON_Z,HOUSE_Z,TRANS_Z,HEALTH_Z)), MARGIN=1, FUN=mean)) %>%
  arrange(GEOID10)
## confirm methodology is same as PSRC
with(opp_4county, all.equal(`EDU-Z`, EDU_Z, tolerance=0.001))
with(opp_4county, all.equal(`ECON-Z`, ECON_Z, tolerance=0.001))
with(opp_4county, all.equal(`TRANS-Z`, TRANS_Z, tolerance=0.001))
with(opp_4county, all.equal(`HOUSE-Z`, HOUSE_Z, tolerance=0.001))
with(opp_4county, all.equal(`HEALTH-Z`, HEALTH_Z, tolerance=0.001))
with(opp_4county, all.equal(`OPP-Z`, OPP_Z, tolerance=0.001))

##  King County standardization
opp_kc<- (edu_kc %>% select(GEOID10,EDU_Z)) %>%
  left_join( (econ_kc %>% select(GEOID10,ECON_Z)), by="GEOID10" ) %>%
  left_join( (house_kc %>% select(GEOID10,HOUSE_Z)), by="GEOID10" ) %>%
  left_join( (trans_kc %>% select(GEOID10,TRANS_Z)), by="GEOID10" ) %>%
  left_join( (health_kc %>% select(GEOID10,HEALTH_Z)), by="GEOID10" ) 
opp_kc<- opp_kc %>%
  mutate(OPP_Z= apply(X=(opp_kc %>% select(EDU_Z,ECON_Z,HOUSE_Z,TRANS_Z,HEALTH_Z)), MARGIN=1, FUN=mean)) %>%
  arrange(GEOID10)
#####

# write KC standardized opportunity index scores to a csv file
write_csv(opp_kc, file="kc_opp_indices_scaled.csv")
