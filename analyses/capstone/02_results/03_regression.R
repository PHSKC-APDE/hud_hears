## Script name: 03_regression.R
##
## Purpose of script: 
##    Step 1) Perform primary analysis, which involves: 
##              a) Deriving propensity scores by fitting a multinomial logistic regression model
##              b) Calculating weights using Inverse Probability Treatment Weighting (IPTW)
##              c) Fitting a weighted Cox Proportional Hazards (PH) model
##
##    Step 2) Perform sensitivity analysis using overlap weights, which involves:
##              a) Repeating Step 1 (primary analysis), but in step b), use overlap weights instead of IPTW
##
##    Step 3) Perform primary analysis for KCHA and SHA separately, which involves:
##              a) Repeating Step 1 parts a) and b), where agency is omitted from propensity score calcuation
##              b) Before fitting a weighted Cox PH model, split the data by agency
##              c) For SHA specific results, subset the data by agency = 'SHA'
##              d) For KCHA specific results, subset the data by agency = 'KCHA'
##
## Author: Hantong Hu, Taylor Keating, Zichen Liu, Niki Petrakos
## Date Created: 3/11/2022

# BRING IN PACKAGES ----
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, multgee, survival)

# Connect to HHSAW
db_hhsaw <- DBI::dbConnect(odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = "kcitazrhpasqlprp16.azds.kingcounty.gov",
                           database = "hhs_analytics_workspace",
                           uid = keyring::key_list("hhsaw")[["username"]],
                           pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]),
                           Encrypt = "yes",
                           TrustServerCertificate = "yes",
                           Authentication = "ActiveDirectoryPassword")

# Select capstone_data_3  ----
tth_data <- setDT(DBI::dbGetQuery(conn = cxn16, "SELECT * FROM [hudhears].[capstone_data_3]"))


#--------------------------------------------------
### Step 1) Perform primary analysis

#-----
## 1a) First, fit the multinomial logistic regression model, using GEE 

# Formula:
# exit_category ~ age_at_exit + gender_me + race_eth_me + agency + 
#                 single_caregiver + hh_size + hh_disability + 
#                 new_housing_time + major_prog + kc_opp_index_score

ps_mod <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + 
                      race_eth_me + agency + single_caregiver + hh_size +
                      hh_disability + new_housing_time + major_prog +
                      kc_opp_index_score,
                    data = tth_data,
                    id = hh_id_kc_pha,
                    LORstr = "independence")

## Next, calculate generalized propensity scores 
ps <- as.data.frame(fitted(ps_mod))
colnames(ps) <- c("Neutral", "Negative", "Positive")

#-----
## 1b) Then, calculate inverse weights and assign to each observation 

tth_data$IPTW <- ifelse(tth_data$exit_category == "Negative", 1/ps$Negative,
                        ifelse(tth_data$exit_category == "Neutral", 1/ps$Neutral,
                               1/ps$Positive))

#-----
## 1c) Finally, fit weighted Cox PH, and cluster on hh_id

tth_data$exit_category <- relevel(factor(tth_data$exit_category), ref = "Neutral")

tth_mod <- coxph(formula = Surv(tt_homeless, event) ~ exit_category,
                 data = tth_data,
                 weights = IPTW,
                 cluster = hh_id_kc_pha)

summary(tth_mod)


#--------------------------------------------------
### Step 2) Perform sensitivity analysis using overlap weights

#-----
## 2a) First, fit the multinomial logistic regression model, using GEE 

# Formula:
# exit_category ~ age_at_exit + gender_me + race_eth_me + agency + 
#                 single_caregiver + hh_size + hh_disability + 
#                 housing_time_at_exit + major_prog + kc_opp_index_score

ps_mod <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + 
                      race_eth_me + agency + single_caregiver + hh_size +
                      hh_disability + housing_time_at_exit + major_prog +
                      kc_opp_index_score,
                    data = tth_data,
                    id = hh_id_kc_pha,
                    LORstr = "independence")

## Next, calculate generalized propensity scores 
ps <- as.data.frame(fitted(ps_mod))
colnames(ps) <- c("Neutral", "Negative", "Positive")

#-----
## 2b) Then, calculate overlap weights and assign to each observation 

tth_data$Overlap <- ifelse(tth_data$exit_category == "Negative", 1-ps$Negative,
                           ifelse(tth_data$exit_category == "Neutral", 1-ps$Neutral,
                                  1-ps$Positive))

#-----
## 2c) Finally, fit weighted Cox PH, and cluster on hh_id

tth_data$exit_category <- relevel(factor(tth_data$exit_category), ref = "Neutral")

tth_mod_overlap <- coxph(formula = Surv(tt_homeless, event) ~ exit_category,
                         data = tth_data,
                         weights = Overlap,
                         cluster = hh_id_kc_pha)

summary(tth_mod_overlap)


#--------------------------------------------------
### Step 3) Perform primary analysis for KCHA and SHA separately

#-----
## 3a) First, fit the multinomial logistic regression model, using GEE 

# Formula:
# exit_category ~ age_at_exit + gender_me + race_eth_me +  
#                 single_caregiver + hh_size + hh_disability + 
#                 housing_time_at_exit + major_prog + kc_opp_index_score

ps_mod <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + 
                      race_eth_me + single_caregiver + hh_size +
                      hh_disability + housing_time_at_exit + major_prog +
                      kc_opp_index_score,
                    data = tth_data,
                    id = hh_id_kc_pha,
                    LORstr = "independence")

## Next, calculate generalized propensity scores
ps <- as.data.frame(fitted(ps_mod))
colnames(ps) <- c("Neutral", "Negative", "Positive")

#-----
## 3b) Then, calculate inverse weights and assign to each observation 
tth_data$IPTW <- ifelse(tth_data$exit_category == "Negative", 1/ps$Negative,
                        ifelse(tth_data$exit_category == "Neutral", 1/ps$Neutral,
                               1/ps$Positive))

tth_data$exit_category <- relevel(factor(tth_data$exit_category), ref = "Neutral")

#-----
## 3c) SHA specific results 

## Subset the data to only include SHA
tth_data_sha <- tth_data %>% subset(agency=="SHA")

## Finally, fit weighted Cox PH, and cluster on hh_id
tth_mod_sha <- coxph(formula = Surv(tt_homeless, event) ~ exit_category,
                     data = tth_data_sha,
                     weights = IPTW,
                     cluster = hh_id_kc_pha)

summary(tth_mod_sha)

#-----
## 3d) KCHA specific results 

## Subset the data to only include SHA
tth_data_kcha <- tth_data %>% subset(agency=="KCHA")

## Finally, fit weighted Cox PH, and cluster on hh_id
tth_mod_kcha <- coxph(formula = Surv(tt_homeless, event) ~ exit_category,
                      data = tth_data_kcha,
                      weights = IPTW,
                      cluster = hh_id_kc_pha)

summary(tth_mod_kcha)

