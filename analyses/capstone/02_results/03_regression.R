# Set up ----
rm(list=ls())
pacman::p_load(data.table, DBI, keyring, glue)

# Uncomment the following line to create a key (run when you change KCIT pwd)
# When a window pops up, please enter your standard KCIT password
keyring::key_set(service = "hhsaw", username = "n-npetrakos@kingcounty.gov")

# Create a connection to hhs_analytics_workspace on Azure server 16 ----
cxn16 <- DBI::dbConnect(odbc::odbc(), 
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


######################## GEE in Step 1 ##########################
##################### Cluster in Step 3 #########################

library(multgee)
library(survival)

# Formula:
# exit_category ~ age_at_exit + gender_me + race_eth_me + agency + 
#                 single_caregiver + hh_size + hh_disability + 
#                 new_housing_time + major_prog + kc_opp_index_score

# First, fit the multinomial logistic regression model, using GEE
ps_mod <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + 
                      race_eth_me + agency + single_caregiver + hh_size +
                      hh_disability + new_housing_time + major_prog +
                      kc_opp_index_score,
                    data = tth_data,
                    id = hh_id_kc_pha,
                    LORstr = "independence")

# Next, calculate generalized propensity scores
ps <- as.data.frame(fitted(ps_mod))
colnames(ps) <- c("Neutral", "Negative", "Positive")

# Then, calculate inverse weights and assign to each obs.
tth_data$IPTW <- ifelse(tth_data$exit_category == "Negative", 1/ps$Negative,
                        ifelse(tth_data$exit_category == "Neutral", 1/ps$Neutral,
                               1/ps$Positive))

# Finally, fit weighted Cox PH
# Cluster on hh_id
tth_data$exit_category <- relevel(factor(tth_data$exit_category), ref = "Neutral")

tth_mod <- coxph(formula = Surv(tt_homeless, event) ~ exit_category,
                 data = tth_data,
                 weights = IPTW,
                 cluster = hh_id_kc_pha)

summary(tth_mod)



######################## GEE in Step 1 ##########################
##################### Cluster in Step 3 #########################
##################### Remove Opp Score ##########################

library(multgee)
library(survival)

# Formula:
# exit_category ~ age_at_exit + gender_me + race_eth_me + agency + 
#                 single_caregiver + hh_size + hh_disability + 
#                 new_housing_time + major_prog

# First, fit the multinomial logistic regression model, using GEE
ps_mod_sensitivity <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + 
                                  race_eth_me + agency + single_caregiver + hh_size +
                                  hh_disability + new_housing_time + major_prog,
                                data = tth_data,
                                id = hh_id_kc_pha,
                                LORstr = "independence")

# Next, calculate generalized propensity scores
ps_sensitivity <- as.data.frame(fitted(ps_mod_sensitivity))
colnames(ps_sensitivity) <- c("Neutral", "Negative", "Positive")

# Then, calculate inverse weights and assign to each obs.
tth_data$IPTW_sensitivity <- ifelse(tth_data$exit_category == "Negative", 1/ps_sensitivity$Negative,
                                    ifelse(tth_data$exit_category == "Neutral", 1/ps_sensitivity$Neutral,
                                           1/ps_sensitivity$Positive))

# Finally, fit weighted Cox PH
# Cluster on hh_id
tth_data$exit_category <- relevel(factor(tth_data$exit_category), ref = "Neutral")

tth_mod_sensitivity <- coxph(formula = Surv(tt_homeless, event) ~ exit_category, 
                             data = tth_data,
                             weights = IPTW_sensitivity,
                             cluster = hh_id_kc_pha)

summary(tth_mod_sensitivity)