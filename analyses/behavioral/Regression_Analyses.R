## HUD HEARS Behavioral Health Exit Type Regression Code ----
#Code updated on 05/02/2022
#Author: Megan Suter 

#Code Purpose: Run regression models for exit type and BH analysis
#NOTE: behavioral health conditions and Medicaid ED visits are based on Medicaid
# data. Those included in this sample have 7 months or greater of full Medicaid coverage 
# before and after date of exit

#Run after file "Outcomes_code_cleaned.R"


#Preliminary Models (With covariate adjustment)----
#Set  unknown race==missing
all_pop <- all_pop %>% mutate(race_eth_me = ifelse(race_eth_me=="Unknown", NA, race_eth_me))
#Change exit category to a factor variable
all_pop$exit_category <- as.factor(all_pop$exit_category)

###Run models that include all, regardless of Medicaid status----

####Unadjusted model----
model1 <-glm(crisis_any ~ exit_category, family="binomial", data=all_pop)
exp(cbind(OR = coef(model1), confint(model1)))

####Covariate adjustment----
model2 <- glm(crisis_any ~ exit_category + gender_me + age_at_exit + race_eth_me  + hh_size + single_caregiver + housing_time_at_exit + reg_care,  family="binomial", data=all_pop)
exp(cbind(OR = coef(model2), confint(model2)))

##Add in adjustment for prior BH crisis (not including Medicaid)
model3 <- glm(crisis_any ~ exit_category + gender_me + age_at_exit + race_eth_me  + hh_size + single_caregiver + housing_time_at_exit + reg_care + crisis_any_before,  family="binomial", data=all_pop)
exp(cbind(OR = coef(model3), confint(model3)))

##MCAID coverage preliminary models that include subset with medicaid coverage 7 months before and after----
mcaid_subset7mo <- all_pop %>% filter(full_cov_7_prior==T & full_cov_7_after==T)

####Unadjusted model----
model11 <-glm(crisis_any_mcaid ~ exit_category, family="binomial", data=mcaid_subset7mo)
exp(cbind(OR = coef(model11), confint(model11)))

####Covariate adjustment----
model22 <- glm(crisis_any_mcaid ~ exit_category + gender_me + age_at_exit + race_eth_me  + hh_size + single_caregiver + housing_time_at_exit + reg_care + crisis_any_mcaid_before,  family="binomial", data=mcaid_subset7mo)
exp(cbind(OR = coef(model22), confint(model22)))

###Covariate adjustment + BH conditions
model33 <- glm(crisis_any_mcaid ~ exit_category + gender_me + age_at_exit + race_eth_me  + hh_size + single_caregiver + housing_time_at_exit + reg_care + crisis_any_mcaid_before +any_cond,  family="binomial", data=mcaid_subset7mo)
exp(cbind(OR = coef(model33), confint(model33)))

##Add in adjustment for prior BH crisis (not including Medicaid)
model44 <- glm(crisis_any ~ exit_category + gender_me + age_at_exit + race_eth_me  + hh_size + single_caregiver + housing_time_at_exit + reg_care + crisis_any_mcaid_before + any_cond,  family="binomial", 
               data=mcaid_subset7mo[(mcaid_subset7mo$gender_me != "Multiple" & mcaid_subset7mo$race_eth_me !="NH/PI"),])
exp(cbind(OR = coef(model44), confint(model44)))


# ##association between any condition and exit type
# model3 <-polr(exit_category ~ any_condition, data=all_pop, Hess=T)
# exp(cbind(OR = coef(model3), confint(model3)))
# 
# ##Checking association between routine care and exit type
# model4 <-polr(exit_category ~ reg_care, data=all_pop, Hess=T)
# exp(cbind(OR = coef(model4), confint(model4)))

#Check association between prior event and bh condition

# model5 <- glm(crisis_any_mcaid_before ~ any_cond,
#               family="binomial", 
#               data=mcaid_subset7mo[(mcaid_subset7mo$gender_me != "Multiple" & mcaid_subset7mo$race_eth_me !="NH/PI"),])
# exp(cbind(OR = coef(model5), confint(model5)))

#Model with propensity score matching----
library(multgee)
library(geepack)
##Models not including Medicaid ED visits ----

### 1a) First, fit the multinomial logistic regression model, using GEE ----

##### Remove anyone with missing variables-----
all_pop2<- all_pop %>%
  filter(!(is.na(exit_category) | is.na(gender_me) | is.na(age_at_exit) |
             is.na(race_eth_me) | race_eth_me == "Unknown" | is.na(hh_size) | is.na(single_caregiver) |
             is.na(housing_time_at_exit) | is.na(reg_care) | is.na(crisis_any_before)
             ))

##### Fit the multinomial logistic regression model, using GEE----
ps_mod <- nomLORgee(formula = exit_category ~ gender_me + age_at_exit + race_eth_me + hh_size
                    + single_caregiver + housing_time_at_exit + reg_care + crisis_any_before,
                    data = all_pop2,
                    id = hh_id_kc_pha,
                    LORstr = "independence")

##### Next, calculate generalized propensity scores----
ps <- as.data.frame(fitted(ps_mod))
colnames(ps) <- c("Negative", "Neutral", "Positive")
ps <- cbind("id_hudhears" = all_pop2$id_hudhears, ps)


### 1b) Then, calculate inverse weights and assign to each observation  ----
all_pop2 <- all_pop2 %>%
  left_join(., ps, by = "id_hudhears") %>%
  mutate(iptw = case_when(exit_category == "Neutral" ~ 1/Neutral,
                          exit_category == "Negative" ~ 1/Negative,
                          exit_category == "Positive" ~ 1/Positive))

### 1c) Finally, fit model----
# This model does not account for clustering by household

#Change referent category
all_pop2$exit_category <- relevel(all_pop2$exit_category*reg_care, ref="Neutral")
no_mcaid_neutral <- glm(crisis_any ~ exit_category, 
                        family=quasibinomial, data=all_pop2, weights=iptw)
exp(cbind(OR = coef(no_mcaid_neutral), confint(no_mcaid_neutral)))


#This model includes everyone, but no Medicaid ED visit
no_mcaid <- glm(crisis_any ~ exit_category + exit_category*reg_care, 
                 family=quasibinomial, data=all_pop2, weights=iptw)
exp(cbind(OR = coef(no_mcaid), confint(no_mcaid)))


#This model includes Medicaid pop only, but NO Medicaid ED visits
no_mcaid_fullcov <- glm(crisis_any ~ exit_category, 
                family=quasibinomial, 
                data=all_pop2[(all_pop2$full_cov_7_prior== T & all_pop2$full_cov_7_after ==T),], 
                weights=iptw)
exp(cbind(OR = coef(no_mcaid_fullcov), confint(no_mcaid_fullcov)))

#Change referent category
all_pop2$exit_category <- relevel(all_pop2$exit_category, ref="Neutral")
no_mcaid_neutral <- glm(crisis_any ~ exit_category, 
                family=quasibinomial, data=all_pop2, weights=iptw)
exp(cbind(OR = coef(no_mcaid_neutral), confint(no_mcaid_neutral)))


#Note this model is not working !
# no_mcaid <-geepack::geeglm(formula = crisis_any ~ exit_category,
#                            weights = iptw,
#                            data = all_pop2,
#                            id=hh_id_kc_pha,
#                            family = "binomial")

              
#Repeat propensity score models with Medicaid Coverage group ----

##This version DOES NOT adjust for BH conditions

### 1a) First, fit the multinomial logistic regression model, using GEE ----

##### Remove anyone with missing variables-----

mcaid_subset7mo2<- mcaid_subset7mo %>%
  filter(!(is.na(exit_category) | is.na(gender_me) | is.na(age_at_exit) |
             is.na(race_eth_me) | race_eth_me == "Unknown" | is.na(hh_size) | is.na(single_caregiver) |
             is.na(housing_time_at_exit) | is.na(reg_care) | is.na (any_cond)) | is.na (crisis_any_mcaid_before))

##### Fit the multinomial logistic regression model, using GEE----
ps_mod2 <- nomLORgee(formula = exit_category ~ gender_me + age_at_exit + race_eth_me + hh_size
                    + single_caregiver + housing_time_at_exit + reg_care + any_cond + crisis_any_mcaid_before,
                    data = mcaid_subset7mo2,
                    id = hh_id_kc_pha,
                    LORstr = "independence")

##### Next, calculate generalized propensity scores----
ps2 <- as.data.frame(fitted(ps_mod2))
colnames(ps2) <- c("Negative", "Neutral", "Positive")
ps2 <- cbind("id_hudhears" = mcaid_subset7mo2$id_hudhears, ps2)


### 1b) Then, calculate inverse weights and assign to each observation  ----
mcaid_subset7mo2 <- mcaid_subset7mo2 %>%
  left_join(., ps2, by = "id_hudhears") %>%
  mutate(iptw2 = case_when(exit_category == "Neutral" ~ 1/Neutral,
                          exit_category == "Negative" ~ 1/Negative,
                          exit_category == "Positive" ~ 1/Positive))

### 1c) Finally, fit model----
# This model does not account for clustering by household
#Change referent category
mcaid_subset7mo2$exit_category <- relevel(mcaid_subset7mo2$exit_category, ref="Neutral")
no_mcaid_neutral <- glm(crisis_any ~ exit_category, 
                        family=quasibinomial, data=a, weights=iptw)
exp(cbind(OR = coef(no_mcaid_neutral), confint(no_mcaid_neutral)))
mcaid <- glm(crisis_any ~ exit_category, 
                family=quasibinomial, mcaid_subset7mo2, weights=iptw2)
exp(cbind(OR = coef(mcaid), confint(mcaid)))

#Note this model is not working !
#mcaid2 <-geepack::geeglm(formula = crisis_any ~ exit_category,
                           # weights = iptw2,
                           # data = mcaid_subset7mo2,
                           # id=hh_id_kc_pha,
                           # family = "binomial")


#Now calculate Medicaid subset, not adjustinhg for BH conditions, but including ED outcomes
mcaid_subset7mo2<- mcaid_subset7mo %>%
  filter(!(is.na(exit_category) | is.na(gender_me) | is.na(age_at_exit) |
             is.na(race_eth_me) | race_eth_me == "Unknown" | is.na(hh_size) | is.na(single_caregiver) |
             is.na(housing_time_at_exit) | is.na(reg_care) | is.na (crisis_any_mcaid_before)))

##### Fit the multinomial logistic regression model, using GEE----
ps_mod2 <- nomLORgee(formula = exit_category ~ gender_me + age_at_exit + race_eth_me + hh_size
                     + single_caregiver + housing_time_at_exit + reg_care + crisis_any_mcaid_before,
                     data = mcaid_subset7mo2,
                     id = hh_id_kc_pha,
                     LORstr = "independence")

##### Next, calculate generalized propensity scores----
ps2 <- as.data.frame(fitted(ps_mod2))
colnames(ps2) <- c("Negative", "Neutral", "Positive")
ps2 <- cbind("id_hudhears" = mcaid_subset7mo2$id_hudhears, ps2)


### 1b) Then, calculate inverse weights and assign to each observation  ----
mcaid_subset7mo2 <- mcaid_subset7mo2 %>%
  left_join(., ps2, by = "id_hudhears") %>%
  mutate(iptw2 = case_when(exit_category == "Neutral" ~ 1/Neutral,
                           exit_category == "Negative" ~ 1/Negative,
                           exit_category == "Positive" ~ 1/Positive))

