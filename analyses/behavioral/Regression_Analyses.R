##Regression Analysis 
####################################################
#Preliminary Models (With covariate adjustment)
####################################################
#Set  unknown race==missing

#Change exit category to a factor variable
all_pop$exit_category <- as.factor(all_pop$exit_category)
##Run preliminary model (unadjusted)
model1 <-glm(crisis_any ~ exit_category, family="binomial", data=all_pop)
exp(cbind(OR = coef(model1), confint(model1)))

model2 <- glm(crisis_any ~ exit_category*reg_care + reg_care + gender_me + age_at_exit + race_eth_me  + hh_size + any_condition, family="binomial", data=all_pop)
exp(cbind(OR = coef(model2), confint(model2)))

##association between any condition and exit type
model3 <-polr(exit_category ~ any_condition, data=all_pop, Hess=T)
exp(cbind(OR = coef(model3), confint(model3)))

##association between routine care and exit type
model4 <-polr(exit_category ~ reg_care, data=all_pop, Hess=T)
exp(cbind(OR = coef(model4), confint(model4)))

####################################################
#Model with propensity score matching
####################################################
library(multgee)

## 1a) First, fit the multinomial logistic regression model, using GEE ----


# Remove anyone with missing variables
all_pop2<- all_pop %>%
  filter(!(is.na(exit_category) | is.na(age_at_exit) | is.na(gender_me) | 
             is.na(race_eth_me) | race_eth_me == "Unknown" | is.na(any_condition) | is.na(reg_care) |
             is.na(hh_size)))

# First, fit the multinomial logistic regression model, using GEE
ps_mod <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + race_eth_me  hh_size,
                    data = all_pop2,
                    id = hh_id_kc_pha,
                    LORstr = "independence")

# Next, calculate generalized propensity scores
ps <- as.data.frame(fitted(ps_mod))
colnames(ps) <- c("Neutral", "Negative", "Positive")
ps <- cbind("id_hudhears" = all_pop2$id_hudhears, ps)


## 1b) Then, calculate inverse weights and assign to each observation  ----
all_pop2 <- all_pop2 %>%
  left_join(., ps, by = "id_hudhears") %>%
  mutate(iptw = case_when(exit_category == "Neutral" ~ 1/Neutral,
                          exit_category == "Negative" ~ 1/Negative,
                          exit_category == "Positive" ~ 1/Positive))

## 1c) Finally, fit logistic regression model, and cluster on hh_id ----
model5 <- glm(crisis_any ~ exit_category,
              family="binomial", 
              weights=iptw,
              data=all_pop2)


