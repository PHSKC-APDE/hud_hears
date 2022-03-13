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
##              a) Repeating Step 1 parts a) and b), where agency is omitted from propensity score calculation
##              b) Before fitting a weighted Cox PH model, split the data by agency
##              c) For SHA specific results, subset the data by agency = 'SHA'
##              d) For KCHA specific results, subset the data by agency = 'KCHA'
##              e) Cox model results, adjusting for agency
##              f) Cox model results, interaction between agency and exit_category
##
##    Step 4) Visualize regression results
##
## Author: Niki Petrakos and Zichen Liu
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

# Select capstone_data_3, the output of 01_format.R  ----
tth_data <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[capstone_data_3]"))

#--------------------------------------------------
### Step 1) Perform primary analysis

#-----
## 1a) First, fit the multinomial logistic regression model, using GEE 

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

#-----
## 3e) PHA adjusted results
tth_mod_pha_adjusted<- coxph(formula = Surv(tt_homeless, event) ~ exit_category + agency,
                             data = tth_data,
                             weights = IPTW,
                             cluster = hh_id_kc_pha)
summary(tth_mod_pha_adjusted)

#-----
## 3f) PHA interaction with exit category results
tth_mod_interaction_pha_exitcat<- coxph(formula = Surv(tt_homeless, event) ~ exit_category * agency,
                                        data = tth_data,
                                        weights = IPTW,
                                        cluster = hh_id_kc_pha)
summary(tth_mod_interaction_pha_exitcat)

#--------------------------------------------------
### 4) Visualize regression results

#-----
## 4a) Primary analysis plot

# Create dataframe of results
primary <- data.frame(index=c("pos", "neg"),
                      hr=c(exp(coef(tth_mod))[[2]], exp(coef(tth_mod))[[1]]),
                      low=c(exp(confint(tth_mod))[[2]], exp(confint(tth_mod))[[1]]),
                      hi=c(exp(confint(tth_mod))[[4]], exp(confint(tth_mod))[[3]]))

# Create a boxplot with ggplot
ggplot(data=primary) +
  
  # Point estimates
  geom_point(aes(x=hr, y=index, color=index), size=3) +
  
  # Add labels under point estimates
  geom_text(label=paste(round(exp(coef(tth_mod))[[2]], 2)),
            x=exp(coef(tth_mod))[[2]], y=1.7, color="#619CFF") +
  geom_text(label=paste(round(exp(coef(tth_mod))[[1]], 2)),
            x=exp(coef(tth_mod))[[1]], y=0.7, color="#F8766D") +
  
  # Confidence intervals
  geom_errorbarh(height=0.2, aes(y=index, xmin=low, xmax=hi, color=index), size=1, alpha=0.5) +
  
  # Hazard ratio = 1 line
  geom_vline(xintercept=1, color="#00BA38", size=1, alpha=0.5) +
  
  # Other settings
  scale_color_manual(values=c("#F8766D", "#619CFF")) +
  scale_x_continuous(limits=c(-0.2, 2.2)) +
  scale_y_discrete(labels=c("Negative exit\n(v. neutral)", "Positive exit\n(v. neutral)")) +
  labs(x="Hazard ratio") +
  theme_bw() +
  theme(legend.position="none",
        legend.title=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line())

#-----
## 4b) Sensitivity analysis plot

# Create dataframe of results
ow <- data.frame(index=c("pos", "neg"),
                 hr=c(exp(coef(tth_mod_overlap))[[2]], exp(coef(tth_mod_overlap))[[1]]),
                 low=c(exp(confint(tth_mod_overlap))[[2]], exp(confint(tth_mod_overlap))[[1]]),
                 hi=c(exp(confint(tth_mod_overlap))[[4]], exp(confint(tth_mod_overlap))[[3]]))

# Create a boxplot with ggplot
ggplot(data=ow) +
  
  # Point estimates
  geom_point(aes(x=hr, y=index, color=index), size=3) +
  
  # Add labels under point estimates
  geom_text(label=paste(round(exp(coef(tth_mod_overlap))[[2]], 2)),
            x=exp(coef(tth_mod_overlap))[[2]], y=1.7, color="#619CFF") +
  geom_text(label=paste(round(exp(coef(tth_mod_overlap))[[1]], 2)),
            x=exp(coef(tth_mod_overlap))[[1]], y=0.7, color="#F8766D") +
  
  # Confidence intervals
  geom_errorbarh(height=0.2, aes(y=index, xmin=low, xmax=hi, color=index), size=1, alpha=0.5) +
  
  # Hazard ratio = 1 line
  geom_vline(xintercept=1, color="#00BA38", size=1, alpha=0.5) +
  
  # Other settings
  scale_color_manual(values=c("#F8766D", "#619CFF")) +
  scale_x_continuous(limits=c(-0.2, 2.2)) +
  scale_y_discrete(labels=c("Negative exit\n(v. neutral)", "Positive exit\n(v. neutral)")) +
  labs(x="Hazard ratio") +
  theme_bw() +
  theme(legend.position="none",
        legend.title=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line())

#-----
## 4c) PHA stratified analysis plots

# i) SHA specific plot

# Create dataframe of results
sha <- data.frame(index=c("pos", "neg"),
                  hr=c(exp(coef(tth_mod_sha))[[2]], exp(coef(tth_mod_sha))[[1]]),
                  low=c(exp(confint(tth_mod_sha))[[2]], exp(confint(tth_mod_sha))[[1]]),
                  hi=c(exp(confint(tth_mod_sha))[[4]], exp(confint(tth_mod_sha))[[3]]))

# Create a boxplot with ggplot
ggplot(data=sha) +
  
  # Point estimates
  geom_point(aes(x=hr, y=index, color=index), size=3) +
  
  # Add labels under point estimates
  geom_text(label=paste(round(exp(coef(tth_mod_sha))[[2]], 2)),
            x=exp(coef(tth_mod_sha))[[2]], y=1.7, color="#619CFF") +
  geom_text(label=paste(round(exp(coef(tth_mod_sha))[[1]], 2)),
            x=exp(coef(tth_mod_sha))[[1]], y=0.7, color="#F8766D") +
  
  # Confidence intervals
  geom_errorbarh(height=0.2, aes(y=index, xmin=low, xmax=hi, color=index), size=1, alpha=0.5) +
  
  # Hazard ratio = 1 line
  geom_vline(xintercept=1, color="#00BA38", size=1, alpha=0.5) +
  
  # Other settings
  scale_color_manual(values=c("#F8766D", "#619CFF")) +
  scale_x_continuous(limits=c(-2.5, 4.5)) +
  scale_y_discrete(labels=c("Negative exit\n(v. neutral)", "Positive exit\n(v. neutral)")) +
  labs(x="Hazard ratio", title="SHA Only") +
  theme_bw() +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line())

# ii) KCHA specific plot

# Create dataframe of results
kcha <- data.frame(index=c("pos", "neg"),
                   hr=c(exp(coef(tth_mod_kcha))[[2]], exp(coef(tth_mod_kcha))[[1]]),
                   low=c(exp(confint(tth_mod_kcha))[[2]], exp(confint(tth_mod_kcha))[[1]]),
                   hi=c(exp(confint(tth_mod_kcha))[[4]], exp(confint(tth_mod_kcha))[[3]]))

# Create a boxplot with ggplot
ggplot(data=kcha) +
  
  # Point estimates
  geom_point(aes(x=hr, y=index, color=index), size=3) +
  
  # Add labels under point estimates
  geom_text(label=paste(round(exp(coef(tth_mod_kcha))[[2]], 2)),
            x=exp(coef(tth_mod_kcha))[[2]], y=1.7, color="#619CFF") +
  geom_text(label=paste(round(exp(coef(tth_mod_kcha))[[1]], 2)),
            x=exp(coef(tth_mod_kcha))[[1]], y=0.7, color="#F8766D") +
  
  # Confidence intervals
  geom_errorbarh(height=0.2, aes(y=index, xmin=low, xmax=hi, color=index), size=1, alpha=0.5) +
  
  # Hazard ratio = 1 line
  geom_vline(xintercept=1, color="#00BA38", size=1, alpha=0.5) +
  
  # Other settings
  scale_color_manual(values=c("#F8766D", "#619CFF")) +
  scale_x_continuous(limits=c(-2.5, 4.5)) +
  scale_y_discrete(labels=c("Negative exit\n(v. neutral)", "Positive exit\n(v. neutral)")) +
  labs(x="Hazard ratio", title="KCHA Only") +
  theme_bw() +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line())

