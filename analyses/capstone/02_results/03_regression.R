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
pacman::p_load(tidyverse, multgee, survival, data.table)

# set working directory (for output of plots- to utilize more easily)
capstone_path <- file.path(here::here(), "analyses/capstone/02_results")

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
# Step 1) Perform primary analysis ----


## 1a) First, fit the multinomial logistic regression model, using GEE ----
# Remove anyone with missing variables
tth_data <- tth_data %>% filter(full_demog == T)

# First, fit the multinomial logistic regression model, using GEE
ps_mod <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + 
                      race_eth_me + agency + single_caregiver + hh_size +
                      hh_disability + housing_time_at_exit + prog_type_use +
                      recent_homeless,
                    data = tth_data,
                    id = hh_id_kc_pha,
                    LORstr = "independence")

# Next, calculate generalized propensity scores
ps <- as.data.frame(fitted(ps_mod))
colnames(ps) <- c("Neutral", "Negative", "Positive")
ps <- cbind("id_hudhears" = tth_data$id_hudhears, ps)


## 1b) Then, calculate inverse weights and assign to each observation  ----
tth_data <- tth_data %>%
  left_join(., ps, by = "id_hudhears") %>%
  mutate(iptw = case_when(exit_category == "Neutral" ~ 1/Neutral,
                          exit_category == "Negative" ~ 1/Negative,
                          exit_category == "Positive" ~ 1/Positive))


## 1c) Finally, fit weighted Cox PH, and cluster on hh_id ----

tth_data$exit_category <- relevel(factor(tth_data$exit_category), ref = "Neutral")

tth_mod <- coxph(formula = Surv(tt_homeless, event) ~ exit_category,
                 data = tth_data,
                 weights = iptw,
                 cluster = hh_id_kc_pha)

summary(tth_mod)


#--------------------------------------------------
# Step 2) Perform sensitivity analysis using overlap weights ----


## 2a) First, calculate overlap weights and assign to each observation ----
tth_data <- tth_data %>% 
  mutate(overlap = case_when(exit_category == "Neutral" ~ 1 - Neutral,
                             exit_category == "Negative" ~ 1 - Negative,
                             exit_category == "Positive" ~ 1 - Positive))


## 2b) Then, fit weighted Cox PH, and cluster on hh_id ----
tth_mod_overlap <- coxph(formula = Surv(tt_homeless, event) ~ exit_category,
                         data = tth_data,
                         weights = overlap,
                         cluster = hh_id_kc_pha)

summary(tth_mod_overlap)

# Create de-identified version for export
tth_data_export <- copy(tth_data)
tth_data_export[, hh_id := .GRP, by = "hh_id_kc_pha"]
tth_data_export <- tth_data_export[, .(hh_id, tt_homeless, event, exit_category, overlap)]
  
tth_mod_overlap_export <- coxph(formula = Surv(tt_homeless, event) ~ exit_category,
                                data = tth_data_export,
                                weights = overlap,
                                cluster = hh_id)

summary(tth_mod_overlap_export)
# save(tth_mod_overlap_export, tth_data_export, file = "C:/temp/tth_mod_overlap.RData")


## Test PH assumptions ----
# Schoenfeld test
test_tth_mod <- cox.zph(tth_mod, transform = rank)


# --------------------------------------------------------------------------------------------------------#



    
    

# Step 3) Perform primary analysis for KCHA and SHA separately ----
# Note: agency is not in the formula

## 3a) First, fit the multinomial logistic regression model, using GEE ----
### KCHA ----
ps_mod_kcha <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + 
                           race_eth_me + single_caregiver + hh_size +
                           hh_disability + housing_time_at_exit + prog_type_use +
                           recent_homeless,
                         data = tth_data[tth_data$agency == "KCHA"],
                         id = hh_id_kc_pha,
                         LORstr = "independence")

# Next, calculate generalized propensity scores
ps_kcha <- as.data.frame(fitted(ps_mod_kcha))
colnames(ps_kcha) <- c("Neutral_kcha", "Negative_kcha", "Positive_kcha")
ps_kcha <- cbind("id_hudhears" = tth_data$id_hudhears[tth_data$agency == "KCHA"], 
                 agency = rep("KCHA", nrow(ps_kcha)),
                 ps_kcha)


### SHA ----
ps_mod_sha <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + 
                          race_eth_me + single_caregiver + hh_size +
                          hh_disability + housing_time_at_exit + prog_type_use +
                          recent_homeless,
                        data = tth_data[tth_data$agency == "SHA"],
                        id = hh_id_kc_pha,
                        LORstr = "independence")

# Next, calculate generalized propensity scores
ps_sha <- as.data.frame(fitted(ps_mod_sha))
colnames(ps_sha) <- c("Neutral_sha", "Negative_sha", "Positive_sha")
ps_sha <- cbind("id_hudhears" = tth_data$id_hudhears[tth_data$agency == "SHA"], 
                agency = rep("SHA", nrow(ps_sha)),
                ps_sha)



## 3b) Then, calculate inverse weights and assign to each observation  ----
tth_data <- tth_data %>%
  left_join(., ps_kcha, by = c("id_hudhears", "agency")) %>%
  left_join(., ps_sha, by = c("id_hudhears", "agency")) %>%
  mutate(iptw_kcha = case_when(exit_category == "Neutral" ~ 1/Neutral_kcha,
                          exit_category == "Negative" ~ 1/Negative_kcha,
                          exit_category == "Positive" ~ 1/Positive_kcha),
         iptw_sha = case_when(exit_category == "Neutral" ~ 1/Neutral_sha,
                               exit_category == "Negative" ~ 1/Negative_sha,
                               exit_category == "Positive" ~ 1/Positive_sha))


## 3c) Finally, fit weighted Cox PH, and cluster on hh_id ----
### KCHA ----
tth_data$exit_category <- relevel(factor(tth_data$exit_category), ref = "Neutral")

tth_mod_kcha <- coxph(formula = Surv(tt_homeless, event) ~ exit_category,
                 data = tth_data[tth_data$agency == "KCHA"],
                 weights = iptw_kcha,
                 cluster = hh_id_kc_pha)

summary(tth_mod_kcha)


### SHA ----
tth_mod_sha <- coxph(formula = Surv(tt_homeless, event) ~ exit_category,
                      data = tth_data[tth_data$agency == "SHA"],
                      weights = iptw_sha,
                      cluster = hh_id_kc_pha)

summary(tth_mod_sha)


### Adjusted results ----
tth_mod_pha_adjusted<- coxph(formula = Surv(tt_homeless, event) ~ exit_category + agency,
                             data = tth_data,
                             weights = iptw,
                             cluster = hh_id_kc_pha)
summary(tth_mod_pha_adjusted)


### PHA interaction with exit category results ----
tth_mod_interaction_pha_exitcat<- coxph(formula = Surv(tt_homeless, event) ~ exit_category * agency,
                                        data = tth_data,
                                        weights = iptw,
                                        cluster = hh_id_kc_pha)
summary(tth_mod_interaction_pha_exitcat)


#--------------------------------------------------
# 4) Visualize regression results ----


## 4a) Primary analysis plot ----

# Create dataframe of results
primary <- data.frame(index=c("pos", "neg"),
                      hr=c(exp(coef(tth_mod))[[2]], exp(coef(tth_mod))[[1]]),
                      low=c(exp(confint(tth_mod))[[2]], exp(confint(tth_mod))[[1]]),
                      hi=c(exp(confint(tth_mod))[[4]], exp(confint(tth_mod))[[3]]))

# Create a boxplot with ggplot
primary_plot <- ggplot(data=primary) +
  
  # Point estimates
  geom_point(aes(x=hr, y=index, color=index), size=3) +
  
  # Add labels under point estimates
  geom_text(label=paste(round(exp(coef(tth_mod))[[2]], 2)),
            x=exp(coef(tth_mod))[[2]], y=1.7, color="#619CFF") +
  geom_text(label=paste(round(exp(coef(tth_mod))[[1]], 2)),
            x=exp(coef(tth_mod))[[1]], y=0.7, color="#F8766D") +
  
  # Confidence intervals
  geom_errorbarh(height=0.05, aes(y=index, xmin=low, xmax=hi, color=index), size=1, alpha=0.5) +
  
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

png(file = paste0(file.path(capstone_path, "regression_primary.png")), width = 800, height = 600)
primary_plot
dev.off()



## 4b) Sensitivity analysis plot ----

# Create dataframe of results
ow <- data.frame(index=c("pos", "neg"),
                 hr=c(exp(coef(tth_mod_overlap))[[2]], exp(coef(tth_mod_overlap))[[1]]),
                 low=c(exp(confint(tth_mod_overlap))[[2]], exp(confint(tth_mod_overlap))[[1]]),
                 hi=c(exp(confint(tth_mod_overlap))[[4]], exp(confint(tth_mod_overlap))[[3]]))

# Create a boxplot with ggplot
ow_plot <- ggplot(data=ow) +
  
  # Point estimates
  geom_point(aes(x=hr, y=index, color=index), size=3) +
  
  # Add labels under point estimates
  geom_text(label=paste(round(exp(coef(tth_mod_overlap))[[2]], 2)),
            x=exp(coef(tth_mod_overlap))[[2]], y=1.7, color="#619CFF") +
  geom_text(label=paste(round(exp(coef(tth_mod_overlap))[[1]], 2)),
            x=exp(coef(tth_mod_overlap))[[1]], y=0.7, color="#F8766D") +
  
  # Confidence intervals
  geom_errorbarh(height=0.05, aes(y=index, xmin=low, xmax=hi, color=index), size=1, alpha=0.5) +
  
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

png(file = paste0(file.path(capstone_path, "regression_sensitivity.png")), width = 800, height = 600)
ow_plot
dev.off()


## 4c) PHA stratified analysis plots -----
### KCHA specific plot ----
# Create dataframe of results
kcha <- data.frame(index=c("pos", "neg"),
                   hr=c(exp(coef(tth_mod_kcha))[[2]], exp(coef(tth_mod_kcha))[[1]]),
                   low=c(exp(confint(tth_mod_kcha))[[2]], exp(confint(tth_mod_kcha))[[1]]),
                   hi=c(exp(confint(tth_mod_kcha))[[4]], exp(confint(tth_mod_kcha))[[3]]))

# Create a boxplot with ggplot
kcha_plot <- ggplot(data=kcha) +
  
  # Point estimates
  geom_point(aes(x=hr, y=index, color=index), size=3) +
  
  # Add labels under point estimates
  geom_text(label=paste(round(exp(coef(tth_mod_kcha))[[2]], 2)),
            x=exp(coef(tth_mod_kcha))[[2]], y=1.7, color="#619CFF") +
  geom_text(label=paste(round(exp(coef(tth_mod_kcha))[[1]], 2)),
            x=exp(coef(tth_mod_kcha))[[1]], y=0.7, color="#F8766D") +
  
  # Confidence intervals
  geom_errorbarh(height=0.05, aes(y=index, xmin=low, xmax=hi, color=index), size=1, alpha=0.5) +
  
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

png(file = paste0(file.path(capstone_path, "regression_kcha.png")), width = 800, height = 600)
kcha_plot
dev.off()


### SHA specific plot ----
# Create dataframe of results
sha <- data.frame(index=c("pos", "neg"),
                  hr=c(exp(coef(tth_mod_sha))[[2]], exp(coef(tth_mod_sha))[[1]]),
                  low=c(exp(confint(tth_mod_sha))[[2]], exp(confint(tth_mod_sha))[[1]]),
                  hi=c(exp(confint(tth_mod_sha))[[4]], exp(confint(tth_mod_sha))[[3]]))

# Create a boxplot with ggplot
sha_plot <- ggplot(data=sha) +
  
  # Point estimates
  geom_point(aes(x=hr, y=index, color=index), size=3) +
  
  # Add labels under point estimates
  geom_text(label=paste(round(exp(coef(tth_mod_sha))[[2]], 2)),
            x=exp(coef(tth_mod_sha))[[2]], y=1.7, color="#619CFF") +
  geom_text(label=paste(round(exp(coef(tth_mod_sha))[[1]], 2)),
            x=exp(coef(tth_mod_sha))[[1]], y=0.7, color="#F8766D") +
  
  # Confidence intervals
  geom_errorbarh(height=0.05, aes(y=index, xmin=low, xmax=hi, color=index), size=1, alpha=0.5) +
  
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

png(file = paste0(file.path(capstone_path, "regression_sha.png")), width = 800, height = 600)
sha_plot
dev.off()
