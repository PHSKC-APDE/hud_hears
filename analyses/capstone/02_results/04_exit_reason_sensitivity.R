## Script name: exit_reason_sensitivity.R
##
## Purpose of script: 
##    1) Create vector of exit reasons for sensitivity analysis 
##    2) Create function to perform analysis
##    3) Create function to omit an exit reason and perform analysis
##    4) Apply fit_one_out() function over vector of exit reasons
##    5) Create Forest Plots
##
## Author: Taylor Keating
## Date Created: 2/7/2022
## Email: n-tkeating@kingcounty.gov
##
## Notes: In progress, still updating
##          - filter for exit reasons that have certain count
##          - problem with 2 exit reasons right now
##          - maybe use ggforestplot or ggplot to create forest plots
##   
##

# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, ggplot2, viridis, hrbrthemes,
               knitr, kableExtra, rmarkdown, multgee, survival, forestplot)

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

# Select table that contains study data
study_data<- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[capstone_data_3]"))

#--------------------------------------------------
# 1) Create list of exit reasons for sensitivity analysis

exit_reason_vector<- study_data$exit_reason_clean %>% unique()
exit_reason_vector<- exit_reason_vector[is.na(exit_reason_vector)==FALSE] # omit NA exit_reason_clean from vector

#--------------------------------------------------
# 2) Create function to perform analysis on study data

# function runs analysis on dataset and returns fitted Cox PH model
run_analysis<- function(data){
  
  # fit multinomial log reg model with GEE and calculate generalized propensity scores
  ps_mod <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me +
                        race_eth_me + agency + single_caregiver + hh_size +
                        hh_disability + housing_time_at_exit + major_prog +
                        kc_opp_index_score,
                      data = data,
                      id = hh_id_kc_pha,
                      LORstr = "independence")
  ps <- as.data.frame(fitted(ps_mod))
  colnames(ps) <- c("Neutral", "Negative", "Positive")
  
  # Then, calculate inverse weights and assign to each obs.
  data$IPTW <- ifelse(data$exit_category == "Negative", 1/ps$Negative,
                          ifelse(data$exit_category == "Neutral", 1/ps$Neutral,
                                 1/ps$Positive))
  
  # Finally, fit weighted Cox PH (cluster on hh_id_kc_pha)
  data$exit_category <- relevel(factor(data$exit_category), ref = "Neutral")
  tth_mod <- coxph(formula = Surv(tt_homeless, event) ~ exit_category,
                   data = data,
                   weights = IPTW,
                   cluster = hh_id_kc_pha)
  return(tth_mod)
}

#--------------------------------------------------
# 3) Create function to omit an exit reason and perform analysis

# function that takes an exit reason, and the full dataset
  # omits rows with that reason from full dataset,
  # performs analysis 
  # and returns number omitted, HR estimates, and CI's for pos and neg
fit_one_out<- function(reason_omitted, data){
  
  # omit rows with that specified exit reason
  new_data<- data %>% filter(exit_reason_clean != reason_omitted) 
  
  # perform analysis and return the fitted Cox PH model
  tth_mod<- run_analysis(new_data)
  
  #remove later
  print(reason_omitted)
  
  # return reason omitted, reason category, number omitted, HR estimates and CI's for pos and neg
  return(list(exit_reason_omitted= reason_omitted,
              exit_category= data %>% filter(exit_reason_clean==reason_omitted) %>% pull(exit_category) %>% unique(), 
              number_omitted= data %>% filter(exit_reason_clean==reason_omitted) %>% nrow(),
              HR_pos= (tth_mod %>% coef() %>% exp())[["exit_categoryPositive"]],
              HR_pos_lower= (tth_mod %>% confint() %>% exp())["exit_categoryPositive", 1],
              HR_pos_upper= (tth_mod %>% confint() %>% exp())["exit_categoryPositive", 2],
              HR_neg= (tth_mod %>% coef() %>% exp())[["exit_categoryNegative"]],
              HR_neg_lower= (tth_mod %>% confint() %>% exp())["exit_categoryNegative", 1],
              HR_neg_upper= (tth_mod %>% confint() %>% exp())["exit_categoryNegative", 2]
              )
         )
}

#--------------------------------------------------
# 4) Apply fit_one_out function over vector of exit reasons to study data

# HR estimates and CI's from loo sensitivity analysis

# # REMOVE LATER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# run_analysis(study_data %>% filter(exit_reason_clean!= "Rent too high")) # system is computationally singular
# run_analysis(study_data %>% filter(exit_reason_clean!= "Moved to Non-Subsidized Rental")) # Robust covariance matrix is not positive definite
# # REMOVE LATER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# perform without "Moved to Non-Subsidized Rental" and "Rent too high" FOR NOW
loo_output<- as_tibble(t(sapply(exit_reason_vector[exit_reason_vector!="Moved to Non-Subsidized Rental" & 
                                                     exit_reason_vector!="Rent too high"], FUN=fit_one_out, data=study_data)))

# format loo_output
loo_output$exit_category<-  relevel(factor(loo_output$exit_category), ref = "Neutral") 
loo_output<- loo_output %>% arrange(exit_category, desc(as.numeric(number_omitted))) # arrange in descending order by number omitted within exit_category
loo_output[,1:3]<- sapply(loo_output[,1:3], MARGIN=2, as.character)
loo_output[,4:ncol(loo_output)]<- sapply(loo_output[,4:ncol(loo_output)], MARGIN=2, as.numeric)

# Full Data HR estimates and CI's
full_data_mod<- run_analysis(study_data)
full_data_output<- 
  tibble(exit_reason_omitted= "Full Data",
         exit_category= "Full Data",
         number_omitted= "0",
         HR_pos= (full_data_mod %>% coef() %>% exp())[["exit_categoryPositive"]],
         HR_pos_lower= (full_data_mod %>% confint() %>% exp())["exit_categoryPositive", 1],
         HR_pos_upper= (full_data_mod %>% confint() %>% exp())["exit_categoryPositive", 2],
         HR_neg= (full_data_mod %>% coef() %>% exp())[["exit_categoryNegative"]],
         HR_neg_lower= (full_data_mod %>% confint() %>% exp())["exit_categoryNegative", 1],
         HR_neg_upper= (full_data_mod %>% confint() %>% exp())["exit_categoryNegative", 2],
         summary=TRUE)

#--------------------------------------------------
# 5) Create Forest Plots
## using forestplot::forestplot()

# set count threshold to plot
count_threshold<- 100

# create forest plot data frame with header and summary row
header<- tibble(exit_reason_omitted= "Exit Reason Omitted",
                number_omitted= "Number Omitted",
                exit_category= "Exit Category",
                summary=TRUE)
empty_row<- tibble(exit_reason_omitted=NA)

# only take rows of exit reasons with count >= threshold
forest_plot_data<- bind_rows(header,
                             loo_output %>% filter(as.numeric(number_omitted)>=count_threshold),
                             empty_row,
                             full_data_output)
#---
#png(file="HR_pos_LOO.png", width=1400, height=1200)
# forest plot for HR positive vs neutral
forest_plot_data %>%
  forestplot::forestplot(labeltext= c(exit_reason_omitted, number_omitted, exit_category),
             txt_gp= fpTxtGp(label=gpar(cex=1), # label text size might be too large to plot (adjust here to 0.25)
                             xlab=gpar(cex=1.25),
                             title=gpar(cex=1.5),
                             ticks=gpar(cex=1)),
             is.summary= summary,
             mean= HR_pos,
             lower= HR_pos_lower,
             upper= HR_pos_upper,
             title= "Positive vs. Neutral Exit",
             xlab="Hazard Ratio",
             zero= forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_pos),
             grid= c(forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_pos_lower),
                     forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_pos_upper))
             )
#dev.off()

#png(file="HR_neg_LOO.png", width=1400, height=1200)
# forest plot for HR negative vs neutral
forest_plot_data %>% 
  forestplot::forestplot(labeltext= c(exit_reason_omitted, number_omitted, exit_category),
             txt_gp= fpTxtGp(label=gpar(cex=1),  # label text size might be too large to plot (adjust here to 0.25)
                             xlab=gpar(cex=1.25),
                             title=gpar(cex=1.5),
                             ticks=gpar(cex=1)),
             is.summary= summary,
             mean= HR_neg,
             lower= HR_neg_lower,
             upper= HR_neg_upper,
             title= "Negative vs. Neutral Exit",
             xlab="Hazard Ratio",
             zero= forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_neg),
             grid= c(forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_neg_lower),
                     forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_neg_upper))
             )
#dev.off()

