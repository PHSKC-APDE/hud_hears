## Script name: 04_exit_reason_sensitivity.R ----
##
## Purpose of script: Run leave-one-out sensitivity analysis on exit reasons and produce resulting forest plots of hazard ratios
##                      - to determine the impact of specific exit reasons on the hazard ratios of experiencing homelessness
##                      - regression methods are from `03_regression.R`
##
##    1) Create vector of exit reasons for sensitivity analysis 
##    2) Create functions to perform analysis (with and without GEE in multinomial log reg)
##    3) Create function to omit an exit reason and perform analysis
##    4) Apply fit_one_out() function over vector of exit reasons
##    5) Create Forest Plots
##        - Plot HR positive vs neutral - LOO_HR_pos_no_GEE.png
##        - Plot HR positive vs neutral (removing negative exit reasons in plot) - LOO_HR_pos_no_GEE_no_neg.png
##        - Plot HR negative vs neutral - LOO_HR_neg_no_GEE.png
##        - Plot HR negative vs neutral (removing positive exit reasons in plot) - LOO_HR_neg_no_GEE_no_pos.png
##
## Author: Taylor Keating
## Date Created: 3/11/2022
##
## Notes:
##    - Problem with 2 exit reasons ("Moved to Non-Subsidized Rental" and "Rent too high") while fitting multinomial log reg with GEE 
##    - Therefore, ALL RUNS in this sensitivity analysis are performed using multinomial log reg W/O GEE
##   
##

# set working directory (for output of plots- to utilize more easily)
capstone_path <- file.path(here::here(), "analyses/capstone/02_results")

# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, ggplot2, viridis, hrbrthemes,
               knitr, kableExtra, rmarkdown, multgee, survival, forestplot, nnet)

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

# Select & tidy table that contains study data and re-level exit category with reference as "Neutral" ----
study_data<- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[capstone_data_3]"))
study_data$exit_category<- relevel(factor(study_data$exit_category), ref="Neutral")

# format exit reasons according to HUD editor comments
study_data[, exit_reason_clean := stringr::str_to_title(exit_reason_clean)]
study_data[, exit_reason_clean := gsub("Pb/Mr", "PB/MR", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("Hqs", "HQS", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("Hcv", "HCV", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("Hsg", "Housing", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("Kcha", "KCHA", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("Sha", "SHA", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("PB/MR Moved Out", "PB/MR Moved Out,", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("Non-Time Limited", "Non-Time-Limited,", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("Moved - Needed A Higher", "Moved—Needed A Higher", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("^Expired - ", "Expired—", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("^Ineligible - ", "Ineligible—", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("^Eviction - ", "Eviction—", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("^Fraud - ", "Fraud—", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("^Absence - ", "Absence—", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("^Noncompliance - ", "Noncompliance—", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("^Moved - ", "Moved—", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("^Graduated - ", "Graduated—", exit_reason_clean)]
study_data[, exit_reason_clean := gsub(" - Criminal$", "—Criminal", exit_reason_clean)]
study_data[, exit_reason_clean := gsub(" - Non-Criminal$", "—Non-Criminal", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("Moved In W/Family/Friends", "Moved In W/Family or Friends", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("Family,Friends", "Family or Friends", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("S8", "Section 8", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("Hospital/Assisted Living", " Hospital or Assisted Living", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("Re-Examination", "Reexamination", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("Judgement", "Judgment", exit_reason_clean)]
study_data[, exit_reason_clean := gsub("\\(Non-Criminal\\)", "(noncriminal)", exit_reason_clean)]


# Remove anyone with missing variables
study_data <- study_data %>% filter(full_demog == T)


#--------------------------------------------------
# 1) Create list of exit reasons for sensitivity analysis ----

exit_reason_vector<- study_data$exit_reason_clean %>% unique()
exit_reason_vector<- exit_reason_vector[is.na(exit_reason_vector)==FALSE] # omit NA exit_reason_clean from vector


#--------------------------------------------------
# 2) Create 2 functions to perform analysis on study data ----
  # i) with GEE in multinomial log reg
  # ii) without GEE in multinomial log reg

# function runs analysis (with GEE in multinomial log reg) on dataset and returns fitted Cox PH model
run_analysis <- function(data, gee = T){
  
  if (gee == T) {
    # fit multinomial log reg model with GEE and calculate generalized propensity scores
    ps_mod <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me +
                          race_eth_me + agency + single_caregiver + hh_size +
                          hh_disability + housing_time_at_exit + prog_type_use +
                          recent_homeless,
                        data = data,
                        id = hh_id_kc_pha,
                        LORstr = "independence")
    ps <- as.data.frame(fitted(ps_mod))
    colnames(ps) <- levels(data$exit_category)
    ps<- cbind("id_hudhears" = data$id_hudhears, ps)
  } else {
    # fit multinomial log reg model without GEE and calculate generalized propensity scores
    ps_mod<- multinom(formula= exit_category ~ age_at_exit + gender_me +
                        race_eth_me + agency + single_caregiver + hh_size +
                        hh_disability + housing_time_at_exit + prog_type_use +
                        recent_homeless,
                      data= data,
                      trace=FALSE) 
    ps<- as.data.frame(fitted(ps_mod))
    ps<- cbind("id_hudhears" = data$id_hudhears, ps)
  }

  # Then, calculate inverse weights and assign to each obs.
  data <- data %>%
    left_join(., ps, by = "id_hudhears") %>%
    mutate(iptw = case_when(exit_category == "Neutral" ~ 1/Neutral,
                            exit_category == "Negative" ~ 1/Negative,
                            exit_category == "Positive" ~ 1/Positive))
  
  # Finally, fit weighted Cox PH (cluster on hh_id_kc_pha)
  tth_mod <- coxph(formula = Surv(tt_homeless, event) ~ exit_category,
                   data = data,
                   weights = iptw,
                   cluster = hh_id_kc_pha)
  return(tth_mod)
}


#--------------------------------------------------
# 3) Create function to omit an exit reason and perform analysis ----

# function that takes an exit reason, and the full dataset
  # omits rows with that reason from full dataset,
  # performs analysis 
  # and returns number omitted, HR estimates, and CI's for pos vs. neut and neg vs. neut
fit_one_out<- function(reason_omitted, data){
  
  # print current reason
  print(reason_omitted)
  
  # omit rows with that specified exit reason
  new_data <- data %>% filter(exit_reason_clean != reason_omitted) 
  
  # run analysis w/o GEE in multinomial log reg, return fitted Cox PH model
  tth_mod <- run_analysis(new_data, gee = T)
  tth_mod_no_gee <- run_analysis(new_data, gee = F)
  
  
  # return reason omitted, reason category, number omitted, HR estimates and CI's for pos and neg
  return(list(exit_reason_omitted= reason_omitted,
              exit_category= data %>% filter(exit_reason_clean==reason_omitted) %>% pull(exit_category) %>% unique() %>% as.character(), 
              number_omitted= data %>% filter(exit_reason_clean==reason_omitted) %>% nrow(),
              HR_pos= (tth_mod %>% coef() %>% exp())[["exit_categoryPositive"]],
              HR_pos_lower= (tth_mod %>% confint() %>% exp())["exit_categoryPositive", 1],
              HR_pos_upper= (tth_mod %>% confint() %>% exp())["exit_categoryPositive", 2],
              HR_neg= (tth_mod %>% coef() %>% exp())[["exit_categoryNegative"]],
              HR_neg_lower= (tth_mod %>% confint() %>% exp())["exit_categoryNegative", 1],
              HR_neg_upper= (tth_mod %>% confint() %>% exp())["exit_categoryNegative", 2],
              HR_pos_no_gee = (tth_mod_no_gee %>% coef() %>% exp())[["exit_categoryPositive"]],
              HR_pos_lower_no_gee = (tth_mod_no_gee %>% confint() %>% exp())["exit_categoryPositive", 1],
              HR_pos_upper_no_gee = (tth_mod_no_gee %>% confint() %>% exp())["exit_categoryPositive", 2],
              HR_neg_no_gee = (tth_mod_no_gee %>% coef() %>% exp())[["exit_categoryNegative"]],
              HR_neg_lower_no_gee = (tth_mod_no_gee %>% confint() %>% exp())["exit_categoryNegative", 1],
              HR_neg_upper_no_gee = (tth_mod_no_gee %>% confint() %>% exp())["exit_categoryNegative", 2]
              )
         )
}


#--------------------------------------------------
# 4) Apply fit_one_out function over vector of exit reasons to study data ----

# HR estimates and CI's from loo sensitivity analysis
loo_output<- as.tibble(t(sapply(exit_reason_vector, FUN=fit_one_out, data=study_data)))

# format loo_output (make columns character/numeric vectors instead of lists)
# arrange in descending order by number omitted within exit_category
loo_output[,1:3]<- sapply(loo_output[,1:3], MARGIN=2, as.character)
loo_output[,4:ncol(loo_output)]<- sapply(loo_output[,4:ncol(loo_output)], MARGIN=2, as.numeric)
loo_output$exit_category<-  relevel(factor(loo_output$exit_category), ref = "Neutral") 
loo_output<- loo_output %>% arrange(exit_category, desc(as.numeric(number_omitted)))

# Full Data HR estimates and CI's (analysis using multinomial log reg w/o GEE)
full_data_mod <- run_analysis(study_data, gee = T)
full_data_mod_no_gee <- run_analysis(study_data, gee = F)

full_data_output <- 
  tibble(exit_reason_omitted= "Full Data",
         exit_category= "Full Data",
         number_omitted= "0",
         HR_pos= (full_data_mod %>% coef() %>% exp())[["exit_categoryPositive"]],
         HR_pos_lower= (full_data_mod %>% confint() %>% exp())["exit_categoryPositive", 1],
         HR_pos_upper= (full_data_mod %>% confint() %>% exp())["exit_categoryPositive", 2],
         HR_neg= (full_data_mod %>% coef() %>% exp())[["exit_categoryNegative"]],
         HR_neg_lower= (full_data_mod %>% confint() %>% exp())["exit_categoryNegative", 1],
         HR_neg_upper= (full_data_mod %>% confint() %>% exp())["exit_categoryNegative", 2],
         HR_pos_no_gee = (full_data_mod_no_gee %>% coef() %>% exp())[["exit_categoryPositive"]],
         HR_pos_lower_no_gee = (full_data_mod_no_gee %>% confint() %>% exp())["exit_categoryPositive", 1],
         HR_pos_upper_no_gee = (full_data_mod_no_gee %>% confint() %>% exp())["exit_categoryPositive", 2],
         HR_neg_no_gee = (full_data_mod_no_gee %>% coef() %>% exp())[["exit_categoryNegative"]],
         HR_neg_lower_no_gee = (full_data_mod_no_gee %>% confint() %>% exp())["exit_categoryNegative", 1],
         HR_neg_upper_no_gee = (full_data_mod_no_gee %>% confint() %>% exp())["exit_categoryNegative", 2],
         summary=TRUE)


#--------------------------------------------------
# 5) Create Forest Plots ----
## using forestplot::forestplot()

# i) set count threshold
# ii) format dataframe for forest plot (header and summary rows)
# iii) plots of HR pos vs neutral
# iv) plots of HR neg vs neutral


## i) set count threshold to plot ----
count_threshold <- 100


## ii) format data frame for forest plot ----
forest_plot_data<- bind_rows(
  # LOO data with only rows of exit reasons with count >= threshold
  # Also filter 'Expired - ported out' for now because it has two categories (this will be fixed)
  loo_output %>% 
    filter(as.numeric(number_omitted)>=count_threshold & exit_reason_omitted != "Expired—Ported Out"),
  # empty row
  tibble(exit_reason_omitted=NA), 
  # results from full model
  full_data_output)

setDT(forest_plot_data)
forest_plot_data[, number_omitted := format(as.numeric(number_omitted), big.mark = ',')]
forest_plot_data <- forest_plot_data[!number_omitted %like% 'NA']
forest_plot_data <- rbind(data.table(exit_reason_omitted= "Exit Reason Omitted",
                                     number_omitted= "Number Omitted",
                                     exit_category= "Exit Category", 
                                     summary = TRUE), 
                          forest_plot_data, 
                          fill = T)


## GEE models ----
### Plot HR Positive vs Neutral ----

# forest plot for HR positive vs neutral (FULL PLOT)
png(file = file.path(capstone_path, "LOO_HR_pos.png"), width=1600, height=1200)
forest_plot_data %>%
  forestplot::forestplot(labeltext= c(exit_reason_omitted, number_omitted, exit_category),
                         txt_gp= fpTxtGp(label=gpar(cex=1.4), # label text size might be too large to plot (adjust here to 0.25)
                                         xlab=gpar(cex=2),
                                         title=gpar(cex=3),
                                         ticks=gpar(cex=1.5),
                                         summary=gpar(cex=1.8)),
                         xticks=seq(0.00,0.40,by=0.05),
                         is.summary= summary,
                         mean= HR_pos,
                         lower= HR_pos_lower,
                         upper= HR_pos_upper,
                         title= "Positive vs. Neutral Exit",
                         xlab="Hazard Ratio of Experiencing Homelessness",
                         zero= forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_pos),
                         grid= c(forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_pos_lower),
                                 forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_pos_upper)),
                         hrzl_lines=list("2" = gpar(lwd=1, columns=c(1:3)), 
                                         "25" = gpar(lwd= 442, lineend="butt", columns=c(1:3), col="#99999922")) 
                         # controls "grey box" used to visually separate exit categories in plot
                         # "25" is vertical location of "line"/box, lwd is vertical width of "line"/box
  )
dev.off()

# forest plot for HR positive vs neutral (REMOVING NEGATIVE REASONS)
png(file = file.path(capstone_path, "LOO_HR_pos_no_neg.png"), width=1600, height=1400)
forest_plot_data %>% filter((exit_category %in% c("Neutral", "Positive", "Exit Category", "Full Data")) |
                              is.na(exit_category)) %>%
  forestplot::forestplot(labeltext= c(exit_reason_omitted, number_omitted, exit_category),
                         txt_gp= fpTxtGp(label=gpar(cex=1.8), # label text size might be too large to plot (adjust here to 0.25)
                                         xlab=gpar(cex=2),
                                         title=gpar(cex=3),
                                         ticks=gpar(cex=1.8),
                                         summary=gpar(cex=2)),
                         xticks=seq(0.00,0.40,by=0.05),
                         is.summary= summary,
                         mean= HR_pos,
                         lower= HR_pos_lower,
                         upper= HR_pos_upper,
                         title= "Positive Versus Neutral Exit",
                         xlab="Hazard Ratio of Experiencing Homelessness",
                         zero= forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_pos),
                         grid= c(forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_pos_lower),
                                 forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_pos_upper)),
                         hrzl_lines=list("2" = gpar(lwd=1, columns=c(1:3)), 
                                         "19" = gpar(lwd=290, lineend="butt", columns=c(1:3), col="#99999922"))
                         # controls "grey box" used to visually separate exit categories in plot
                         # "19" is vertical location of "line"/box, lwd is vertical width of "line"/box
  )
dev.off()


### plot HR Negative vs Neutral ----

# forest plot for HR negative vs neutral (FULL PLOT)
png(file = file.path(capstone_path, "LOO_HR_neg.png"), width=1600, height=1200)
forest_plot_data %>% 
  forestplot::forestplot(labeltext= c(exit_reason_omitted, number_omitted, exit_category),
                         txt_gp= fpTxtGp(label=gpar(cex=1.4),  # label text size might be too large to plot (adjust here to 0.25)
                                         xlab=gpar(cex=2),
                                         title=gpar(cex=3),
                                         ticks=gpar(cex=1.5),
                                         summary=gpar(cex=1.8)),
                         xticks=seq(1.50,3.00,by=0.25),
                         is.summary= summary,
                         mean= HR_neg,
                         lower= HR_neg_lower,
                         upper= HR_neg_upper,
                         title= "Negative vs. Neutral Exit",
                         xlab="Hazard Ratio of Experiencing Homelessness",
                         zero= forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_neg),
                         grid= c(forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_neg_lower),
                                 forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_neg_upper)),
                         hrzl_lines=list("2" = gpar(lwd=1,columns=c(1:3)), 
                                         "25" = gpar(lwd= 442, lineend="butt", columns=c(1:3), col="#99999922"))
                         # controls "grey box" used to visually separate exit categories in plot
                         # "25" is vertical location of "line"/box, lwd is vertical width of "line"/box
  )
dev.off()

# forest plot for HR negative vs neutral (REMOVING POSITIVE REASONS)
png(file = file.path(capstone_path, "LOO_HR_neg_no_pos.png"), width=1600, height=1400)
forest_plot_data %>% filter((exit_category %in% c("Neutral", "Negative", "Exit Category", "Full Data")) |
                              is.na(exit_category)) %>%
  forestplot::forestplot(labeltext= c(exit_reason_omitted, number_omitted, exit_category),
                         txt_gp= fpTxtGp(label=gpar(cex=1.6),  # label text size might be too large to plot (adjust here to 0.25)
                                         xlab=gpar(cex=2),
                                         title=gpar(cex=3),
                                         ticks=gpar(cex=1.8),
                                         summary=gpar(cex=2)),
                         xticks=seq(1.50,3.00,by=0.25),
                         is.summary= summary,
                         mean= HR_neg,
                         lower= HR_neg_lower,
                         upper= HR_neg_upper,
                         title= "Negative Versus Neutral Exit",
                         xlab="Hazard Ratio of Experiencing Homelessness",
                         zero= forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_neg),
                         grid= c(forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_neg_lower),
                                 forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_neg_upper)),
                         hrzl_lines=list("2" = gpar(lwd=1, columns=c(1:3)), 
                                         "25" = gpar(lwd= 620, lineend="butt", columns=c(1:3), col="#99999922"))
                         # controls "grey box" used to visually separate exit categories in plot
                         # "27" is vertical location of "line"/box, lwd is vertical width of "line"/box
  )
dev.off()


## Non-GEE models ----
### Plot HR Positive vs Neutral ----
# forest plot for HR positive vs neutral (FULL PLOT)
png(file = file.path(capstone_path, "LOO_HR_pos_no_GEE.png"), width=1600, height=1200)
forest_plot_data %>%
  forestplot::forestplot(labeltext= c(exit_reason_omitted, number_omitted, exit_category),
             txt_gp= fpTxtGp(label=gpar(cex=1.4), # label text size might be too large to plot (adjust here to 0.25)
                             xlab=gpar(cex=2),
                             title=gpar(cex=3),
                             ticks=gpar(cex=1.5),
                             summary=gpar(cex=1.8)),
             xticks=seq(0.15,0.55,by=0.05),
             is.summary= summary,
             mean= HR_pos_no_gee,
             lower= HR_pos_lower_no_gee,
             upper= HR_pos_upper_no_gee,
             title= "Positive vs. Neutral Exit",
             xlab="Hazard Ratio of Experiencing Homelessness",
             zero= forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_pos_no_gee),
             grid= c(forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_pos_lower_no_gee),
                     forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_pos_upper_no_gee)),
             hrzl_lines=list("2" = gpar(lwd=1, columns=c(1:3)), 
                             "25" = gpar(lwd= 442, lineend="butt", columns=c(1:3), col="#99999922")) 
                              # controls "grey box" used to visually separate exit categories in plot
                              # "25" is vertical location of "line"/box, lwd is vertical width of "line"/box
             )
dev.off()

# forest plot for HR positive vs neutral (REMOVING NEGATIVE REASONS)
# Find where the positive rows start
pos_row <- which(str_detect(forest_plot_data %>% 
                              filter((exit_category %in% c("Neutral", "Positive", "Exit Category", "Full Data")) | is.na(exit_category)) %>% 
                              select(exit_category) %>% unlist(., use.names = F),
                            "Positive"))

png(file = file.path(capstone_path, "LOO_HR_pos_no_neg_no_GEE.png"), width=1600, height=1400)
forest_plot_data %>% filter((exit_category %in% c("Neutral", "Positive", "Exit Category", "Full Data")) |
                              is.na(exit_category)) %>%
  forestplot::forestplot(labeltext= c(exit_reason_omitted, number_omitted, exit_category),
                         txt_gp= fpTxtGp(label=gpar(cex=1.8), # label text size might be too large to plot (adjust here to 0.25)
                                         xlab=gpar(cex=2),
                                         title=gpar(cex=3),
                                         ticks=gpar(cex=1.8),
                                         summary=gpar(cex=2)),
                         xticks=seq(0.15,0.55,by=0.05),
                         is.summary= summary,
                         mean= HR_pos_no_gee,
                         lower= HR_pos_lower_no_gee,
                         upper= HR_pos_upper_no_gee,
                         title= "Positive vs. Neutral Exit",
                         xlab="Hazard Ratio of Experiencing Homelessness",
                         zero= forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_pos_no_gee),
                         grid= c(forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_pos_lower_no_gee),
                                 forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_pos_upper_no_gee)),
                         hrzl_lines=list("2" = gpar(lwd=1, columns=c(1:3)), 
                                         "19" = gpar(lwd=290, lineend="butt", columns=c(1:3), col="#99999922"))
                                          # controls "grey box" used to visually separate exit categories in plot
                                          # "19" is vertical location of "line"/box, lwd is vertical width of "line"/box
  )
dev.off()


### plot HR Negative vs Neutral ----

# forest plot for HR negative vs neutral (FULL PLOT)
png(file = file.path(capstone_path, "LOO_HR_neg_no_GEE.png"), width=1600, height=1200)
forest_plot_data %>% 
  forestplot::forestplot(labeltext= c(exit_reason_omitted, number_omitted, exit_category),
             txt_gp= fpTxtGp(label=gpar(cex=1.4),  # label text size might be too large to plot (adjust here to 0.25)
                             xlab=gpar(cex=2),
                             title=gpar(cex=3),
                             ticks=gpar(cex=1.5),
                             summary=gpar(cex=1.8)),
             xticks=seq(1.50,3.00,by=0.25),
             is.summary= summary,
             mean= HR_neg_no_gee,
             lower= HR_neg_lower_no_gee,
             upper= HR_neg_upper_no_gee,
             title= "Negative vs. Neutral Exit",
             xlab="Hazard Ratio of Experiencing Homelessness",
             zero= forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_neg_no_gee),
             grid= c(forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_neg_lower_no_gee),
                     forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_neg_upper_no_gee)),
             hrzl_lines=list("2" = gpar(lwd=1,columns=c(1:3)), 
                             "25" = gpar(lwd= 442, lineend="butt", columns=c(1:3), col="#99999922"))
                              # controls "grey box" used to visually separate exit categories in plot
                              # "25" is vertical location of "line"/box, lwd is vertical width of "line"/box
             )
dev.off()

# forest plot for HR negative vs neutral (REMOVING POSITIVE REASONS)
png(file = file.path(capstone_path, "LOO_HR_neg_no_pos_no_GEE.png"), width=1600, height=1400)
forest_plot_data %>% filter((exit_category %in% c("Neutral", "Negative", "Exit Category", "Full Data")) |
                              is.na(exit_category)) %>%
  forestplot::forestplot(labeltext= c(exit_reason_omitted, number_omitted, exit_category),
                         txt_gp= fpTxtGp(label=gpar(cex=1.6),  # label text size might be too large to plot (adjust here to 0.25)
                                         xlab=gpar(cex=2),
                                         title=gpar(cex=3),
                                         ticks=gpar(cex=1.8),
                                         summary=gpar(cex=2)),
                         xticks=seq(1.50,3.00,by=0.25),
                         is.summary= summary,
                         mean= HR_neg_no_gee,
                         lower= HR_neg_lower_no_gee,
                         upper= HR_neg_upper_no_gee,
                         title= "Negative vs. Neutral Exit",
                         xlab="Hazard Ratio of Experiencing Homelessness",
                         zero= forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_neg_no_gee),
                         grid= c(forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_neg_lower_no_gee),
                                 forest_plot_data %>% filter(exit_reason_omitted=="Full Data") %>% pull(HR_neg_upper_no_gee)),
                         hrzl_lines=list("2" = gpar(lwd=1, columns=c(1:3)), 
                                         "25" = gpar(lwd= 620, lineend="butt", columns=c(1:3), col="#99999922"))
                                          # controls "grey box" used to visually separate exit categories in plot
                                          # "25" is vertical location of "line"/box, lwd is vertical width of "line"/box
  )
dev.off()


