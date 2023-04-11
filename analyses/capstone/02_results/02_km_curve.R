## Script name: 02_km_curve.R
##
## Purpose of script: Create Kaplan-Meier curves of time to homelessness by exit type for Capstone Hudhears project
##
##            1) KM curves by exit type - KM_curve.png
##            2) KM curves by exit type, stratified by KCHA/SHA - KM_curve_pha.png
##            3) KM curves by exit type, faceted by KCHA and SHA (separate plots) - KM_curve_pha_facet.png
##
## Author: Taylor Keating
## Date Created: 3/11/2022
##
## Notes: Also produces estimates for probability remaining housed at 365 days after exit (by exit type and by exit type/PHA combination)
##   
##

# set working directory (for output of plots- to utilize more easily)
output_path <- paste0(here::here(), "/analyses/capstone/02_results/")

# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, odbc, glue, data.table, ggplot2, viridis, hrbrthemes,
               knitr, kableExtra, rmarkdown, survival, survminer)

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

# Select table that contains time-to-homelessness variable
tth_data <- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[capstone_data_3]"))
# Create "survival" object column in data
tth_data <- tth_data %>% mutate(surv_object = Surv(time=tt_homeless, event=event))


# 1) KM curves by exit type ----
# Create KM estimate survival curve by exit type
fit_exit_type <- survfit(surv_object ~ exit_category, data = tth_data, conf.type = "log-log")

## Plotting
# Create object of Kaplan-Meier survival curve by exit type
km_exit_type <- ggsurvplot(fit_exit_type, data=tth_data,
                          xlab = "Time from Public Housing Exit (Days)",
                          ylab = "Probability Remaining Housed",
                          title = "Kaplan-Meier Estimates of Time from Exit to Homelessness",
                          linetype = "strata",
                          palette = "Dark2",
                          break.time.by = 50,
                          xlim = c(0,max(tth_data$tt_homeless)),
                          ylim = c(0.7,1),
                          break.y.by= 0.05,
                          conf.int = TRUE,
                          legend.title = "Exit Type",
                          legend.labs = na.omit(unique(tth_data$exit_category)),
                          risk.table = TRUE, # risk table added
                          risk.table.title = "Number at Risk",
                          cumevents = TRUE, # cumulative events table added
                          cumevents.title = "Cumulative Number of Events",
                          tables.col = "strata",
                          #tables.height=0.3,
                          censor.size=4,
                          gg_theme=theme_bw())

# remove x-lab from risk table and cumulative events table
km_exit_type$table <- km_exit_type$table +
  labs(x = "")
km_exit_type$cumevents <- km_exit_type$cumevents + 
  labs(x = "")

# Add commas to numbers
km_exit_type$table$layers[[1]]$data$llabels <- scales::comma(km_exit_type$table$layers[[1]]$data$llabels)
km_exit_type$cumevents$layers[[1]]$data$cum.n.event <- scales::comma(km_exit_type$cumevents$layers[[1]]$data$cum.n.event)

# plot object
png(file = paste0(output_path, "KM_curve_report.png"), width = 600, height = 800)
km_exit_type
dev.off()


## Other Descriptive Stats
# Non-parametric KM-estimated survival at time= 365 days (by exit type)
summary(fit_exit_type, times = 365)


# log-minus-log plot (if PH assumption holds, lines should be parallel)
png(file = paste0(output_path, "KM_log_log.png"), width = 800, height = 600)
rms::survplot(rms::npsurv(surv_object ~ exit_category, data = tth_data), loglog=T)
dev.off()


# 2) KM curves by exit type, stratified by KCHA/SHA ----

# Create KM estimate survival curve by exit type
fit_exit_type_pha <- survfit(surv_object ~ exit_category + agency, 
                            data = tth_data, 
                            conf.type = "log-log")


## Plotting
# Create object of Kaplan-Meier survival curve by exit type, stratified by PHA
km_exit_type_pha <- ggsurvplot(fit_exit_type_pha, data = tth_data,
                              xlab = "Time from Public Housing Exit (Days)",
                              ylab = "Probability Remaining Housed",
                              title = "Kaplan-Meier Estimates of Time from Exit to Homelessness",
                              break.time.by = 50,
                              xlim = c(0,max(tth_data$tt_homeless)),
                              ylim = c(0.7,1),
                              break.y.by= 0.05,
                              conf.int = TRUE,
                              legend.title = "Exit Type and PHA Strata",
                              legend.labs = c("Negative KCHA","Negative SHA",
                                            "Neutral KCHA", "Neutral SHA",
                                            "Positive KCHA", "Positive SHA"),
                              censor.size = 4,
                              gg_theme = theme_bw())

# plot object
png(file = paste0(output_path, "KM_curve_pha.png"), width = 600, height = 450)
km_exit_type_pha
dev.off()


## Other Descriptive Stats
# Non-parametric KM-estimated survival at time= 365 days (by exit type / PHA combination)
summary(fit_exit_type_pha, times = 365)


# 3) KM curves by exit type, faceted by KCHA and SHA (separate plots) ----
km_exit_type_pha_facet <- ggsurvplot(fit_exit_type, data = tth_data,
                                    facet.by = "agency",
                                    short.panel.labs = TRUE,
                                    xlab = "Time from Public Housing Exit (Days)",
                                    ylab = "Probability Remaining Housed",
                                    title = "Kaplan-Meier Estimates of Time from Exit to Homelessness",
                                    break.time.by = 50,
                                    xlim = c(0,max(tth_data$tt_homeless)),
                                    ylim = c(0.7,1),
                                    break.y.by= 0.05,
                                    conf.int = TRUE,
                                    legend.title = "Exit Type",
                                    legend.labs = na.omit(unique(tth_data$exit_category)),
                                    censor.size = 4,
                                    gg_theme = theme_bw())

png(file = paste0(output_path, "KM_curve_pha_facet.png"), width = 600, height = 450)
km_exit_type_pha_facet
dev.off()

