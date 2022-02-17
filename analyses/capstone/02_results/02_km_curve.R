## Script name: km_curve.R
##
## Purpose of script: Create Kaplan-Meier curves of time to homelessness by exit type for Capstone Hudhears project
##
## Author: Taylor Keating
## Date Created: 1/12/2022
## Email:n-tkeating@kingcounty.gov
##
## Notes: Also produces estimates for probability remaining housed at 365 days after exit (by exit type)
##   
##

# SET OPTIONS AND BRING IN PACKAGES ----
options(scipen = 6, digits = 4, warning.length = 8170)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

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
tth_data<- setDT(DBI::dbGetQuery(conn = db_hhsaw, "SELECT * FROM [hudhears].[capstone_data_2]"))


#-------------------------------------
# Create "survival" object column in data
tth_data<- tth_data %>% mutate(surv_object= Surv(time=tt_homeless, event=event))

# Create survival curve by exit type
fit_exit_type<- survfit(surv_object ~ exit_category, data=tth_data, conf.type="log-log")
#-------------------------------------


#-------------------------------------
## Plotting
# Create plot of Kaplan-Meier survival curve by exit type
km_exit_type<- ggsurvplot(fit_exit_type, data=tth_data,
                            xlab="Time from Public Housing Exit (Days)",
                            ylab="Probability Remaining Housed",
                            title="Kaplan-Meier Estimates of Time from Exit to Homelessness",
                            break.time.by=50,
                            xlim=c(0,max(tth_data$tt_homeless)),
                            ylim=c(0.7,1),
                            conf.int=TRUE,
                            legend.title="Exit Type",
                            legend.labs= na.omit(unique(tth_data$exit_category)),
                            risk.table=TRUE, # risk table added
                            cumevents=TRUE, # cumulative events table added
                            tables.col="strata",
                            #tables.height=0.3,
                            censor.size=4,
                            gg_theme=theme_bw())
# remove x-lab from risk table
km_exit_type$table<- km_exit_type$table +
  labs(x="")
km_exit_type$cumevents<- km_exit_type$cumevents + 
  labs(x="")


#png(file="KM_curve_plot.png", width=600, height=800)
km_exit_type
#dev.off()
#-------------------------------------


#-------------------------------------
## Other Descriptive Stats
# Non-parametric KM-estimated survival at time= 365 days
summary(fit_exit_type, times=365)


