# Header ----
# Author: Danny Colombara
# Date: June 6, 2023
# R version 4.2.2
# Purpose: graph distribution of raw data 
#
#

# Set up ----
    rm(list=ls())
    options(scipen = 999)
    pacman::p_load(lubridate, rads, data.table, DBI, odbc, ggplot2, lme4, margins, Microsoft365R)
    # library(lmerTest)  # commented out because want to be sure lmer function is called from lme4 by default

    # output folder
      site <- get_team("DPH Health And Housing")
      drv <- site$get_drive("Documents")
      outputdir <- "HUD HEARS Study/wage_analysis/output/hpd_revision_1/"
    
    # easy SQL connections
    devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/apde/main/R/create_db_connection.R") 
    
# Create functions ----
    # std.error() ... standard error function from https://github.com/plotrix/plotrix/blob/master/R/std.error.R ----
      std.error<-function(x,na.rm) {
      vn<-function(x) return(sum(!is.na(x)))
      dimx<-dim(x)
      if(is.null(dimx)) {
        stderr<-sd(x,na.rm=TRUE)
        vnx<-vn(x)
      }
      else {
        if(is.data.frame(x)) {
          vnx<-unlist(sapply(x,vn))
          stderr<-unlist(sapply(x,sd,na.rm=TRUE))
        }
        else {
          vnx<-unlist(apply(x,2,vn))
          stderr<-unlist(apply(x,2,sd,na.rm=TRUE))
        }
      }
      return(stderr/sqrt(vnx))  
    }
    
    # saveplots() ... create function to save plots with proper dimensions ----
      saveplots <- function(plot.object = NULL, plot.name = NULL){
        tempy <- tempfile(fileext = ".pdf")
        ggsave(filename = tempy,
               plot = plot.object, 
               dpi=600, 
               width = 11, 
               height = 8.5, 
               units = "in") 
        drv$upload_file(src = tempy, 
                        dest = paste0(outputdir, plot.name, ".pdf"))          
        
        tempy <- tempfile(fileext = ".tiff")
        ggsave(filename = tempy,
               plot = plot.object, 
               dpi=1200, 
               width = 6, 
               height = 4, 
               units = "in", 
               compression = "lzw") 
        drv$upload_file(src = tempy, 
                        dest = paste0(outputdir, plot.name, ".tiff")) 
      }

# Load data ----
    # open connection 
    hhsaw16 = create_db_connection( # this is prod
      server = "hhsaw", 
      prod = T, 
      interactive = F
    )
    
    # pull from SQL
    raw <- setDT(DBI::dbGetQuery(conn = hhsaw16, "SELECT * FROM [hudhears].[wage_analytic_table]"))
    
# Preparatory data manipulation ----
    # set up exit flag(s) ----
    raw[, exit_category := factor(exit_category, levels = c("Positive", "Neutral", "Negative"))] # to force specific order in graph  
    raw[, exit := as.integer(exit)] # convert logical 0|1 to binary integer
    
    # for plots of positive, neutral, & negative at 1 year prior, at exit, and 1 year post ----
    dt1 <- rbind(copy(raw), copy(raw)[, prog_type_use := "All Programs"])[!is.na(prog_type_use)]
        dt1[, prog_type_use := factor(prog_type_use, 
                                      levels = c("All Programs", "TBV", "PBV", "PH"), 
                                      labels = c("All Programs", "Tenant-Based Vouchers", "Project-Based Vouchers", "Public Housing"))]
        dt1 <- dt1[qtr %in% c(-4, 0, 4)]
        dt1[qtr == -4, time := -1]
        dt1[qtr == 0, time := 0]
        dt1[qtr == 4, time := 1]
        dt1[, time2 := factor(time, levels = c(-1, 0, 1), labels = c("1 year prior", "Exit", "1 year post"))]
        
        dt1[, se := std.error(wage), .(qtr, exit_category, prog_type_use)]
        dt1[, mean := mean(wage), .(qtr, exit_category, prog_type_use)]
        dt1[, upper := mean + (qnorm(.975) * se)]
        dt1[, lower := mean - (qnorm(.975) * se)]
        
        dt1[!is.na(wage_hourly) & !is.nan(wage_hourly) & !is.infinite(wage_hourly), se.hourly := std.error(wage_hourly), .(qtr, exit_category, prog_type_use)]
        dt1[!is.na(wage_hourly) & !is.nan(wage_hourly) & !is.infinite(wage_hourly), mean.hourly := mean(wage_hourly), .(qtr, exit_category, prog_type_use)]
        dt1[, upper.hourly := mean.hourly + (qnorm(.975) * se.hourly)]
        dt1[, lower.hourly := mean.hourly - (qnorm(.975) * se.hourly)]
        
        dt1 <- dt1[, .(time, exit_category, wage, mean, se, upper, lower, wage_hourly, mean.hourly, se.hourly, upper.hourly, lower.hourly, prog_type_use)]
        
        dt1.stats <- unique(dt1[!is.na(time), .(time, exit_category, mean, se, upper, lower, wage_hourly, mean.hourly, se.hourly, upper.hourly, lower.hourly, prog_type_use)])
        dt1.stats[exit_category == "Negative", time := time + 0.215] # adding an offset for visualization purposes
        dt1.stats[exit_category == "Positive", time := time - 0.215] # adding an offset for visualization purposes
        
# Create plots ----        
  # Plot quarterly wages pre-post ----
      #plot.new()      
      set.seed(98104) # because jitter is 'random'
      plot1 <- ggplot() +
        geom_point(data = dt1[!is.na(time)] ,  aes(x = time, y = wage, color = exit_category, shape = exit_category), 
                   position=position_jitterdodge(dodge.width=0.65, jitter.height=0, jitter.width=0.15), alpha=0.7) +
        geom_point(data = dt1.stats[exit_category == 'Positive'],  
                   aes(x = time, y = mean), 
                   size = 1) +
        geom_errorbar(data = dt1.stats[exit_category == 'Positive'],  
                      stat = 'identity', 
                      aes(x = time, ymax = upper, ymin = lower), 
                      linewidth = 0.5, 
                      width = 0) +      
        geom_point(data = dt1.stats[exit_category == 'Neutral'],  
                   aes(x = time, y = mean), 
                   size = 1) +
        geom_errorbar(data = dt1.stats[exit_category == 'Neutral'],  
                      stat = 'identity', aes(x = time, ymax = upper, ymin = lower), 
                      linewidth = 0.5, 
                      width = 0) +
        geom_point(data = dt1.stats[exit_category == 'Negative'],  
                   aes(x = time, y = mean), 
                   size = 1) +
        geom_errorbar(data = dt1.stats[exit_category == 'Negative'],  
                      stat = 'identity', aes(x = time, ymax = upper, ymin = lower), 
                      linewidth = 0.5, 
                      width = 0) +
        labs(x = "", 
             y = "", 
             caption = "The black points and error bars are the mean and 95% confidence interval, respectively.") +
        scale_shape_manual("Exit Type", 
                           values = c('Positive' = 15,
                                      'Neutral' = 17, 
                                      'Negative' = 16)) +
        scale_color_manual("Exit Type",
                           values=c('Positive' = '#2c7fb8',
                                    'Neutral' = '#2ca25f',
                                    'Negative' = '#E60000')) +
        scale_y_continuous(labels=scales::dollar_format())+
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-1, 0, 1)) +
        theme(panel.grid.major = element_line(color = "white"), 
              panel.background = element_rect(fill = "white"), 
              panel.border = element_rect(colour = "black", fill=NA, linewidth = .5),  
              plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(size=8),
              legend.position = "right",
              legend.background = element_rect(fill="white", linewidth =0.5, linetype="solid", color ="white"), 
              legend.title = element_text(size = 12), 
              legend.key = element_rect(fill = "white", color = "white"),
              legend.text = element_text(size = 10)) +
        facet_wrap(~prog_type_use, nrow = 2, strip.position = "top") 

      # dev.new(width = 11,  height = 8.5, unit = "in", noRStudioGD = TRUE)
      plot(plot1)
      
# Save plots ----      
  # Save quarterly wages pre-post ----    
      saveplots(plot.object = plot1, plot.name = 'figure_1_quarterly_wages_by_exit_type')
      
# the end ----