# Header ----
# Author: Danny Colombara
# Date: June 1, 2022
# R version 4.1.2
# Purpose: graph distribution of raw data 
#
#

# Set up ----
    rm(list=ls())
    options(scipen = 999)
    pacman::p_load(lubridate, rads, data.table, DBI, odbc, ggplot2, lme4, margins)
    # library(lmerTest)  # commented out because want to be sure lmer function is called from lme4 by default

    # output folder
    outputdir <- "C:/Users/dcolombara/King County/DPH Health And Housing - Documents/HUD HEARS Study/wage_analysis/output/"
    
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
        ggsave(paste0(outputdir, '/pdf/', plot.name, ".pdf"),
               plot = plot.object, 
               dpi=600, 
               width = 6, 
               height = 4, 
               units = "in") 
        ggsave(paste0(outputdir, '/png/', plot.name, ".png"),
               plot = plot.object, 
               dpi=600, 
               width = 6, 
               height = 4, 
               units = "in") 
      }

# Load data ----
    # open connection 
    hhsaw16 = create_db_connection( # this is prod
      server = "hhsaw", 
      prod = T, 
      interactive = F
    )
    
    # pull from SQL
    raw <- setDT(DBI::dbGetQuery(conn = hhsaw16, 
                                 "SELECT * FROM [hudhears].[wage_analytic_table]"))
    
# Preparatory data manipulation ----
    # set up exit flag(s) ----
    raw[, exit_category := factor(exit_category, levels = c("Positive", "Negative"))] # to force specific order in graph  
    raw[, exit := as.integer(exit)] # convert logical 0|1 to binary integer
    
    # for plots of positive and negative at 1 year prior, at exit, and 1 year post ----
    dt1 <- copy(raw) 
        dt1 <- dt1[qtr %in% c(-4, 0, 4)]
        dt1[qtr == -4, time := -1]
        dt1[qtr == 0, time := 0]
        dt1[qtr == 4, time := 1]
        dt1[, time2 := factor(time, levels = c(-1, 0, 1), labels = c("1 year prior", "Exit", "1 year post"))]
        
        dt1[, se := std.error(wage), .(qtr, exit_category)]
        dt1[, mean := mean(wage), .(qtr, exit_category)]
        dt1[, upper := mean + (qnorm(.975) * se)]
        dt1[, lower := mean - (qnorm(.975) * se)]
        
        dt1[!is.na(wage_hourly) & !is.nan(wage_hourly) & !is.infinite(wage_hourly), se.hourly := std.error(wage_hourly), .(qtr, exit_category)]
        dt1[!is.na(wage_hourly) & !is.nan(wage_hourly) & !is.infinite(wage_hourly), mean.hourly := mean(wage_hourly), .(qtr, exit_category)]
        dt1[, upper.hourly := mean.hourly + (qnorm(.975) * se.hourly)]
        dt1[, lower.hourly := mean.hourly - (qnorm(.975) * se.hourly)]
        
        dt1 <- dt1[, .(time, exit_category, wage, mean, se, upper, lower, wage_hourly, mean.hourly, se.hourly, upper.hourly, lower.hourly)]
        
        dt1.stats <- unique(dt1[!is.na(time), .(time, exit_category, mean, se, upper, lower, wage_hourly, mean.hourly, se.hourly, upper.hourly, lower.hourly)])
        dt1.stats[exit_category == "Negative", time := time + 0.17] # adding an offset for visualization purposes
        dt1.stats[exit_category == "Positive", time := time - 0.17] # adding an offset for visualization purposes
        
    # for viewing secular changes ----
    dt2 <- copy(raw) 
        dt2[, se := std.error(wage), .(qtr_date)]
        dt2[, mean := mean(wage), .(qtr_date)]
        dt2[, upper := mean + (qnorm(.975) * se)]
        dt2[, lower := mean - (qnorm(.975) * se)]

        dt2[!is.na(wage_hourly) & !is.nan(wage_hourly) & !is.infinite(wage_hourly), se.hourly := std.error(wage_hourly), .(qtr_date)]
        dt2[!is.na(wage_hourly) & !is.nan(wage_hourly) & !is.infinite(wage_hourly), mean.hourly := mean(wage_hourly), .(qtr_date)]
        dt2[, upper.hourly := mean.hourly + (qnorm(.975) * se.hourly)]
        dt2[, lower.hourly := mean.hourly - (qnorm(.975) * se.hourly)]

        dt2 <- dt2[, .(time = qtr_date , wage, mean, se, upper, lower, wage_hourly, mean.hourly, se.hourly, upper.hourly, lower.hourly)]

        dt2.stats <- unique(dt2[!is.na(time), .(time, mean, se, upper, lower, wage_hourly, mean.hourly, se.hourly, upper.hourly, lower.hourly)])


# Create plots ----        
  # Plot quarterly wages pre-post ----
      plot.new()      
      
      set.seed(98104) # because jitter is 'random'
      plot1 <- ggplot() +
        geom_point(data = dt1[!is.na(time)],  aes(x = time, y = wage, color = exit_category), 
                   position=position_jitterdodge(dodge.width=0.65, jitter.height=0, jitter.width=0.15), alpha=0.7) +
        geom_point(data = dt1.stats[exit_category == 'Positive'],  
                   aes(x = time, y = mean), 
                   size = 1) +
        geom_errorbar(data = dt1.stats[exit_category == 'Positive'],  
                      stat = 'identity', 
                      aes(x = time, ymax = upper, ymin = lower), 
                      size = 0.5, 
                      width = .03) +      
        geom_point(data = dt1.stats[exit_category == 'Negative'],  
                   aes(x = time, y = mean), 
                   size = 1) +
        geom_errorbar(data = dt1.stats[exit_category == 'Negative'],  
                      stat = 'identity', aes(x = time, ymax = upper, ymin = lower), 
                      size = 0.5, 
                      width = .03) +
        labs(x = "", 
             y = "", 
             caption = "The black points and error bars are the mean and 95% confidence interval, respectively.") +
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 'Negative' = '#2ca25f')) +
        scale_y_continuous(labels=scales::dollar_format())+
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-1, 0, 1)) +
        theme(panel.grid.major = element_line(color = "white"), 
              panel.background = element_rect(fill = "white"), 
              panel.border = element_rect(colour = "black", fill=NA, size=.5),  
              plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(size=8),
              legend.position = "right",
              legend.background = element_rect(fill="white", size=0.5, linetype="solid", color ="white"), 
              legend.title = element_text(size = 12), 
              legend.key = element_rect(fill = "white", color = "white"),
              legend.text = element_text(size = 10))
      
      dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      
      plot(plot1)
      
      

  # Plot quarterly wages secular trend ----
      # to ensure that there are no bizarre jumps or dips in the data
      plot.new()      
      
      set.seed(98104) # because jitter is 'random'
      plot2 <- ggplot() +
        geom_point(data = dt2[!is.na(time)],  aes(x = time, y = wage, color = 'orange2'), 
                   position=position_jitterdodge(dodge.width=25, jitter.height=0, jitter.width=10), alpha=0.7) +
        geom_point(data = dt2.stats[],  
                   aes(x = time, y = mean), 
                   size = 1.5) +
        geom_errorbar(data = dt2.stats[],  
                      stat = 'identity', 
                      aes(x = time, ymax = upper, ymin = lower), 
                      size = .75, 
                      width = .25) +
        labs(x = "", 
             y = "", 
             caption = "The black points and error bars are the mean and 95% confidence interval, respectively.") +
        # scale_color_manual("Exit type", 
        #                    values=c('Positive' = '#2c7fb8', 'Negative' = '#2ca25f')) +
        scale_y_continuous(labels=scales::dollar_format())+
        theme(panel.grid.major = element_line(color = "white"), 
              panel.background = element_rect(fill = "white"), 
              panel.border = element_rect(colour = "black", fill=NA, size=.5),  
              plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(size=8),
              legend.position = "none",
              legend.background = element_rect(fill="white", size=0.5, linetype="solid", color ="white"), 
              legend.title = element_text(size = 12), 
              legend.key = element_rect(fill = "white", color = "white"),
              legend.text = element_text(size = 10))
      
      dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      
      plot(plot2)
      
# Save plots ----      
  # Save quarterly wages pre-post ----    
      saveplots(plot.object = plot1, plot.name = 'figure_1_quarterly_wages_by_exit_type')
      
  # Save quarterly wages secular trend ----
      saveplots(plot.object = plot2, plot.name = 'figure_0_secular_trend_wages_quarterly')
      
# the end ----