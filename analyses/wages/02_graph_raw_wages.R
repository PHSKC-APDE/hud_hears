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
    
    # standard error function from https://github.com/plotrix/plotrix/blob/master/R/std.error.R
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
    
# Create function to save plots with proper dimensions ----
    saveplots <- function(plot.object = NULL, plot.name = NULL){
      ggsave(paste0(outputdir, plot.name, ".pdf"),
             plot = plot.object, 
             dpi=600, 
             width = 6.5, 
             height = 6.5, 
             units = "in") 
      ggsave(paste0(outputdir, plot.name, ".png"),
             plot = plot.object, 
             dpi=600, 
             width = 6.5, 
             height = 6.5, 
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
    raw[, quarter := as.factor(quarter(exit_date))]
    
    raw[qtr == -4, time := -1]
    raw[qtr == 0, time := 0]
    raw[qtr == 4, time := 1]
    raw[, time2 := factor(time, levels = seq(-1, 1, 1), labels = c("1 year prior", "Exit", "1 year post"))]
    
    raw[exit_category == "Negative", exit := 0]
    raw[exit_category == "Positive", exit := 1]
    
    raw[, se := std.error(wage), .(qtr, exit_category)]
    raw[, mean := mean(wage), .(qtr, exit_category)]
    raw[, upper := mean + (qnorm(.975) * se)]
    raw[, lower := mean - (qnorm(.975) * se)]
    
    raw <- raw[, .(time, exit_category, wage, mean, se, upper, lower)]
    
    raw.stats <- unique(raw[!is.na(time), .(time, exit_category, mean, se, upper, lower)])
    raw.stats[exit_category == "Negative", time := time - 0.17]
    raw.stats[exit_category == "Positive", time := time + 0.17]
    
    raw[, exit_category := factor(exit_category, levels = c("Positive", "Negative"))] # to force specific order in graph  
    
# Plot ----
    plot.new()      
    
    set.seed(98104) # because jitter is 'random'
    plot1 <- ggplot() +
      geom_point(data = raw[!is.na(time)],  aes(x = time, y = wage, color = exit_category), 
                 position=position_jitterdodge(dodge.width=0.65, jitter.height=0, jitter.width=0.15), alpha=0.7) +
      geom_point(data = raw.stats[exit_category == 'Positive'],  
                 aes(x = time, y = mean), 
                 size = 0.5) +
      geom_errorbar(data = raw.stats[exit_category == 'Positive'],  
                    stat = 'identity', 
                    aes(x = time, ymax = upper, ymin = lower), 
                    size = 0.4, 
                    width = .12) +      
      geom_point(data = raw.stats[exit_category == 'Negative'],  
                 aes(x = time, y = mean), 
                 size = 0.5) +
      geom_errorbar(data = raw.stats[exit_category == 'Negative'],  
                    stat = 'identity', aes(x = time, ymax = upper, ymin = lower), 
                    size = 0.4, 
                    width = .12) +
      labs(title = paste0("Raw quarterly wages"), 
           subtitle = "", 
           x = "", 
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
    
    saveplots(plot.object = plot1, plot.name = 'figure_0_raw_wages')
    

# the end ----