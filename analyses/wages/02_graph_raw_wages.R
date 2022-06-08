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
    raw[, exit_category := factor(exit_category, levels = c("Positive", "Negative"))] # to force specific order in graph  
    raw[exit_category == "Negative", exit := 0]
    raw[exit_category == "Positive", exit := 1]

    dt1 <- copy(raw) # for plots of positive and negative at 1 year prior, at exit, and 1 year post
        dt1[, quarter := as.factor(quarter(exit_date))]
        
        dt1[qtr == -4, time := -1]
        dt1[qtr == 0, time := 0]
        dt1[qtr == 4, time := 1]
        dt1[, time2 := factor(time, levels = seq(-1, 0, 1), labels = c("1 year prior", "Exit", "1 year post"))]
        
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
        dt1.stats[exit_category == "Negative", time := time - 0.17]
        dt1.stats[exit_category == "Positive", time := time + 0.17]
        
    
    dt2 <- copy(raw) # for viewing secular changes 
        dt2[, se := std.error(wage), .(exit_qtr, exit_category)]
        dt2[, mean := mean(wage), .(exit_qtr, exit_category)]
        dt2[, upper := mean + (qnorm(.975) * se)]
        dt2[, lower := mean - (qnorm(.975) * se)]
        
        dt2[!is.na(wage_hourly) & !is.nan(wage_hourly) & !is.infinite(wage_hourly), se.hourly := std.error(wage_hourly), .(exit_qtr, exit_category)]
        dt2[!is.na(wage_hourly) & !is.nan(wage_hourly) & !is.infinite(wage_hourly), mean.hourly := mean(wage_hourly), .(exit_qtr, exit_category)]
        dt2[, upper.hourly := mean.hourly + (qnorm(.975) * se.hourly)]
        dt2[, lower.hourly := mean.hourly - (qnorm(.975) * se.hourly)]
        
        dt2 <- dt2[, .(time = exit_qtr, exit_category, wage, mean, se, upper, lower, wage_hourly, mean.hourly, se.hourly, upper.hourly, lower.hourly)]
        
        dt2.stats <- unique(dt2[!is.na(time), .(time, exit_category, mean, se, upper, lower, wage_hourly, mean.hourly, se.hourly, upper.hourly, lower.hourly)])
        dt2.stats[exit_category == "Negative", time := time - 7]
        dt2.stats[exit_category == "Positive", time := time + 7]

# Create plots ----        
  # Plot quarterly wages----
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
        labs(title = paste0("quarterly wages"), 
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
      
      saveplots(plot.object = plot1, plot.name = 'figure_0_wages_quarterly')
      
  # Plot hourly wages----
      plot.new()      
      
      set.seed(98104) # because jitter is 'random'
      plot2 <- ggplot() +
        geom_point(data = dt1[!is.na(time)],  aes(x = time, y = wage_hourly, color = exit_category), 
                   position=position_jitterdodge(dodge.width=0.65, jitter.height=0, jitter.width=0.15), alpha=0.7) +
        geom_point(data = dt1.stats[exit_category == 'Positive'],  
                   aes(x = time, y = mean.hourly), 
                   size = 1) +
        geom_errorbar(data = dt1.stats[exit_category == 'Positive'],  
                      stat = 'identity', 
                      aes(x = time, ymax = upper.hourly, ymin = lower.hourly), 
                      size = .5, 
                      width = .03) +      
        geom_point(data = dt1.stats[exit_category == 'Negative'],  
                   aes(x = time, y = mean.hourly), 
                   size = 1) +
        geom_errorbar(data = dt1.stats[exit_category == 'Negative'],  
                      stat = 'identity', aes(x = time, ymax = upper.hourly, ymin = lower.hourly), 
                      size = .5, 
                      width = .03) +
        labs(title = paste0("hourly wages"), 
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
      
      plot(plot2)
      
      saveplots(plot.object = plot2, plot.name = 'figure_0_wages_hourly')

  # Plot quarterly wages (calendar time) ----
      plot.new()      
      
      set.seed(98104) # because jitter is 'random'
      plot3 <- ggplot() +
        geom_point(data = dt2[!is.na(time)],  aes(x = time, y = wage, color = exit_category), 
                   position=position_jitterdodge(dodge.width=25, jitter.height=0, jitter.width=10), alpha=0.7) +
        geom_point(data = dt2.stats[exit_category == 'Positive'],  
                   aes(x = time, y = mean), 
                   size = 1.5) +
        geom_errorbar(data = dt2.stats[exit_category == 'Positive'],  
                      stat = 'identity', 
                      aes(x = time, ymax = upper, ymin = lower), 
                      size = .75, 
                      width = .25) +      
        geom_point(data = dt2.stats[exit_category == 'Negative'],  
                   aes(x = time, y = mean), 
                   size = 1.5) +
        geom_errorbar(data = dt2.stats[exit_category == 'Negative'],  
                      stat = 'identity', aes(x = time, ymax = upper, ymin = lower), 
                      size = .75, 
                      width = .25) +
        labs(title = paste0("quarterly wages"), 
             subtitle = "calendar time", 
             x = "", 
             y = "", 
             caption = "The black points and error bars are the mean and 95% confidence interval, respectively.") +
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 'Negative' = '#2ca25f')) +
        scale_y_continuous(labels=scales::dollar_format())+
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
      
      plot(plot3)
      
      saveplots(plot.object = plot3, plot.name = 'figure_0_wages_quarterly_calendar')
      
  # Plot hourly wages (calendar time) ----
      plot.new()      
      
      set.seed(98104) # because jitter is 'random'
      plot4 <- ggplot() +
        geom_point(data = dt2[!is.na(time)],  aes(x = time, y = wage_hourly, color = exit_category), 
                   position=position_jitterdodge(dodge.width=25, jitter.height=0, jitter.width=10), alpha=0.7) +
        geom_point(data = dt2.stats[exit_category == 'Positive'],  
                   aes(x = time, y = mean.hourly), 
                   size = 1.5) +
        geom_errorbar(data = dt2.stats[exit_category == 'Positive'],  
                      stat = 'identity', 
                      aes(x = time, ymax = upper.hourly, ymin = lower.hourly), 
                      size = .75, 
                      width = .25) +      
        geom_point(data = dt2.stats[exit_category == 'Negative'],  
                   aes(x = time, y = mean.hourly), 
                   size = 1.5) +
        geom_errorbar(data = dt2.stats[exit_category == 'Negative'],  
                      stat = 'identity', aes(x = time, ymax = upper.hourly, ymin = lower.hourly), 
                      size = .75, 
                      width = .25) +
        labs(title = paste0("hourly wages"), 
             subtitle = "calendar time", 
             x = "", 
             y = "", 
             caption = "The black points and error bars are the mean and 95% confidence interval, respectively.") +
        scale_color_manual("Exit type", 
                           values=c('Positive' = '#2c7fb8', 'Negative' = '#2ca25f')) +
        scale_y_continuous(labels=scales::dollar_format())+
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
      
      plot(plot4)
      
      saveplots(plot.object = plot4, plot.name = 'figure_0_wages_hourly_calendar')
      
# Summary tables ----
  # Quarterly wage difference by Pre/Exit/Post ----
    pep.diff.qtr <- merge(dt1[!is.na(time) & exit_category == "Positive", .(pos.mean = mean(wage), pos.sd = sd(wage)), time], 
                          dt1[!is.na(time) & exit_category == "Negative", .(neg.mean = mean(wage), neg.sd = sd(wage)), time], 
                          by = 'time', 
                          all = T)
    pep.diff.qtr[, difference := pos.mean - neg.mean]
    pep.diff.qtr[, difference.sd := sqrt((pos.sd^2) + (neg.sd^2))]
    pep.diff.qtr[time == -1, t_test_pvalue := t.test(x = dt1[!is.na(time) & exit_category == "Positive" & time == -1,]$wage, 
                                                     y = dt1[!is.na(time) & exit_category == "Negative" & time == -1,]$wage)$p.value]
    pep.diff.qtr[time == 0, t_test_pvalue := t.test(x = dt1[!is.na(time) & exit_category == "Positive" & time == 0,]$wage, 
                                                    y = dt1[!is.na(time) & exit_category == "Negative" & time == 0,]$wage)$p.value]
    pep.diff.qtr[time == 1, t_test_pvalue := t.test(x = dt1[!is.na(time) & exit_category == "Positive" & time == 1,]$wage, 
                                                    y = dt1[!is.na(time) & exit_category == "Negative" & time == 1,]$wage)$p.value]
    pep.diff.qtr[t_test_pvalue < 0.05, significant := "*"]
    pep.diff.qtr <- pep.diff.qtr[, .(wage = 'quarterly', 
                                     time = factor(time, levels = c(-1, 0, 1), labels = c("1 year prior", "Exit", "1 year post")), 
                                     positive = paste0(round2(pos.mean), " (", round2(pos.sd), ")"), 
                                     negative = paste0(round2(neg.mean), " (", round2(neg.sd), ")"),
                                     difference = paste0(round2(difference), " (", round2(difference.sd), ")"),
                                     significant)]
    
  # Hourly wage difference by Pre/Exit/Post ----
    pep.diff.hrs <- merge(dt1[!is.na(time) & exit_category == "Positive" & !is.na(wage_hourly), .(pos.mean = mean(wage_hourly), pos.sd = sd(wage_hourly)), time], 
                          dt1[!is.na(time) & exit_category == "Negative"& !is.na(wage_hourly), .(neg.mean = mean(wage_hourly), neg.sd = sd(wage_hourly)), time], 
                          by = 'time', 
                          all = T)
    pep.diff.hrs[, difference := pos.mean - neg.mean]
    pep.diff.hrs[, difference.sd := sqrt((pos.sd^2) + (neg.sd^2))]
    pep.diff.hrs[time == -1, t_test_pvalue := t.test(x = dt1[!is.na(time) & exit_category == "Positive" & time == -1,]$wage_hourly, 
                                                     y = dt1[!is.na(time) & exit_category == "Negative" & time == -1,]$wage_hourly)$p.value]
    pep.diff.hrs[time == 0, t_test_pvalue := t.test(x = dt1[!is.na(time) & exit_category == "Positive" & time == 0,]$wage_hourly, 
                                                    y = dt1[!is.na(time) & exit_category == "Negative" & time == 0,]$wage_hourly)$p.value]
    pep.diff.hrs[time == 1, t_test_pvalue := t.test(x = dt1[!is.na(time) & exit_category == "Positive" & time == 1,]$wage_hourly, 
                                                    y = dt1[!is.na(time) & exit_category == "Negative" & time == 1,]$wage_hourly)$p.value]
    pep.diff.hrs[t_test_pvalue < 0.05, significant := "*"]
    pep.diff.hrs <- pep.diff.hrs[, .(wage = 'hourly', 
                                     time = factor(time, levels = c(-1, 0, 1), labels = c("1 year prior", "Exit", "1 year post")), 
                                     positive = paste0(round2(pos.mean), " (", round2(pos.sd), ")"), 
                                     negative = paste0(round2(neg.mean), " (", round2(neg.sd), ")"),
                                     difference = paste0(round2(difference), " (", round2(difference.sd), ")"),
                                     significant)]
    
  # Quarterly wage difference by calendar dates ----
    dt.diff.qtr <- merge(dt2[!is.na(time) & exit_category == "Positive", .(pos.mean = mean(wage), pos.sd = sd(wage)), time], 
                         dt2[!is.na(time) & exit_category == "Negative", .(neg.mean = mean(wage), neg.sd = sd(wage)), time], 
                         by = 'time', 
                         all = T)
    dt.diff.qtr[, difference := pos.mean - neg.mean]
    dt.diff.qtr[, difference.sd := sqrt((pos.sd^2) + (neg.sd^2))]
    for(mytime in unique(dt.diff.qtr$time)){
      dt.diff.qtr[time == mytime, t_test_pvalue := t.test(x = dt2[!is.na(time) & exit_category == "Positive" & time == mytime,]$wage, 
                                                          y = dt2[!is.na(time) & exit_category == "Negative" & time == mytime,]$wage)$p.value]
    }
    dt.diff.qtr[t_test_pvalue < 0.05, significant := "*"]
    dt.diff.qtr <- dt.diff.qtr[, .(wage = 'quarterly', 
                                   time = factor(time, labels = sort(unique(dt.diff.qtr$time))), 
                                   positive = paste0(round2(pos.mean), " (", round2(pos.sd), ")"), 
                                   negative = paste0(round2(neg.mean), " (", round2(neg.sd), ")"),
                                   difference = paste0(round2(difference), " (", round2(difference.sd), ")"),
                                   significant)]
    
  # Hourly wage difference by calendar dates ----
    dt.diff.hrs <- merge(dt2[!is.na(time) & exit_category == "Positive" & !is.na(wage_hourly), .(pos.mean = mean(wage_hourly), pos.sd = sd(wage_hourly)), time], 
                         dt2[!is.na(time) & exit_category == "Negative" & !is.na(wage_hourly), .(neg.mean = mean(wage_hourly), neg.sd = sd(wage_hourly)), time], 
                         by = 'time', 
                         all = T)
    dt.diff.hrs[, difference := pos.mean - neg.mean]
    dt.diff.hrs[, difference.sd := sqrt((pos.sd^2) + (neg.sd^2))]
    for(mytime in unique(dt.diff.hrs$time)){
      dt.diff.hrs[time == mytime, t_test_pvalue := t.test(x = dt2[!is.na(time) & exit_category == "Positive" & time == mytime,]$wage_hourly, 
                                                          y = dt2[!is.na(time) & exit_category == "Negative" & time == mytime,]$wage_hourly)$p.value]
    }
    dt.diff.hrs[t_test_pvalue < 0.05, significant := "*"]
    dt.diff.hrs <- dt.diff.hrs[, .(wage = 'hourly', 
                                   time = factor(time, labels = sort(unique(dt.diff.hrs$time))), 
                                   positive = paste0(round2(pos.mean), " (", round2(pos.sd), ")"), 
                                   negative = paste0(round2(neg.mean), " (", round2(neg.sd), ")"),
                                   difference = paste0(round2(difference), " (", round2(difference.sd), ")"),
                                   significant)]
  # Combine tables of wage differences by time ----
    wage.differences <- rbind(pep.diff.qtr, 
                              pep.diff.hrs, 
                              dt.diff.qtr, 
                              dt.diff.hrs)
    
    
# the end ----