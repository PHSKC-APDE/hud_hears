#### Header ----
# Author: Danny Colombara
# Date: June 27, 2023
# R version 4.2.2
# Purpose: Model the relationship between exit type and wages. 
#          Original submission used covariate adjustment, after confirming the 
#          covariates were associated with the exposure and outcome.
#          Reviewers did not like this data driven process. In addition, a 
#          different reviewer said originally considering a DiD model was not 
#          the right way to go. She/he suggested a Heckman selection/correction 
#          two stage model. I spent a day (literally!) reading about the Heckman
#          selection model and didn't see any advantage over something using 
#          inverse probability treatment weights to address the issue of non-
#          randomization into the different exit type. So, I pursude IPTW with
#          as the first stage, providing weights for my hierarchical / RE model. 
# 
# Notes: Need to account for repeated measures on individuals and clustering by 
#        by households. Will address both issues with inclusion of random effects
#
#        Prefer to use percent area median income (AMI) rather than the exact
#        wage when possible because % AMI accounts for household size and is 
#        also the criteria for entrance into public housing. 
#
# Notes on regression: 
#        * use lmerTest rather than lme4 because gives p.values but estimates and SE are identical 
#        * MAY use Nelder_Mead optimizer if default optimizer (nloptwrap) failed to converge 
#
# Notes on predictions:
#        * Used marginaleffects::predictions because provides estimates + SE & CI
#        * margins::prediction & prediction::prediction do not provide SE & CI
#        * stats::predict (predict.merMod) doesn't provide SE & CI & need to specify 
#          all fixed effects

#### Set up ----
    rm(list=ls())
    options(scipen = 999)
    pacman::p_load(lubridate, rads, data.table, DBI, 
                   odbc, ggplot2, openxlsx, Microsoft365R,
                   lme4, # for hierarchical models
                   multgee, # for multinomial logistic regression
                   marginaleffects) # for predictions
    
    # SharePoint connections and output folder
    site <- get_team("DPH Health And Housing")
    drv <- site$get_drive("Documents")
    outputdir <- "HUD HEARS Study/wage_analysis/output/hpd_revision_1/"
    
    # create an empty Excel workbook to fill with tables as they are built
    wb <- createWorkbook()
    
#### Custom functions ----
  # model.clean() >> clean model estimates ----
      model.clean <- function(mymod, myformat = NA){
        # basic tidying ----
        mymod.tidy <- as.data.table(broom.mixed::tidy(mymod, conf.int = T))
        roundvars <- c("estimate", "conf.low", "conf.high", "std.error")
        mymod.tidy[, (roundvars) := rads::round2(.SD, 0), .SDcols = roundvars]
        mymod.tidy[, p.value := as.character(rads::round2(p.value, 3))]
        mymod.tidy[as.numeric(p.value) < 0.001, p.value := "<0.001"]
        if(!is.na(myformat) && myformat == 'dollar'){
          mymod.tidy <- mymod.tidy[, .(effect, 
                                       group, 
                                       term, 
                                       estimate = paste0("$",
                                                         prettyNum(estimate, big.mark = ','),
                                                         " ($", 
                                                         prettyNum(conf.low, big.mark = ','), 
                                                         ", $", 
                                                         prettyNum(conf.high, big.mark = ','), 
                                                         ")") , 
                                       'p-value' = p.value
                                       # , SE = std.error
          )]
        }
        if(!is.na(myformat) && myformat == 'percent'){
          mymod.tidy <- mymod.tidy[, .(effect, 
                                       group, 
                                       term, 
                                       estimate = paste0(prettyNum(estimate, big.mark = ','),
                                                         "% (", 
                                                         prettyNum(conf.low, big.mark = ','), 
                                                         "%, ", 
                                                         prettyNum(conf.high, big.mark = ','), 
                                                         "%)") , 
                                       'p-value' = p.value)]
        }        
        mymod.tidy[, term := gsub("exit_category", "", term)]
        mymod.tidy[term == 'Positive', term := "Positive exit"]
        mymod.tidy[term == 'Neutral', term := "Neutral exit"]
        mymod.tidy[, term := gsub("as.integer\\(time\\)", "time", term)]
        mymod.tidy[grepl('spline', term), term := paste0(gsub("splines::bs\\(time, df = 3, Boundary.knots = c\\(-4, 4\\)\\)", "Time cubic spline (basis function ", term), ")")]
        mymod.tidy[, term := gsub("Neutral:", "Neutral exit * ", term)]
        mymod.tidy[, term := gsub("Positive:", "Positive exit * ", term)]
        mymod.tidy[, estimate := gsub("\\$-", "-\\$", estimate)]
        mymod.tidy[, estimate := gsub(" \\(\\$NA\\, \\$NA\\)", "", estimate)]
        mymod.tidy[, estimate := gsub("\\$NaN", NA, estimate)]
        
        mymod.tidy[group == 'id_kc_pha', group := 'Person ID']
        mymod.tidy[group == 'hh_id_kc_pha', group := 'Household ID']
        
        # add referent for exit_category ----
        split.row <- which(mymod.tidy[,3] == "Neutral exit")
        if(length(split.row) != 0){  # need condition, because IPTW model doesn't have Exit Year as direct adjustment var
          mymod.tidy <- rbind(
            mymod.tidy[1:(split.row-1)], 
            data.table(effect = 'fixed', term = "Negative exit", estimate = "Referent", `p-value` = NA_character_), 
            mymod.tidy[(split.row):nrow(mymod.tidy)], 
            fill = TRUE
          )
        }
        
        # add referent for program ----
        split.row <- which(mymod.tidy[,3] == "prog_type_usePBV")
        if(length(split.row) != 0){
          mymod.tidy <- rbind(
            mymod.tidy[1:(split.row-1)], 
            data.table(effect = 'fixed', term = "Program: Tenant-Based Voucher", estimate = "Referent", `p-value` = NA_character_), 
            mymod.tidy[(split.row):nrow(mymod.tidy)], 
            fill = TRUE
          )
          mymod.tidy[term == 'prog_type_usePBV', term := "Program: Project-Based Voucher"]
          mymod.tidy[term == 'prog_type_usePH', term := "Program: Public Housing"]
        }
        
        setnames(mymod.tidy, "estimate", "Estimate (95% CI)")
      }
  
  # prediction_summary() >> calculate mean values of predictions for my dataset using random draws ----
    prediction_summary <- function(predDT, ndraw = 1000){
      myest <- suppressWarnings(copy(predDT)[, c("lower", "upper") := NULL])
      
      # create summary table of each combination of time and exit_category
      mysets <- unique(myest[, .(time, exit_category, prog_type_use, wage = NA_real_, se = NA_real_)])
      
      # loop over each unique combination of time, exit_category, & prog_type_use to calculate the mean and se of predictions
      for(i in 1:nrow(mysets)){
        # create subset of the data for the unique time * exit_category
        mysubset <- myest[time == mysets[i, ][['time']] & exit_category == mysets[i, ][['exit_category']] & prog_type_use == mysets[i, ][['prog_type_use']] ]
        
        # take draws from prediction and standard error for each row of the subset
        set.seed(98104)
        mydraws <- c() # empty vector to store draws
        
        # create vector for All Programs, which will hold TBV, PBV, and PH
        mydraws.all <- paste0(mysets[i, ][['exit_category']], "_", mysets[i, ][['time']])
        if(!exists(mydraws.all)){assign(mydraws.all, vector(mode = "numeric", length = 0))}
        
        for(ii in 1:nrow(mysubset)){
          newdraws <- rnorm(ndraw, mysubset[ii]$estimate, mysubset[ii]$std.error)
          mydraws <- c(mydraws, newdraws) # add to the time / exit type / program specific vector
          assign(mydraws.all, c(get(mydraws.all), newdraws)) # add to the time / exit type specific vector
        }
        
        # calculate summary (mean and se) from the draws
        mysets[i, wage := mean(mydraws)]
        mysets[i, se := sd(mydraws)] # This standard error seems a bit large, but reasonable considering that it is summarizing all of the predictions, for different populations
        mysets[i, lower := wage - qnorm(0.975) * se]
        mysets[i, upper := wage + qnorm(0.975) * se]
      }
      
      # now loop to create the 'All Program' estimates, which are means of the draws of the program specific estimates
      mysets_allprog <- data.table()
      for(PROGy in c('Positive', 'Negative', 'Neutral')){
        for(TIMEy in seq(-4, 4, 1)){
          tempy_all <- data.table(time = TIMEy, exit_category = PROGy, prog_type_use = 'All Programs', 
                                  wage = mean(get(paste0(PROGy, "_", TIMEy))), 
                                  se = sd((get(paste0(PROGy, "_", TIMEy)))))
          tempy_all[, lower := wage - qnorm(0.975) * se]
          tempy_all[, upper := wage + qnorm(0.975) * se]
          mysets_allprog <- rbind(mysets_allprog, tempy_all)
        }
      }
      
      # Combine the program specific estimates and the All Programs estimates
      mysets <- rbind(mysets, mysets_allprog)
      
      mysets[, program := factor(prog_type_use, 
                                 levels = c('All Programs', 'TBV', 'PBV', 'PH'), 
                                 labels = c("All Programs", "Tenant-Based Vouchers", "Project-Based Vouchers", "Public Housing"))]
      
      # return object
      return(mysets)
    }
  
  # formatplots() >> standard ggplot formatting ----
      formatplots <- function(myplot){
        myplot <- myplot +
          scale_shape_manual("Exit Type", 
                             values = c('Positive' = 15,
                                        'Neutral' = 17, 
                                        'Negative' = 16)) +
          scale_color_manual("Exit Type",
                             values=c('Positive' = '#2c7fb8',
                                      'Neutral' = '#2ca25f',
                                      'Negative' = '#E60000')) +
          scale_y_continuous(labels=scales::dollar_format())+
          theme(panel.grid.major = element_line(color = "white"), 
                panel.background = element_rect(fill = "white"), 
                panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5),  
                plot.title = element_text(hjust = 0.5), 
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(size=6),
                legend.position = "right",
                legend.background = element_rect(fill="white", linewidth = 0.5, linetype="solid", color ="white"), 
                legend.title = element_text(size = 12), 
                legend.key = element_rect(fill = "white", color = "white"),
                legend.text = element_text(size = 10)) +
          guides(color = guide_legend(reverse=T), 
                 shape = guide_legend(reverse=T))
        
        return(myplot)
      }

  # saveplots() >> save plots to SharePoint as TIFF and PDF ----
      saveplots <- function(plot.object = NULL, plot.name = NULL){
        tempy.pdf <- tempfile(fileext = ".pdf")
        tempy.tiff <- tempfile(fileext = ".tiff")
        ggsave(filename = tempy.pdf,
               plot = plot.object, 
               dpi=600, 
               width = 11, 
               height = 8.5, 
               units = "in") 
        drv$upload_file(src = tempy.pdf, 
                        dest = paste0(outputdir, plot.name, ".pdf"))    
        
        ggsave(filename = tempy.tiff,
               plot = plot.object, 
               dpi=1200, 
               width = 6, 
               height = 4, 
               units = "in", 
               compression = "lzw")
        drv$upload_file(src = tempy.tiff, 
                        dest = paste0(outputdir, plot.name, ".tiff")) 
      }
      
    
#### Grab and prep data ----    
    # open connection 
    devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/apde/main/R/create_db_connection.R") 
    hhsaw16 = create_db_connection( # this is prod
      server = "hhsaw", 
      prod = T, 
      interactive = F
    )
    
    # pull from SQL
    raw <- setDT(DBI::dbGetQuery(conn = hhsaw16, "SELECT * FROM [hudhears].[wage_analytic_table]"))
    
    # prep data
    raw[, exit_year := factor(exit_year)]
    raw[, exit := as.integer(exit)]
    raw[, exit_category := factor(exit_category, levels = c('Negative', 'Neutral', 'Positive'))]
    raw[, quarter := as.factor(quarter(exit_date))]
    raw[, season := factor(quarter(exit_date), levels = 1:4, labels = c("Winter", "Spring", "Summer", "Fall"))]
    
    raw <- raw[qtr %in% -4:4] # limit to quarters of interest
    raw[, time := as.integer(qtr)]
    
    # set reference for factors
    raw[, race_eth_me := factor(race_eth_me)]
    raw$race_eth_me <- relevel(raw$race_eth_me, ref = 'Black')
    raw[, prog_type_use := factor(prog_type_use, levels = c("TBV", "PBV", "PH"))]

#### Identify covariates of interest ----
    covariates <- c('age_at_exit', 'gender_me', 'race_eth_me', 'exit_year', 'season', 'hh_gt_1_worker',
                    'housing_time_at_exit', 'hh_disability', 'single_caregiver', 'agency', 
                    'prog_type_use') # prog_type_use will not be in the propensity score, but will be used in wage model    
    
#### Two stage regression with IPTW: wages as outcome of interest ----

  # Step 0: Subset data to when all are present ----
      model.data <- copy(raw)
      model.data[, complete_covariates := 1]
      for(cv in covariates){
        model.data[is.na(get(cv)), complete_covariates := 0]
      }
      model.data <- model.data[complete_covariates == 1]
  
  # Step 1: Get propensity scores using multinomial logistic regression (using GEE with complete data using 'All Programs') ----
      ps_model <- nomLORgee(formula = exit_category ~ age_at_exit + gender_me + 
                              race_eth_me + exit_year + season + hh_gt_1_worker + housing_time_at_exit + 
                              hh_disability + single_caregiver + agency,
                          data = setDF(copy(model.data)[qtr == 0]), # just model at the time of exit
                          id = hh_id_kc_pha, # important because can have >1 person in a single household
                          LORstr = "independence")
  
      ps <- as.data.table(fitted(ps_model)) # get fitted value ... i.e., the probabilities
      colnames(ps) <- c('Negative', 'Neutral', 'Positive') # fitted value columns are in same order as the factor label for exit_category
      ps <- cbind("id_kc_pha" = model.data[qtr == 0]$id_kc_pha, ps)
      head(ps)
  
  # Step 2: Calculate the inverse probability treatment weight (IPTW) and assign it to each observation ----
      model.data <- merge(model.data, 
                          ps, 
                          by = 'id_kc_pha', 
                          all.x = T, 
                          all.y = T)
      model.data[, iptw := fcase(exit_category == 'Negative', 1/Negative, 
                                 exit_category == 'Neutral', 1/Neutral, 
                                 exit_category == 'Positive', 1/Positive)]
  
  # Step 3: Fit the mixed model, using the IPTW as a weight, and get predictions ----
      message('Using IPTW as a weight, vs as a covariate, is more robust to misspecification.')
      # The B-spline has zero knots because a 3rd degree polynomial (cubic spline) ... df - degrees = knots
      wage_model <- lmerTest::lmer(formula = wage ~ 
                                             exit_category*splines::bs(time, df = 3, Boundary.knots = c(-4, 4)) + 
                                             prog_type_use +
                                             (1 | id_kc_pha) + 
                                             (1 + exit | hh_id_kc_pha), 
                                   data = setDF(copy(model.data)), 
                                   weights = iptw
                                   )
      
      # test if p-value for interaction term is < 0.05
        wage_model.alt <- lmerTest::lmer(formula = wage ~ 
                                                   exit_category + splines::bs(time, df = 3, Boundary.knots = c(-4, 4)) + 
                                                   prog_type_use +
                                                   (1 | id_kc_pha) + 
                                                   (1 + exit | hh_id_kc_pha), 
                                         data = setDF(copy(model.data)), 
                                         weights = iptw)
        
        wage_model.test = anova(wage_model, wage_model.alt, test = 'LRT') # compare the two models
        if(wage_model.test[["Pr(>Chisq)"]][2] < 0.05){message("Keep interaction")}else{message('Drop interaction')}
      
      
      wage_model.tidy <- model.clean(wage_model, myformat = 'dollar')
      addWorksheet(wb, 'Table_2_regression_wages') 
      writeDataTable(wb, sheet = 'Table_2_regression_wages', wage_model.tidy, 
                     rowNames = F, colNames = T)   
      
      
      wage_model.preds <- as.data.table(marginaleffects::predictions(wage_model, 
                                                                    newdata = setDF(copy(model.data)), 
                                                                    re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      
      wage_model.preds_tidy <- prediction_summary(wage_model.preds, ndraw = 10000)

  # Step 4: plot ----
      setDT(model.data)
      wage_model.plot <- ggplot() +
        geom_line(data = wage_model.preds_tidy, aes(x = time, y = wage, color = exit_category), linewidth = 1) +
        geom_point(data = wage_model.preds_tidy, 
                   aes(x = time, y = wage, color = exit_category, shape = exit_category), 
                   size = 2.5) +
        labs(
          x = "", 
          y = "Predicted quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      wage_model.plot <- formatplots(wage_model.plot) + 
        scale_y_continuous(limits = c(4000, 9100), breaks=c(seq(4000, 9000, 1000)), labels=scales::dollar_format()) +
        facet_wrap(~program, nrow = 2, strip.position = "top") +
        theme(panel.spacing = unit(15, "pt"))
      
      # dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(wage_model.plot)

      # save the plot
      saveplots(plot.object = wage_model.plot, 
                plot.name = 'figure_2_mean_predicted_wages')
      
#### Two regression with IPTW: % AMI as outcome of interest----
  # Step 0: Subset data to when all are present ----
      model.data <- copy(raw)[!is.na(percent_ami) & !is.na(prog_type_use)]
      # model.data <- model.data[!id_kc_pha %in% unique(model.data[is.na(percent_ami)]$id_kc_pha)] # drop if do not have AMI for each time point
      
  # Step 1: Get propensity scores using multinomial logistic regression (using GEE with complete data using 'All Programs') ----
      # same model as when calculating wages above, because the true outcome of interest is irrelevant in the propensity score model
      head(ps)
      
  # Step 2: Calculate the inverse probability treatment weight (IPTW) and assign it to each observation ----
      model.data <- merge(model.data, 
                          ps, 
                          by = 'id_kc_pha', 
                          all.x = T, 
                          all.y = F)
      model.data[, iptw := fcase(exit_category == 'Negative', 1/Negative, 
                                 exit_category == 'Neutral', 1/Neutral, 
                                 exit_category == 'Positive', 1/Positive)]
      
  # Step 3: Fit the mixed model, using the IPTW as a weight, and get predictions ----
      ami_model <- lmerTest::lmer(formula = percent_ami ~ 
                                            exit_category*splines::bs(time, df = 3, Boundary.knots = c(-4, 4)) + 
                                            prog_type_use +
                                            (1 | id_kc_pha) + 
                                            (1 + exit | hh_id_kc_pha), 
                                  data = setDF(copy(model.data)), 
                                  weights = iptw
      )
      
      ami_model.tidy <- model.clean(ami_model, myformat = 'percent')
      addWorksheet(wb, 'Table_3_regression_AMI') 
      writeDataTable(wb, sheet = 'Table_3_regression_AMI', ami_model.tidy, 
                     rowNames = F, colNames = T)   
      
      ami_model.preds <- as.data.table(marginaleffects::predictions(ami_model, 
                                                                     newdata = setDF(copy(model.data)), 
                                                                     re.form=~0)) # re.form=~0 means include no random effects, so population level estimates
      
      ami_model.preds_tidy <- prediction_summary(ami_model.preds, ndraw = 10000)
      setnames(ami_model.preds_tidy, "wage", "percent_ami")
      
  # Step 4: plot ----
      setDT(model.data)
      ami_model.plot <- ggplot() +
        geom_line(data = ami_model.preds_tidy, aes(x = time, y = percent_ami, color = exit_category), linewidth = 1) +
        geom_point(data = ami_model.preds_tidy, 
                   aes(x = time, y = percent_ami, color = exit_category, shape = exit_category), 
                   size = 2.5) +
        labs(
          x = "", 
          y = "Predicted percentage Area Median Income (AMI)") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      ami_model.plot <- formatplots(ami_model.plot) + 
        scale_y_continuous(limits = c(15, 70), breaks = seq(20, 70, 10), labels=scales::label_percent(scale = 1)) +
        facet_wrap(~program, nrow = 2, strip.position = "top") +
        theme(panel.spacing = unit(15, "pt"))
      
      # dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(ami_model.plot)
      
      # save the plot
      saveplots(plot.object = ami_model.plot, 
                plot.name = 'figure_3_ mean_predicted_AMI')
      
#### Sensitivity analysis dropping those with disabilities: two stage regression with IPTW: wages as outcome of interest ----
      
  # Step 0: Subset data to when all are present ----
      model.data <- copy(raw)
      model.data <- model.data[!is.na(hh_disability) & hh_disability == 0]
      
  # Step 1: Get propensity scores using multinomial logistic regression (using GEE with complete data using 'All Programs') ----
      # same model as when calculating wages above, because the true outcome of interest is irrelevant in the propensity score model
      head(ps)
      
  # Step 2: Calculate the inverse probability treatment weight (IPTW) and assign it to each observation ----
      model.data <- merge(model.data, 
                          ps, 
                          by = 'id_kc_pha', 
                          all.x = T, 
                          all.y = F)
      model.data[, iptw := fcase(exit_category == 'Negative', 1/Negative, 
                                 exit_category == 'Neutral', 1/Neutral, 
                                 exit_category == 'Positive', 1/Positive)]
      
  # Step 3: Fit the mixed model, using the IPTW as a weight, and get predictions ----
      sensitivity_model <- lmerTest::lmer(formula = wage ~ 
                                                    exit_category*splines::bs(time, df = 3, Boundary.knots = c(-4, 4)) + 
                                                    prog_type_use +
                                                    (1 | id_kc_pha) + 
                                                    (1 + exit | hh_id_kc_pha), 
                                          data = setDF(copy(model.data)), 
                                          weights = iptw)
      

      sensitivity_model.tidy <- model.clean(sensitivity_model, myformat = 'dollar')
      addWorksheet(wb, 'Table_X_regression_notdisabled') 
      writeDataTable(wb, sheet = 'Table_X_regression_notdisabled', sensitivity_model.tidy, 
                     rowNames = F, colNames = T)   
      
      # The # of fitted values is less than the number of rows in the original data set, so, to be sure I can actually predict, I will use the dataset saved the regression model
        mynewdata <- as.data.table(sensitivity_model@frame) # get dataset from model
        setnames(mynewdata, names(mynewdata)[3:10], c('basis1', 'basis2', 'basis3', 'prog_type_use', 'id_kc_pha', 'exit', 'hh_id_kc_pha', 'iptw')) # change to data.table and label columns
        myspline = cbind(time = -4:4, as.data.table(splines::bs(-4:4, degree = 3, Boundary.knots = c(-4, 4)))) # get key of times and spline bases
        setnames(myspline, names(myspline)[2:4], c('basis1', 'basis2', 'basis3')) # standardize naming from spline function to match that in regression data
        mynewdata <- merge(mynewdata, myspline, by = c('basis1', 'basis2', 'basis3'), all = T) # merge on spline data to get the underlying time (-4:4)
        mynewdata[, c('basis1', 'basis2', 'basis3') := NULL] # drop spline bases
      
      sensitivity_model.preds <- as.data.table(marginaleffects::predictions(sensitivity_model, 
                                                                            newdata = mynewdata, # setDF(copy(model.data)), 
                                                                            re.form=~0)) # re.form=~0 means include no random effects, so population level estimates

      sensitivity_model.preds_tidy <- prediction_summary(sensitivity_model.preds, ndraw = 10000)
      
  # Step 4: plot ----
      setDT(model.data)
      sensitivity_model.plot <- ggplot() +
        geom_line(data = sensitivity_model.preds_tidy, aes(x = time, y = wage, color = exit_category), linewidth = 1) +
        geom_point(data = sensitivity_model.preds_tidy, 
                   aes(x = time, y = wage, color = exit_category, shape = exit_category), 
                   size = 2.5) +
        labs(
          x = "", 
          y = "Predicted quarterly wages") +
        scale_x_continuous(labels=c("1 year prior", "Exit", "1 year post"), breaks=c(-4, 0, 4))
      
      sensitivity_model.plot <- formatplots(sensitivity_model.plot) + 
        scale_y_continuous(limits = c(4000, 9100), breaks=c(seq(4000, 9000, 1000)), labels=scales::dollar_format()) +
        facet_wrap(~program, nrow = 2, strip.position = "top") +
        theme(panel.spacing = unit(15, "pt"))
      
      # dev.new(width = 6,  height = 4, unit = "in", noRStudioGD = TRUE)
      plot(sensitivity_model.plot)
      
      # save the plot
      saveplots(plot.object = sensitivity_model.plot, 
                plot.name = 'modeled_sensivity_analysis')
      
          
#### Export Regression output to Excel file in SharePoint ----
    tempy.xlsx <- tempfile(fileext = ".xlsx") # tempfile in memory to hold Excel file
    saveWorkbook(wb, file = tempy.xlsx, overwrite = T) # write to tempfile
    
    drv$upload_file(src = tempy.xlsx, 
                    dest = paste0(outputdir, "/Tables_regression.xlsx")) 
        
#### Compare the observed and expected & save as Excel file in SharePoint----
    observed <- raw[, .(Observed = rads::round2(mean(wage, na.rm = T), 0)), .(qtr, exit_category)]
    observed[, Quarter := as.character(qtr)]
    observed[Quarter == '0', Quarter := 'Exit']
    
    expected = wage_model.preds[, .(Predicted = rads::round2(mean(estimate), 0)), .(qtr, exit_category)]
    
    observed_expected <- merge(observed, expected, by = c('qtr', 'exit_category'), all = T)
    setorder(observed_expected, -exit_category, qtr)
    observed_expected <- observed_expected[, .(qtr, Quarter, 'Exit Type' = exit_category, Observed, Predicted)]
    observed_expected[, qtr := NULL]
    observed_expected[, 'Difference' := Observed - Predicted]
    observed_expected[, per := paste0(rads::round2(100*Difference / Observed, 1), "%")]
    observed_expected[, Observed := paste0("$", format(Observed, big.mark = ','))]
    observed_expected[, Predicted := paste0("$", format(Predicted, big.mark = ','))]
    observed_expected[, Difference := paste0("$", format(Difference, big.mark = ','), " (", per, ")")]
    observed_expected[, per := NULL]
    observed_expected[, Difference := gsub("$-", "-$", Difference)]
    observed_expected[!grepl("\\.", Difference), Difference := gsub("%", ".0%", Difference)]
    
    wb2 <- createWorkbook() # initiate a new / empty workbook
    addWorksheet(wb2, 'appdx_obs_vs_pred') 
    writeDataTable(wb2, sheet = 'appdx_obs_vs_pred', observed_expected, rowNames = F, colNames = T)    
    tempy <- tempfile(fileext = ".xlsx") # tempfile in memory to hold Excel file
    saveWorkbook(wb2, file = tempy, overwrite = T)
    drv$upload_file(src = tempy, 
                    dest = paste0(outputdir, "/appendix_table_1_obs_v_preds.xlsx")) 
    
# The end! ----



