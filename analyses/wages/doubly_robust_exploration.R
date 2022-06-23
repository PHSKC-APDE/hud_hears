# targeted minimum loss-based estimation (TMLE) 
# Way to perform doubly-robust regression
# Set-up ----
    rm(list=ls())
    options(scipen = 999)
    pacman::p_load(rads, data.table, DBI, odbc)
    
    outputdir <- "C:/Users/dcolombara/King County/DPH Health And Housing - Documents/HUD HEARS Study/wage_analysis/output/"
    
    devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/apde/main/R/create_db_connection.R") 
    
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
    
    # get confounder vars
    load(paste0(outputdir, "confounders.Rdata"))
    confounders <- confounders$confounders
    if(length(confounders) > 1){ 
      confounders <- paste(confounders, collapse = " + ")
    }
    
    # get pscovariates
    load(paste0(outputdir, "pscovariates.Rdata"))
    pscovariates <- pscovariates$pscovariates
    if(length(pscovariates) > 1){ 
      pscovariates <- paste(pscovariates, collapse = " + ")
    }
  
# Create dt3 ----
    dt3 <- copy(raw)
    dt3[qtr == 4, time := 1]
    dt3[qtr == 0, time := 0]
    dt3[exit_category == "Negative", exit := 0]
    dt3[exit_category == "Positive", exit := 1]
    dt3 <- dt3[!is.na(time)]      

# Calc propensity score ----
    mod3.psformula <- paste0("exit ~ ", pscovariates) # no random effects bc only at time zero
    mod3.ps <- glm(mod3.psformula, 
                   family = binomial(link=logit),
                   data = dt3[qtr==0])
    mod3.ps.prob <- data.table(hh_id_kc_pha = dt3[qtr==0]$hh_id_kc_pha, 
                               id_kc_pha = dt3[qtr==0]$id_kc_pha, 
                               exit = dt3[qtr==0]$exit, 
                               prob = predict(mod3.ps, type = 'response')) # predicted probability
    mod3.ps.prob[exit == 1, ipw := 1 / prob] # weight for IPW
    mod3.ps.prob[exit == 0, ipw := 1 / (1-prob)] # weight for IPW
    dt3 <- merge(dt3, mod3.ps.prob)

# Manually prep data for regression ----
    dt3[, exit := as.integer(exit)]
    dt3[, interaction := exit * time]
    dt3[, race_eth_me := factor(race_eth_me)]
    dt3[, exit_year := factor(exit_year)]
    dt3[, clusterid := as.numeric(factor(id_kc_pha))]
    head(dt3)

    Y = dt3$wage # outcome
    A = dt3$exit # exposure
    W = setDF(dt3[, c('wage', 'exit', 'time', 'interaction', 'clusterid', 'race_eth_me', 'housing_time_at_exit', 'exit_year')]) # covariates
    g1W = dt3$prob # propensity score, a.k.a. predicted probability of exposure
    gn = dt3$prob # propensity score, a.k.a. predicted probability of exposure
    gn10 <- list(gn, 1-gn)
    rhs <- c("A+time+interaction+housing_time_at_exit")    
    rhs <- c("A+time+interaction+housing_time_at_exit+exit_year+race_eth_me")    
    
# Doubly Robust Regression with tmle package ----
    # basic model ----
      mod3.x <- tmle::tmle(
        Y=Y, # outcome
        A=A, # exposure
        W=W, # covariates 
        Qform = as.formula(Y~A+time+interaction+housing_time_at_exit), # adjusted model 
        id = dt3$id_kc_pha, # clustering
        family = 'gaussian', # glm family
        g1W=g1W
      )
      summary(mod3.x)
      rm(mod3.x)
      # mod3.x$estimates$ATE$psi
      # mod3.x$estimates$ATE$CI
      
    # complete model ----
      message("Fails because cannot use factor variables as covariates.")
      mod3.x <- tmle::tmle(
        Y=Y, # outcome
        A=A, # exposure
        W=W, # covariates 
        Qform = as.formula(Y~A+time+interaction+housing_time_at_exit+exit_year), # adjusted model 
        #Qform = Y~A+time+interaction+race_eth_me+exit_year+housing_time_at_exit, # adjusted model 
        id = dt3$id_kc_pha, # clustering
        family = 'gaussian', # glm family
        g1W=g1W
      )
      summary(mod3.x)
      rm(mod3.x)

# library(drtmle)
# Doubly Robust Regression with drtmle package ----
    message("No way to address clustering")
    mod3.x <- drtmle::drtmle(
      Y=Y, # outcome
      A=A, # exposure
      W=W, # covariates 
      gn = gn10, # propensity scores
      glm_Q = rhs, # rhs of adjusted model
      glm_Qr = "gn",
      glm_gr = 'Qn',
      family = gaussian(),
      stratify = FALSE
    )
    drtmle::ci(mod3.x)
    drtmle::ci(mod3.x, contrast = c(-1, 1)) # average treatment effect
    drtmle::wald_test(mod3.x, contrast = c(-1, 1), null = (0)) # hypothesis test that marginal means == 0
    rm(mod3.x)
    
# Doubly Robust DiD with DRDID ----
    # library(DRDID)
    # cannot have clustering by household, just individual
    mod3.x <- DRDID::drdid(
      yname = 'wage', # outcome
      tname = 'time', # time
      idname = 'clusterid', # unit|cluster id
      dname = 'exit', # binary treatment|exposure
      xformla = ~ housing_time_at_exit + exit_year + race_eth_me, # covariates
      data = dt3,
      panel = TRUE,
      estMethod = c("imp", "trad"), # improved locally efficient DR DID estimator
      weightsname = NULL, # sampling weights
      boot = FALSE,
      boot.type = c("weighted", "multiplier"),
      nboot = 999,
      inffunc = FALSE
    )
    mod3.x
    rm(mod3.x)
    
    data(nsw_long)
    # Form the Lalonde sample with CPS comparison group
    tempy1 <- subset(nsw_long, nsw_long$treated == 0 | nsw_long$sample == 2)
    
    out <- drdid(yname = "re", tname = "year", idname = "id", dname = "experimental",
                 xformla= ~ age + educ + black + married + nodegree + hisp + re74,
                 data = tempy1, panel = TRUE)
    summary(out)

