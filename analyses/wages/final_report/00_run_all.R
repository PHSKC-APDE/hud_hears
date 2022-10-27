# Header ----
# Author: Danny Colombara
# Date: October 27, 2022
# R version 4.2.1
# Purpose: Run all code to create output for HUDHEARS final report

# Set up ----
pacman::p_load(httr)

# 01_prep ----
  eval(parse(text = httr::content(httr::GET( 
    url = "https://raw.githubusercontent.com/PHSKC-APDE/hud_hears/main/analyses/wages/final_report/01_data_prep.R", 
    httr::authenticate(Sys.getenv("GITHUB_TOKEN"), "")), "text"))) 

# 02_graph_raw ----
  eval(parse(text = httr::content(httr::GET( 
    url = "https://raw.githubusercontent.com/PHSKC-APDE/hud_hears/main/analyses/wages/final_report/02_graph_raw_wages.R", 
    httr::authenticate(Sys.getenv("GITHUB_TOKEN"), "")), "text"))) 

# 03_descriptives ----
  eval(parse(text = httr::content(httr::GET( 
    url = "https://raw.githubusercontent.com/PHSKC-APDE/hud_hears/main/analyses/wages/final_report/03_tables_descriptive.R", 
    httr::authenticate(Sys.getenv("GITHUB_TOKEN"), "")), "text"))) 

# 04_regression ----
  eval(parse(text = httr::content(httr::GET( 
    url = "https://raw.githubusercontent.com/PHSKC-APDE/hud_hears/main/analyses/wages/final_report/04_regression.R", 
    httr::authenticate(Sys.getenv("GITHUB_TOKEN"), "")), "text"))) 

# The end ----