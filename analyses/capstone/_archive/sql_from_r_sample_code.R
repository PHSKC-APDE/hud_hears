# Header ----
# Author: Danny Colombara
# Date: December 1, 2021
# R version: 4.0.3
# Purpose: Capstone sample code for connecting to SQL via R
#

# Set up ----
    rm(list=ls())
    pacman::p_load(data.table, DBI, keyring, glue)

    # Uncomment the following line to create a key (run when you change KCIT pwd)
    # When a window pops up, please enter your standard KCIT password
    #  keyring::key_set(service = "hhsaw", username = "YourKCUsername")

    message("Hint! There are different flavors of SQL. When googling for how to do something, 
            be sure to look for TSQL | T-SQL | Transact-SQL")
        
# Create a connection to hhs_analytics_workspace on Azure server 16 ----
    cxn16 <- DBI::dbConnect(odbc::odbc(), 
                            driver = "ODBC Driver 17 for SQL Server", 
                            server = "kcitazrhpasqlprp16.azds.kingcounty.gov", 
                            database = "hhs_analytics_workspace", 
                            uid = keyring::key_list("hhsaw")[["username"]], 
                            pwd = keyring::key_get("hhsaw", keyring::key_list("hhsaw")[["username"]]), 
                            Encrypt = "yes", 
                            TrustServerCertificate = "yes", 
                            Authentication = "ActiveDirectoryPassword")
    
# Select entire homeless status table ----
    unhoused1 <- setDT(DBI::dbGetQuery(conn = cxn16, "SELECT * FROM [hudhears].[pha_homeless_status]"))
    
# Select homeless status when status is homeless ----
    unhoused2 <- setDT(DBI::dbGetQuery(conn = cxn16, "SELECT * FROM [hudhears].[pha_homeless_status] 
                                       WHERE housing_status = 'homeless'"))
    
# Select homeless status when status is homeless & dates between [01-01-2013, 12-31-2020] ----
    unhoused3 <- setDT(DBI::dbGetQuery(conn = cxn16, "SELECT * FROM [hudhears].[pha_homeless_status] 
                                       WHERE housing_status = 'homeless' AND 
                                       start_date BETWEEN '2013-01-01' AND '2020-12-31' "))    
    
# Write data to SQL table ----
    message("CAUTION! This will overwrite anything that is already in the table.")
    
    tempdt <- data.table(letters = c("A", "B"), numbers = c(1, 2))
    
    DBI::dbWriteTable(conn = cxn16, 
                      name = DBI::Id(schema = "hudhears", table = "utternonsense"), 
                      value = setDF(copy(tempdt)), 
                      append = F, 
                      overwrite = T)
    
    check.tempdt <- setDT(DBI::dbGetQuery(conn = cxn16, "SELECT * FROM hudhears.utternonsense"))
    
    # confirm that what is in SQL is the same as what we wanted to load
    nrow(fsetdiff(tempdt, check.tempdt)) == 0
    nrow(fsetdiff(check.tempdt, tempdt)) == 0
    
# Append data to SQL table SAFELY ----    
    message("You can use the code above and change append = T and overwrite = F. 
            However, I think mistakes will inevitably happen resulting in the 
            overwriting of data. I think it's better to use the append function")
    tempdt2 <- data.table(letters = c("C", "D"), numbers = 3:4)
    DBI::dbAppendTable(conn = cxn16,
                       name = DBI::Id(schema = "hudhears", table = "utternonsense"), 
                       value = setDF(copy(tempdt2)))
    
    # confirm that table now has all four rows
    setDT(DBI::dbGetQuery(conn = cxn16, "SELECT * FROM hudhears.utternonsense"))[]
    
    
# Delete / drop table ----
    message("CAUTION!!!!!!!! Please be sure you only delete tables you created!")
    DBI::dbSendQuery(conn = cxn16, "DROP TABLE hudhears.utternonsense")
    
        
# the end ----
    
    
    
    