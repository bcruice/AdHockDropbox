#### Drainage Well Monitoring Check
#### Data gathering
#### Written by : Brian Cruice
#### Written on: 01/18/2023

### 0.1 packages
library(tidyverse)
library(odbc)
library(DBI)
library(pwdgsi)
library(lubridate)

### 0.2 connect
cw_con <- odbc::dbConnect(odbc::odbc(),"CityWorks",uid = "cwread", pwd = "readcw", database = "PWD_Cityworks")
mars_con <- odbc::dbConnect(odbc::odbc(), "mars14_datav2")


# update with smp in question
# smps <- c("1024-1-1","1025-1-1","1029-1-1")
# limit to 1029-1
smps <- c("1029-1-1")

current_date <- today()


smp_2_sys <- function(smp_id){
  x <- str_split(smp_id, pattern = "-") %>% unlist()
  sys_id <- paste0(x[1],"-",x[2])
  return(sys_id)
}

systems <- sapply(smps, smp_2_sys) %>% as.vector()

# Grab componet ID's associated with these systems

asset_query <- paste0("SELECT * FROM external.mat_assets WHERE system_id IN ('",paste(systems, collapse = "', '"),"')")

assets <- dbGetQuery(mars_con, asset_query)

# Repeat for all assets, including fittings
facility_asset_ids <- paste("('{",paste(assets$facility_id, collapse = "}', '{"),"}')", sep = "")


wo_asset_query <- paste0("SELECT * FROM Azteca.WORKORDER wo LEFT JOIN Azteca.WORKORDERENTITY woe ON
    wo.WORKORDERID = woe.WORKORDERID WHERE
    woe.ENTITYUID IN ", facility_asset_ids, "OR woe.FEATUREUID IN ",facility_asset_ids)

asset_workorders <- dbGetQuery(cw_con, wo_asset_query)

#remove duplicates
asset_workorders <- asset_workorders[!duplicated(colnames(asset_workorders))]

# check work order type
descriptions <- unique(asset_workorders$DESCRIPTION)

# look for inlet protection maintenance
check <- asset_workorders %>% dplyr::filter(DESCRIPTION == 'SURFACE INLET PROTECTION MAINTENANCE')

