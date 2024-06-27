#### One off for Shawn ####
# Let's Grab every file with the sensor serial number in the raw data folders

library(tidyverse)
library(magrittr)
library(stringr)
library(stringi)

# Start folder
par_folder <- "\\\\pwdoows\\OOWS\\Watershed Sciences\\GSI Monitoring\\02 GSI Monitoring Sites"

setwd(par_folder)

# site folders
site_folders_public <- list.files(par_folder, full.names = TRUE)
site_folders_private <- list.files(paste0(par_folder,"\\z_Private Monitoring Sites"), full.name = TRUE)
site_folders_gw <- list.files(paste0(par_folder),"\\z_Groundwater Wells", full.name = TRUE)
site_folders_spec_proj <- list.files("\\\\pwdoows\\OOWS\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\01 Special Monitoring Locations", full.name = TRUE)

site_folders <- c(site_folders_public, site_folders_private, site_folders_gw,site_folders_spec_proj)

# inititiate raw data file df
raw_files <- c()

#iterate through site folders, looking into raw data folder
for(i in 1:length(site_folders)){
  
files_x <- list.files(site_folders[i], recursive = TRUE, full.names = TRUE)
raw_files_x <-  raw_files_x <-  grep(pattern = "Raw Data", files_x, value = TRUE)

raw_files <- c(raw_files, raw_files_x)

}

# let's look at specific serial numbers
sensor_serials <- c(991593, 9951698, 10526846, 10526848, 10526854, 10563847, 10752542, 11013546, 20501639, 10338603, 10338612, 9951594, 10719649, 20573927, 20285183, 10563866, 20573915, 11013559, 9951608, 20573967, 20566378, 10563855, 10563863, 10938746, 10338619, 11013533, 11013531, 11013555, 10338629, 20285188, 10744832, 20566636, 10526883, 10526888, 10338616, 10338627, 10338625, 10523180, 11013548, 10753539, 10744824, 9951599, 11013508, 10526866, 10338599, 10217763, 10526843, 10526873, 9951606, 10217756, 10338594, 10523185, 11013521)

# initial file list
file_list  <- as.data.frame(matrix(nrow = 1, ncol = 3))
colnames(file_list) <- c("Sensor_Serial","File_Path","Date")

for( i in 1:length(sensor_serials)){
  
  serial_files_x <- grep(pattern = paste0(sensor_serials[i]), raw_files, value = TRUE)
  
  if(length(serial_files_x) > 0){
    file_list_x <- as.data.frame(serial_files_x)
    colnames(file_list_x) <-  "File_Path"
    file_list_x$Sensor_Serial <- sensor_serials[i]
    date_x <- str_extract(serial_files_x,pattern = '20\\d{6}')
    file_list_x$Date <- date_x
    file_list <- rbind(file_list,file_list_x)
    
    }
  
}

file_list <- file_list %>% dplyr::mutate(FullDate = lubridate::as_date(Date))

write.csv(file_list, file = "\\\\pwdoows\\OOWS\\Watershed Sciences\\GSI Monitoring\\01 Admin\\02 Equipment\\03 Water Level\\HOBO Water Level Sensors\\06 Sensor Accounting\\raw_data_files.csv")
