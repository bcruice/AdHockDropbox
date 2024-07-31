start_wd <- "\\\\pwdoows\\OOWS\\Watershed Sciences\\GSI Monitoring\\02 GSI Monitoring Sites"

setwd(start_wd)

list_folds <- list.files(start_wd, recursive = FALSE, include.dirs = TRUE)

folders_list <- paste0(start_wd,"\\",list_folds,"\\QAQC\\")


all_files <- as.data.frame(matrix(ncol = 8, nrow = 0))
colnames(all_files) <- c("full_path","smp_id","ow_suffix","calendar_quarter","serial_number","date","reviewer_initials","fiscal_quarter")
z <- 1


for(i in 1:length(folders_list)){
  
  files_x <- list.files(folders_list[i], full.names = TRUE)
  if(length(files_x) > 0){
    
    full_path_x <- files_x[grepl(files_x, pattern = ".xlsx$")]
    file_name_x <- full_path_x %>% str_extract("[^\\\\]*.xlsx$")
    
    if(length(file_name_x) > 0){
      for(j in 1:length(file_name_x)){
        parts <- file_name_x[j] %>% str_split("_") %>% unlist
        
        smp_x <- parts[grepl(parts,pattern = "\\d*-\\d*-\\d*")]
        # ow_x <- parts[grepl(parts, pattern = "OW\\d{0,2}|CO\\d{0,2}|SW\\d{0,2}|CS\\d{0,2}|CS\\d{0,2}|FB\\d{0,2}|DL\\d{0,2}|GI\\d{0,2}GI\\d{0,2}")]
        ow_x <- parts[grepl(parts, pattern = "[A-Z]{2}\\d{1,2}")]
        calq_x <- parts[grepl(parts, pattern = "\\d{2}Q[1-4]")]
        serial_x <- parts[grepl(parts, pattern = "\\d{8}")]
        date_x <- parts[grepl(parts, pattern = "^20\\d{4,6}$")]
        reviewer_initials_x <- parts[grepl(parts, pattern = "^\\[a-zA-Z]{2,3}$")]
        
        # set to NA if no match
        if(length(smp_x) == 0){smp_x <- NA}
        if(length(ow_x) == 0){ow_x <- NA}
        if(length(calq_x) == 0){calq_x <- NA}
        if(length(serial_x) == 0){serial_x <- NA}
        if(length(date_x) == 0){date_x <- NA}
        if(length(reviewer_initials_x) == 0){reviewer_initials_x <- NA}
        
        all_files[z,] <- c(full_path_x[j], smp_x, ow_x, calq_x, serial_x, date_x, reviewer_initials_x, NA)
        z <- z + 1 
      }
    }
  }
}
