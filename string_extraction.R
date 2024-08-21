start_wd <- "\\\\pwdoows\\OOWS\\Watershed Sciences\\GSI Monitoring\\02 GSI Monitoring Sites"

setwd(start_wd)

list_folds <- list.files(start_wd, recursive = FALSE, include.dirs = TRUE)

folders_list <- paste0(start_wd,"\\",list_folds,"\\QAQC\\")


all_files <- as.data.frame(matrix(ncol = 8, nrow = 0))
colnames(all_files) <- c("full_path","smp_id","ow_suffix","calendar_quarter","serial_number","date","reviewer_initials","fiscal_quarter")
z <- 1


for(i in 1:length(folders_list)){
  
  files_x <- list.files(folders_list[i], full.names = TRUE, recursive = TRUE)
  if(length(files_x) > 0){
    
    full_path_x <- files_x[grepl(files_x, pattern = ".xlsx$")]
    file_name_x <- full_path_x %>% str_extract("[^\\\\]*.xlsx$")
    file_name_x <- full_path_x %>% gsub(pattern = "^.*[//]", replacement = "")
    file_name_x <- file_name_x %>% gsub(pattern = "^.*[\\\\]", replacement = "")
    
    if(length(file_name_x) > 0){
      for(j in 1:length(file_name_x)){
        parts <- file_name_x[j] %>% str_split("_") %>% unlist
        parts <- gsub(x = parts,pattern = ".xlsx", replacement = "") 
        # # removing the part "QAQC" - this allows for easier matching of initials
        # parts <- parts[parts !="QAQC"]
        
        
        # Pull individual pieces from "parts"
        smp_x <- parts[grepl(parts,pattern = "[0-9]*-[0-9]*-[0-9]*")]
          # remove after extracted
          if(length(smp_x) == 0){smp_x <- NA}
          parts <- parts[parts != smp_x]
        ow_x <- parts[grepl(parts, pattern = "[A-Z]{2}\\d{1,2}")]
          # remove after extracted
          if(length(ow_x) == 0){ow_x <- NA}
          parts <- parts[parts != ow_x]
        calq_x <- parts[grepl(parts, pattern = "\\d{2}Q[1-4]")]
          # remove after extracted
          if(length(calq_x) == 0){calq_x <- NA}
          parts <- parts[parts != calq_x]
        date_x <- parts[grepl(parts, pattern = "20[0-2][0-9][0-1][0-9][0-3][0-9]")]
          # remove after extracted
          if(length(date_x) == 0){date_x <- NA}
          parts <- parts[parts != date_x]
        serial_x <- parts[grepl(parts, pattern = "\\d{8}")]
          # remove after extracted
          if(length(serial_x) == 0){serial_x <- NA}
          parts <- parts[parts != serial_x]
        reviewer_initials_x <- parts[grepl(parts, pattern = "^[a-zA-Z]{2,3}$")]
          # remove after extracted
          if(length(reviewer_initials_x) == 0){reviewer_initials_x <- NA}
          parts <- parts[parts != reviewer_initials_x]
        
        # set to NA if no match
        # if(length(smp_x) == 0){smp_x <- NULL}
        # if(length(ow_x) == 0){ow_x <- NULL}
        # if(length(calq_x) == 0){calq_x <- NULL}
        # if(length(serial_x) == 0){serial_x <- NULL}
        # if(length(date_x) == 0){date_x <- NULL}
        # if(length(reviewer_initials_x) == 0){reviewer_initials_x <- NULL}
 
        all_files[z,] <- c(full_path_x[j], smp_x, ow_x, calq_x, serial_x[1], date_x[1], reviewer_initials_x, NA)
        z <- z + 1 
      }
    }
  }
}
