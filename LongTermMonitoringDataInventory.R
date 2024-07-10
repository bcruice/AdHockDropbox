
#### Setup ####
library(odbc); library(DBI); library(tidyverse)
library(sf); library(RPostgres); library(RPostgreSQL)
library(tmap)



#### Database connection; database reading ####
central_db <- dbConnect(odbc::odbc(), "CentralDB")
mars_con <- dbConnect(odbc::odbc(), "mars14_datav2")


# GISDB <- dbConnect(odbc(),
#                    Driver = "ODBC Driver 17 for SQL Server",
#                    Server = "PWDGISSQL",
#                    Database = "GISDATA",
#                    uid = Sys.getenv("gis_uid"),
#                    pwd= Sys.getenv("gis_pwd"))
# 

#GIS Connections
dsn_infra_pub <- paste0("MSSQL:server=PWDGISSQL;",
                        "database=GIS_APPS;",
                        "UID=", Sys.getenv("gis_uid"), ";",
                        "PWD=", Sys.getenv("gis_pwd"), ";")


#### Long Term Monitoring Variables Geoprocessing ####
##### CSS network #####
gi_inlets <- (st_read(dsn_infra_pub, query = "select * from gisad.GSWIINLET", quiet = TRUE)) %>% 
             st_set_crs(2272)


greengrey_inlets <- ((st_read(dsn_infra_pub, query = "SELECT * FROM gisad.wwInlet WHERE CONNECTS_TO_GREEN  = 'YES'", quiet = TRUE))) %>% 
                 st_set_crs(2272)


# Only grab laterals where sticker numbers match green-grey inlets
stickers <- greengrey_inlets$StickerNumber
# knock the na
stickers <- stickers[!is.na(stickers)] 

greengrey_lat_qry <- paste0("SELECT * FROM gisad.wwInletPipe WHERE StickerNumber IN (",paste(stickers, collapse = ", "),")")

greengrey_lat <- ((st_read(dsn_infra_pub, query = greengrey_lat_qry, quiet = TRUE))) %>% 
  st_set_crs(2272) 


gravmain <- suppressWarnings((st_read(dsn_infra_pub, query = "SELECT * FROM gisad.wwGravityMain", quiet = TRUE))) %>%
  st_set_crs(2272) 





ggfitting <- ((st_read(dsn_infra_pub, query = "SELECT TOP 2000 * FROM gisad.gswiFitting")))

# Thank you, open data philly

city_poly <- ((st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson")))

cso_poly <- ((st_read("https://opendata.arcgis.com/datasets/18bfad528ccf4f7b9ec0d7f03a9a786f_0.geojson")))



##### SMPS #####
basin_poly <- ((st_read(dsn_infra_pub, query = "select * from gisad.gswiBasin", quiet = TRUE))) %>% 
               st_set_crs(2272)

blueroof_poly <- ((st_read(dsn_infra_pub, query = "select * from gisad.gswiBlueRoof", quiet = TRUE))) %>% 
  st_set_crs(2272)

bumpout_poly <- ((st_read(dsn_infra_pub, query = "select * from gisad.gswiBumpout", quiet = TRUE))) %>% 
  st_set_crs(2272)

cistern_poly  <- ((st_read(dsn_infra_pub, query = "select * from gisad.gswiCistern", quiet = TRUE))) %>% 
  st_set_crs(2272)

dwell_poly <- ((st_read(dsn_infra_pub, query = "select * from gisad.gswiDrainageWell", quiet = TRUE))) %>% 
  st_set_crs(2272) 

greenroof_poly <- ((st_read(dsn_infra_pub, query = "select * from gisad.gswiGreenRoof", quiet = TRUE))) %>% 
  st_set_crs(2272) 

permpave_poly <- ((st_read(dsn_infra_pub, query = "select * from gisad.gswiPermeablePavement", quiet = TRUE))) %>% 
  st_set_crs(2272) 

planter_poly <- ((st_read(dsn_infra_pub, query = "select * from gisad.gswiPlanter", quiet = TRUE))) %>% 
  st_set_crs(2272) 

raingarden_poly <- ((st_read(dsn_infra_pub, query = "select * from gisad.gswiRainGarden", quiet = TRUE))) %>% 
  st_set_crs(2272) 

swale_poly <- ((st_read(dsn_infra_pub, query = "select * from gisad.gswiSwale", quiet = TRUE))) %>% 
  st_set_crs(2272) 

trench_poly <- ((st_read(dsn_infra_pub, query = "select * from gisad.gswiTrench", quiet = TRUE))) %>% 
  st_set_crs(2272) 

treetrench_poly <- ((st_read(dsn_infra_pub, query = "select * from gisad.gswiTreeTrench", quiet = TRUE))) %>% 
  st_set_crs(2272) 

wetland_poly <- ((st_read(dsn_infra_pub, query = "select * from gisad.gswiWetland", quiet = TRUE))) %>%
  st_set_crs(2272)  #Convert from PA State Plane to WGS 1984

##### Elevation Data Coverage ######
### Percentages for green/gray inlets ###
total_gg_inlets <- nrow(greengrey_inlets)
gg_rim_vals <- nrow(greengrey_inlets %>% dplyr::filter(!is.na(ElevationRim)))
gg_depth_vals <- nrow(greengrey_inlets %>% dplyr::filter(!is.na(Depth)))
gg_elev_invt_vals <- nrow(greengrey_inlets %>% dplyr::filter(!is.na(ELEVATIONINVERT)))

gg_full_data <- greengrey_inlets %>%
                dplyr::filter(!is.na(ELEVATIONINVERT)) %>%
                dplyr::filter(!is.na(ElevationRim)) %>%
                dplyr::filter(!is.na(Depth)) %>% nrow

# percentages
rim_pct <- (gg_rim_vals/total_gg_inlets)*100 %>% round(2)
depth_pct <- (gg_depth_vals/total_gg_inlets)*100 %>% round(2)
invt_elev_pct <- (gg_elev_invt_vals/total_gg_inlets)*100 %>% round(2)

full_data_pct <- (gg_full_data/total_gg_inlets)*100 %>% round(2)

### Percentages for green inlets ###
total_gi_inlets <- gi_inlets %>% nrow
gi_rim_vals <- gi_inlets %>% dplyr::filter(!is.na(ElevationRim)) %>% nrow
gi_depth_vals <- gi_inlets %>% dplyr::filter(!is.na(Depth)) %>% nrow
gi_elev_invt_vals <- gi_inlets %>% dplyr::filter(!is.na(ELEVATIONINVERT)) %>% nrow

gi_full_data <- gi_inlets %>%
                dplyr::filter(!is.na(ElevationRim)) %>%
                dplyr::filter(!is.na(Depth)) %>%
                dplyr::filter(!is.na(ELEVATIONINVERT)) %>% nrow
# percentages
rim_pct <- (gi_rim_vals/total_gi_inlets)*100 %>% round(2)
depth_pct <- (gi_depth_vals/total_gi_inlets)*100 %>% round(2)
invt_elev_pct <- (gi_elev_invt_vals/total_gi_inlets)*100 %>% round(2)

full_data_pct <- (gi_full_data/total_gi_inlets)*100 %>% round(2)


##### Geoprocessing #####

# Buffers
lat_buffer <- st_buffer(greengrey_lat,dist = 15)
inlet_buffer <- st_buffer(greengrey_inlets, dist = 15)

intersect_test1 <- st_intersection(gravmain, lat_buffer)

intersect_test2 <- st_intersection(gravmain, inlet_buffer)

#combine sf's


basin_poly$SMP_TYPE <- "Basin"
blueroof_poly$SMP_TYPE <- "Blue Roof"
bumpout_poly$SMP_TYPE <- "Bumpout"
cistern_poly$SMP_TYPE <- "Cistern"
dwell_poly$SMP_TYPE <- "Drainage Well"
greenroof_poly$SMP_TYPE <- "Green Roof"
permpave_poly$SMP_TYPE <- "Permeable Pavement"
planter_poly$SMP_TYPE <- "Planter"
raingarden_poly$SMP_TYPE <- "Rain Garden"
swale_poly$SMP_TYPE <- "Swale"
trench_poly$SMP_TYPE <- "Trench"
treetrench_poly$SMP_TYPE <- "Tree Trench"
wetland_poly$SMP_TYPE <- "Wetland"


# create smp groupings of note
smp_veg <- c("Basin", "Bumpout",  "Green Roof",
             "Planter", "Rain Garden", "Swale",
             "Wetland")

# potential to have subsurface component
smp_sub <- c("Basin", "Bumpout", "Permeable Pavement", "Planter", "Rain Garde",
             "Trench","Tree Trench")

# removed drainage well since it's not what we're looking at

smp_sf_list <- list(basin_poly, blueroof_poly, bumpout_poly, cistern_poly,
                    permpave_poly, planter_poly, raingarden_poly, swale_poly,
                    trench_poly, treetrench_poly, wetland_poly)

# we need the bare minimum from each SMP Type
for(i in 1:length(smp_sf_list)){
  
  #One at a time
  smp_x <- smp_sf_list[[i]]
  smp_x <- smp_x %>% dplyr::select(OBJECTID, SMP_ID, SMP_TYPE, X_STATEPLANE, Y_STATEPLANE, DOWNSTREAM_MANHOLEID, GDB_GEOMATTR_DATA, SHAPE)
  
  smp_sf_list[[i]] <- smp_x
  
  print(paste0("Iteration: ",i,". Number of columns: ",ncol(smp_x),". SMP Type: ", smp_x$SMP_TYPE[1]))
  }

# simple version
smp_sf <- sf::st_as_sf(data.table::rbindlist(smp_sf_list))

#public smps
pub_smp_sf <- smp_sf %>% dplyr::filter(grepl(SMP_ID, pattern = "\\d*-\\d*-\\d*"))


# add columns for SMP number and system number
pub_smp_sf$smp_number <- gsub(pub_smp_sf$SMP_ID, pattern = "\\d*-\\d*-", replacement = "")
pub_smp_sf$system_id <- gsub(pub_smp_sf$SMP_ID, pattern = "-\\d*$", replacement = "")

# buffer smp's
smp_buff <- st_buffer(smp_sf, dist = 75)

#intersect with gravity mains
smp_main_int <- st_intersection(gravmain,smp_buff)

smp_main_int <- smp_main_int %>% dplyr::mutate(buffer_dist_ft = coalesce(Diameter/24, Width/24)) %>%
                                 dplyr::filter(!is.na(buffer_dist_ft))
 

# Iterate through to grab the best option for each smp

# variable frame
smp_data <- as.data.frame(matrix(nrow = 1, ncol = 12))

colnames(smp_data) <- c("SMP_OBJECTID", "SEWER_OBJECTID", "smp_id","system_id","SMP_TYPE","X_STATEPLANE", "Y_STATEPLANE",
                        "DOWNSTREAM_MANHOLEID","SEWER_MATERIAL","SEWER_YEAR", "SEWER_DISTANCE_FT","UPSTREAM_VEGETATION")


for (i in 1:nrow(pub_smp_sf)){
  
  # grab smp id, system processing
  smp_x <- pub_smp_sf$SMP_ID[i]
  sys_x <- pub_smp_sf$system_id[i]
  smp_numb_x <- pub_smp_sf$smp_number[i]
  
  # grab sewers within 50 ft
  swr_x <- smp_main_int %>% dplyr::filter(SMP_ID == smp_x)
  
  if(nrow(swr_x) == 0){
  # no sewer within 50 ft  
    # fill up the row
    smp_data[i,] <- c(pub_smp_sf$OBJECTID[i], min_swr$OBJECTID, smp_x, sys_x, pub_smp_sf$SMP_TYPE[i],
                      pub_smp_sf$X_STATEPLANE[i], pub_smp_sf$Y_STATEPLANE[i], pub_smp_sf$DOWNSTREAM_MANHOLEID[i],
                      NA, NA, NA, NA)
    
    
  } else {
    #calculate distance
    dist_x <- st_distance(pub_smp_sf[i,], swr_x) %>% as.vector
    swr_x$distance_ft <- dist_x
    min_swr <- swr_x %>% dplyr::filter(distance_ft == min(dist_x))
    
    swr_x <- st_buffer(swr_x, dist = swr_x$buffer_dist_ft)
    
    # select first if identical distances
    min_swr <- min_swr[1,]
    
    #upstream veg time
    brothers <- pub_smp_sf %>% dplyr::filter(system_id == sys_x & smp_number != smp_numb_x)
    if(nrow(brothers) == 0){
      upstream <- FALSE
    } else if(sum(brothers$smp_number < smp_numb_x) > 0){
      older_siblings <- brothers[(brothers$smp_number < smp_numb_x),]
      if(sum(older_siblings$SMP_TYPE %in% smp_veg) > 0){ upstream <- TRUE} else {upstream <- FALSE}
    } else {
      upstream <- FALSE
    }
    
    # fill up the row
    smp_data[i,] <- c(pub_smp_sf$OBJECTID[i], min_swr$OBJECTID, smp_x, sys_x, pub_smp_sf$SMP_TYPE[i],
                      pub_smp_sf$X_STATEPLANE[i], pub_smp_sf$Y_STATEPLANE[i], pub_smp_sf$DOWNSTREAM_MANHOLEID[i],
                      min_swr$Material, min_swr$Year_Installed, min_swr$distance_ft, upstream)
  }
  

}

# join the greenit/cipit stuff
grnit_smp_data <- dbGetQuery(mars_con, "select * from external.tbl_smpbdv")
grnit_sys_data <- dbGetQuery(mars_con, "select * from external.tbl_systembdv")
cipit_data <- dbGetQuery(mars_con, "select * from external.tbl_cipit_project")
ow_assets <- dbGetQuery(mars_con, "select * from external.mat_assets where asset_type = 'Observation Well'")

#trim to stuff we want
grnit_sys_join <- grnit_sys_data %>% dplyr::select(worknumber, system_id, sys_lrimpervda_ft2, sys_creditedstormsizemanaged_in, sys_sysfunction)
cipit_join <- cipit_data %>% dplyr::select(worknumber, construction_start_date, construction_complete_date)

# fill in con complete gaps
cipit_join <- cipit_join %>% dplyr::mutate(construction_complete_year = dplyr::coalesce(
                                             lubridate::year(construction_complete_date),
                                             lubridate::year(construction_start_date + years(1))))
  
# tack cipit and greenit data on
smp_data <- smp_data %>% left_join(grnit_sys_join, by = "system_id") %>%
                         left_join(cipit_join, by = "worknumber")

# it's infiltrating, has potential for a subsurface component, and has an observation well
infil_smp_data <- smp_data %>% dplyr::filter(sys_sysfunction == "Infiltration") %>%
                               dplyr::filter(SMP_TYPE %in% smp_sub) %>%
                               dplyr::filter(smp_id %in% ow_assets$smp_id)

infil_smp_data$SEWER_DISTANCE_FT <- infil_smp_data$SEWER_DISTANCE_FT %>% as.numeric
infil_smp_data$SEWER_YEAR <- infil_smp_data$SEWER_YEAR %>% as.numeric
infil_smp_data$SEWER_YEAR[infil_smp_data$SEWER_YEAR == 9999] <- NA
infil_smp_data$SEWER_AGE <- 2024 - infil_smp_data$SEWER_YEAR

##### Short-circuiting categorization #####
infil_smp_data$SEWER_DISTANCE_FT %>% median(na.rm = TRUE)
infil_smp_data$SEWER_DISTANCE_FT %>% mean(na.rm = TRUE)
infil_smp_data$SEWER_DISTANCE_FT %>% sd(na.rm = TRUE)
hist(infil_smp_data$SEWER_DISTANCE_FT)

infil_smp_data$SEWER_AGE %>% median(na.rm = TRUE)
infil_smp_data$SEWER_AGE %>% mean(na.rm = TRUE)
infil_smp_data$SEWER_AGE %>% sd(na.rm = TRUE)
hist(infil_smp_data$SEWER_AGE)


# Define quantile cutoffs
high_age <- quantile(infil_smp_data$SEWER_AGE, na.rm = TRUE)[4]
mid_age <- quantile(infil_smp_data$SEWER_AGE, na.rm = TRUE)[3]

high_dist <- quantile(infil_smp_data$SEWER_DISTANCE_FT, na.rm = TRUE)[2]
mid_dist <- quantile(infil_smp_data$SEWER_DISTANCE_FT, na.rm = TRUE)[3]

#brick and mortar, most I&I
high_mat <- c("BMP")
#concrete, clay, terra cotta, mid I&I
mid_mat <- c("CC","VCP","TCP")

infil_smp_data$SHORT_CIRCUITING_SCORE <- 0
infil_smp_data$SHORT_CIRCUITING_FLAG <- 0
#Score each value
for(i in 1:nrow(infil_smp_data)){

  #DISTANCE
  if(is.na(infil_smp_data$SEWER_DISTANCE_FT[i])){
    infil_smp_data$SHORT_CIRCUITING_FLAG[i] <- infil_smp_data$SHORT_CIRCUITING_FLAG[i] + 1
  } else
  if(infil_smp_data$SEWER_DISTANCE_FT[i] <= high_dist){
    infil_smp_data$SHORT_CIRCUITING_SCORE[i] <- infil_smp_data$SHORT_CIRCUITING_SCORE[i] + 2
  } else
  if(infil_smp_data$SEWER_DISTANCE_FT[i] <= mid_dist){
    infil_smp_data$SHORT_CIRCUITING_SCORE[i] <- infil_smp_data$SHORT_CIRCUITING_SCORE[i] + 1
  }

  #AGE
  if(is.na(infil_smp_data$SEWER_AGE[i])){
    infil_smp_data$SHORT_CIRCUITING_FLAG[i] <- infil_smp_data$SHORT_CIRCUITING_FLAG[i] + 1 
  } else
  if(infil_smp_data$SEWER_AGE[i] >= high_age){
    infil_smp_data$SHORT_CIRCUITING_SCORE[i] <- infil_smp_data$SHORT_CIRCUITING_SCORE[i] + 2
  } else
  if(infil_smp_data$SEWER_AGE[i] >= mid_age){
    infil_smp_data$SHORT_CIRCUITING_SCORE[i] <- infil_smp_data$SHORT_CIRCUITING_SCORE[i] + 1
  }
  
  #MATERIAL
  if(is.na(infil_smp_data$SEWER_MATERIAL[i])){
    infil_smp_data$SHORT_CIRCUITING_FLAG[i] <- infil_smp_data$SHORT_CIRCUITING_FLAG[i] + 1
  } else
  if(infil_smp_data$SEWER_MATERIAL[i] %in% high_mat){
    infil_smp_data$SHORT_CIRCUITING_SCORE[i] <- infil_smp_data$SHORT_CIRCUITING_SCORE[i] + 2
  } else
  if(infil_smp_data$SEWER_MATERIAL[i] %in% mid_mat){
    infil_smp_data$SHORT_CIRCUITING_SCORE[i] <- infil_smp_data$SHORT_CIRCUITING_SCORE[i] + 1
  } else
  if(infil_smp_data$SEWER_MATERIAL[i] == "Unknown"){
    infil_smp_data$SHORT_CIRCUITING_FLAG[i] <- infil_smp_data$SHORT_CIRCUITING_FLAG[i] + 1
  }    

}

##### Write the results #####


infil_smp_data$clustering_batch <- 1

results <- dbWriteTable(mars_con, DBI::SQL("metrics.tbl_longterm_cluster_variables"), infil_smp_data, append = TRUE, row.names = FALSE)

# only subsurface systems with an observation well component

##### plot data #####

ggplot(city_poly) + geom_sf(fill = "honeydew3", alpha = 0.4) +
  geom_sf(data = cso_poly, fill = "grey45", alpha = 0.8) +
  # geom_sf(data = test_combine_buffer, fill = "red4") +
  geom_sf(data = smp_main_int, col = "red2") +
  # geom_sf(data = greengrey_inlets, fill = "chartreuse4", shape = 22) +
  # geom_sf(data = gi_inlets, fill = "lightgreen", shape = 22) +
  # geom_sf(data = lat_buffer, fill = "chartreuse") +
  # geom_sf(data = greengrey_lat, col = "green3") + 
  # geom_sf(data= intersect_test1, col = "black") +
  # geom_sf(data = all_trench_poly, fill = "red4") +
  theme_minimal()





lat_plot <- ggplot(cso_poly) + geom_sf(fill = "honeydew3", alpha = 0.7) +
  # geom_sf(data = greengrey_inlets, fill = "chartreuse4", shape = 22) +
  # geom_sf(data = gi_inlets, fill = "lightgreen", shape = 22) +
  geom_sf(data = greengrey_lat, col = "chartreuse4") +
  geom_sf(data = smp_x, col = "seagreen")




# leaflet map settup
smp_polys <- tm_shape(lat_buffer) +
              tm_polygons(fill = "chartreuse2") +
             tm_shape(smp_main_int) +
              tm_lines(col = "chartreuse4") +
             tm_shape(greengrey_inlets) +
              tm_dots(col = "seagreen") +
             tm_shape(smp_xyz, fill = "red4") +
              tm_polygons(fill = "red4")


smp_polys <- tm_shape(buff_x) +
             tm_polygons(fill = "green3")

tmap_options(check.and.fix = TRUE)

# leaflet time
tmap_leaflet(smp_polys)

# leaflet() %>%   setView(lat = 39.95, lng = -75.17, zoom=11.5) %>%
#   addTiles(group="OSM") %>%
#   addProviderTiles(providers$CartoDB.DarkMatter, group="Dark") %>%
#   addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
#   addLayersControl(baseGroups=c('OSM','Dark','Light'))
















