
#### Setup ####
library(odbc); library(DBI); library(tidyverse)
library(sf); library(RPostgres); library(RPostgreSQL)
library(tmap)



#### Database connection; database reading ####
central_db <- dbConnect(odbc::odbc(), "CentralDB")


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
  st_set_crs(2272) %>% st_transform(4326)  #Convert from PA State Plane to WGS 1984

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

testmerge <- st_union(basin_poly, blueroof_poly) %>%
              st_union(bumpout_poly) %>%
              st_union(cistern_poly) %>%
              st_union(dwell_poly) %>%
              st_union(greenroof_poly) %>%
              st_union(permpave_poly) %>%
              st_union(planter_poly) %>%
              st_union(raingarden_poly) %>%
              st_union(trench_poly) %>%
              st_union(treetrench_poly) %>%
              st_union(wetland_poly)

##### plot data #####

ggplot(city_poly) + geom_sf(fill = "honeydew3", alpha = 0.4) +
  geom_sf(data = cso_poly, fill = "grey45", alpha = 0.8) +
  geom_sf(data = testmerge, fill = "red4") +
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
  geom_sf(data = treetrench_poly, col = "seagreen") +
  geom_sf(data = trench_poly, col = "seagreen") +
  geom_sf(data = basin_poly, col = "seagreen")
  




# # leaflet map settup 
# smp_polys <- tm_shape(lat_buffer) +
#               tm_polygons(fill = "chartreuse2") +
#              tm_shape(greengrey_lat) +
#               tm_lines(col = "chartreuse4") +
#              tm_shape(greengrey_inlets) +
#               tm_dots(col = "seagreen") +
#              tm_shape(intersect_testing) +
#               tm_lines(fill = "black") +
#              tm_shape(all_trench_poly) +
#               tm_polygons(fill = "red4")
# 
# 
# # leaflet time
# tmap_leaflet(smp_polys)

# leaflet() %>%   setView(lat = 39.95, lng = -75.17, zoom=11.5) %>%
#   addTiles(group="OSM") %>%
#   addProviderTiles(providers$CartoDB.DarkMatter, group="Dark") %>%
#   addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
#   addLayersControl(baseGroups=c('OSM','Dark','Light'))


#### Working Document on Plan Review -- new crosstab table to replace existing ####

# Plan Review Connection
planreview <- dbConnect(odbc(), 
                        Driver = "ODBC Driver 17 for SQL Server", 
                        Server = "PWDSPRA", 
                        Database = "SPRA_ReportingDB", 
                        uid = Sys.getenv("gis_uid"),
                        pwd= Sys.getenv("gis_pwd"))


tryCatch({view_smp_designation <- dbGetQuery(planreview, "select p.spra_legacyprojectid as \"ProjectID\", s.spra_name as \"SMPID\", p.spra_projectname as \"Projectname\", p.spra_trackingid as \"TrackingNumber\",
	d.Designation as \"Designation\", p.spra_smipfundedname as \"SMIP\", p.spra_garpfundedname as \"GARP\",
	pt.spra_oowprogramtype as \"OOWProgramType\"
from spra_project p inner join 
	(select * from spra_projectsmpdetails s where s.spra_smptypename <> 'Site Characteristics') s
		on p.spra_trackingid = s.spra_projectname
	inner join View_Project_Designation d on p.spra_trackingid = d.TrackingNumber
	left join spra_programtype pt on p.spra_programtypes = pt.spra_programtypeid
order by p.spra_trackingid, s.spra_name")
}, # append the data
error = function(e){
  kill <<- TRUE
  errorCode <<- 2
  errorCodes$message[errorCode+1] <<- e$message #Error object is a list
  success <<- TRUE
}
)

view_smp_designation <- dbGetQuery(planreview, "select p.spra_legacyprojectid as \"ProjectID\", s.spra_name as \"SMPID\", p.spra_projectname as \"Projectname\", p.spra_trackingid as \"TrackingNumber\",
	d.Designation as \"Designation\", p.spra_smipfundedname as \"SMIP\", p.spra_garpfundedname as \"GARP\",
	pt.spra_oowprogramtype as \"OOWProgramType\"
from spra_project p inner join 
	(select * from spra_projectsmpdetails s where s.spra_smptypename <> 'Site Characteristics') s
		on p.spra_trackingid = s.spra_projectname
	inner join View_Project_Designation d on p.spra_trackingid = d.TrackingNumber
	left join spra_programtype pt on p.spra_programtypes = pt.spra_programtypeid
order by p.spra_trackingid, s.spra_name")


cross_tab_rework <-dbGetQuery(planreview,
                              "select bv.SMPID as 'smp_id',
                                      bv.FootPrint_best as 'footprint_ft2',
                                      bv.DCIA_best as 'dcia_ft2',
                                      bv.SlowReleaseVolume_best as 'slow_release_vol_ft3',
                                      bv.WaterQualityVolume_best as 'water_quality_vol_ft3',
                                      bv.StaticStorage_best as 'static_storage_vol_ft3',
                                      bv.SysType_AP, bv.SysType_AB, bv.Systype_AM,
                                      coalesce(bv.SysType_AM,bv.SysType_AB,bv.SysType_AP) as 'system_type'
                               from View_SMP_BestValues bv")


cross_tab_cols <-dbGetQuery(planreview,
                              "select *
                               from View_SMP_BestValues bv")
