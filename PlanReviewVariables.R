#### Working Document on Plan Review -- new crosstab table to replace existing ####
#### Setup ####
library(odbc); library(DBI); library(tidyverse)
##library(RPostgres); library(RPostgreSQL)
library(magrittr)


mars_con <- dbConnect(odbc::odbc(), "mars14_datav2")

# Plan Review Connection
planreview <- dbConnect(odbc(), 
                        Driver = "ODBC Driver 17 for SQL Server", 
                        Server = "PWDSPRA", 
                        Database = "SPRA_ReportingDB",
                        uid = Sys.getenv("gis_uid"),
                        pwd= Sys.getenv("gis_pwd"))


#### Recreating crosstab plan review table for snapshot feature #### 
view_smp_designation <- dbGetQuery(planreview, "select p.spra_legacyprojectid as 'projectid', s.spra_name as 'smpid', p.spra_projectname as 'projectname', p.spra_trackingid as 'trackingnumber',
	d.Designation as 'designation', p.spra_smipfundedname as 'smip', p.spra_garpfundedname as 'garp',
	pt.spra_oowprogramtype as 'oow_programtype'
from spra_project p inner join 
	(select * from spra_projectsmpdetails s where s.spra_smptypename <> 'Site Characteristics') s
		on p.spra_trackingid = s.spra_projectname
	inner join View_Project_Designation d on p.spra_trackingid = d.TrackingNumber
	left join spra_programtype pt on p.spra_programtypes = pt.spra_programtypeid
  order by p.spra_trackingid, s.spra_name")


head_vals <- dbGetQuery(planreview, "select       spra_name as 'smp_id',
                                                  spra_smptypename as 'smp_type',
                                                  coalesce(spra_depthofmedium_asmaintained, spra_depthofmedium_asbuilt, spra_depthofmedium_approved) as 'medium_depth_ft',
                                                  coalesce(spra_effectivehead_asmaintained, spra_effectivehead_asbuilt, spra_effectivehead_approved) as 'effective_head_ft',
                                                  coalesce(spra_stonestoragedepth_asmaintained/12, spra_stonestoragedepth_asbuilt/12, spra_stonestoragedepth_approved/12) as 'stone_storage_depth_ft',
                                                  coalesce(spra_footprint_asmaintained, spra_footprint_asbuilt, spra_footprint_approved) as 'footprint_ft2',
                                                  coalesce(spra_staticstorage_asmaintained, spra_staticstorage_asbuilt, spra_staticstorage_approved) as 'static_volume_ft3',
                                                  coalesce(spra_waterqualityvolume_asmaintained, spra_waterqualityvolume_asbuilt, spra_waterqualityvolume_approved) as 'wq_volume_ft3',
                                                  coalesce(spra_testinfiltrationrate_asmaintained, spra_testinfiltrationrate_asbuilt, spra_testinfiltrationrate_approved) as 'infil_dsg_rate_inhr'
                                      from spra_projectsmpdetails s")  %>% 
              dplyr::mutate(wq_head_ft = wq_volume_ft3/footprint_ft2 ) %>%
              dplyr::mutate(static_head_ft = static_volume_ft3/footprint_ft2)  

cross_tab <-dbGetQuery(planreview,
                              "select bv.SMPID as 'smp_id',
                                      bv.FootPrint_best as 'footprint_ft2',
                                      bv.DCIA_best as 'dcia_ft2',
                                      bv.SlowReleaseVolume_best as 'slow_release_vol_ft3',
                                      bv.WaterQualityVolume_best as 'water_quality_vol_ft3',
                                      bv.StaticStorage_best as 'static_storage_vol_ft3',
                                      bv.SysType_AP, bv.SysType_AB, bv.Systype_AM,
                                      coalesce(bv.SysType_AM,bv.SysType_AB,bv.SysType_AP) as 'system_type',
                                      coalesce(bv.Loca_AM, bv.Loca_AB, bv.Loca_AP) as 'location'
                               from View_SMP_BestValues bv")


smip_garp_des <- view_smp_designation %>% dplyr::select(smpid, smip, garp)
smip_garp_des <- smip_garp_des %>% dplyr::mutate(smp_id = smpid) %>% dplyr::select(-smpid)

cross_tab <- cross_tab %>% dplyr::left_join(smip_garp_des, by = "smp_id")
cross_tab <- cross_tab %>% dplyr::left_join(head_vals, by = "smp_id")


#### Working area: do not send to Taylor ####
#bio-infiltration/bio-retention -> use stone storage depth (in inches), if unavailable assume (WQ Vol/footprint)/0.4
#basin -> use effective head, if unavailable assume (WQ Vol/footprint)/0.4
# this will be handled in the postgreSQL view

# existing smps
mars_private <- dbGetQuery(mars_con, "SELECT * FROM fieldwork.tbl_ow where smp_id NOT LIKE '%-%'")

cross_tab_mon <- cross_tab_rework %>% dplyr::filter(smp_id %in% private_mon_smps)

private_mon_smps <- mars_private$smp_id %>% unique


cross_tab_cols <-dbGetQuery(planreview,
                            "select *
                               from View_SMP_BestValues bv") %>% colnames

pd_cols  <- dbGetQuery(planreview,
                       "SELECT * FROM spra_projectssmpdetials") %>% colnames


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


