#### Working Document on Plan Review -- new crosstab table to replace existing ####
#### Setup ####
library(odbc); library(DBI); library(tidyverse)
library(RPostgres); library(RPostgreSQL)
library(magrittr)

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
