#### Quantifying the variance in well depth measurements ####
library(odbc); library(DBI); library(tidyverse); library(magrittr)

# Connection
mars_con <- DBI::dbConnect(odbc::odbc(), 'mars14_dataV2')

# Well Measurements
well_meas <- dbGetQuery(mars_con, 'select wm.*, coalesce(ow.smp_id,sn.site_name) as smp_site_name, ow.ow_suffix from fieldwork.tbl_well_measurements wm
                                   left join fieldwork.tbl_ow ow on ow.ow_uid = wm.ow_uid
                                   left join fieldwork.tbl_site_name_lookup sn on ow.site_name_lookup_uid = sn.site_name_lookup_uid')



well_meas_vals <- well_meas %>% dplyr::filter(!is.na(well_depth_ft)) %>%
                                dplyr::group_by(ow_uid) %>%
                                dplyr::reframe(
                                    ow_uid = ow_uid,
                                    smp_site_name = smp_site_name,
                                    ow_suffix = ow_suffix,
                                    mean_well_depth_ft = mean(well_depth_ft),
                                    count = n(),
                                    sd_well_depth_ft = sd(well_depth_ft),
                                    var_well_depth_ft = var(well_depth_ft))

multi_well_meas_vals <- well_meas_vals %>% dplyr::filter(count != 1)

hist(well_meas_vals$mean_well_depth_ft)
hist(multi_well_meas_vals$sd_well_depth_ft)



selection_vals <- dbGetQuery(mars_con, 'select orifice_choice,sumpdepth_choice, count(well_measurements_uid) from fieldwork.tbl_well_measurements wm
left join fieldwork.tbl_ow ow
on wm.ow_uid = ow.ow_uid
left join fieldwork.tbl_orifice_lookup ol
on ol.orifice_lookup_uid = wm.orifice_lookup_uid
left join fieldwork.tbl_sumpdepth_lookup sl
ON sl.sumpdepth_lookup_uid = wm.sumpdepth_lookup_uid
where
ow.smp_id is NOT NULL AND
(ow.ow_suffix LIKE \'OW%\' OR ow.ow_suffix LIKE \'GI%\' OR ow.ow_suffix LIKE \'CS%\')
AND (cap_elev IS NOT NULL OR custom_sumpdepth_ft IS NOT NULL OR custom_orificedepth_ft IS NOT NULL)
group by sumpdepth_choice, orifice_choice
order by orifice_choice asc, sumpdepth_choice asc')


