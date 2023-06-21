library(RNetCDF)
library(dplyr)

tds_meta = data.frame(
    version = c(1.2, 2.0, 2.1),
    type = c("USGS", "USGS", "USGS"),
    ncml = c(
      'nwm_retro_full.ncml',
      'nwm_v2_retro_full.ncml',
      'nwm_v21_retro_full.ncml'
    )) %>%
  mutate(
    url =  paste0(get_tds(type = version), ncml),
    startDate = as.POSIXct("1900-01-01 00:00:00", tz = "UTC"),
    endDate   = as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
  )


for(i in 1:nrow(x)){
  nc = open.nc(tds_meta$url[i])
  tds_meta$time_units[i] = att.get.nc(nc,"time", "units")
  t = utcal.nc(tds_meta$time_units[i], var.get.nc(nc, 'time'), type = "c")
  tds_meta$startDate[i] = t[1]
  tds_meta$endDate[i] = tail(t,1)
}

usethis::use_data(tds_meta, overwrite = TRUE, internal = FALSE)

