#' NWM Data Types

"nwm_data"

#' NWM THREDDS Servers

"tds_meta"

#' @importFrom RNetCDF open.nc close.nc create.nc var.get.nc var.inq.nc var.def.nc var.put.nc dim.inq.nc  dim.def.nc att.get.nc att.put.nc utcal.nc
#' @importFrom terra values rast nlyr writeCDF ext crs align ext project vect ymax ymin xmin xmax flip crop sds `time<-`
#' @importFrom glue glue
#' @importFrom dplyr mutate filter left_join group_by_at add_tally summarize_at vars ungroup `%>%` select bind_rows
#' @importFrom lubridate ymd_hm hours with_tz tz as_datetime year month day hour yday ymd
#' @importFrom rvest html_attr html_elements
#' @importFrom xml2 read_html
#' @importFrom httr content RETRY GET write_disk progress
#' @importFrom nhdplusTools get_nhdplus 
#' @importFrom dataRetrieval findNLDI readNWISdv renameNWISColumns
#' @importFrom utils tail

NULL


