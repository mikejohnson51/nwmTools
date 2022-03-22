#' Get TDS path
#' @return character
#' @export
#' 
get_tds = function(type = "hydroshare"){
  
  if(type == "hydroshare"){
  ## Top level Folder (OpenDap archive)
  #'http://thredds.hydroshare.org/thredds/dodsC/nwm_retrospective/'
  #'# CHANGED in March 2021
    return('http://thredds.hydroshare.org/thredds/dodsC/nwm/retrospective/')
  } else {
    return('https://cida.usgs.gov/thredds/dodsC/demo/morethredds/nwm/')
  }
  
}

#' NWM metadata
#' @return tibble
#' @export
#' @importFrom dplyr add_row filter
#' @importFrom lubridate ymd_hm
#' 
get_nwm_meta = function(version = NULL){
  
  df = data.frame(
             version = c(1.2, 2.0, 2.1), 
             type = c("hydroshare", "hydroshare", "USGS"),
             minDate = c(ymd_hm("1993-01-01 00:00"), 
                         ymd_hm("1993-01-01 00:00"),
                         ymd_hm("1979-02-02 18:00")),
             maxDate = c(ymd_hm("2017-12-31 23:00"), 
                         ymd_hm("2018-12-31 00:00"),
                         ymd_hm("2020-12-31 00:00")),
             ncml = c('nwm_retro_full.ncml', 
                      'nwm_v2_retro_full.ncml',
                      'nwm_v21_retro_full.ncml'))
  
  df = df[df$version %in% version,]

  if (nrow(df) == 0) {
    stop(paste('NWM version must be one of:', paste(df$version, collapse = ", " )) , call. = F)
  }
  
  df 
}


#' @title Meta Object Creation with Error Checks
#' @description this function ensures that the all input objects are correct.
#' User input is modified to the desired TZ, model versions are checked, and the NCML file path returned
#' @param startDate a user defined startDate ("YYYY-MM-DD") 
#' @param endDate a user defined end Date ("YYYY-MM-DD") 
#' @param tz a user defined timezone  
#' @param version a user defined model version
#' @importFrom lubridate with_tz
#' @return a list containing the model version, NCML path, and time requests
#' @keywords internal

error.checks = function(startDate, endDate, tz, version){
  
  ## Make sure requested Timezone is valid
  if(!tz %in% OlsonNames()){ 
    stop(paste(tz, "not recognized timezone"), call. = FALSE) 
    } 
  
  startDate = if(!is.null(startDate)) {as.POSIXct(startDate, tz = tz) }
  endDate   = if(!is.null(endDate)) { as.POSIXct(endDate, tz = tz) }

  ## Isolate version of interest
  this.version = get_nwm_meta(version)

  if(is.null(startDate)){
    startDate = this.version$minDate
    endDate   = this.version$maxDate
  }
  
  # If no endDate is given, default to start date + 23 hours
  if(is.null(endDate)){endDate = startDate + 82800}
  
  ## Change Start and EndDate to User Timezones:
  df = data.frame(usr.tz.start  = with_tz(startDate, tz),
                  usr.tz.end    = with_tz(endDate, tz))
  df$usr.utc.start = with_tz(df$usr.tz.start, tzone = "UTC")
  df$usr.utc.end = with_tz(df$usr.tz.end, tzone = "UTC")
  df$version = this.version$version
  df$url = paste0(get_tds(this.version$type), this.version$ncml)
  df$minDate = this.version$minDate
  df$maxDate = this.version$maxDate
  
  ## Check startDate
  if(any(df$usr.utc.start < this.version$minDate)){
    stop("First values for version ", version, ' is ', this.version$minDate, " 00:00:00 UTC \n")
  }
  ## Check endDate
  if(any(df$usr.utc.end > this.version$maxDate)){
    stop("Last values for version ", version, ' is ', this.version$maxDate,  " UTC.")
  }

  
  # If everything is good-to-go return baseURL
  return(df)
}

#' @title Retro Call Meta Data
#' @description build out THREDDs call and meta information
#' @param comid an NHD COMID(s)
#' @param meta.obj a metadata object generated with `error.check`
#' @importFrom dplyr filter
#' @importFrom lubridate ymd_hm with_tz
#' @importFrom RNetCDF open.nc var.get.nc
#' @return a data.frame and opend DAP object
#' @keywords internal

retro_call = function(comid, meta.obj){

  comid = comid[!is.na(comid)]
  
  if(length(comid) > 0){
  
  ids_file = open.nc(system.file("extdata", "retro_feature_ids.nc", package = "nwmTools"))
  call = unique(ifelse(meta.obj$version == 1, "feature_ids_v_one", "feature_ids_v_two"))
  id  = match(comid, var.get.nc(ids_file, call))
  close.nc(ids_file)

  if(length(id) > 0){
    s  = meta.obj$usr.utc.start
    e  = meta.obj$usr.utc.end
    model = hour_seq(meta.obj$minDate, meta.obj$maxDate, "UTC")
    s1 = which(s == model)
    e1 = which(e == model)
    rm(model)
    
    call.meta = data.frame(
              COMID = comid,
              index = id,
              startDate  = s,
              endDate    = e,
              startIndex = s1, 
              endIndex   = e1,
              count = (e1-s1) + 1,
              version = meta.obj$version,
              url = meta.obj$url,
              stringsAsFactors = FALSE) 
    
    call.meta = call.meta[!is.na(call.meta$index),]
   
    if(nrow(call.meta) > 0 ){
      return(list(rows = nrow(call.meta),
                  call.meta = call.meta, 
                  open.dap.file = open.nc(call.meta$url[1])))
    } else { 
      return(NULL) 
    }
  }
  } else {
    return(NULL)
  }
}

#' @title Extract OPENDAP data using meta file
#' @param i index of meta.calls data
#' @param urls meta.calls data.frame
#' @param dap open DAP connection
#' @importFrom RNetCDF var.get.nc
#' @importFrom tibble tibble
#' @return
#' @keywords internal
#' @noRd

extract_thredds = function(i, urls, dap) {
  tibble::tibble(
    model     = paste0('NWM', urls$version[i]),
    comid     = urls$COMID[i],
    dateTime  = hour_seq(urls$startDate[i], 
                     urls$endDate[i], 
                     "UTC"),
    flow_cms  = var.get.nc(dap, "streamflow",
                           start = c(urls$startIndex[i], urls$index[i]),
                           count = c(urls$count[i], 1),
                           unpack = TRUE)
  )
}


# library(RNetCDF)
# time_steps <- utcal.nc(
#   unitstring = att.get.nc(dap, 'time', "units"),
#   value = var.get.nc(dap, 'time'),
#   type = "c"
# )
# 
# table(time_steps) |> names() |> min()
# max(names(table(time_steps))) - length()
# 
# lubridate::ymd_hms(max(names(table(time_steps)))) - lubridate::hours(length(time_steps))

#' @title Time Sequence Generator
#' @description Construct time series from start and end data
#' @param startDate 
#' @param endDate 
#' @importFrom lubridate with_tz
#' @return vector of dates
#' @keywords internal

hour_seq  = function(startDate, endDate, tz){
  seq(
    from = with_tz(startDate, tzone = tz),
    to   = with_tz(endDate,    tzone = tz),
    by   = "hour"
  )
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs

NULL



