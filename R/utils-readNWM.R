#' NWM metadata
#' @param version Which version of NWM should be returned? (1.2, 2.0, 2.1)
#' @return data.frame
#' @export
 
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
                      'nwm_v21_retro_full.ncml'),
             varname = c('feature_ids_v12', 
                          'feature_ids_v20',
                          'feature_ids_v21'))
  
  if(is.null(version)){
    return(df)
  } else {
    df2 = df[df$version %in% version,]
    
    if (nrow(df2) == 0) {
      stop(paste('NWM version must be one of:', paste(df$version, collapse = ", " )) , call. = F)
    }
    df2
  }
}

#' @title Get TDS path
#' @param type what TDS to use?
#' @return a URL path
#' @keywords internal

get_tds = function(type = "hydroshare"){
  
  nwm = unlist(lapply(1:length(type), function(x){
    if(type[x] == "hydroshare"){
    ## Top level Folder (OpenDap archive)
    #'http://thredds.hydroshare.org/thredds/dodsC/nwm_retrospective/'
    #'# CHANGED in March 2021
    'http://thredds.hydroshare.org/thredds/dodsC/nwm/retrospective/'
  } else {
    'https://cida.usgs.gov/thredds/dodsC/demo/morethredds/nwm/'
  }
  }))
  
}

#' @title Meta Object Creation with Error Checks
#' @description this function ensures that the all input objects are correct.
#' User input is modified to the desired TZ, model versions are checked, and the NCML file path returned
#' @param startDate a user defined startDate ("YYYY-MM-DD") 
#' @param endDate a user defined end Date ("YYYY-MM-DD") 
#' @param tz a user defined timezone  
#' @param version a user defined model version
#' @importFrom lubridate with_tz hours
#' @return a list containing the model version, NCML path, and time requests
#' @keywords internal
#' @noRd

error.checks = function(startDate, endDate, tz, version){
  
  this.version = get_nwm_meta(version)
  
  if(!is.null(startDate)){
    if(nchar(startDate) == 10) { startDate = paste(startDate, "00:00:00") }
  }
  
  if(!is.null(endDate)){
    if(nchar(endDate)   == 10) { endDate   = paste(endDate,   "23:00:00") }
  }

  ## Make sure requested Timezone is valid
  if(!tz %in% OlsonNames()){ 
    stop(paste(tz, "not recognized timezone"), call. = FALSE) 
  } 
  
  if(!is.null(startDate)) {startDate = as.POSIXct(startDate, tz = tz)}
  if(!is.null(endDate))   {endDate   = as.POSIXct(endDate, tz = tz)  }
  
  if(is.null(startDate) & is.null(endDate)){
    startDate = with_tz(this.version$minDate, "UTC")
    endDate   = with_tz(this.version$maxDate, "UTC")
  }
  
  if(is.null(startDate)){
    startDate = with_tz(this.version$minDate, "UTC")
  }
  
  if(is.null(endDate)){
    endDate = with_tz(startDate, "UTC") + hours(23)
  }
  
  if(endDate < startDate){
    stop("endDate must come after startDate")
  }

  ## Change Start and EndDate to User Timezones:
  df = data.frame(usr.tz.start  = with_tz(startDate, tz),
                  usr.tz.end    = with_tz(endDate, tz))
  df$usr.utc.start = with_tz(df$usr.tz.start, tzone = "UTC")
  df$usr.utc.end = with_tz(df$usr.tz.end, tzone = "UTC")
  df$version = this.version$version
  df$url = paste0(get_tds(this.version$type), this.version$ncml)
  df$minDate = this.version$minDate
  df$maxDate = this.version$maxDate
  df$varname = this.version$varname
  
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
#' @importFrom lubridate ymd_hm with_tz
#' @importFrom RNetCDF open.nc var.get.nc
#' @return a data.frame and opend DAP object
#' @keywords internal
#' @noRd

retro_call = function(comid, meta.obj){

  comid = comid[!is.na(comid)]
  
  if(length(comid) > 0){
  
  ids_file = open.nc(system.file("extdata", "retro_feature_ids.nc", package = "nwmTools"))
  id  = match(comid, var.get.nc(ids_file, meta.obj$varname))
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
#' @return
#' @keywords internal
#' @noRd

extract_thredds = function(i, urls, dap) {
  data.frame(
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
#' @noRd

hour_seq  = function(startDate, endDate, tz){
  seq(
    from = with_tz(startDate, tzone = tz),
    to   = with_tz(endDate,    tzone = tz),
    by   = "hour"
  )
}



