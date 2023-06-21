#' NWM metadata
#' @param version Which version of NWM should be returned? (1.2, 2.0, 2.1)
#' @return data.frame
#' @export
 
get_nwm_meta = function(version = NULL){
  
  t = nwmTools::tds_meta
  
  if(is.null(version)){
    return(t)
  } else {
    df2 = t[t$version %in% version,]
    
    if (nrow(df2) == 0) {
      stop(paste('NWM version must be one of:', paste(t$version, collapse = ", " )) , call. = F)
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
  
  nwm
  
}

#' @title Meta Object Creation with Error Checks
#' @description this function ensures that the all input objects are correct.
#' User input is modified to the desired TZ, model versions are checked, and the NCML file path returned
#' @param startDate a user defined startDate ("YYYY-MM-DD") 
#' @param endDate a user defined end Date ("YYYY-MM-DD") 
#' @param tz a user defined timezone  
#' @param version a user defined model version
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
    startDate = with_tz(this.version$startDate, "UTC")
    endDate   = with_tz(this.version$endDate, "UTC")
  }
  
  if(is.null(startDate)){
    startDate = with_tz(this.version$startDate, "UTC")
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
  df$minDate = this.version$startDate
  df$maxDate = this.version$endDate
  df$varname = this.version$varname
  
  ## Check startDate
  if(any(df$usr.utc.start < this.version$startDate)){
    stop("First values for version ", version, ' is ', this.version$startDate, " 00:00:00 UTC \n")
  }
  ## Check endDate
  if(any(df$usr.utc.end > this.version$endDate)){
    stop("Last values for version ", version, ' is ', this.version$endDate,  " UTC.")
  }

  
  # If everything is good-to-go return baseURL
  return(df)
}

#' @title Retro Call Meta Data
#' @description build out THREDDs call and meta information
#' @param comid an NHD COMID(s)
#' @param meta.obj a metadata object generated with `error.check`
#' @return a data.frame and DAP object
#' @keywords internal
#' @noRd

retro_call = function(comid, meta.obj){

  comid = comid[!is.na(comid)]
  
  if(length(comid) > 0){
    
    nc = open.nc(meta.obj$url)
    time   = utcal.nc(att.get.nc(nc,"time", "units"), var.get.nc(nc, 'time'), type = "c")
    feat   = var.get.nc(nc, 'feature_id')

    start  = match(meta.obj$usr.tz.start, time) 
    end    = match(meta.obj$usr.tz.end, time) 
    count  = end - start + 1
    
    comid = comid[comid %in% feat]
    
    ll = list()
    
    if(length(comid) > 0){
      for(i in 1:length(comid)){
        
        index  = match(comid[i], feat)
        
        if(is.na(index)){
          message("comid: ", comid[i], " not found.")
          ll[[i]] = NULL
        } else {
          ll[[i]] = data.frame(
            model     = paste0('NWM', meta.obj$version),
            comid     = comid[i],
            dateTime  = hour_seq(meta.obj$usr.tz.start, meta.obj$usr.tz.end, "UTC"),
            flow_cms  = var.get.nc(nc, "streamflow",
                                   start = c(start, index),
                                   count = c(count, 1),
                                   unpack = TRUE)
          )
        }
      }
    } else {
      return(NULL)
    }

    return(bind_rows(ll))
    
  
  } else {
    return(NULL)
  }
}

#' @title Extract OPENDAP data using meta file
#' @param i index of meta.calls data
#' @param urls meta.calls data.frame
#' @param dap open DAP connection
#' @return data.frame
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


#' @title Time Sequence Generator
#' @description Construct time series from start and end data
#' @param startDate 
#' @param endDate 
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



