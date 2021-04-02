#' @title Meta Object Creation with Error Checks
#' @description this function ensures that the all input objects are correct.
#' User input is cohersed to the desired TZ, model versions are checked, and the NCML file path returned
#' @param startDate a user defined startDate ("YYYY-MM-DD") 
#' @param endDate a user defined end Date ("YYYY-MM-DD") 
#' @param tz a user defined timezone  
#' @param version a user defined model version
#' @importFrom lubridate ymd_hm with_tz
#' @importFrom dplyr filter add_row
#' @return a list containing the model version, NCML path, and time requests
#' @keywords internal

error.checks = function(startDate, endDate, tz, version){
  
  ## Make sure requested Timezone is valid
  if(!tz %in% OlsonNames()){ 
    stop(paste(tz, "not recognized timezone"), call. = F)
  } 
  
  ## Top level Folder (OpenDap archive)
  # CHANGED in March 2021
  parent <- 'http://thredds.hydroshare.org/thredds/dodsC/nwm/retrospective/'
    #'http://thredds.hydroshare.org/thredds/dodsC/nwm_retrospective/'
  
  ## Reanalysis MetaData (update this with each new release!!)  
  reanalysis.meta = data.frame(version = 1.2, 
                               minDate = ymd_hm("1993-01-01 00:00"), 
                               maxDate = ymd_hm("2017-12-31 23:00"),
                               ncml = 'nwm_retro_full.ncml') %>% 
    # Version 2.0
    dplyr::add_row(data.frame(version = 2.0, 
                              minDate = ymd_hm("1993-01-01 00:00"), 
                              maxDate = ymd_hm("2018-12-31 23:00"),
                              ncml = 'nwm_v2_retro_full.ncml'))
  
  ## Make sure version is available
  if (!version %in% reanalysis.meta$version) {
    stop(paste('NWM version must be either',
               paste(reanalysis.meta$version, collapse = ", ")), 
         call. = F)
  }
  
  ## Isolate version of interest
  this.version = filter(reanalysis.meta, version == !!version)
  
  # If no endDate is given, default to start date + 23 hours
  if(is.null(endDate)){endDate = startDate + 82800}
  
  ## Change Start and EndDate to User Timezones:
  df = data.frame(usr.tz  = with_tz(c(startDate, endDate), tz))
  df$usr.utc = with_tz(df$usr.tz, tzone = "UTC")
  
  ## Check startDate
  if(any(df$usr.utc[1] < this.version$minDate)){
    message("First forecast for version ", version, ' is\n',
            this.version$minDate, " 00:00 UTC \n",
            "changing requested startDate to:\n",
            with_tz(this.version$minDate, tz), " ", tz)
    df$usr.tz[1] = with_tz(this.version$minDate, tzone = tz)
  }
  ## Check endDate
  if(any(df$usr.utc[2] > this.version$maxDate)){
    message("Last forecast for version ", version, 
            ' is\n', this.version$maxDate,  " UTC \n",
            "changing requested endDate to:\n",
            with_tz(this.version$maxDate, tzone = tz), " ", tz)
    
    df$usr.tz[2] = with_tz(this.version$maxDate, tzone = tz)
  }
  
  df$usr.utc = with_tz(df$usr.tz, tzone = "UTC")
  df$model.utc = c(this.version$minDate, this.version$maxDate)
  
  # If everything is good-to-go return baseURL
  return(
    list(
      version = version,
      baseURL = paste0(parent, this.version$ncml),
      time.requests = df))
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
  call = ifelse(meta.obj$version == 2, "feature_ids_v_two", "feature_ids_v_one")
  id  = match(comid, var.get.nc(ids_file, call))
  close.nc(ids_file)

  if(length(id) > 0){
  ## Model
    all = hour_seq(min(meta.obj$time.requests$model.utc), 
                   max(meta.obj$time.requests$model.utc), "UTC")
  ## User
    s  = min(meta.obj$time.requests$usr.utc)
    e  = max(meta.obj$time.requests$usr.utc)
    s1 = which(s == all)
    e1 = which(e == all)

    call.meta = data.frame(
              COMID = comid,
              index = id,
              startDate  = s,
              endDate    = e,
              startIndex = s1, 
              endIndex   = e1,
              count = (e1-s1)  + 1,
              version = meta.obj$version,
              stringsAsFactors = FALSE) %>% 
      filter(!is.na(index))

   
    if(nrow(call.meta) > 0 ){
      return(list(rows = nrow(call.meta),
                  call.meta = call.meta, 
                  open.dap.file = open.nc(meta.obj$baseURL)))
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



