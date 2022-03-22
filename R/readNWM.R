#' Check for a package
#' @param pkg package name
#' @noRd

check_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Package '",
         pkg,
         "' is required for this functionality, but is not installed. \nTry `install.packages('",
         pkg,
         "')`", call. = FALSE)
}


#' Internal NWM read 
#' @param comid a NHD common identifier
#' @param siteID a USGS NWIS site identifier (eight digits)
#' @param tz the desired timezone of the data. Can be found with \code{OlsonNames}
#' @param base NWM query metadata
#' @noRd

.readNWMinside = function(comid, siteID, tz, base){
  
  rc  <- retro_call(comid, meta.obj = base)
  
  if (!is.null(rc)) {

    res = do.call('rbind', lapply(1:rc$rows, FUN  = extract_thredds,  urls = rc$call.meta,  dap  = rc$open.dap.file))
    
    close.nc(rc$open.dap.file)
    
    res$dateTime = with_tz(res$dateTime, tz)
    
    if (!is.null(siteID)) { res$siteID = siteID }
    
    res
    
  } else {
    message("The requested feature ID is not in the NWM v", base$version, " archive.")
    NULL
  }
  
}

#' @title NWM Reanalysis Extraction
#' @description Download hourly flow values for an NHD COMID from the National 
#' Water Model version 1.2 or 2.0. Returned data is available between 
#' "1993-01-01 00" and "2017-12-31 23" but can be 
#' subset using a startDate and endDate.
#' @param comid a NHD common identifier
#' @param siteID a USGS NWIS site identifier (eight digits)
#' @param startDate a start date (YYYY-MM-DD) or (YYYY-MM-DD HH)
#' @param endDate an end date (YYYY-MM-DD) or (YYYY-MM-DD HH)
#' @param tz the desired timezone of the data. Can be found with \code{OlsonNames}
#' @param version the NWM version to extract (current = 1.2 or 2 (default))
#' @param addObs should observation data be added? Only available when !is.null(siteID)
#' @return data.frame
#' @export
#' @importFrom RNetCDF close.nc
#' @importFrom lubridate with_tz tz as_datetime
#' @importFrom httr content RETRY
#' @examples 
#' \dontrun{
#' readNWMdata(comid = 101)
#' readNWMdata(comid = 101, version = 1.2)
#' readNWMdata(comid = 101, tz = "US/Pacific")
#' }

readNWMdata = function(comid  = NULL,
                       siteID = NULL,
                       startDate = NULL,
                       endDate   = NULL,
                       tz = "UTC",
                       version = 2.1,
                       addObs = FALSE) {
  
  if(is.null(siteID) & addObs){
    stop("addObs is only avaliable when siteID is not NULL")
  }
 
  base = do.call('rbind', lapply(1:length(version), function(x) {
    error.checks(startDate, endDate, tz, version[x])
    })
  )
  
  if (!is.null(siteID)) { 
    check_pkg('dataRetrieval')
    comid = unique(c(comid, dataRetrieval::findNLDI(nwis = siteID)$comid))
    cols = c('comid', 'dateTime', "siteID")
  } else {
    cols = c('comid', 'dateTime')
  }

  res = lapply(1:nrow(base), function(x) { 
    .readNWMinside(comid, siteID, tz, base = base[x,]) 
  })
  
  if(length(res) == 1){
    res = res[[1]]
    res[[paste0("flow_cms_v", gsub("NWM", "", res$model[1]))]] = res$flow_cms
    res$flow_cms = NULL
    res$model = NULL
  } else {
    out = lapply(1:length(res), function(x){
      res[[x]][[paste0("flow_cms_v", gsub("NWM", "", res[[x]]$model[1]))]] = res[[x]]$flow_cms
      res[[x]]$flow_cms = NULL
      res[[x]]$model = NULL
      res[[x]]
    })

    
    res = Reduce(function(dtf1,dtf2) merge(dtf1, dtf2, by = cols), out )
  }
  
  if(addObs){
    check_pkg('dataRetrieval')
    nwis = dataRetrieval::readNWISdv(siteNumbers = unique(res$siteID), parameterCd = "00060")
    
    if(nrow(nwis) > 0){ 
      nwis = dataRetrieval::renameNWISColumns(nwis)
      nwis = nwis[, c("site_no",'Flow', "Date")]
      nwis$Flow = nwis$Flow * 0.028316847
      names(nwis) = c('siteID', "flow_cms_nwis", "Date")
      
      res$Date = as.Date(with_tz(res$dateTime, tzone = "UTC"))
      
      res = merge(res, nwis, all.x = TRUE, by = c('siteID', "Date"))
      
      res$Date = NULL
    }
  } 
  
  res
 
}

