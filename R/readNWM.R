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
#' @return data.frame
#' @export
#' @importFrom RNetCDF close.nc
#' @importFrom dplyr bind_rows
#' @importFrom lubridate with_tz tz as_datetime
#' @importFrom dataRetrieval findNLDI
#' @examples 
#' \dontrun{
#' readNWMdata(comid = 101)
#' readNWMdata(comid = 101, version = 1.2)
#' readNWM(comid = 101, tz = "US/Pacific")
#' }

readNWMdata = function(comid  = NULL,
                       siteID = NULL,
                       startDate = "1993-01-01",
                       endDate   = "2018-12-31",
                       tz = "UTC",
                       version = 2) {
  
  startDate = as.POSIXct(startDate, tz = tz)
  endDate   = as.POSIXct(endDate, tz = tz)
  
  base = error.checks(startDate, endDate, tz, version)
  
  if (!is.null(siteID)) {
    comid = dataRetrieval::findNLDI(nwis = siteID)$comid
  }
  
  rc  <- retro_call(comid, meta.obj = base)
  
  if (!is.null(rc)) {
    
    res <-  dplyr::bind_rows(lapply(1:rc$rows,
                                    FUN  = extract_thredds,  
                                    urls = rc$call.meta,  
                                    dap  = rc$open.dap.file))
    
    
    close.nc(rc$open.dap.file)
    
    res$dateTime = with_tz(res$dateTime, tz)
    
    if (!is.null(siteID)) { res$siteID = siteID }
    
    res
    
  } else {
    message("The requested feature ID is not in the NWM v", version, " archive.")
    NULL
  }
}

