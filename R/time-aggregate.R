#' @title Aggregate by Record 
#' @details NWM data is extracted as hourly values. 
#' 
#' To aggregate hourly data to different time chunks the nwmHistoric package offers a family of aggregate functions. 
#' 
#' Each of these begins with the prefix 'aggregate_' and is followed by the date symbol to aggregate to.
#'
#' | Symbol        | Aggregate           | 
#' | ------------- |:-------------:| 
#' | y      | year | 
#' | m      | month  |   
#' | d      | day | 
#' | j      | julien day |  
#' | s      | season | 
#' | wy      | water year  |   
#' @md
#' @param rawData data extracted with \code{readNWMdata} 
#' @param fun 	function to be applied to the flows column default  = 'mean'
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @importFrom lubridate year month day
#' @family aggregate functions
#' @examples 
#' \dontrun{
#' # Get flow record for COMID 101
#' flows = readNWMdata(comid = 101)
#' 
#' # Aggregate to yearly average (y)
#' yearly = aggregate_y(flows, fun = 'mean')
#' 
#' # Aggregate to monthly
#' # minimum and maximum per year (ym)
#' ym = aggregate_ym(flows, fun = list(min, max))
#' 
#' # Aggregate to seasonal 95th percetile 
#' # with using custom function
#' s95 = aggregate_s(flows, fun = function(x){quantile(x,.95)})
#' }
#' @export

aggregate_record   = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid')
  nwmHistoric_agg(rawData, cols, fun, na.rm)
}

#' @title Aggregate by Year
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @export

aggregate_y   = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', "year")
  nwmHistoric_agg(rawData, cols, fun, na.rm)
}

#' @title Aggregate by Month
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @export
aggregate_m   = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', "month")
  nwmHistoric_agg(rawData, cols, fun, na.rm) 
}

#' @title Aggregate by Julien Day
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @export
aggregate_j   = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', 'julian')
  nwmHistoric_agg(rawData, cols, fun, na.rm) 
}

#' @title Aggregate by Season
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @export
aggregate_s   = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', 'season')
  nwmHistoric_agg(rawData, cols, fun, na.rm) 
}

#' @title Aggregate by Water Year
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @export
aggregate_wy  = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', 'wy')
  nwmHistoric_agg(rawData, cols, fun, na.rm) 
}

#' @title Aggregate by Year-Month
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @importFrom lubridate ymd
#' @export
aggregate_ym  = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', "year", "month")
  out = nwmHistoric_agg(rawData, cols, fun, na.rm)
  out$ym = ymd(paste(out$year, out$month, sep = "-"), truncated = 1)
  out
}

#' @title Aggregate by Year-Julien Day
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @export
aggregate_yj  = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', "year", "julian")
  out = nwmHistoric_agg(rawData, cols, fun, na.rm)
  out$yj = paste(out$year, out$julian, sep = "-")
  out
}

#' @title Aggregate by Julien Day
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @export
aggregate_j  = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', "julian")
  out = nwmHistoric_agg(rawData, cols, fun, na.rm)
  out$j = paste(out$julian, sep = "-")
  out
}

#' @title Aggregate by Year-Month-Day
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @importFrom lubridate ymd
#' @export
aggregate_ymd = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', "year", "month", "day")
  out = nwmHistoric_agg(rawData, cols, fun, na.rm)
  out$ymd = ymd(paste(out$year, out$month, out$day, sep = "-"))
  out
}

#' @title Aggregate by Year-Season
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @export
aggregate_ys  = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', 'year', 'season')
  out = nwmHistoric_agg(rawData, cols, fun, na.rm)
  out$ys = paste0(out$season, "-", out$year)
  out
}

#' @title Aggregate by Water Year - Month
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @importFrom lubridate ymd
#' @export
aggregate_wym  = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', "wy", "month")
  out = nwmHistoric_agg(rawData, cols, fun, na.rm)
  out$wym = ymd(paste(out$wy, out$month, '01', sep = "-"))
  out
}

#' @title Aggregate by Water Year - Month - Day
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @importFrom lubridate ymd
#' @export
aggregate_wymd = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', "wy", "month", "day")
  
  out = nwmHistoric_agg(rawData, cols, fun, na.rm)
  out$wymd = ymd(paste(out$wy, out$month, out$day, sep = "-"))
  out
}

#' @title Aggregate by Water Year - Season
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @export
aggregate_wys  = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', 'wy', 'season')
  
  out = nwmHistoric_agg(rawData, cols, fun, na.rm)
  out$wys = paste0(out$season, "-", out$wy)
  out
}

#' @title Aggregate by DOWY
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @export
aggregate_dowy  = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', 'DOWY')
  
  out = nwmHistoric_agg(rawData, cols, fun, na.rm)
  out$wy = add_waterYear(rawData$time)
  out
}



