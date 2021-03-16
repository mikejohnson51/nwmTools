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
#' @importFrom dplyr group_by summarize_at mutate ungroup select
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
#' @importFrom dplyr rename
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
#' @importFrom dplyr rename
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
#' @importFrom dplyr mutate n
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
#' @importFrom dplyr mutate
#' @export
aggregate_ym  = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', "year", "month")
    nwmHistoric_agg(rawData, cols, fun, na.rm) %>% 
    mutate(ym = ymd(paste(year, month, sep = "-"), truncated = 1)) 
}

#' @title Aggregate by Year-Julien Day
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @importFrom dplyr mutate
#' @export
aggregate_yj  = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', "year", "julian")
  nwmHistoric_agg(rawData, cols, fun, na.rm) %>% 
    mutate(yj = paste(year, julian, sep = "-")) 
}

#' @title Aggregate by Julien Day
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @importFrom dplyr mutate
#' @export
aggregate_j  = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', "julian")
  nwmHistoric_agg(rawData, cols, fun, na.rm) %>% 
    mutate(j = paste(julian, sep = "-")) 
}

#' @title Aggregate by Year-Month-Day
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate
#' @export
aggregate_ymd = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', "year", "month", "day")
    nwmHistoric_agg(rawData, cols, fun, na.rm) %>%
    mutate(ymd = ymd(paste(year, month, day, sep = "-"))) 
}

#' @title Aggregate by Year-Season
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @importFrom dplyr mutate
#' @export
aggregate_ys  = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', 'year', 'season')
  nwmHistoric_agg(rawData, cols, fun, na.rm) %>%
    mutate(ys = paste0(season, "-", year))
}

#' @title Aggregate by Water Year - Month
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate
#' @export
aggregate_wym  = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', "wy", "month")
  nwmHistoric_agg(rawData, cols, fun, na.rm) %>% 
    mutate(wym = ymd(paste(wy, month, '01', sep = "-"))) 
}

#' @title Aggregate by Water Year - Month - Day
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate
#' @export
aggregate_wymd = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', "wy", "month", "day")
  nwmHistoric_agg(rawData, cols, fun, na.rm) %>%
    mutate(wymd = ymd(paste(wy, month, day, sep = "-"))) 
}

#' @title Aggregate by Water Year - Season
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @importFrom dplyr mutate
#' @export
aggregate_wys  = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', 'wy', 'season')
  nwmHistoric_agg(rawData, cols, fun, na.rm) %>%
    mutate(wys = paste0(season, "-", wy))  
}

#' @title Aggregate by DOWY
#' @inherit aggregate_record description
#' @inherit aggregate_record details
#' @inherit aggregate_record examples
#' @inheritParams aggregate_record
#' @family aggregate functions
#' @importFrom dplyr mutate
#' @export
aggregate_dowy  = function(rawData, fun = "mean", na.rm = TRUE){
  cols = c("model", 'comid', 'DOWY')
  nwmHistoric_agg(rawData, cols, fun, na.rm) %>% 
    mutate(wy = add_waterYear(rawData$time))
}



