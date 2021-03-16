#' Aggregate NWM data usning column and function input
#' @param rawData raw data returned from readNWMdata
#' @param cols a vector of colum names to group by
#' @param fun a function of list of functions to apply to group data 
#' @param na.rm logical. Should NA values be removed before appling fun
#' @param season logical. Should season be added
#' @param waterYear logical. Should water year be added?
#' @importFrom dplyr mutate filter group_by_at add_tally summarize_at ungroup vars
#' @return an aggregated data.frame
#' @keywords internal

nwmHistoric_agg = function(rawData, cols, fun, na.rm = TRUE ){
 
  time_col = grep("dateTime", names(rawData), value = TRUE)
  flow_col = grep("flow_csm", names(rawData), value = TRUE)
  
  if(length(time_col) != 1 | !length(flow_col) != 1){
    stop("rawData must have a flow and time column")
  }
  
  df = split_time(rawData, time_col = time_col)
  
  if(na.rm){df = filter(df, !is.na(flow_cms))}
  
  group_by_at(df, cols) %>% 
    add_tally(name = 'obs') %>% 
    summarize_at(dplyr::vars(flow_cms, obs), fun)  %>% 
    ungroup()
}

#' @title Split Y-M-D-H into time components
#' @param rawData rawData with time column
#' @param time_col the column name holding dateTime
#' @return data.frame with added time components
#' @importFrom dplyr mutate
#' @importFrom lubridate year month day hour yday
#' @export

split_time = function(rawData, time_col){
  mutate(rawData,
         time   = get(time_col), 
         year   = year(time),
         month  = month(time),
         day    = day(time),
         hour   = hour(time),
         season = add_season(time),
         wy     = add_waterYear(time),
         julian = as.numeric(format(time, "%j")),
         DOWY = yday(time) + ifelse(month(time) >=10, -273, 92))
}

#' Add Season Column 
#' @param rawData raw data returned from readNWMdata
#' @return rawData with added season column
#' @keywords internal
#' @importFrom dplyr recode mutate

add_season = function(dateVec){
  
  recode(month(dateVec), 
                    `1`="Winter", `2`="Winter",
                    `3`="Spring", `4`="Spring", `5`="Spring",
                    `6`="Summer", `7`="Summer", `8`="Summer",
                    `9`="Fall", `10`="Fall", `11`="Fall",
                    `12`="Winter")

}

#' Add Water Year Column 
#' @param dateVec raw data returned from readNWMdata
#' @return vector of water years
#' @importFrom lubridate year month
#' @export

add_waterYear = function(dateVec){
    calYear <-  year(dateVec)
    calMon  <-  month(dateVec)
    whichPastOct <- calMon >= 10
    waterYear <- calYear
    waterYear[whichPastOct] <- calYear[whichPastOct] + 1
    return(waterYear)
}