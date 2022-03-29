#' Aggregate NWM data usning column and function input
#' @param rawData raw data returned from readNWMdata
#' @param cols a vector of colum names to group by
#' @param fun a function of list of functions to apply to group data 
#' @param na.rm logical. Should NA values be removed before applying fun
#' @importFrom dplyr group_by_at add_tally summarize_at vars ungroup `%>%` 
#' @return an aggregated data.frame
#' @keywords internal

nwmHistoric_agg = function(rawData, cols, fun, na.rm = TRUE ){
 
  time_col = grep("dateTime", names(rawData), value = TRUE)
  flow_col = grep("flow_cms", names(rawData), value = TRUE)
  
  if(length(time_col) != 1 | length(flow_col) != 1){
    stop("rawData must have a flow and time column")
  }
  
  df = split_time(rawData, time_col = time_col)
  
  if(na.rm){ df = df[!is.na(flow_col),]}
  cols = cols[cols %in% names(df)]

  group_by_at(df, cols)  %>%  
    add_tally(name = 'obs') %>%  
    summarize_at(vars(flow_col, 'obs'), fun)  %>%  
    ungroup()
}

#' @title Split Y-M-D-H into time components
#' @param rawData rawData with time column
#' @param time_col the column name holding dateTime
#' @return data.frame with added time components
#' @importFrom lubridate year month day hour yday
#' @export

split_time = function(rawData, time_col){
         time   = rawData[[time_col]] 
         rawData$year   = year(time)
         rawData$month  = month(time)
         rawData$day    = day(time)
         rawData$hour   = hour(time)
         rawData$season = add_season(time)
         rawData$wy     = add_waterYear(time)
         rawData$julian = as.numeric(format(time, "%j"))
         rawData$DOWY = yday(time) + ifelse(month(time) >=10, -273, 92)

  rawData
}

#' Add Season Column 
#' @param rawData raw data returned from readNWMdata
#' @return rawData with added season column
#' @keywords internal

add_season = function(dateVec){
  m = as.numeric(month(dateVec))
  c = m
  c[m <= 2] <- "Winter"
  c[m >= 3 & m <= 5] <- "Spring"
  c[m >= 6 & m <= 8] <- "Summer"
  c[m >= 9 & m <= 11] <- "Fall"
  c[m >= 12] <- "Winter"
  return(c)
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