nwm_filter = function(source, version = NULL, config = NULL, ensemble = NULL, 
                      output= NULL, domain = NULL, date = NULL){
  
  
  validate = function(complete, field, value){
    
    if(field %in% names(complete) & !is.null(value)){
      opts = unique(complete[[field]])
      
      if(any(grepl(value, opts))){
        return(filter(complete, grepl(!!value, get(field))))
      } else {
        stop(glue("{value} not a valid {field}. Choose from: {paste(opts, collapse = ', ')}"))
      }
    } else {
      return(complete)
    }
  }
  
  # Source ------------------------------------------------------------------
  
  meta = validate(nwm_data, "source", source) %>% 
         validate("domain", domain) %>% 
         validate("version", version) %>% 
         validate("output", output) %>% 
         validate("config", config) %>% 
         validate("ensemble", ensemble) 
  

  # Date ------------------------------------------------------------------ 
  if(!is.null(date)){
    
    meta = meta %>% 
      mutate(stateDate = ifelse(startDate == "..", 
                                "1900-01-01",
                                startDate),
             endDate = ifelse(endDate == "..",  as.character(Sys.Date()), endDate))
    
    tmp = filter(meta, as.POSIXct(date, tz = "UTC") >= as.POSIXct(startDate, tz = "UTC"),
                 as.POSIXct(date, tz = "UTC") <= as.POSIXct(endDate, tz = "UTC"))
    
    if(nrow(tmp) == 0){ stop("date not within allowed range: ", meta$startDate, "--", meta$endDate) } else { meta = tmp }
  }
  
  
  if(nrow(meta) > 1){warning('More then one viable source found...', call. = FALSE)}
  
  meta
}


#' @title Error Checking for configuration/ensemble pairs
#' @param type a NWM configuration
#' @param ensemble an ensemble member number
#' @return a kosher configuration
#' @noRd
#' @keywords internal

error_checking_type = function(type, ensemble){
  
  current_type =  c('analysis_assim', 
                    'analysis_assim_extend',
                    'analysis_assim_long',
                    'analysis_assim_hawaii',
                    'long_range',
                    'medium_range',
                    'short_range',
                    'short_range_hawaii')
  
  if(!type %in% current_type){
    stop("type must be one of:\n\t ", paste(current_type, collapse = ",\n\t "), call. = F)
  }
  
  if(type %in% c('long_range', 'medium_range')){
    if(is.null(ensemble)){
      stop("Ensemble member needed for ", type, ": member 1 selected", call. = F)
      ensemble <- 1
    }
    
    if(type == "medium_range"){
      check = ensemble %in% c(1:7)
      if(!check){
        stop('Only 7 medium range ensembles available', call. = FALSE)
      }
    }
    
    if(type == "long_range"){
      check = ensemble %in% c(1:4)
      if(!check){
        stop('Only 4 long range ensembles available', call. = FALSE)
      }
    }
    
    type = paste0(type, "_mem", ensemble)
  }
  
  return(type)
  
}

error_checking_hour = function(type, hours){
  
  if(type == "short_range" & dplyr::between(hour, 0, 23)){
    return(sprintf("%02s", hour))
  } else {
    stop("short range hours must be between 0:23")
  }
  
  if(type == "medium_range" & type %in% c(0,6,12,18)){
    return(sprintf("%02s", hour))
  } else {
    stop("medium range hours must be 0,6,12 or 18")
  }
}

error_checking_num = function(type, num, pad){
  
  if(type == "short_range" & num <= 18){
    return(sprintf(paste0("%0", pad, "s"), 1:num))
  } else {
    stop("short range steps must be <= 18")
  }
  
  if(type == "medium_range" & num <= 80){
    return(sprintf(paste0("%0", pad, "s"), 1:num*3))
  } else {
    stop("medium range steps must be <= 18")
  }
}

error_checking_date = function(date){
  
  date = as.Date(date)
  if(date > as.Date('2018-09-17') & date < Sys.Date()){
    return(gsub("-", "", date))
  } else {
    stop("Date not valid")
  }
}

#' Get GCP file list
#' @param config 
#' @param domain 
#' @param date 
#' @param hour 
#' @param minute 
#' @param num 
#' @param ensemble 
#' @param output 
#'
#' @return character vector
#' @export
#' @importFrom dplyr filter mutate
#' @importFrom glue glue

get_gcp_urls = function(config = "short_range",
                        domain = "conus",
                        date, 
                        hour = "00",
                        minute = "00",
                        num, 
                        ensemble = NA, 
                        output = "channel_rt"){
  
  meta = nwm_filter(source = "gcp",
                    version = NULL, 
                    config = config, 
                    date = date,
                    ensemble = ensemble, 
                    output = output,
                    domain = domain)
  
  
  dates = seq.POSIXt(as.POSIXlt(paste(date, hour, minute), tz = 'UTC'), 
                     length.out = num  + 1, 
                     by = paste(meta$timestep, ifelse(meta$timestep > 10, "minutes", "hours")))

  YYYYDDMM = format(dates, "%Y%m%d")[2:length(dates)]
  forward = sprintf("%03s", format(dates, "%H"))[2:length(dates)]
  
  urls = glue(meta$http_pattern,
       YYYYDDMM = YYYYDDMM,
       config = meta$config[1],
       HH = hour,
       foward = forward,
       output = meta$output[1],
       domain = meta$domain,
       ensemble = meta$ensemble[1], 
       prefix = meta$prefix[1])
  
  dates  = lubridate::ymd_hm(paste0(date[1], hour, "00")) + lubridate::hours(0:(num-1))

  data.frame(dateTime = dates, 
             urls      = urls,
             output    = output)
}

get_aws_filelist = function(version = 2.1, 
                            output  = "CHRTOUT", 
                            config  = NULL,
                            ensemble  = NULL,
                            date    = "2010-10-29", 
                            hour    = "00", 
                            minute  = "00",
                            num      = 3){
  
  meta = nwm_filter(source = "aws", 
                    version = version, 
                    config = config, 
                    output = output,
                    ensemble = NULL, 
                    domain = NULL)
  
  dates = seq.POSIXt(as.POSIXlt(paste(date, hour, minute), tz = 'UTC'), 
                     length.out = num, 
                     by = paste(meta$timestep, ifelse(meta$timestep > 10, "minutes", "hours")))

  urls = glue(meta$http_pattern,
       bucket = meta$bucket,
       config = meta$config,
       output = meta$output,
       YYYY = format(dates, "%Y"),
       YYYYMMDDHHMM = format(dates, "%Y%m%d%H%M"))
  
  data.frame(dateTime = dates, 
             urls      = urls,
             output    = output)
}



#' Get NOMADs File List
#'
#' @param config 
#' @param domain 
#' @param date 
#' @param hour 
#' @param minute 
#' @param num 
#' @param ensemble 
#' @param output 
#' @param version 
#'
#' @return character vector
#' @export
#' @importFrom rvest html_attr html_elements
#' @importFrom xml2 read_html

get_nomads_filelist2 = function(config = "short_range",
                                domain = "conus",
                                date = NULL, 
                                hour = NULL,
                                minute = "00",
                                num, 
                                ensemble = NULL, 
                                output = "channel_rt",
                                version = "prod") {
  
  
  meta = nwm_filter(source = "nomads", 
                    version = version, 
                    config = config, 
                    output = output,
                    ensemble = ensemble, 
                    domain = domain)
  
  meta = meta[1,]

  tmp     = glue(strsplit(meta$http_pattern, 'nwm\\.')[[1]][1])
  
  dates   = html_attr(html_elements(read_html(tmp), "a"), "href")
  dates = gsub("/", "", gsub("nwm.", "", dates))

  if(is.null(date)){ 
    YYYYMMDD = tail(dates,1)
    YYYYMMDD2 = tail(dates, 2)[1]
  } else if(!gsub("-", "", date) %in% dates){
    stop(date, " not avaliable")
  } else {
    YYYYMMDD = gsub("-", "", date)
    YYYYMMDD2 = gsub("-", "", date)
  }
  
  
  files   = tryCatch({
    date <<-YYYYMMDD
    html_attr(html_elements(read_html(glue('{tmp}/nwm.{YYYYMMDD}/{meta$bucket}')), "a"), "href")
  }, error = function(e){
    date <<-YYYYMMDD2
    html_attr(html_elements(read_html(glue('{tmp}/nwm.{YYYYMMDD2}/{meta$bucket}')), "a"), "href")
  })
  
  files  = grep(meta$output, files, value = TRUE)
  files  = grep(".nc$", files, value = TRUE)
  
  if(is.null(hour)){
    current = strsplit(tail(files, 1), "\\.")[[1]][[2]]
    hour = gsub("t", "", gsub("z", "", current))
  } 

  files  = grep(paste0("t", hour, "z"), files, value = TRUE)[1:num]
  dates  = lubridate::ymd_hm(paste0(date, hour, "00")) + lubridate::hours(0:(num-1))
  
  data.frame(dateTime = dates, 
             urls     =  glue('{tmp}/nwm.{date}/{meta$bucket}/{files}'),
             output   = output)

}
 
