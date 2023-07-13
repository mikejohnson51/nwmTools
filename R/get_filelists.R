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

nwm_filter = function(source, version = NULL, config = NULL, ensemble = NULL, 
                      output= NULL, domain = NULL, date = NULL){
  
  startDate <- endDate <- NULL
  
  # Source ------------------------------------------------------------------
  
  meta = validate(nwmTools::nwm_data, "source", source) %>% 
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

#' Get GCP file list
#' @param config the NWM model configurarion
#' @param domain the NWM model domain
#' @param date date of interest
#' @param hour hour of interest
#' @param minute minute of interest
#' @param num number of files to get (forward from provides data-hour-minute)
#' @param ensemble the NWM ensemble number
#' @param output the NWM model output type
#' @return data.frame
#' @export

get_gcp_urls = function(config = "short_range",
                        domain = "conus",
                        date, 
                        hour = "00",
                        minute = "00",
                        num, 
                        ensemble = NULL, 
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
  
  dates  = ymd_hm(paste0(date[1], hour, "00")) + hours(0:(num-1))

  df = data.frame(dateTime = dates, 
             urls      = urls,
             output    = output)
  
  if(!is.null(outdir)){
    df = download_files(df,  outdir = outdir)
  }
  
  return(df)
}

#' Get GCP file list
#' @inheritParams get_gcp_urls
#' @param version NWM model version
#' @return data.frame
#' @export

get_aws_urls = function(version = 2.1, 
                            output  = "CHRTOUT", 
                            config  = NULL,
                            ensemble  = NULL,
                            date    = "2010-10-29", 
                            hour    = "00", 
                            minute  = "00",
                            num      = 3,
                            outdir = NULL){
  
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
  
  df = data.frame(dateTime = dates, 
             urls      = urls,
             output    = output)
  
  if(!is.null(outdir)){
    df = download_files(df,  outdir = outdir)
  }
  
  return(df)
}



#' Get NOMADs File List
#' @inheritParams get_gcp_urls
#' @param version server version (prod or para)
#' @return data.frame
#' @export


get_nomads_urls = function(config = "short_range",
                           domain = "conus",
                           date = NULL, 
                           hour = NULL,
                           minute = "00",
                           num, 
                           ensemble = NULL, 
                           output = "channel_rt",
                           version = "prod",
                           outdir = NULL) {
  
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
    date <<- YYYYMMDD
    html_attr(html_elements(read_html(glue('{tmp}/nwm.{YYYYMMDD}/{meta$bucket}')), "a"), "href")
  }, error = function(e){
    date <<- YYYYMMDD2
    html_attr(html_elements(read_html(glue('{tmp}/nwm.{YYYYMMDD2}/{meta$bucket}')), "a"), "href")
  })
  
  files  = grep(meta$output, files, value = TRUE)
  files  = grep(".nc$", files, value = TRUE)
  
  if(is.null(hour)){
    current = strsplit(tail(files, 1), "\\.")[[1]][[2]]
    hour = gsub("t", "", gsub("z", "", current))
  } 

  files  = grep(paste0("t", hour, "z"), files, value = TRUE)[1:num]
  dates  = ymd_hm(paste0(date, hour, "00")) + hours(0:(num-1))
  
  df = data.frame(dateTime = dates, 
             urls     =  glue('{tmp}/nwm.{date}/{meta$bucket}/{files}'),
             output   =  output)
  
  if(!is.null(outdir)){
    df = download_files(df,  outdir = outdir)
  }
  
  return(df)

}
 
