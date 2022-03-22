#' @title make NOMADS url from date and configuration
#' @param date a data in form (YYY-MM-DD)
#' @param type a NWM configuration
#' @return a url path
#' @noRd
#' @keywords internal

make_url = function(date, type){
  base =  "http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/prod/nwm."
  paste0(base, gsub("-", "", date), "/", type, "/")
}

#' @title Error Checking for configuration/ensemble pairs
#' @param type a NWM configuration
#' @param ensemble an ensemble member number
#' @return a kosher configuration
#' @noRd
#' @keywords internal

error_checking = function(type, ensemble){
  
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
      check = ensemble %in% c(1:4)
      if(!check){
        stop('Only 4 medium range ensembles available', call. = FALSE)
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

#' @title Define 'f'
#' @param type a NWM configuration
#' @param files a file list
#' @param domain a domain (conus or hawaii)
#' @return a vector of f values.
#' @noRd
#' @keywords internal

find_f = function(type, files, domain){
  if(grepl("analysis_assim", type)){
    pattern = paste0('^.*tm\\s*|\\s*.', domain ,'.*$')
  } else {
    pattern = paste0('^.*f\\s*|\\s*.', domain ,'.*$')
  }
  as.numeric(gsub(pattern, '', files))
}

#' @title Build file metadata for most current NWM configuration
#' @param type a NWM configuration
#' @param ensemble an ensemble member number
#' @param num a number of files to download, default = all
#' @return a list of meta.data information
#' @export
#' @examples
#' \dontrun{
#'  urls     <-  get_nomads_filelist(type = "medium_range", num = 20,  ensemble = 4)
#'  in_files <-  download_nomads(urls, dir = tempdir())
#' }

get_nomads_filelist = function(type = NULL,
                               ensemble = NULL,
                               num = 6) {
  
  time     <-  f <- NULL
  type     <-  error_checking(type, ensemble)
  ensemble <-  paste0("_mem", ensemble)
  
  if(grepl("hawaii", type)){
    domain = 'hawaii'
    base.type = gsub("_hawaii", "", type)
  } else {
    domain = "conus"
    base.type = type
  }
  
  tmp = 'https://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/prod/'
  
  avail.days = grep("nwm\\.", readLines(tmp), value = TRUE)
  avail.days = gsub(".*[m.]([^.]+)[<].*", "\\1", avail.days)
  date = max(as.Date(gsub("/", "", avail.days), format = "%Y%m%d"))
  
  files    <- tryCatch({
    suppressMessages(readLines( make_url(date, type), warn = FALSE))}, 
    error = function(e){
      date <<- date - 1
      suppressMessages(readLines(make_url(date, type), warn = FALSE))
    },
    warning = function(w){
      date <<- date - 1
      suppressMessages(readLines(make_url(date, type), warn = FALSE))
    })
  
  filenames = regmatches(files, gregexpr('(\").*?(\")', files, perl = TRUE))
  filenames = filenames[grep(paste0(gsub(ensemble, "", base.type),".channel"), filenames)] 
  fileList = gsub("^\"|\"$", "", filenames)
  
  if(is.null(num)){num = 100000}
  
  paste0(make_url(date, type), fileList[1:num])
}

#' @title Download NOMADs files
#'
#' @param urls return from \code{get_nomads_filelist}
#' @param dir a directory to write data to
#' @param quiet should messages be silenced? Default = FALSE
#' @return a list of meta.data information
#' @importFrom httr GET write_disk
#' @export
#' @examples 
#' \dontrun{
#'  urls     <-  get_nomads_filelist(type = "medium_range", num = 20,  ensemble = 4)
#'  in_files <-  download_nomads(urls, dir = tempdir())
#' }

download_nomads = function(urls = NULL, dir = NULL, quiet = FALSE){
  
  if (!dir.exists(dir)) { dir.create(dir, recursive = T) }
  
  local = file.path(dir, basename(urls))
  
  for(i in seq_along(local)){
    
    if (!file.exists(local[i])) {
      
      message("Downloading ", basename(urls[i]))
      resp <-  httr::GET(urls[i],
                         httr::write_disk(local[i], overwrite = TRUE))
      
      if (resp$status_code != 200) {
        stop("Download unsuccessfull :(")
      }
    } else {
      if(!quiet){ message(basename(urls[i]), " already exisits") }
    }
  }
  
  return(local)
}