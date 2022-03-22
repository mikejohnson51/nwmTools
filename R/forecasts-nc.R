#' @title Check to See if NCO is on the system
#' @description NCO is a fantastic open source tool for working with NetCDF files. 
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a boolean condition
#' @importFrom sys exec_internal
#' @examples 
#' check_nco()
#' @export

check_nco = function(){
  m = "Check your NCO install, or download: http://nco.sourceforge.net/#Source. \n\nYou can also use `create_nomads_nc`, which does not require NCO, but will give you a NetCDF file not optimized for timeseries extraction. For the operational products there is generally neglibable performance gains this can achieve so do as you must :)."
  
  tryCatch({
    x = exec_internal("ncks", "--version")
    grepl("version", rawToChar(x$stderr))
  }, error = function(e){
    message(m)
    FALSE
  }, warning = function(w){
    message(m)
    FALSE
  }) 
}


#' @title Create a Single NetCDF for list of NWM files
#' @description This function will take a set of NWM files and concatenate them
#' along the time access. If NCO is installed, this file will be pivoted and 
#' optimized for time series extraction. While this function will work without NCO
#' it is highly recommended to install the NCO binaries (http://nco.sourceforge.net/#Source). If a file list is not provided, defining the type,ensemble, and number (num) of files to download, will query the most current NWM forecasts on NOMADS.
#' @param fileList a vector of file paths to merge
#' @param variable the channel file variable to extract
#' @param dstfile the file path to write to
#' @param type a NWM configuration
#' @param ensemble an ensemble member number
#' @param num a number of files to download, default = all
#' @param nco should NCO be used? Default = TRUE
#' @param pivot should the concatenated file be pivoted? Default = TRUE
#' @param purge should the fileList files be deleted? Default = FALSE
#' @param quiet should messages be silenced? Default = FALSE
#' @return file path
#' @export
#' @examples 
#' \dontrun{
#'  create_nwm_nc(type = "short_range", dstfile = tempfile(ext = ".nc"))
#'  
#'  urls     <-  get_nomads_filelist(type = "medium_range", num = 4,  ensemble = 4)
#'  in_files <-  download_nomads(urls, dir = tempdir())
#'  create_nwm_nc(fileList = in_files, dstfile = tempfile(fileext = ".nc"))
#' }

create_nwm_nc = function(fileList = NULL,
                         variable = "streamflow",
                         dstfile = NULL,
                         type = NULL,
                         num  = NULL,
                         ensemble = NULL,
                         nco = TRUE,
                         pivot = TRUE, 
                         purge = TRUE,
                         quiet = FALSE) {
  
  if(pivot & !check_nco()){ stop("You can only pivot the NetCDF if NCO is installed")}
  
  if(nco & !check_nco()){ stop("Using NCO requires NCO to be installed. Set `nco` 
                               to FALSE or install NCO")}
  
  if(file.exists(dstfile)){file.remove(dstfile)}

  if(is.null(fileList)) {
    fileList = get_nomads_filelist(type = type, num  = num, ensemble = ensemble)
    dir      = paste0(dirname(dstfile), "/")
    fileList = download_nomads(fileList, dir = dir)
  }
  
  if(nco){
    use_nco(in_files = fileList, variable =  variable, dstfile = dstfile, pivot = pivot)
  } else {
    use_rnetcdf(in_files = fileList, variable =  variable, dstfile = dstfile)
  }
  
  if(purge){ file.remove(fileList) }
  
  dstfile
  
}


#' @title Use NCO to transform files
#' @description Workflow for transforming set of files to a single, pivoted, 
#' NetCDF file using NCO (see: check_nco). 
#' @param in_files a vector of file paths
#' @param variable the variable to extract
#' @param dstfile the location to store the output file
#' @param pivot should the 2D variable array be pivoted? Default = TRUE
#' @importFrom RNetCDF att.get.nc
#' @return a file path
#' @noRd
#' @keywords internal

use_nco = function(in_files, variable = "streamflow",
                   dstfile = NULL, pivot = TRUE) {
  
  tmp    <- open.nc(in_files[1])
  scale  <- att.get.nc(tmp, "streamflow", "scale_factor")
  offset <- att.get.nc(tmp, "streamflow", "add_offset")

  dir.create(paste0(tempdir(), "/nco/"))
  
  new_files <- paste0(tempdir(), "/nco/", basename(in_files))
                     
  for(i in 1:length(in_files)){
    system(paste0('ncks -h -O -4 -L 1 --cnk_plc=all --cnk_map=dmn -C -v feature_id,time,',
                  variable," ", in_files[i], " ", new_files[i]))
    system(paste('ncatted -h -O -a "scale_factor,streamflow,d,," -a "add_offset,streamflow,d,,"', 
                 new_files[i],  new_files[i])) 
    system(paste0('ncap2 -h -O -s ', variable, '[time,feature_id]=', 
                  variable, " ", new_files[i], " ", new_files[i]))
    system(paste('ncks  -h -O  --mk_rec_dmn time', new_files[i], new_files[i])) 
  } 
  
  system(paste('ncrcat -h -O -6', paste(new_files, collapse = " "), dstfile))

  if(pivot){ system(paste('ncpdq -h -O -a feature_id,time', dstfile, dstfile)) }
  
  system(paste0('ncatted -h -O -a "scale_factor,streamflow,o,f,', 
                scale, '" ', dstfile, " ", dstfile))
  
  system(paste0('ncks -O -4 --cnk_plc=g2d --cnk_dmn feature_id,10000 --cnk_dmn time,', 
                length(in_files),' --deflate 0 ', dstfile, " ", dstfile)
  )
  
  system(paste("ncks -4 -L 3 -h -O", dstfile, dstfile))
  
  unlink(paste0(tempdir(), "/nco/"), recursive = TRUE)

  return(dstfile)
}


#' @title Use R to transform files
#' @description Workflow for transforming set of files to a single, 
#' NetCDF file using RNetCDF.
#' @param in_files a vector of file paths
#' @param variable the variable to extract
#' @param dstfile the location to store the output file
#' @return a file path
#' @importFrom RNetCDF att.get.nc att.put.nc close.nc create.nc dim.def.nc var.def.nc var.put.nc
#' @noRd
#' @keywords internal

use_rnetcdf = function(in_files, variable = "streamflow", dstfile = NULL) {
  
  extract_var = function(x, var){ 
    nc   = open.nc(x)
    vals = var.get.nc(nc, var, unpack = FALSE)
    close.nc(nc)
    vals
  }
  
  v = do.call(cbind, lapply(in_files, extract_var, var = variable))
  t = do.call(cbind, lapply(in_files, extract_var, var = "time"))
  
  tmp    = open.nc(in_files[1])

  na_val =  att.get.nc(tmp,  "streamflow", "missing_value")
  scale  =  att.get.nc(tmp,  "streamflow", "scale_factor")
  offset =  att.get.nc(tmp,  "streamflow", "add_offset")
  
  comids = var.get.nc(tmp, "feature_id")
  close.nc(tmp)

  file1 <- dstfile
  nc <- create.nc(file1, format = 'netcdf4')
  nc = open.nc(file1, write  = TRUE)
  
  dim.def.nc(nc, "feature_id", length(comids))
  dim.def.nc(nc, "time", unlim = TRUE)
  
  ##  Create three variables, one as coordinate variable
  var.def.nc(nc, "time", "NC_INT", "time", deflate = 4)
  var.def.nc(nc, "feature_id", "NC_INT", "feature_id", deflate = 4)
  # Awkwardness arises mainly from one thing: NetCDF data 
  # are written with the last dimension varying fastest, 
  # whereas R works opposite. Thus, the order of the dimensions 
  # according to the CDL conventions (e.g., time, latitude, longitude) 
  # is reversed in the R array (e.g., longitude, latitude, time).
  var.def.nc(nc, "streamflow", "NC_FLOAT", c(0,1), deflate = 4,
             chunking = TRUE, chunksizes = c(10000, length(in_files)))

  ##  Put some _FillValue attribute for temperature
  att.put.nc(nc, "streamflow", "missing_value", "NC_FLOAT", na_val)
  att.put.nc(nc, "streamflow", "add_offset",    'NC_FLOAT', offset)
  att.put.nc(nc, "streamflow", "scale_factor",  "NC_FLOAT", scale)

  ##  Put all of the data:
  var.put.nc(nc, "time", as.vector(t))
  var.put.nc(nc, "feature_id", comids)
  var.put.nc(nc, "streamflow", v)
  close.nc(nc)
  
  dstfile
}



#' @title Extract NWM flow series
#' @param file a NetCDF file produced with create_nomads_nc, or create_nomads_ts
#' @param comids a vector of NHD comids to extract
#' @param variable a channel file variable to extract 
#' (should be same as the one used to create the file)
#' @return a data.frame of comid,dateTime,value
#' @importFrom RNetCDF open.nc var.get.nc dim.inq.nc var.inq.nc
#' @export
#' @examples
#' \dontrun{
#'  outfile = create_nwm_nc(type = "short_range", dstfile = tempfile(ext = ".nc"))
#'  flows = extract_nwm(outfile, comids = c(101,1001,102900))
#' }

extract_nwm = function(file, comids, variable = "streamflow"){
  nc = open.nc(file)
  ind = match(comids, var.get.nc(nc, 'feature_id'))
  
  feat_ind  = dim.inq.nc(nc, "feature_id")$id
  time_ind  = dim.inq.nc(nc, "time")$id
  time_length  = dim.inq.nc(nc, "time")$length
  var_order = var.inq.nc(nc, variable)$dimids
  
  ar = list()
  
  for(i in 1:length(ind)){

    if(var_order[1] == feat_ind){
      start = c(ind[i], 1)
      count = c(1, NA)
    } else {
      start = c(1,  ind[i])
      count = c(NA, 1)
    }
    
    ar[[i]] = var.get.nc(nc, variable, start = start, count = count, unpack = TRUE)
  }
  
  data.frame(comid = rep(comids, each = time_length),
             dateTime = rep(as.POSIXct(var.get.nc(nc, "time")*60, origin = "1970-01-01", tz = 'UTC'), times = length(comids)),
             values = c(unlist(ar)))
}


