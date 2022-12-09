#' Crop Flipped Raster
#' @param x  SpatRast object
#' @param AOI a sf polygon
#' @return SpatRast object (x cropped to AOI)
#' @export
#' @importFrom  terra ext crs align ext project vect ymax ymin xmin xmax flip crop

crop_flipped_nwm <- function(x, AOI) {
  
  template = list(
    ext = ext(
      -2303999.62876143,
      2304000.37123857,
      -1920000.70008381,
      1919999.29991619
    ),
    crs = 'PROJCS["Sphere_Lambert_Conformal_Conic",GEOGCS["GCS_Sphere",DATUM["D_Sphere",SPHEROID["Sphere",6370000.0,0.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Lambert_Conformal_Conic"],PARAMETER["false_easting",0.0],PARAMETER["false_northing",0.0],PARAMETER["central_meridian",-97.0],PARAMETER["standard_parallel_1",30.0],PARAMETER["standard_parallel_2",60.0],PARAMETER["latitude_of_origin",40.000008],UNIT["Meter",1.0]];-35691800 -29075200 126180232.640845;-100000 10000;-100000 10000;0.001;0.001;0.001;IsHighPrecision'
  )
  
  terra::ext(x) <- template$ext
  terra::crs(x) <- template$crs
  
  e =  align(ext(project(vect(AOI), crs(x))), x)
  
  ymax <- ymax(x) - (e$ymin - ymin(x))
  ymin <- ymax(x) - (e$ymax - ymin(x))
  
  flipe <- ext(c(xmin(e), xmax(e), as.numeric(ymin),  as.numeric(ymax)))
  
  z <- flip(crop(x, flipe), "vertical")
  terra::ext(z) <- e
  z
}

#' Extract Timeseries from fileList
#' @param fileList a list of non-gridded NWM outputs
#' @param ids a set of ids to limit the returned data to
#' @param index_id the name of the id attributes
#' @param varname the name of the variable
#' @return data.frame
#' @export
#' @importFrom terra values rast
#' @importFrom glue glue

get_timeseries = function(fileList,
                          ids = NULL,
                          index_id = "feature_id",
                          varname = "streamflow",
                          outfile = NULL,
                          ...){
  
  get_values = function(url, var, ind = NULL){
    v = suppressWarnings(values(rast(glue("{url}://{var}")), mat = FALSE))
    if(!is.null(ind)){ v[ind] } else { v }
  }
  
  urls =  glue('HDF5:"/vsicurl/{fileList$urls}"')
  
  all_ids = get_values(url = urls[1], index_id)
  
  if(is.null(ids)){
    ind = NULL
    ids = all_ids
  } else {
    ind = which(all_ids %in% ids)
  }
  
  
  values = lapply(1:length(urls), FUN = function(x){
    tryCatch({
      get_values(url = urls[x], varname, ind)
      }, error = function(e){
        message('Broken at: ', urls[x] )
      })
  })
  
  time = lapply(1:length(urls), FUN = function(x){
    get_values(url = urls[x], "time")
  })
  
  time = as.POSIXct(unlist(time) * 60, origin = "1970-01-01", tz = "UTC")
  
  out = data.frame(do.call('cbind',  c(list(ids), values)))
  names(out) = c(index_id, as.character(time))
  
  if(!is.null(outfile)){
    write_timeseries_nc(data = out, 
                        outfile = outfile, 
                        index_id = index_id, 
                        varname = varname)
    return(outfile)
  } else {
    return(out)
  }
  
  
}


#' Extract Timeseries from fileList
#' @param fileList a list of gridded NWM outputs
#' @param AOI area of interest (sf POLYGON) to subset
#' @param varname the name of the variable to extract
#' @return data.frame
#' @export
#' @importFrom terra rast
#' @importFrom glue glue


get_gridded_data = function(fileList,
                            AOI, 
                            varname,
                            outfile = NULL,
                            ...){
  
  urls = fileList$urls
  lyrs =   suppressWarnings( names(rast(paste0("/vsicurl/", urls[1]))) )
  goodnames = grep(paste(varname, collapse = "|"), lyrs, value = TRUE)
  rast_list = list()
  
  for(i in 1:length(goodnames)){
    l = list()
    
    for(j in 1:length(urls)){
      l[[j]] = suppressWarnings({
        rast(paste0("/vsicurl/", urls[j]), paste0("//", goodnames[i]))
      })
    }
    
    d = crop_flipped_nwm(rast(l),  AOI = AOI)
    names(d) =  paste0(goodnames[i], "_", 1:nlyr(d))
    time(d) =  fileList$dateTime
    
    rast_list[[goodnames[i]]] = d
  }
  
  
  if(!is.null(outfile)){
    write_gridded_data(rast_list, outfile, ...)
  }
  
  rast_list
}


write_gridded_data = function(data, outfile, overwrite = TRUE){
  suppressWarnings({
    writeCDF(sds(data), outfile, overwrite = overwrite)
  })
} 


write_timeseries_nc = function(data, outfile, index_id, varname,
                               na_val = -999900 , scale = .01, offset = 0){
  
  comids = data[[index_id]]
  time = as.POSIXct(names(data)[2:ncol(data)], tz="UTC")
  values = as.matrix(dplyr::select(data, -!!index_id))
  
  unlink(outfile)
  file1 <- outfile
  nc <- create.nc(file1, format = 'netcdf4')
  nc = open.nc(file1, write  = TRUE)
  
  dim.def.nc(nc, index_id, length(comids))
  dim.def.nc(nc, "time", unlim = TRUE)
  
  ##  Create three variables, one as coordinate variable
  var.def.nc(nc, "time", "NC_INT", "time", deflate = 4)
  var.def.nc(nc, index_id, "NC_INT", index_id, deflate = 4)
  # Awkwardness arises mainly from one thing: NetCDF data 
  # are written with the last dimension varying fastest, 
  # whereas R works opposite. Thus, the order of the dimensions 
  # according to the CDL conventions (e.g., time, latitude, longitude) 
  # is reversed in the R array (e.g., longitude, latitude, time).
  var.def.nc(nc, varname, "NC_FLOAT", c(0,1), deflate = 4,
             chunking = TRUE, chunksizes = c(10000, length(time)))
  
  ##  Put some _FillValue attribute for temperature
  att.put.nc(nc, varname, "missing_value", "NC_FLOAT", na_val)
  att.put.nc(nc, varname, "add_offset",    'NC_FLOAT', offset)
  att.put.nc(nc, varname, "scale_factor",  "NC_FLOAT", scale)
  
  ##  Put all of the data:
  var.put.nc(nc, "time", as.vector(time))
  var.put.nc(nc, index_id, comids)
  var.put.nc(nc, varname, values)
  close.nc(nc)
  
  return(outfile)
  
}

