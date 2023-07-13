#' Download Remote Files
#' @param fileList fileList object
#' @param outdir directory to write files
#' @return data.frame
#' @export

download_files = function(fileList, outdir = "."){
  
  fileList$outfiles = paste0(outdir,"/", basename(fileList$urls))
  
  for(i in 1:nrow(fileList)){
    if(!file.exists(fileList$outfiles[i])){
      httr::GET(fileList$urls[i], 
                write_disk(fileList$outfiles[i]),
                httr::progress())
    }
  }

  return(fileList)

}

#' Extract Timeseries from local file list
#' @param fileList a list of non-gridded NWM outputs
#' @param ids a set of ids to limit the returned data to
#' @param index_id the name of the id attributes
#' @param varname the name of the variable
#' @param outfile file path to save data to (.nc extension)
#' @return data.frame
#' @export

get_timeseries_local = function(fileList,
                                ids = NULL,
                                index_id = "feature_id",
                                varname = "streamflow",
                                outfile = NULL){
      
          index = list()
          vars  = list()
          time  = list()
          
          nc = RNetCDF::open.nc(fileList$outfiles[1])
          
          for(i in 1:length(index_id)){
            index[[i]] = var.get.nc(nc, index_id[i], unpack = TRUE)
          }
          
          for(i in 1:length(varname)){
            vars[[length(vars) + 1]] = var.get.nc(nc, varname[i], unpack = TRUE)
          }
          
          time[[i]]  <- utcal.nc(
            unitstring = att.get.nc(nc, "time", "units"),
            value = var.get.nc(nc, "time", unpack = TRUE),
            type = "c"
          )
        
          if(nrow(fileList) > 1){
          
            for(j in 2:nrow(fileList)){
              nc = RNetCDF::open.nc(fileList$outfiles[j])
              
              for(i in 1:length(varname)){
                vars[[length(vars) + 1]] = var.get.nc(nc, varname[i], unpack = TRUE)
              }
              
              time[[j]]  <- utcal.nc(
                unitstring = att.get.nc(nc, "time", "units"),
                value = var.get.nc(nc, "time", unpack = TRUE),
                type = "c"
              )
              
              close.nc(nc)
            }
          }
          
          
          df = cbind(do.call('cbind', index),
                do.call('cbind', vars)) %>% 
            data.frame()
      
          time = as.POSIXct(unlist(time), origin = "1970-01-01", tz = "UTC")
        
          names(df) = c(index_id, paste0(varname, "_", as.character(time)))
          
          if(!is.null(ids)){
            df = filter(df, index_id %in% ids)
          }
        
          if(!is.null(outfile)){
            if(length(index_id) == 1){
              write_timeseries_nc(data = df, 
                                  outfile = outfile, 
                                  index_id = index_id, 
                                  varname = varname)
              
            return(outfile)
            }
            
            warning("Can only write file with one index_id.")
            return(df)
      
          } else {
            return(df)
          }
}

#' Extract Gridded Data from local fileList
#' @param fileList a list of gridded NWM outputs
#' @param AOI area of interest (sf POLYGON) to subset
#' @param varname the name of the variable to extract
#' @param outfile filepath to save data (with .nc extension)
#' @return data.frame
#' @export
#' 
get_gridded_local = function(fileList,
                             AOI, 
                             varname,
                             outfile = NULL){
  rast_list = list()
  
  for(i in 1:length(varname)){
    
    l = list()
    
    for(j in 1:nrow(fileList)){
      l[[j]] = suppressWarnings({
        rast(fileList$outfiles[j], lyr = varname[i])
      })
    }
    
    d = crop(rast(l), project(vect(AOI), crs(l[[1]])))
    names(d) =  paste0(varname[i], "_", 1:nlyr(d))
    time(d) =  fileList$dateTime
    
    rast_list[[varname[i]]] = d
  }
  
  rast_list
}
  