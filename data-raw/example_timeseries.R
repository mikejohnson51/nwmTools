library(httr)

files = get_aws_urls(date = "2010-10-01", num = 24*5)

out = paste0("data/", basename(files$urls))

for(i in 1:nrow(files)){ GET(files$urls[i], write_disk(out[i])) }

ll = list()

for(i in 1:length(out)){
  
  nc   = open.nc(out[i])
  feat = var.get.nc(nc, "feature_id")
  q    = var.get.nc(nc, "streamflow")
  
  ind = which(feat == 9528411)
  
  ll[[i]] = data.frame(
    dateTime      = lubridate::ymd_hm(strsplit(basename(out[i]), "[.]")[[1]][1]),
    comid         = var.get.nc(nc, "feature_id")[ind],
    streamflow    = var.get.nc(nc, "streamflow", unpack = TRUE)[ind]
  )
    
}

b = bind_rows(ll)

saveRDS(b, "inst/extdata/sample_ts.rds")

unlink(out)

