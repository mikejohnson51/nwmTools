test_that("timeseries", {

  xx = get_aws_urls(num = 2) %>% 
    get_timeseries(ids = 101)
  
  expect_equal(nrow(xx), 1)
  expect_equal(ncol(xx), 3)
  
  outfile = tempfile(fileext = ".nc")
  
  get_aws_urls(num = 2) %>% 
    get_timeseries() %>% 
    write_timeseries_nc(outfile = outfile, index_id = "feature_id", varname = "streamflow")
  
  nc =  RNetCDF::open.nc(outfile)
  expect_equal(dim(var.get.nc(nc, "streamflow")), c(2776738,       2))
  expect_equal(length(var.get.nc(nc, "feature_id")), 2776738)

  unlink(outfile)
  
})


test_that("gridded", {
  
  xx = get_aws_urls(num = 2, output = "LDASOUT_DOMAIN1") %>% 
    get_gridded_data(AOI = AOI::aoi_get("Fort Collins"), varname = "ACCET")
  
  expect_true(inherits(xx$ACCET, "SpatRaster"))
  expect_equal(terra::nlyr(xx[[1]]),  2)
  expect_equal(terra::res(xx[[1]]),  c(1000,1000))
  
  outfile = tempfile(fileext = ".nc")
  
  get_aws_urls(num = 2, output = "LDASOUT_DOMAIN1") %>% 
    get_gridded_data(AOI = AOI::aoi_get("Fort Collins"), varname = "ACCET") %>% 
    write_gridded_data(outfile)
  
  nc =  terra::rast(outfile)
  expect_true(inherits(nc, "SpatRaster"))
  expect_equal(terra::nlyr(nc),  2)
  expect_equal(terra::res(nc),  c(1000,1000))
  expect_equal(terra::varnames(nc),  "ACCET")
  
  unlink(outfile)
  
})
  
