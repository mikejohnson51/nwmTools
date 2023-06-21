test_that("utils", {
  expect_true(grepl("http", get_tds()))
  expect_equal(nrow(get_nwm_meta()), 3)
})

test_that("query errors", {
  # TZ
  expect_error(readNWMdata(comid = 101,
                           startDate = "1993-01-01", endDate = NULL, 
                           tz = "XXX", version = 2))

  # StartDate to early
  expect_error(error.checks(startDate = "1980-01-01", endDate = NULL, tz = "UTC", version = 2))
  # EndDate to late
  expect_error(error.checks(startDate = NULL, endDate = "2030-01-01", tz = "UTC", version = 2))
  # EndDate to late for version
  expect_error(error.checks(startDate = "1993-01-01", endDate = "2018-12-31", tz = "UTC", version = 1.2))
  # Wrong Version
  expect_error(error.checks(startDate = "1993-01-01", endDate = NULL, tz = "UTC", version = 2.5))
})

test_that("query success", {
  q = readNWMdata(comid = c(1001, 101), startDate = "1993-01-01", endDate = NULL, version = 2)
  expect_true(ncol(q) == 3)
  expect_true(nrow(q) == 48)
  #https://thredds.hydroshare.org/thredds/dodsC/nwm/retrospective/nwm_v2_retro_full.ncml.ascii?streamflow%5B0:1:0%5D%5B0:1:0%5D
  expect_true(round(q$flow_cms[25],2) == 2.90)
  expect_true(min(q$dateTime) == as.POSIXlt("1993-01-01 00:00:00", tz = "UTC"))
  expect_true(max(q$dateTime) == as.POSIXlt("1993-01-01 23:00:00", tz = "UTC"))
  
  q = readNWMdata(comid = 101, startDate = NULL, endDate = NULL, version = 1.2)
  expect_true(ncol(q) == 3)
  expect_true(nrow(q) == 219144)
  expect_true(min(q$dateTime) == as.POSIXlt("1993-01-01 00:00:00", tz = "UTC"))
  expect_true(max(q$dateTime) == as.POSIXlt("2017-12-31 23:00:00 UTC", tz = "UTC"))
  
  
  expect_equal(nrow(aggregate_s(q)), 4)
  expect_equal(nrow(aggregate_y(q)), 25)
  expect_equal(nrow(aggregate_m(q)), 12)
  expect_equal(nrow(aggregate_ymd(q)), 9131)
  expect_equal(nrow(aggregate_record(q)), 1)
  expect_equal(nrow(aggregate_j(q)), 366)
  expect_equal(nrow(aggregate_wy(q)), 26)
  
  expect_equal(nrow(aggregate_ym(q)), 300)
  
  expect_equal(nrow(aggregate_yj(q)), 9131)
  expect_equal(nrow(aggregate_ys(q)), 100)
  expect_equal(nrow(aggregate_wym(q)), 300)
  
  expect_equal(nrow(aggregate_wymd(q)), 9131)
  
  expect_equal(nrow(aggregate_wys(q)), 102)
  
  expect_equal(nrow(aggregate_dowy(q)), 366)
  
  expect_error(aggregate_dowy(dplyr::select(q, -dateTime)))
})

test_that("add_*", {
  
  q = readNWMdata(comid = c(101), startDate = "1993-01-01", add_nhd = TRUE)
  expect_true(inherits(q, "sf"))
  
  q = readNWMdata(comid = c(101), startDate = "1993-01-01", version = c(1.2, 2))
  expect_equal(nrow(q), 24)
  expect_equal(ncol(q), 4)
  
  
  expect_error(readNWMdata(comid = c(101), startDate = "1993-01-01", addObs = TRUE))
  q = readNWMdata(siteID = '06741510', startDate = "1993-01-01", addObs = TRUE)
  expect_equal(nrow(q), 24)
  expect_equal(ncol(q), 5)
  
  q = readNWMdata(siteID = '06741510', startDate = "1993-01-01", add_nhd  = TRUE, addObs = TRUE)
  expect_true(inherits(q, "sf"))
  expect_equal(nrow(q), 24)
  expect_equal(ncol(q), 6)
  
  q = readNWMdata(AOI = AOI::aoi_get("Fort Collins"), startDate = "1993-01-01")
  
  expect_equal(nrow(q), 2664)
  expect_equal(ncol(q), 3)
  
  expect_message(readNWMdata(comid = c(1), startDate = "1993-01-01"))
})


test_that("accuracy", {

  truth = readRDS(system.file("extdata", "sample_ts.rds", package = "nwmTools"))
  read  = readNWMdata(comid = truth$comid[1], startDate = truth$dateTime[1], endDate = truth$dateTime[nrow(truth)] )
  expect_equal(sum(abs(truth$streamflow - read$flow_cms_v2.1) < .1), nrow(truth))
})
