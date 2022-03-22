test_that("TDS", {
  expect_true(grepl("http", get_tds()))
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
  q = readNWMdata(comid = 101, startDate = "1993-01-01", endDate = NULL, version = 2)
  expect_true(ncol(q) == 3)
  expect_true(nrow(q) == 24)
  #https://thredds.hydroshare.org/thredds/dodsC/nwm/retrospective/nwm_v2_retro_full.ncml.ascii?streamflow%5B0:1:0%5D%5B0:1:0%5D
  expect_true(round(q$flow_cms[1],2) == 2.90)
  expect_true(min(q$dateTime) == as.POSIXlt("1993-01-01 00:00:00", tz = "UTC"))
  expect_true(max(q$dateTime) == as.POSIXlt("1993-01-01 23:00:00", tz = "UTC"))
})


