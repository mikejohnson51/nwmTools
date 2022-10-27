test_that("testfile lists", {
  
  # AWS ------------------------------

  aws = get_aws_filelist(version = "2.1",
                         output = 'CHRTOUT',
                         date = "2015-01-01",
                         num = 2)
  
  expect_true(length(aws) == 2)
  
  res = httr::GET(aws[1])
  
  expect_true(res$status_code == 200)
  
  # GCP ------------------------------
  
  gcp = get_gcp_urls(config = "short_range",
                     domain = "conus",
                     date = "2019-01-01", 
                     hour = "00",
                     minute = "00",
                     num = 4, 
                     ensemble = NULL, 
                     output = "channel_rt")
  
  expect_true(length(gcp) == 4)
  
  res = httr::GET(gcp[1])
  
  expect_true(res$status_code == 200)
  
  # NOMADS ------------------------------
  nomads = get_nomads_filelist2(config = "short_range",
                                domain = "conus",
                                num = 3, 
                                output = "channel_rt",
                                version = "v2.2")
    
  expect_true(length(nomads) == 3)
  
  res = httr::GET(nomads[1])

  expect_true(res$status_code == 200)
  
  # Medium Range ------------------------------
  
  nomads = get_nomads_filelist2(config = "medium_range",
                                num = 3,
                                ensemble = 1,
                                output = "channel_rt",
                                version = "v2.2")
  
  expect_true(length(nomads) == 3)
  
  res = httr::GET(nomads[1])
  
  expect_true(res$status_code == 200)
  
  
  nomads = get_gcp_urls(config = "medium_range",
                        date = "2022-10-10",
                        num = 3,
                        ensemble = 1,
                        output = "channel_rt")
  
  expect_true(length(nomads) == 3)
  
  res = httr::GET(nomads[1])
  
  expect_true(res$status_code == 200)

})
