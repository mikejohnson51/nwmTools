test_that("testfile lists", {
  
  # AWS ------------------------------

  aws = get_aws_filelist(version = 2.1,
                         output = 'CHRTOUT',
                         date = "2015-01-01",
                         num = 2)
  
  expect_true(nrow(aws) == 2)
  
  res = httr::GET(aws$urls[1])
  
  expect_true(res$status_code == 200)
  
  aws = get_aws_filelist(version = "2.0",
                         output = 'CHRTOUT',
                         date = "2015-01-01",
                         config = "full_physics",
                         num = 2)
  
  expect_true(nrow(aws) == 2)
  
  res = httr::GET(aws$urls[1])
  
  expect_true(res$status_code == 200)
  
  aws = get_aws_filelist(version = 1.2,
                         output = 'CHRTOUT',
                         date = "2015-01-01",
                         num = 2)
  
  expect_true(nrow(aws) == 2)
  
  res = httr::GET(aws$urls[1])
  
  expect_true(res$status_code == 200)
  
  # GCP ------------------------------
  
  gcp = get_gcp_urls(config = "short_range",
                     domain = "conus",
                     date = "2022-10-01", 
                     hour = "00",
                     minute = "00",
                     num = 4, 
                     ensemble = NULL, 
                     output = "channel_rt")
  
  
  
  expect_true(nrow(gcp) == 4)
  
  res = httr::GET(gcp$urls[1])
  
  expect_true(res$status_code == 200)
  
  gcp = get_gcp_urls(config = "short_range",
                     domain = "conus",
                     date = "2018-10-01", 
                     hour = "00",
                     minute = "00",
                     num = 4, 
                     ensemble = NULL, 
                     output = "channel_rt")
  
  expect_true(nrow(gcp) == 4)
  
  res = httr::GET(gcp$urls[1])
  
  expect_true(res$status_code == 200)
  
  # NOMADS ------------------------------
  nomads = get_nomads_filelist(config = "short_range",
                                domain = "conus",
                                num = 3, 
                                output = "channel_rt",
                                version = "v2.2")
    
  expect_true(nrow(nomads) == 3)
  
  res = httr::GET(nomads$urls[1])

  expect_true(res$status_code == 200)
  
  # Medium Range ------------------------------
  
  nomads = get_nomads_filelist(config = "medium_range",
                                num = 3,
                                ensemble = 1,
                                output = "channel_rt",
                                version = "v2.2")
  
  expect_true(nrow(nomads) == 3)
  
  res = httr::GET(nomads$urls[1])
  
  expect_true(res$status_code == 200)
  
  gcp1 = get_gcp_urls(config = "medium_range",
                      date = "2022-10-10",
                      num = 3,
                      ensemble = 1,
                      output = "channel_rt")
  
  expect_true(length(gcp1) == 3)
  
  res = httr::GET(gcp1$urls[1])
  
  expect_true(res$status_code == 200)
  
  expect_error(get_gcp_urls(config = "medium_range",
                     date = "2018-11-01", 
                     num = 3,
                     ensemble = 1, 
                     output = "channel_rt"))
  
  gcp2 = get_gcp_urls(config = "medium_range",
                      date = "2018-11-01", 
                      num = 3,
                      output = "channel_rt")
  
  expect_true(length(gcp2) == 3)
  
  res = httr::GET(gcp2$urls[1])
  
  expect_true(res$status_code == 200)

})


test_that("testfile land lists", {
  
  # AWS ------------------------------
    aws = get_aws_filelist(version = 2.1,
                         output = 'LDASOUT',
                         date = "2015-01-01",
                         num = 2)

    expect_true(nrow(aws) == 2)
  
    res = httr::GET(aws$urls[1])
    
    expect_true(res$status_code == 200)
    
  # GCP ------------------------------
    gcp = get_gcp_urls(output = 'land',
                       date = "2020-01-01",
                       num = 2)
    
    expect_true(nrow(gcp) == 2)
    
    res = httr::GET(gcp$urls[1])
    
    expect_true(res$status_code == 200)
    
  # NOMDAS ------------------------------
    nomads = get_nomads_filelist(output = 'land', num = 2)
    
    expect_true(nrow(nomads) == 2)
    
    res = httr::GET(nomads$urls[1])
    
    expect_true(res$status_code == 200)
})
