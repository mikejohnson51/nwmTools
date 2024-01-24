pacman::p_load(dplyr, glue, tidyr, readr, xml2, rvest)

# AWS Scrapping Function ------------------------------

scrape_aws = function(version, bucket, config, year, startDate, endDate, domain, format){
  
  unlink('data/aws_scrape.csv')
  
  if(!is.na(config)){
    pattern = glue('{bucket}/{config}/{year}/{year}')
    http_pattern = "{gsub('s3', 'http', {bucket})}.s3.amazonaws.com/{config}/{YYYY}/{YYYYMMDDHHMM}.{output}.comp"
    system(glue('aws s3 ls {pattern} >> data/aws_scrape.csv'))
  } else {
    pattern = glue('{bucket}/{year}/{year}')
    http_pattern = "{gsub('s3', 'http', {bucket})}.s3.amazonaws.com/{YYYY}/{YYYYMMDDHHMM}.{output}.comp"
    system(glue('aws s3 ls {pattern} >> data/aws_scrape.csv'))
  }
  if(version == '3.0' | version == '3'){
    pattern = glue('{bucket}/{domain}/{format}/{config}/{year}/{year}')
    system(glue('aws s3 ls {pattern} >> data/aws_scrape.csv'))
    if(domain != 'CONUS' & config == 'FORCING'){
      http_pattern = "{gsub('s3', 'http', {bucket})}.s3.amazonaws.com/{domain}/{format}/{config}/{YYYY}/{YYYYMMDDHH}.{output}"
      system(glue('aws s3 ls {pattern} >> data/aws_scrape.csv'))
    }
    else{
      http_pattern = "{gsub('s3', 'http', {bucket})}.s3.amazonaws.com/{domain}/{format}/{config}/{YYYY}/{YYYYMMDDHHMM}.{output}"
    }
  }
  yy = data.table::fread("data/aws_scrape.csv") %>% 
    select(path = V4) %>% 
    filter(path != "") %>% 
    filter(path != ":") %>% 
    mutate(source = "aws", config = !!config, bucket = bucket) %>% 
    separate(path, sep = "\\.", c("date", "output", "ext")) %>% 
    mutate(http_pattern = http_pattern,
           hour = as.numeric(substr(date,9,10)),
           minute = ifelse(str_length(date) <= 10, 0, as.numeric(substr(date, 11, 12))),
           date = NULL,
           ext = NULL) %>% 
    distinct() %>% 
    group_by(output) %>% 
    arrange(hour) %>% 
    mutate(timestep = ifelse(median(minute - lag(minute), na.rm = TRUE) == 15, 
                             0.25, 
                             median(hour - lag(hour), na.rm  = TRUE)),
           domain = domain, 
           hour = NULL,
           horizion = 1,
           prefix = "",
           version = !!version,
           startDate = startDate, endDate = endDate) %>% 
    select(-c(minute)) %>%
    distinct() %>%
    ungroup() %>% 
    distinct()

  unlink('data/aws_scrape.csv')
  
return(yy)

}

# GCP Scrapping Function ------------------------------

## data changes on nwm.20190619 --> v1.0 > v2.0

scrape_gcp = function(version, token = "nwm.20190620", startDate, endDate){
  
  outfile = 'data/gcp_scrape.csv'
  unlink(outfile)
  
  http_pattern = paste0(
    'https://storage.googleapis.com/national-water-model/', 
    'nwm.{YYYYDDMM}/{bucket}/nwm.t{HH}z.{config}.{output}.{prefix}{forward}.{domain}.nc')
  
  system(glue('gsutil ls -r gs://national-water-model/{token} >> {outfile}'))
  
  data = read_csv(outfile, show_col_types = FALSE)
  
  names(data) = "path"
  
  a =  data %>% 
    mutate(path = gsub(glue("gs://national-water-model/{token}/"), "", path)) %>% 
    filter(path != "") %>% 
    separate(path, sep = "/", into = c("bucket", "path")) %>% 
    filter(path != ":") %>% 
    filter(!grepl("usgstimeslice", tolower(path))) %>% 
    separate(path, sep = "\\.", c("model", "time", "config", "output", "hour", "domain", "ext")) %>% 
    mutate(model = NULL, ext = NULL, time = NULL) %>% 
    group_by(config, output, domain) %>% 
    mutate(horizion = length(unique(hour)),
           prefix = gsub('[[:digit:]]+', '', hour),
           forward = parse_number(hour),
           timestep = median(forward - lag(forward), na.rm  = TRUE)) %>% 
    select(-hour, -forward) %>% 
    ungroup() %>% 
    mutate(ensemble = suppressWarnings({ as.numeric(gsub(".*?([0-9]+).*", "\\1", output)) }),
           output = gsub('_[[:digit:]]+', '', output),
    ) %>% 
    mutate(source  = "gcp", 
           version = version, 
           startDate = startDate, 
           endDate   = endDate,
           http_pattern = paste0("https://storage.googleapis.com/national-water-model/nwm.{YYYYDDMM}/", 
                                 bucket, '/nwm.t{HH}z.', config,'.', output, '{ifelse(!is.na(ensemble), paste0("_", ensemble), "")}','.', prefix, '{forward}.', domain, '.nc'))  %>% 
    distinct()

  unlink(outfile)
 return(a)
}

# NOMADS Scrapping Function ------------------------------

scrape_nomads = function(url, version = 'para'){
  
  URL  = paste0(url, version, "/")
  URL2 = paste0(URL, html_attr(html_nodes(read_html(URL), "a"), "href")[2])
  all_urls = paste0(URL2, html_attr(html_nodes(read_html(URL2), "a"), "href")[-1])
  
  o = list()
  
  for(i in 1:length(all_urls)){
    o[[i]] = tryCatch({
      data.frame(
        bucket = basename(all_urls[i]),
        link = html_attr(html_nodes(read_html(all_urls[i]), "a"), "href")[-1]
      ) %>% 
        filter(!grepl(".shef$|.ncdf$", link)) %>% 
        tidyr::separate_wider_delim(link, delim = ".",
                                    names = c("model", "time", "config", "output", 'forward', "domain", "ext")) %>% 
        mutate(model = NULL, ext = NULL, source = "nomads", version = version, startDate = "..", endDate = "..",
               time = readr::parse_number(time),
               forward = readr::parse_number(forward),
               prefix =  gsub('[[:digit:]]+', '', forward),
               ensemble = suppressWarnings({ as.numeric(gsub(".*?([0-9]+).*", "\\1", output)) }),
               output = gsub('_[[:digit:]]+', '', output)) %>% 
        group_by(bucket, config, output, domain) %>% 
        mutate(timestep = max(diff(forward), na.rm = TRUE),
               horizion = n()) %>% 
        ungroup() %>% 
        mutate(http_pattern = paste0(URL, 'nwm.{YYYYDDMM}/', config, '/nwm.t{HH}z.', config, '.',
                                     output, '{ifelse(!is.na(ensemble), paste0("_", ensemble), "")}', '.', prefix, '{foward}.',domain,'.nc'),
               forward = NULL, 
               time = NULL) %>% 
        select(bucket, config, output, domain, horizion, prefix, timestep, ensemble, source, version, startDate, endDate, http_pattern) %>% 
        distinct() 
    }, error = function(e){
      NULL
    }, warning = function(w){
      NULL
    })
  }
  
  bind_rows(o)
}

## DATA CHANGES ON nwm.20190619 ###


a = scrape_gcp(version = "2.1", token = "nwm.20181029", startDate = "2018-09-17", endDate = "2019-06-18")

b = scrape_gcp(version = "2.2", token = "nwm.20221029", startDate = "2019-06-19", endDate = "..")

c = scrape_aws(version = "2.1", 
               bucket = "s3://noaa-nwm-retrospective-2-1-pds",  
               config = "model_output",  year = 2015,
               startDate = as.character(get_nwm_meta(version = "2.1")$startDate),
               endDate   =  as.character(get_nwm_meta(version = "2.1")$endDate))

d = scrape_aws(version = "2.1", bucket = "s3://noaa-nwm-retrospective-2-1-pds",  
               config = "forcing", year = 2015,
               startDate =  as.character(get_nwm_meta(version = "2.1")$startDate), 
               endDate   =  as.character(get_nwm_meta(version = "2.1")$endDate))

e = scrape_aws(version = "2.0", bucket = "s3://noaa-nwm-retro-v2.0-pds",  
               config = "full_physics", year = 2015,
               startDate =  as.character(get_nwm_meta(version = "2")$startDate), 
               endDate   =  as.character(get_nwm_meta(version = "2")$endDate))

f = scrape_aws(version = "2.0", bucket = "s3://noaa-nwm-retro-v2.0-pds",
               config = "long_range", year = 2015,
               startDate =  as.character(get_nwm_meta(version = "2")$startDate), 
               endDate   =  as.character(get_nwm_meta(version = "2")$endDate))

g = scrape_aws(version = "1.2", bucket = "s3://nwm-archive", 
               config = NA, year = 2015,
               startDate = as.character(get_nwm_meta(version  = "1.2")$startDate), 
               endDate   =  as.character(get_nwm_meta(version = "1.2")$endDate))

n1 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'FORCING', year = 2010, domain = 'Alaska', format = 'netcdf',
               startDate =  as.character("1/1/81 1:00"),
               endDate   =  as.character("12/31/19 23:00"))
n2 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'CHRTOUT', year = 2010, domain = 'Alaska', format = 'netcdf',
               startDate =  as.character("1/1/81 1:00"),
               endDate   =  as.character("12/31/19 23:00"))
n3 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'GWOUT', year = 2010, domain = 'Alaska', format = 'netcdf',
               startDate =  as.character("1/1/81 1:00"),
               endDate   =  as.character("12/31/19 23:00"))
n4 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'LAKEOUT', year = 2010, domain = 'Alaska', format = 'netcdf',
               startDate =  as.character("1/1/81 1:00"),
               endDate   =  as.character("12/31/19 23:00"))
n5 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'LDASOUT', year = 2010, domain = 'Alaska', format = 'netcdf',
               startDate =  as.character("1/1/81 1:00"),
               endDate   =  as.character("12/31/19 23:00"))
n6 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'RTOUT', year = 2010, domain = 'Alaska', format = 'netcdf',
               startDate =  as.character("1/1/81 1:00"),
               endDate   =  as.character("12/31/19 23:00"))

o1 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'FORCING', year = 2010, domain = 'CONUS', format = 'netcdf',
               startDate =  as.character("2/1/79 1:00"),
               endDate   =  as.character("12/31/23 23:00"))
o2 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'CHRTOUT', year = 2010, domain = 'CONUS', format = 'netcdf',
               startDate =  as.character("2/1/79 1:00"),
               endDate   =  as.character("12/31/23 23:00"))
o3 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'GWOUT', year = 2010, domain = 'CONUS', format = 'netcdf',
               startDate =  as.character("2/1/79 1:00"),
               endDate   =  as.character("12/31/23 23:00"))
o4 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'LAKEOUT', year = 2010, domain = 'CONUS', format = 'netcdf',
               startDate =  as.character("2/1/79 1:00"),
               endDate   =  as.character("12/31/23 23:00"))
o5 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'LDASOUT', year = 2010, domain = 'CONUS', format = 'netcdf',
               startDate =  as.character("2/1/79 1:00"),
               endDate   =  as.character("12/31/23 23:00"))
o6 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'RTOUT', year = 2010, domain = 'CONUS', format = 'netcdf',
               startDate =  as.character("2/1/79 1:00"),
               endDate   =  as.character("12/31/23 23:00"))
o7 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'CHANOBS', year = 2010, domain = 'CONUS', format = 'netcdf',
               startDate =  as.character("2/1/79 1:00"),
               endDate   =  as.character("12/31/23 23:00"))

p1 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'FORCING', year = 2010, domain = 'Hawaii', format = 'netcdf',
               startDate =  as.character("1/1/94 1:00"),
               endDate   =  as.character("1/1/14 23:00"))
p2 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'CHRTOUT', year = 2010, domain = 'Hawaii', format = 'netcdf',
               startDate =  as.character("1/1/94 1:00"),
               endDate   =  as.character("1/1/14 23:00"))
p3 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'GWOUT', year = 2010, domain = 'Hawaii', format = 'netcdf',
               startDate =  as.character("1/1/94 1:00"),
               endDate   =  as.character("1/1/14 23:00"))
p4 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'LAKEOUT', year = 2010, domain = 'Hawaii', format = 'netcdf',
               startDate =  as.character("1/1/94 1:00"),
               endDate   =  as.character("1/1/14 23:00"))
p5 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'LDASOUT', year = 2010, domain = 'Hawaii', format = 'netcdf',
               startDate =  as.character("1/1/94 1:00"),
               endDate   =  as.character("1/1/14 23:00"))
p6 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'RTOUT', year = 2010, domain = 'Hawaii', format = 'netcdf',
               startDate =  as.character("1/1/94 1:00"),
               endDate   =  as.character("1/1/14 23:00"))
p7 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'CHANOBS', year = 2010, domain = 'Hawaii', format = 'netcdf',
               startDate =  as.character("1/1/94 1:00"),
               endDate   =  as.character("1/1/14 23:00"))
p8 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'OUTPUT_RTOUT_COMPRESSED', year = 2010, domain = 'Hawaii', format = 'netcdf',
               startDate =  as.character("1/1/94 1:00"),
               endDate   =  as.character("1/1/14 23:00"))
p9 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
                config = 'OUTPUT_LDASOUT_COMPRESSED', year = 2010, domain = 'Hawaii', format = 'netcdf',
                startDate =  as.character("1/1/94 1:00"),
                endDate   =  as.character("1/1/14 23:00"))
p10 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
                config = 'OUTPUT_LAKEOUT', year = 2010, domain = 'Hawaii', format = 'netcdf',
                startDate =  as.character("1/1/94 1:00"),
                endDate   =  as.character("1/1/14 23:00"))

q1 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'FORCING', year = 2010, domain = 'PR', format = 'netcdf',
               startDate =  as.character("1/1/81 1:00"),
               endDate   =  as.character("5/1/23 23:00"))
q2 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'CHRTOUT', year = 2010, domain = 'PR', format = 'netcdf',
               startDate =  as.character("1/1/81 1:00"),
               endDate   =  as.character("5/1/23 23:00"))
q3 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'GWOUT', year = 2010, domain = 'PR', format = 'netcdf',
               startDate =  as.character("1/1/81 1:00"),
               endDate   =  as.character("5/1/23 23:00"))
q4 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'LAKEOUT', year = 2010, domain = 'PR', format = 'netcdf',
               startDate =  as.character("1/1/81 1:00"),
               endDate   =  as.character("5/1/23 23:00"))
q5 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'LDASOUT', year = 2010, domain = 'PR', format = 'netcdf',
               startDate =  as.character("1/1/81 1:00"),
               endDate   =  as.character("5/1/23 23:00"))
q6 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'RTOUT', year = 2010, domain = 'PR', format = 'netcdf',
               startDate =  as.character("1/1/81 1:00"),
               endDate   =  as.character("5/1/23 23:00"))
q7 = scrape_aws(version = "3.0", bucket = "s3://noaa-nwm-retrospective-3-0-pds",
               config = 'CHANOBS', year = 2010, domain = 'PR', format = 'netcdf',
               startDate =  as.character("1/1/81 1:00"),
               endDate   =  as.character("5/1/23 23:00"))


#'http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/'
h = scrape_nomads('http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/', "prod")
i = scrape_nomads('http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/', "v2.2")
j = scrape_nomads('http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/', "para")
k = scrape_nomads('http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/', "v3.0")


#'https://para.nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/'
l = scrape_nomads(url = 'https://para.nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/', version = "para")
m = scrape_nomads('https://para.nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/', version = "v3.0")


nwm_data = bind_rows(a,
                     b,
                     c,
                     d,
                     e,
                     f,
                     g,
                     h,
                     i,
                     j,
                     k,
                     l,
                     m,
                     n1, n2, n3, n4, n5, n6,
                     o1, o2, o3, o4, o5, o6, o7,
                     p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
                     q1, q2, q3, q4, q5, q6, q7
                     )

usethis::use_data(nwm_data, overwrite = TRUE)


