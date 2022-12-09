pacman::p_load(dplyr, glue, tidyr, readr, xml2, rvest)

# AWS Scrapping Function ------------------------------

scrape_aws = function(version, bucket, config, year, startDate, endDate){
  
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
  
  yy = data.table::fread("data/aws_scrape.csv") %>% 
    select(path = V4) %>% 
    filter(path != "") %>% 
    filter(path != ":") %>% 
    mutate(source = "aws", config = !!config, bucket = bucket) %>% 
    separate(path, sep = "\\.", c("date", "output", "ext")) %>% 
    mutate(http_pattern = http_pattern,
           hour = as.numeric(substr(date,9,10)),
           date = NULL,
           ext = NULL) %>% 
    distinct() %>% 
    group_by(output) %>% 
    arrange(hour) %>% 
    mutate(timestep = median(hour - lag(hour), na.rm  = TRUE),
           domain = "conus", 
           hour = NULL,
           horizion = 1,
           prefix = "",
           version = !!version,
           startDate = startDate, endDate = endDate) %>% 
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

## DATA CHANGES ON nwm.20190619 ###

a = scrape_gcp(version = "2.1", token = "nwm.20181029", startDate = "2018-09-17", endDate = "2019-06-18")

b = scrape_gcp(version = "2.2", token = "nwm.20221029", startDate = "2019-06-19", endDate = "..")

c = scrape_aws(version = "2.1", 
               bucket = "s3://noaa-nwm-retrospective-2-1-pds",  
               config = "model_output",  year = 2015,
               startDate = as.character(get_nwm_meta(version = "2.1")$minDate), 
               endDate   = as.character(get_nwm_meta(version = "2.1")$maxDate))

table(c$output)

d = scrape_aws(version = "2.1", bucket = "s3://noaa-nwm-retrospective-2-1-pds",  
               config = "forcing", year = 2015,
               startDate =  as.character(get_nwm_meta(version = "2.1")$minDate), 
               endDate   =  as.character(get_nwm_meta(version = "2.1")$maxDate))

e = scrape_aws(version = "2.0", bucket = "s3://noaa-nwm-retro-v2.0-pds",  
               config = "full_physics", year = 2015,
               startDate =  as.character(get_nwm_meta(version = "2")$minDate), 
               endDate   =  as.character(get_nwm_meta(version = "2")$maxDate))

f = scrape_aws(version = "2.0", bucket = "s3://noaa-nwm-retro-v2.0-pds",
               config = "long_range", year = 2015,
               startDate =  as.character(get_nwm_meta(version = "2")$minDate), 
               endDate   =  as.character(get_nwm_meta(version = "2")$maxDate))

g = scrape_aws(version = "1.2", bucket = "s3://nwm-archive", 
               config = NA, year = 2015,
               startDate = as.character(get_nwm_meta(version = "1.2")$minDate), 
               endDate   =  as.character(get_nwm_meta(version = "1.2")$maxDate))

h = b %>% 
  mutate(source = "nomads", version = "v2.2", stateDate = "..", endDate = "..") %>% 
  mutate(http_pattern = paste0('http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/',version,
                 '/nwm.{YYYYDDMM}/', config, '/nwm.t{HH}z.', config, '.',
                 output, '.', prefix, '{foward}.',domain,'.nc'))

i =  b %>% 
  mutate(source = "nomads", version = "prod", stateDate = "..", endDate = "..") %>% 
  mutate(http_pattern = paste0('http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/',version,
                               '/nwm.{YYYYDDMM}/', config, '/nwm.t{HH}z.', config, '.',
                               output, '.', prefix, '{foward}.',domain,'.nc'))

nwm_data = bind_rows(a,b,c,d,e,f,g,h,i)


usethis::use_data(nwm_data, overwrite = TRUE, internal = TRUE)



