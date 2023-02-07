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
               startDate = as.character(get_nwm_meta(version = "2.1")$minDate),
               endDate   =  as.character(get_nwm_meta(version = "2.1")$maxDate))

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
               startDate = as.character(get_nwm_meta(version  = "1.2")$minDate), 
               endDate   =  as.character(get_nwm_meta(version = "1.2")$maxDate))

#'https://para.nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/'
h = scrape_nomads('http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/', "prod")
i = scrape_nomads('http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/', "v2.2")

#'https://para.nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/'
j = scrape_nomads(url = 'https://para.nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/', version = "para")
k = scrape_nomads('https://para.nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/', "v3.0")


nwm_data = bind_rows(a,b,c,d,e,f,g,h,i,j,k)

usethis::use_data(nwm_data, overwrite = TRUE, internal = TRUE)


