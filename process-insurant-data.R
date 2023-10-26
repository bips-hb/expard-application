library(dplyr)
library(readr)

source("init.R")

# Reads in the insurants data and determines for which time points they 
# were insured

data_hkk <- readr::read_rds("data/insurant_hkk.rds")
data_aok <- readr::read_rds("data/insurant_aok.rds")

data <- dplyr::bind_rows(data_hkk, data_aok)

# returns a time point, where Q1 2004 is time point 1 and 
# Q4 in 2017 is time point 56
return_time_point <- function(year, quarter) { 
  # earliest year is 2004 
  year <- year - 2004 
  
  return(4*year + quarter)
}

# get year and quarter for beginning and end insurance
data$quarter_begin <- lubridate::quarter(data$period_beg)
data$year_begin    <- lubridate::year(data$period_beg)

data$quarter_end <- lubridate::quarter(data$period_last_end)
data$year_end    <- lubridate::year(data$period_last_end)

data <- data %>% mutate(
  time_begin = return_time_point(year_begin, quarter_begin), 
  time_end = return_time_point(year_end, quarter_end) 
)

readr::write_rds(data, "data/processed-insurant-data.rds")

