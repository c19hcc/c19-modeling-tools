##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: Functions to pull JHU data 
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

JHU_BASE_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
pull_cumulative_jhu_data <- function(date=NULL, offset=1, export=TRUE){
  if(is.null(date)){
    # get data from previous day (change that current day hasn't been published yet)
    date <- Sys.Date() - offset
  }
  latest_date <- format(as.Date(date), format='%m-%d-%Y')
  url <- paste0(JHU_BASE_URL, latest_date, '.csv')
  df <- read.csv(url)
  if(export){
    write.csv(df, file='./data/jhu_data_latest.csv')
  }
  return(df)
}

pull_jhu_data <- function(){
  if (TRUE){
    all_data <- read.csv("./data/stateLevelJHUdata_US.csv", stringsAsFactors = FALSE)
  }
  else{
    deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
    cases <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
    recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
    
    ## Convert County-level Counts to State Labels, merge into State counts
    
    cases$mState.Providence = str_extract(cases$Province.State, "[A-Z]{2}")
    cases$mState.Providence = state.name[match(cases$mState.Providence,state.abb)]
    cases$mState.Providence = ifelse(cases$Province.State == "Washington, D.C.", "District of Columbia",cases$mState.Providence)
    cases$Lat = ifelse(!is.na(cases$mState.Providence),0,cases$Lat)
    cases$Long = ifelse(!is.na(cases$mState.Providence),0,cases$Long)
    cases$mState.Providence = ifelse(is.na(cases$mState.Providence),as.character(cases$Province.State),cases$mState.Providence)
    cases$mState.Providence = as.factor(cases$mState.Providence)
    cases = cases %>% select(-Province.State)
    cases <- aggregate(. ~  mState.Providence + Country.Region, data = cases, sum)
    
    deaths$mState.Providence = str_extract(deaths$Province.State, "[A-Z]{2}")
    deaths$mState.Providence = state.name[match(deaths$mState.Providence,state.abb)]
    deaths$mState.Providence = ifelse(deaths$Province.State == "Washington, D.C.", "District of Columbia",deaths$mState.Providence)
    deaths$Lat = ifelse(!is.na(deaths$mState.Providence),0,deaths$Lat)
    deaths$Long = ifelse(!is.na(deaths$mState.Providence),0,deaths$Long)
    deaths$mState.Providence = ifelse(is.na(deaths$mState.Providence),as.character(deaths$Province.State),deaths$mState.Providence)
    deaths$mState.Providence = as.factor(deaths$mState.Providence)
    deaths = deaths %>% select(-Province.State)
    deaths <- aggregate(. ~  mState.Providence + Country.Region, data = deaths, sum)
    
    recovered$mState.Providence = str_extract(recovered$Province.State, "[A-Z]{2}")
    recovered$mState.Providence = state.name[match(recovered$mState.Providence,state.abb)]
    recovered$mState.Providence = ifelse(recovered$Province.State == "Washington, D.C.", "District of Columbia",recovered$mState.Providence)
    recovered$Lat = ifelse(!is.na(recovered$mState.Providence),0,recovered$Lat)
    recovered$Long = ifelse(!is.na(recovered$mState.Providence),0,recovered$Long)
    recovered$mState.Providence = ifelse(is.na(recovered$mState.Providence),as.character(recovered$Province.State),recovered$mState.Providence)
    recovered$mState.Providence = as.factor(recovered$mState.Providence)
    recovered = recovered %>% select(-Province.State)
    recovered <- aggregate(. ~  mState.Providence + Country.Region, data = recovered, sum)
    
    ## Merge and Format Data
    cases_long <- gather(cases, date, case_count, -mState.Providence, -Country.Region, -Lat, -Long)
    cases_long$date = parse_date_time(substring(cases_long[,5], 2),orders="mdy")
    
    deaths_long <- gather(deaths, date, death_count, -mState.Providence, -Country.Region, -Lat, -Long)
    deaths_long$date = parse_date_time(substring(deaths_long[,5], 2),orders="mdy")
    
    recovered_long <- gather(recovered, date, recovered_count, -mState.Providence, -Country.Region, -Lat, -Long)
    recovered_long$date = parse_date_time(substring(recovered_long[,5], 2),orders="mdy")
    
    long_data <- cases_long %>% left_join(deaths_long) %>% left_join(recovered_long)
    
    long_data_lag <- long_data %>% group_by(mState.Providence, Country.Region) %>%
      mutate(prev_cases = lag(case_count, order_by = date)) %>%
      mutate(prev_deaths = lag(death_count, order_by = date)) %>%
      mutate(prev_recovered = lag(recovered_count, order_by = date)) 
    
    long_data_lag$delta_cases = long_data_lag$case_count - long_data_lag$prev_cases
    long_data_lag$delta_deaths = long_data_lag$death_count - long_data_lag$prev_deaths
    long_data_lag$delta_recovered = long_data_lag$recovered_count - long_data_lag$prev_recovered
    
    firstDeathDate <- long_data_lag %>% 
      filter(death_count>0) %>% group_by(mState.Providence, Country.Region) %>% filter(date == min(date)) %>%
      select(mState.Providence, Country.Region, date) %>% dplyr::rename(firstDeathDate = date)
    
    all_data <- long_data_lag %>% left_join(firstDeathDate)
    
    all_data$daysFromFirstDeath = (all_data$date - all_data$firstDeathDate)/86400
    
    # By Country
    bycountry = all_data %>% group_by(Country.Region, date) %>% 
      dplyr::summarize(total_cases = sum(case_count), total_cases_delta = sum(delta_cases, na.rm = T), 
                       total_deaths = sum(death_count), total_deaths_delta = sum(delta_deaths, na.rm = T), 
                       total_recovered = sum(recovered_count), total_recovered_delta = sum(delta_recovered, na.rm = T)
      )
    
    firstDeathDateCountry = bycountry %>% dplyr::filter(total_deaths>0) %>% group_by(Country.Region) %>%
      dplyr::filter(date == min(date)) %>%
      dplyr::select(Country.Region, date) %>% dplyr::rename(firstDeathDateCountry = date)
    
    bycountry <- bycountry %>% left_join(firstDeathDateCountry)
    bycountry$daysFromFirstDeathCountry = (bycountry$date - bycountry$firstDeathDateCountry)/86400
    
    #All data, time from country's first death
    all_data <- all_data %>% left_join(firstDeathDateCountry)
    all_data$daysFromFirstDeathCountry = (all_data$date - all_data$firstDeathDateCountry)/86400
  }
  return(all_data)
}
