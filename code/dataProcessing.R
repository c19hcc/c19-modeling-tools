##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: Data processing functions to support database and app.
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(usmap)
library(hash)
library(googlesheets4)
library(readxl)
library(pracma)
library(data.table)
sheets_deauth()
library(dplyr)

source('code/pull_jhu_function.R')

STATE_NAMES = unique(statepop[,c('full','abbr')])
STATE_LOOKUP_NAME <- hash::hash(STATE_NAMES$full, STATE_NAMES$abbr)
STATE_LOOKUP_ABBR <- hash::hash(STATE_NAMES$abbr, STATE_NAMES$full)
WORLD_POP <- data.frame(fread('data/database/world_pop.csv', stringsAsFactors = F))

get_national_stats <- function(all_covid_data=NULL, rt_df=NULL, doubling_days=5){
  # now only pulling recovery statistics from JHU
  # jhu_data <- get_national_covid_data(all_covid_data) %>%
  #   mutate('date' = as.Date(date))
  # pulling death and case counts from usafacts county-level data now.
  usafacts_data <- get_national_time_series()
  return(get_infection_stats(usafacts_data, region='United States', rt_df=rt_df, doubling_days=doubling_days))
}

# get_national_covid_data <- function(jhu_data=NULL){
#   if(is.null(jhu_data)){
#     jhu_data = pull_jhu_data()
#   }
#   jhu_data = jhu_data %>% filter(Country.Region == 'US') %>%
#     mutate('date' = as.Date(date)) %>%
#     group_by(date) %>%
#     dplyr::summarize( "recovered_count" = sum(recovered_count, na.rm=T))
#   usafacts_data <- get_national_time_series()
#   comb_df <- usafacts_data %>% left_join(jhu_data, by='date', all.x=T)
#   return(comb_df)
# }

get_states_infection_stats <- function(state_list=STATE_NAMES$full, n=NULL, rt_df=NULL, pop_df=NULL){
  #state_list <- Filter(isState, state_list)
  covid_data <- STATE_INF_STATS #TODO KRF test; lapply(state_list, function(x){get_state_time_series(x)})
  if(is.null(pop_df)){pop_df = WORLD_POP %>% filter(Country.Region=='United States')}
  states_res <- lapply(state_list, function(x){get_state_infection_stats(x, covid_data, doubling_days=n, rt_df=rt_df, pop_df=pop_df)})
  res_df <- as.data.frame(t(matrix(unlist(states_res), nrow=length(unlist(states_res[1])))))
  colnames(res_df) <- names(states_res[[1]])
  date_cols <- c('fi_date','fd_date', 'rt_date')
  measure_cols <- setdiff(colnames(res_df), date_cols)
  res_df$state <- state_list
  for(col in measure_cols){
    res_df[,col] <- suppressWarnings(as.numeric(as.character(res_df[,col])))
  }
  for(date_col in date_cols){
    res_df[,date_col] <- suppressWarnings(as.Date(as.integer(as.character(res_df[,date_col])), origin="1970-01-01"))
  }
  res_df <- res_df[,c('state',date_cols,measure_cols)]
  return(res_df)
}

get_state_infection_stats <- function(state, all_state_data=NULL, doubling_days=NULL, rt_df=NULL, pop_df=NULL){
  if(is.null(all_state_data)){
   state_df <- get_state_covid_data(state)
  }else{
    if(nchar(state) > 2){state <- state_lookup(state)}
    state_df <- all_state_data %>% filter(State == state)
  }
  return(get_infection_stats(state_df, region=state, doubling_days=doubling_days, rt_df=rt_df, pop_df=pop_df))
}

get_county_infection_stats <- function(county_full, all_counties_data=NULL, doubling_days=NULL, rt_df=NULL, pop_df=NULL){
  if(is.null(all_counties_data)){
    county_df = COUNTY_INF_STATS
  }
  county_df = county_df %>% filter(mState.Providence==county_full)
  return(get_infection_stats(county_df, region=county_full, rt_df=rt_df, doubling_days=doubling_days, pop_df=pop_df))
}

get_infection_stats <- function(df, region=NULL, doubling_days=NULL, rt_df=NULL, pop_df=NULL, pop_col='population'){
  # only care about us populations atm
  if(is.null(pop_df)){
    pop_df = WORLD_POP %>% filter(Country.Region=='United States')
  }
  
  # date of first infection
  first_inf_date_idx <- which(df$case_count > 0)[1]
  first_inf_date <- df[first_inf_date_idx,'date'][[1]]
  # infection count on first infection date
  first_inf_ct <-  df[first_inf_date_idx,'case_count']
  # first death date (if any)
  first_dth_date_idx <- which(df$death_count > 0)
  if(length(first_dth_date_idx) > 0){
    dth_idx <- first_dth_date_idx[1]
    first_dth_date <- df[dth_idx,'date'][[1]]
    first_dth_ct <-  df[dth_idx,'death_count']
  }else{
    first_dth_date <- NA
    first_dth_ct <- NA
  }
  # calculate doubling time
  doubling_time <- df %>% 
    filter(date > first_inf_date) %>%
    calc_doubling_time(n=doubling_days)
  
  # get population if region provided
  if(!is.null(region)){
    # if region is state abbreviation, convert it to state name
    if(nchar(region)==2) region = state_lookup(region)
    pop = pop_df[pop_df$mState.Providence==region,pop_col]
  }else{
    pop = NULL
  }
  
  # if rt_df exists, and specific state chosen, get that states latest RT data
  if(!is.null(rt_df) && !is.null(region)){
    region_name = region
    region_rt = rt_df %>% filter(region==region_name) %>% arrange(date)
    ndays = min(doubling_days, nrow(region_rt)-1)
    # region_rt = region_rt %>% mutate(rt_n_day_lag = lag(ML, n=ndays), 
    #                                  rt_n_day_chg = (ML - rt_n_day_lag)/rt_n_day_lag)
    rt_date = max(region_rt$date)
    rt_std = round(sqrt(var(region_rt$ML, na.rm=T)), 3)
    rt = region_rt[region_rt$date==rt_date, 'ML'][[1]]
    #rt_n_day_pct_chg = round(region_rt[region_rt$date==rt_date, 'rt_n_day_chg'][[1]], 3)
  }else{
    rt = NA
    rt_date = NA
    rt_std= NA
    #rt_n_day_pct_chg = NA
  }
  return(list('fi_date'=first_inf_date,
              'fi_ct'=first_inf_ct,
              'fd_date' = first_dth_date,
              'fd_ct' = first_dth_ct,
              'tot_deaths' = max(df$death_count), # usafacts - cumulative
              'tot_cases' = max(df$case_count), # usafacts - cumulative
              #'tot_recovered' = sum(df$recovered_count, na.rm=T), # jhu - timeseries
              'doubling_time' = doubling_time, 
              'rt'=rt,
              'rt_date' = rt_date,
              #'rt_n_day_pct_chg' = rt_n_day_pct_chg,
              'rt_std' = rt_std,
              'pop'=pop))
}


get_states_resource_stats <- function(resource_df, state_list=NULL,  def_bed_util=.66, def_icu_util=.68, scale=1000){
  if(is.null(state_list)) state_list <- c('DC', STATE_NAMES$abbr)
  states_res <- lapply(state_list, function(state){get_state_resource_stats(resource_df,state,def_bed_util,def_icu_util,scale)})
  res_df <- as.data.frame(t(matrix(unlist(states_res), nrow=length(unlist(states_res[1])))))
  measure_cols <- setdiff(names(states_res[[1]]), c('region'))
  colnames(res_df) <- c('state',measure_cols)
  for(col in measure_cols){
    res_df[,col] <- as.numeric(as.character(res_df[,col]))
  }
  return(res_df)
}

# Get county-level hospital resource statistics for SEIR model. 
# Assumes resource df is at the hospital level and includes all hosps in US. Also don't have ICU util, so using national avg.
get_county_resource_stats <- function(resource_df, county_full, def_bed_util=.66, def_icu_util=.68, scale=1000){
  county_name = strsplit(county_full, ', ')[[1]][[1]]
  state = strsplit(county_full, ', ')[[1]][[2]]
  county_df = resource_df %>%
    filter(County==county_name, State==state)
  pop = WORLD_POP %>% filter(mState.Providence==paste(county_name, state, sep=', ')) %>% .$population
  county_df = county_df %>% filter(!is.na(NUM_LICENSED_BEDS)) %>%
    mutate(BED_UTILIZATION = replace_na(BED_UTILIZATION, def_bed_util))
  tot_beds <- sum(county_df$NUM_STAFFED_BEDS, na.rm=T)
  tot_licensed_beds <- sum(county_df$NUM_LICENSED_BEDS, na.rm=T)
  tot_icu_beds <- sum(county_df$NUM_ICU_BEDS, na.rm=T)
  avg_bed_util <- sum(county_df$BED_UTILIZATION*county_df$NUM_STAFFED_BEDS, na.rm=T) /
    sum(county_df$NUM_STAFFED_BEDS, na.rm=T)
  return(list('region' = paste(county_name, state, sep=', '),
              'HospBedper'= round(tot_beds/pop * scale, 1),
              'HospBedOcc' = round(avg_bed_util, 2),
              'ICUBedper' = round(tot_icu_beds/pop * scale, 3),
              'ICUBedOcc' = round(def_icu_util, 2),
              'TotHospBedsAvail' = tot_licensed_beds,
              'TotHospBedsOcc' = round(sum(county_df$BED_UTILIZATION*county_df$NUM_LICENSED_BEDS,na.rm=TRUE),0),
              'TotICUHospBedsAvail' = tot_icu_beds,
              'TotICUHospBedsOcc' = round(sum(county_df$BED_UTILIZATION*county_df$NUM_ICU_BEDS,na.rm=TRUE),0)
  )
  )
}

# Get state-level hospital resource statistics for SEIR model. 
# assumes resource df is at the hospital level and includes all hosps in US. Also don't have ICU util, so using national avg.
get_state_resource_stats<- function(resource_df, state_abbr=NULL, def_bed_util=.66, def_icu_util=.68, scale=1000){
  if(!is.null(state_abbr)){
    resource_df <- resource_df %>% filter(State==state_abbr)
    pop = statepop[statepop$abbr==state_abbr, 'pop_2015'][[1]]
    region = state_lookup(state_abbr)
  }else{
    pop =  sum(statepop$pop_2015)
    region = 'United States'
  }
  # only include hospitals that have bed counts
  resource_df <- resource_df %>% filter(!is.na(NUM_LICENSED_BEDS)) %>%
    mutate(BED_UTILIZATION = replace_na(BED_UTILIZATION, def_bed_util))
  tot_beds <- sum(resource_df$NUM_STAFFED_BEDS, na.rm=T)
  tot_licensed_beds <- sum(resource_df$NUM_LICENSED_BEDS, na.rm=T)
  tot_icu_beds <- sum(resource_df$NUM_ICU_BEDS, na.rm=T)
  avg_bed_util <- sum(resource_df$BED_UTILIZATION*resource_df$NUM_STAFFED_BEDS, na.rm=T) /
    sum(resource_df$NUM_STAFFED_BEDS, na.rm=T)
  return(list('region' = region,
              'HospBedper'= round(tot_beds/pop * scale, 1),
              'HospBedOcc' = round(avg_bed_util, 2),
              'ICUBedper' = round(tot_icu_beds/pop * scale, 3),
              'ICUBedOcc' = round(def_icu_util, 2),
              'TotHospBedsAvail' = tot_licensed_beds,
              'TotHospBedsOcc' = round(sum(resource_df$BED_UTILIZATION*resource_df$NUM_LICENSED_BEDS,na.rm=TRUE),0),
              'TotICUHospBedsAvail' = tot_icu_beds,
              'TotICUHospBedsOcc' = round(sum(resource_df$BED_UTILIZATION*resource_df$NUM_ICU_BEDS,na.rm=TRUE),0)
              )
  )
  
}

get_state_covid_data <- function(state){
  if(nchar(state) > 2){
    state_name = state
    state <- state_lookup(state_name)
  }else{
    state_name = state_lookup(state)
  }
  usafacts_state <- get_state_time_series(state)
  #state_df <- usafacts_state %>% left_join(jhu_state[,c('date','recovered_count')], by='date', all.x=T)
  return(usafacts_state)
}

isState <- function(region){
  return(!startsWith(tolower(region), 'distric'))
}

get_jhu_state_counts <- function(state_name){
  jhu_data <- STATE_INF_STATS
  # get regional data
  jhu_state <- jhu_data %>% filter(Province_State==state_name)
  # sort by date
  jhu_state <- jhu_state  %>% mutate('date' = as.Date(Last_Update)) %>% arrange(date)
  return(jhu_state)
}


#============Time series from USAFACTS=========#
get_national_time_series <- function(char_cols = c(1:4)){
  conf <- USFACTS_CONFIRMED %>%
    select(-char_cols)  %>% mutate('region' = 'US')
  deaths <- USFACTS_DEATHS %>%
    select(-char_cols) %>% mutate('region' = 'US')
  nat_conf_long <- aggregate(. ~  region, data = conf, sum) %>% select(-region) %>%
    gather(date, case_count) %>% strip_dates() 
  nat_deaths_long <- aggregate(. ~  region, data = deaths, sum) %>% select(-region) %>%
    gather(date, death_count) %>% strip_dates() 
  nat_comb <- left_join(nat_conf_long, nat_deaths_long, by='date', all.x=T)
  return(nat_comb)
}

get_all_states_national_time_series_v2 <- function(){
  state_dfs <- plyr::ldply(.data = STATE_NAMES$full, .fun = get_state_time_series)
  comb_df <- state_dfs #bind_rows(state_dfs)
  return(comb_df)
}


get_state_time_series <- function(state){
  if(nchar(state) > 2){
    state_name = state
    state = state_lookup(state)
  }else{
    state_name = state_lookup(state)
  }
  conf <- USFACTS_CONFIRMED
  deaths <- USFACTS_DEATHS
  state_conf_long <- county_to_state_counts(conf, state) %>% rename('case_count'='value') %>%
    mutate('StateName'=state_name)
  state_death_long<- county_to_state_counts(deaths, state) %>%
    rename('death_count'='value') %>% select(-State)
  state_comb <-  left_join(state_conf_long, state_death_long, by='date', all.x=T) %>%
    mutate(
      recovered_count=NA, #not filling it in currently
      Country.Region = "United States",
      firstDeathDate = suppressWarnings(min(date[death_count>0], na.rm = TRUE)),
      firstDeathDate = as.Date(firstDeathDate),
      firstCaseDate = suppressWarnings(min(date[case_count>0], na.rm = TRUE)),
      firstCaseDate = as.Date(firstCaseDate)
    ) %>%
    select(mState.Providence=StateName,State, Country.Region, date, case_count, death_count, recovered_count, firstDeathDate, firstCaseDate)
  return(state_comb)
}

# metropolitan area time series
# get_mbsa_time_series <- function(){
#   mbsa_df <-  read.csv('data/mbsa-aggreagate.csv') %>%
#     mutate(date=as.Date(Last_Update)) %>%
#     group_by(NAME) %>%
#     mutate(
#       death_count = Deaths,
#       case_count = Confirmed,
#       recovered_count= Recovered,
#       Country.Region = "United States",
#       firstDeathDate = suppressWarnings(min(date[death_count>0], na.rm = TRUE)),
#       firstDeathDate = as.Date(firstDeathDate),
#       firstCaseDate = suppressWarnings(min(date[case_count>0], na.rm = TRUE)),
#       firstCaseDate = as.Date(firstCaseDate)
#     ) %>%
#     ungroup() %>%
#     select(mState.Providence=NAME, Country.Region, date, case_count, death_count, recovered_count, firstDeathDate, firstCaseDate)
#   return(mbsa_df)
# }

# metropolitan area time series
get_mbsa_time_series <- function(){
  conf <- USFACTS_CONFIRMED
  deaths <- USFACTS_DEATHS
  msa.dat <- read.csv('./data/msa-crosswalk.csv', col.names = c("countyFIPS", "msa_code", "msa_name"), stringsAsFactors = FALSE)
  
  msa_conf_long <- gather(conf %>% left_join(msa.dat, by = "countyFIPS"), date, value, -countyFIPS, -County.Name, -State, -stateFIPS, -msa_code, -msa_name) %>%
    strip_dates()  %>% rename('case_count'='value') %>% 
    select(msa_name, date, case_count) %>% filter(!is.na(msa_name)) %>% 
    group_by(msa_name, date) %>% summarise(case_count = sum(case_count)) 
  msa_death_long<- gather(deaths %>% left_join(msa.dat, by = "countyFIPS"), date, value, -countyFIPS, -County.Name, -State, -stateFIPS, -msa_code, -msa_name) %>%
    strip_dates()  %>% rename('death_count'='value') %>% 
    select(msa_name, date, death_count) %>% filter(!is.na(msa_name)) %>% 
    group_by(msa_name, date) %>% summarise(death_count = sum(death_count)) 
  
  mbsa_df <- left_join(msa_conf_long, msa_death_long, by=c('msa_name','date'), all.x=T) %>% 
    mutate(
      recovered_count= NA,
      Country.Region = "United States",
      firstDeathDate = suppressWarnings(min(date[death_count>0], na.rm = TRUE)),
      firstDeathDate = as.Date(firstDeathDate),
      firstCaseDate = suppressWarnings(min(date[case_count>0], na.rm = TRUE)),
      firstCaseDate = as.Date(firstCaseDate)
    ) %>%
    ungroup() %>%
    select(mState.Providence=msa_name, Country.Region, date, case_count, death_count, recovered_count, firstDeathDate, firstCaseDate)
  return(mbsa_df)
}

# county-level time series
get_counties_time_series <- function(daycutoff=NULL){
  cases <- USFACTS_CONFIRMED %>%
    gather(key="date", value="case_count", -countyFIPS,-County.Name, -State, -stateFIPS) %>%
    strip_dates() %>%
    mutate(CountyState = paste(County.Name, State, sep=', '))
  
  ## remove unusual spikes at the county level that return to zero
  last_zero <- cases %>% filter(case_count == 0) %>% group_by(countyFIPS, stateFIPS) %>% summarise(lastzero = max(date))
  cases_lastzero <- cases %>% left_join(last_zero, by = c("countyFIPS", "stateFIPS")) 
  cases_lastzero$case_count <- ifelse(is.na(cases_lastzero$lastzero), cases_lastzero$case_count,
                                      ifelse(cases_lastzero$date <= cases_lastzero$lastzero, 0, cases_lastzero$case_count))
  
  cases <- cases_lastzero %>% select(countyFIPS, County.Name, State, stateFIPS, date, case_count, CountyState)
  
  deaths <- USFACTS_DEATHS %>%
    gather(key="date", value="death_count", -countyFIPS,-County.Name, -State, -stateFIPS) %>%
    mutate(CountyState = paste(County.Name, State, sep=', ')) %>%
    strip_dates() %>% 
    select(countyFIPS, date, death_count, stateFIPS) 
    
  comb_df <- cases %>% 
    left_join(deaths, by=c('countyFIPS', 'stateFIPS', 'date')) %>%
    group_by(countyFIPS, stateFIPS) %>%
    mutate(
      recovered_count= NA, # not collecting recovery data at the county level anymore
      Country.Region = "United States",
      firstDeathDate = suppressWarnings(min(date[death_count>0], na.rm = TRUE)),
      firstDeathDate = as.Date(firstDeathDate),
      firstCaseDate = suppressWarnings(min(date[case_count>0], na.rm = TRUE)),
      firstCaseDate = as.Date(firstCaseDate)
    ) %>%
    ungroup() %>%
    select(mState.Providence=CountyState, Country.Region, date, case_count, death_count, recovered_count, firstDeathDate, firstCaseDate, countyFIPS, stateFIPS) %>%
    distinct()
  
  if (!is.null(daycutoff)){
    latest.day.dat <- comb_df %>%
      group_by(mState.Providence, Country.Region, countyFIPS, stateFIPS) %>%
      filter(date==max(date,na.rm=TRUE)) %>%
      ungroup() %>%
      select(mState.Providence, Country.Region, countyFIPS, stateFIPS)
    
    ret.dat <- latest.day.dat %>%
      left_join(comb_df %>%
                  filter(as.numeric(date-firstCaseDate)>=(-1*daycutoff) | as.numeric(date-firstDeathDate)>=(-1*daycutoff) ),
                by = c("mState.Providence", "Country.Region", "countyFIPS", "stateFIPS")) %>%
      select(mState.Providence, Country.Region, date, case_count, death_count, recovered_count, firstDeathDate, firstCaseDate) %>%
      mutate(mState.Providence = ifelse(mState.Providence=="Broomfield County and City, CO", "Broomfield County, CO", mState.Providence),
             mState.Providence = ifelse(mState.Providence=="Jackson County (including other portions of Kansas City), MO", "Jackson County, MO", mState.Providence),
             mState.Providence = ifelse(mState.Providence=="City of St. Louis, MO", "St. Louis City, MO", mState.Providence)
             )
  }else{
    ret.dat <- comb_df
  }
  
  return(ret.dat)
}

get_all_states_national_time_series <- function(){
  ret.dat <- USFACTS_CONFIRMED %>%
    gather(key="date", value="case_count", -countyFIPS,-County.Name, -State, -stateFIPS) %>%
    strip_dates() %>%
    group_by(State, date) %>%
    summarise(case_count=sum(case_count, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(
      USFACTS_DEATHS %>%
        gather(key="date", value="death_count", -countyFIPS,-County.Name, -State, -stateFIPS) %>%
        strip_dates() %>%
        group_by(State, date) %>%
        summarise(death_count=sum(death_count, na.rm = TRUE)) %>%
        ungroup(),
      by = c("State", "date")
    ) %>%
    mutate(State = as.character(State)) %>%
    left_join(STATE_NAMES, by = c("State"="abbr")) %>%
    group_by(full) %>%
    mutate(
      recovered_count=NA, #not filling it in currently
      Country.Region = "United States",
      firstDeathDate = suppressWarnings(min(date[death_count>0], na.rm = TRUE)),
      firstDeathDate = as.Date(firstDeathDate),
      firstCaseDate = suppressWarnings(min(date[case_count>0], na.rm = TRUE)),
      firstCaseDate = as.Date(firstCaseDate)
    ) %>%
    ungroup() %>%
    select(mState.Providence=full, Country.Region, date, case_count, death_count, recovered_count, firstDeathDate, firstCaseDate) %>%
    mutate(State = sapply(mState.Providence, state_lookup))
  
  return(ret.dat)
  
}

get_fips = function(region_name){
  if(detect_county(region_name)){
    cty_name = str_split(region_name, ', ')[[1]][[1]]
    st = str_split(region_name, ', ')[[1]][[2]]
    #fips = as.character(COUNTY_CLEANUP %>% filter(County.Pretty == cty_name) %>% .$fips)
    fips = as.character(USFACTS_CONFIRMED %>% filter(County.Name==cty_name, State==st)%>% .$countyFIPS)
    if(nchar(fips) < 5) fips = paste0('0',fips)
  }else{
    if(nchar(region_name) > 2) region_name = state_lookup(region_name)
    fips = as.character(state.fips[state.fips$abb == region_name, 'fips'][[1]])
    if(nchar(fips) < 2) fips = paste0('0',fips, collapse='')
  }
  return(fips)
}


get_all_rt_data <- function(export=T){
  # rt_data = read.csv('https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv', stringsAsFactors = F) %>%
  rt_data = read_data_from_file('data/rt_states.csv', date_cols=c('date')) %>%
    mutate(date = as.Date(date),
           state_abbr=as.character(sapply(region, state_lookup)),
           state=region,
           region=state,
           ML=mean) %>%
    rename(High_90=upper_90, Low_90=lower_90, High_50=upper_50, Low_50=lower_50) %>%
  union_all(
    read_data_from_file('data/rt_counties.csv', date_cols=c('date')) %>%
    select(region,date,mean,median,lower_90,upper_90,lower_50,upper_50) %>%
    mutate(date = as.Date(date),
           state_abbr=as.character(sapply(region, function(r){str_split(r, ', ')[[1]][[2]]})),
           state=as.character(sapply(state_abbr, state_lookup)),
           ML=mean) %>%
      rename(High_90=upper_90, Low_90=lower_90, High_50=upper_50, Low_50=lower_50)
  ) %>%
  union_all(
    read_data_from_file('data/rt_national.csv', date_cols=c('date')) %>%
      mutate(date = as.Date(date),
             state_abbr='USA',
             state='United States',
             ML=mean) %>%
      rename(High_90=upper_90, Low_90=lower_90, High_50=upper_50, Low_50=lower_50) 
  ) %>% mutate(state_abbr = ifelse(state_abbr=="NULL", NA, state_abbr),
    state.region.key = ifelse(is.na(state_abbr), paste0(region, '___Mexico'), paste0(region, '___United States')))
  if(export) write.csv(rt_data, file='data/database/rt.csv', row.names=F)
  return(rt_data)
}
# had to change this back because the DT function was causing errors when I rebuilt the database. 
read_data_from_file <- function(fname, date_cols = c('date','firstDeathDate','firstCaseDate')){
  df <- data.frame(fread(fname, stringsAsFactors = F))
  for(date_col in date_cols){
    tryCatch(expr = {df[, date_col] = as.Date(df[,date_col])}, 
             error = function(e){warning(paste0('error parsing dates in ', date_col))})
  }
  return(data.frame(df))
}



# read_data_from_file <- function(fname, date_cols = c('date','firstDeathDate','firstCaseDate')){
#   df <- fread(fname, stringsAsFactors = F)
#   if (length(date_cols)>0){
#     tryCatch(expr = {setDT(df)[, (date_cols) := lapply(.SD, anytime::anydate), .SDcols = date_cols]},
#              error = function(e){warning(paste("error parsing dates for file:", fname))})
#   }
#   #for(date_col in date_cols){
#   #  tryCatch(expr = {df[, date_col := as.Date(date_col)]}, # = as.Date(df[,date_col])},
#   #           error = function(e){warning(paste0('Couldnt parse dates in ', date_col))})
#   #} 
#   return(data.frame(df))
# }


# get_npi_data <- function(state_params=NULL){
# 
#   npi.three.to.one.map <- c("SAH"="H", "NBC"="B", "LGB"="G", "SC"="S", "BAR"="R", "MQT"="T")
#   ret.dat <- read.csv("./data/npi_categorized.csv", stringsAsFactors = FALSE, 
#                       col.names = c("State", "Policy_Long", "Policy", "Start_Date", "Details", "Category")) %>%
#     rename(state=State, policy=Policy, policy_long=Policy_Long, start_date=Start_Date) %>%
#     strip_dates(date_col='start_date') %>%
#     mutate(exp_start_date = NA,
#            exp_end_date = NA,
#            Prop_Reduction_Mild_Trans = NPI_Mild_Effectiveness_Map[policy_long],
#            Prop_Reduction_Severe_Trans = 0,
#            Prop_Reduction_Death = 0,
#            Country.Region="United States"
#     ) %>%
#     select(state, policy_long, policy, start_date, Details, Category, exp_start_date, exp_end_date, 
#            Prop_Reduction_Mild_Trans, Prop_Reduction_Severe_Trans, Prop_Reduction_Death, Country.Region) %>%
#     filter(policy_long %in% CURRENT_NPIS) 
#     
#     # if tuned npi values provided, update state npis accordingly
#     if(!is.null(state_params)){
#       ret.dat <- ret.dat %>% 
#         left_join(state_params[,c('state', 'Stay At Home Order', 'Non-Essential Business Closures')] %>%
#                     gather(policy_long, Prop_Reduction_Mild_Trans, -state) %>%
#                     rename(Opt_Prop_Reduction_Mild_Trans=Prop_Reduction_Mild_Trans), 
#                   by=c('state', 'policy_long')) %>%
#         mutate(Prop_Reduction_Mild_Trans = ifelse(is.na(Opt_Prop_Reduction_Mild_Trans),
#                                                   Prop_Reduction_Mild_Trans, Opt_Prop_Reduction_Mild_Trans)) %>%
#         select(-Opt_Prop_Reduction_Mild_Trans)
#     }
#     ret.dat <- ret.dat %>% 
#       union_all(
#         read.csv("./data/npis_country.csv", stringsAsFactors = FALSE, 
#                col.names = c("Country", "Policy_Long", "Policy", "Start_Date", "Details")) %>%
#         rename(Country.Region=Country, policy=Policy, policy_long=Policy_Long, start_date=Start_Date) %>%
#         mutate(
#           state = Country.Region,
#           start_date = as.Date(start_date, format="%m/%d/%y"),
#           exp_start_date = NA,
#           exp_end_date = NA,
#           Prop_Reduction_Mild_Trans = NPI_Mild_Effectiveness_Map[policy_long],
#           Prop_Reduction_Severe_Trans = 0,
#           Prop_Reduction_Death = 0,
#           Category = ""
#         ) %>%
#         filter(policy_long %in% CURRENT_NPIS) %>%
#         select(state, policy_long, policy, start_date, Details, Category, exp_start_date, exp_end_date, 
#                Prop_Reduction_Mild_Trans, Prop_Reduction_Severe_Trans, Prop_Reduction_Death, Country.Region)
#     ) %>%
#     left_join(STATE_NAMES %>%
#                 rename(state_abb = abbr), by = c("state"="full")) %>%
#     mutate(state_abb = ifelse(is.na(state_abb) & Country.Region=="United States", "DC", state_abb))
#   
#   MBSA_COUNTY_NPIS <- MBSA_INF_STATS %>%
#     union_all(COUNTY_INF_STATS) %>%
#     select(mState.Providence, Country.Region) %>%
#     distinct() %>%
#     separate(col=mState.Providence, into = c("City", "States"), sep = ", ", remove = FALSE, fill = "right") %>%
#     separate(col=States, into=c(paste0("state", 1:4)), sep="-", remove = FALSE, fill="right") %>%
#     mutate(state1 = ifelse(is.na(state1), States, state1)) %>%
#     select(-States) %>%
#     gather(key="state_type", value = "state_abb", state1, state2, state3, state4) %>%
#     filter(!is.na(state_abb)) %>%
#     select(-state_type) %>%
#     left_join(STATE_NAMES, by = c("state_abb"="abbr")) %>%
#     left_join(ret.dat, by = c("full" = "state", "Country.Region", "state_abb")) %>%
#     #mutate(Country.Region = state_abb) %>% #this is kinda janky/hacky but gets at having 
#     select(state=mState.Providence, policy_long, policy, start_date, Details, Category, exp_start_date, exp_end_date, 
#            Prop_Reduction_Mild_Trans, Prop_Reduction_Severe_Trans, Prop_Reduction_Death, Country.Region, state_abb) %>%
#     distinct() %>%
#     mutate(state_abb = ifelse(is.na(state_abb) & Country.Region=="United States", "DC", state_abb))
#   
#   # merging it all together!
#   ret.dat <- ret.dat %>%
#     union_all(MBSA_COUNTY_NPIS) %>%
#     filter(!is.na(policy_long)) %>%
#     mutate(policy = sapply(policy, FUN = function(x){return(npi.three.to.one.map[x])}))
#   
#   return(ret.dat)
# }

get_npi_data <- function(state_params=NULL){
  
  # start_dates <- read.xlsx2("data/MITRE COVID-19 NPI Data.xlsx",sheetName = "NPI Date Onset Input") %>% gather(Policy, Start_Date, 2:8)
  # end_dates <- read.xlsx2("data/MITRE COVID-19 NPI Data.xlsx",sheetName = "NPI Date Turn Off") %>% gather(Policy, End_Date, 2:7)
  start_dates <- read.csv("data/mitre_covid_npi_data_date_onset_input.csv", 
                          col.names = c("State", "Emergency.declarations","School.closure", "Mass.gathering.restrictions",
                                        "Bar.restaurant.closures", "Non.essential.business.closures", "Movement.restrictions.shelter.in.place",
                                        "Mandatory.Masks", "Reopening.Plan.Reversal"),
                          stringsAsFactors = FALSE) %>% 
    mutate(State = str_sub(State, start = 1, end = 2)) %>%
    gather(key = "Policy", value = "Start_Date", -State)
  end_dates <- read.csv("data/mitre_covid_npi_data_date_turn_off.csv",
                        col.names = c("State", "Emergency.declarations","School.closure", "Mass.gathering.restrictions",
                                      "Bar.restaurant.closures", "Non.essential.business.closures", "Movement.restrictions.shelter.in.place",
                                      "Mandatory.Masks", "Reopening.Plan.Reversal"),
                        stringsAsFactors = FALSE) %>% 
    mutate(State = str_sub(State, start = 1, end = 2)) %>%
    gather(key = "Policy", value = "End_Date", -State)
  
  # start_dates$Start_Date <- excel_numeric_to_date(as.numeric(start_dates$Start_Date))
  # end_dates$End_Date <- excel_numeric_to_date(as.numeric(end_dates$End_Date))
  
  start_dates$Start_Date <- as.Date(start_dates$Start_Date, tryFormats = c('%m/%d/%y'))
  end_dates$End_Date <- sapply(end_dates$End_Date, function(x){ifelse((nchar(x)==0), NA, as.Date(x, tryFormats=c('%m/%d/%y')))})
  end_dates$End_Date <- as.Date(end_dates$End_Date, origin='1970-01-01')
  sheet.to.app.map <- c("School.closure" = "State-Mandated School Closures", 
                        "Non.essential.business.closures" = "Non-Essential Business Closures",
                        "Bar.restaurant.closures" = "Bar/Restaurant Limits", 
                        "Mass.gathering.restrictions" = "Large Gatherings Ban", 
                        "Movement.restrictions.shelter.in.place" = "Stay At Home Order", 
                        "Emergency.declarations" = "Emergency Declaration", 
                        "Mandatory.Masks" = "Mandatory Masks",
                        "Reopening.Plan.Reversal" = "Reopening Plan Reversal")
  long.to.short.map <- c("State-Mandated School Closures" = "S", 
                        "Non-Essential Business Closures" = "B",
                        "Bar/Restaurant Limits" = "R", 
                        "Large Gatherings Ban" = "G", 
                        "Stay At Home Order" = "H", 
                        "Emergency Declaration" = "E", 
                        "Mandatory Masks" = "M",
                        "Mandatory Quarantine for Travelers" = "T",
                        "Reopening Plan Reversal" = "U")
  
  ret.dat <- start_dates %>% 
    left_join(end_dates) %>% 
    rename(state=State, start_date=Start_Date, end_date = End_Date) %>%
    mutate(policy_long = sapply(Policy, FUN = function(x){return(sheet.to.app.map[x])})) %>%
    mutate(policy = sapply(policy_long, FUN = function(x){return(long.to.short.map[x])})) %>%
    mutate(state = substr(state, 1,2)) %>%
    mutate(exp_start_date = NA,
           exp_end_date = NA,
           Prop_Reduction_Mild_Trans = NPI_Mild_Effectiveness_Map[policy_long],
           Prop_Reduction_Severe_Trans = 0,
           Prop_Reduction_Death = 0,
           Country.Region="United States"
    ) %>%
    select(state, policy_long, policy, start_date, end_date, exp_start_date, exp_end_date, 
           Prop_Reduction_Mild_Trans, Prop_Reduction_Severe_Trans, Prop_Reduction_Death, Country.Region) %>%
    filter(policy_long %in% CURRENT_NPIS)
  
  # if tuned npi values provided, update state npis accordingly
  if(!is.null(state_params)){
    ret.dat <- ret.dat %>% 
      left_join(state_params[,c('state', 'Stay At Home Order', 'Non-Essential Business Closures')] %>%
                  gather(policy_long, Prop_Reduction_Mild_Trans, -state) %>%
                  rename(Opt_Prop_Reduction_Mild_Trans=Prop_Reduction_Mild_Trans), 
                by=c('state', 'policy_long')) %>%
      mutate(Prop_Reduction_Mild_Trans = ifelse(is.na(Opt_Prop_Reduction_Mild_Trans),
                                                Prop_Reduction_Mild_Trans, Opt_Prop_Reduction_Mild_Trans)) %>%
      select(-Opt_Prop_Reduction_Mild_Trans)
  }
  ret.dat <- ret.dat %>% 
    left_join(STATE_NAMES, by = c("state"="abbr")) %>% 
    rename(state_abb = state, state = full) %>%
    union_all(
      read.csv("./data/npis_country.csv", stringsAsFactors = FALSE, 
               col.names = c("Country", "Policy_Long", "Policy", "Start_Date", "Details")) %>%
        rename(Country.Region=Country, policy=Policy, policy_long=Policy_Long, start_date=Start_Date) %>%
        mutate(
          state = Country.Region,
          start_date = as.Date(start_date, format="%m/%d/%y"),
          end_date = NA,
          exp_start_date = NA,
          exp_end_date = NA,
          Prop_Reduction_Mild_Trans = NPI_Mild_Effectiveness_Map[policy_long],
          Prop_Reduction_Severe_Trans = 0,
          Prop_Reduction_Death = 0,
          Category = ""
        ) %>%
        #filter(policy_long %in% CURRENT_NPIS) %>%
        select(state, policy_long, policy, start_date, Details, Category, exp_start_date, exp_end_date, 
               Prop_Reduction_Mild_Trans, Prop_Reduction_Severe_Trans, Prop_Reduction_Death, Country.Region) %>%
        rename(state_abb = state) %>%
        mutate(state = state_abb)
    )
  
  MBSA_COUNTY_NPIS <- MBSA_INF_STATS %>%
    union_all(COUNTY_INF_STATS) %>%
    select(mState.Providence, Country.Region) %>%
    distinct() %>%
    separate(col=mState.Providence, into = c("City", "States"), sep = ", ", remove = FALSE, fill = "right") %>%
    separate(col=States, into=c(paste0("state", 1:4)), sep="-", remove = FALSE, fill="right") %>%
    mutate(state1 = ifelse(is.na(state1), States, state1)) %>%
    select(-States) %>%
    gather(key="state_type", value = "state_abb", state1, state2, state3, state4) %>%
    filter(!is.na(state_abb)) %>%
    select(-state_type) %>%
    left_join(STATE_NAMES, by = c("state_abb"="abbr")) %>%
    left_join(ret.dat, by = c("full" = "state", "Country.Region", "state_abb")) %>%
    #mutate(Country.Region = state_abb) %>% #this is kinda janky/hacky but gets at having 
    select(state=mState.Providence, policy_long, policy, start_date, end_date, Details, Category, exp_start_date, exp_end_date, 
           Prop_Reduction_Mild_Trans, Prop_Reduction_Severe_Trans, Prop_Reduction_Death, Country.Region, state_abb) %>%
    distinct() %>%
    mutate(state_abb = ifelse(is.na(state_abb) & Country.Region=="United States", "DC", state_abb))
  
  # merging it all together!
  ret.dat <- ret.dat %>%
    union_all(MBSA_COUNTY_NPIS) %>%
    filter(!is.na(policy_long)) %>%
    mutate(policy = sapply(policy_long, FUN = function(x){return(long.to.short.map[x])}))
  
  return(ret.dat)
}

pull_jhu_data_sm <- function(typ, is_local){
  if (typ == "cases"){
    tmp <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", stringsAsFactors = FALSE)
    if (is_local){
      write.csv(x = tmp, file = "./data/backup_jhu_global_cases_data.csv", row.names=F)
    }
  }else if (typ == "deaths"){
    tmp <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", stringsAsFactors = FALSE)
    if (is_local){
      write.csv(x = tmp, file = "./data/backup_jhu_global_deaths_data.csv", row.names=F)
    }
  }else{
    tmp <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", stringsAsFactors = FALSE)
    if (is_local){
      write.csv(x = tmp, file = "./data/backup_jhu_global_recovered_data.csv", row.names=F)
    }
  }
  tmp
}

use_backup_data <- function(typ){
  if (typ == "cases"){
    warning(paste("Error reading global cases data from GitHub; Falling back to most recent data copy"))
    tmp <- read.csv(file="./data/backup_jhu_global_cases_data.csv", stringsAsFactors = FALSE)
  }else if (typ== "deaths"){
    warning(paste("Error reading global deaths data from GitHub; Falling back to most recent data copy"))
    tmp <- read.csv(file="./data/backup_jhu_global_deaths_data.csv", stringsAsFactors = FALSE)
  }else if (typ=="recovered"){
    warning(paste("Error reading global recovered data from GitHub; Falling back to most recent data copy"))
    tmp <- read.csv(file="./data/backup_jhu_global_recovered_data.csv", stringsAsFactors = FALSE)
  }else if (typ=="descartes"){
    tmp <- read.csv(file="./data/backup_descartes_mobility_data.csv", stringsAsFactors = FALSE)
  }else if (typ=="cu_60"){
    warning(paste("Error reading cu_60 from github"))
    tmp <- read.csv(file = "./data/backup_cu60.csv", stringsAsFactors=F)
  }else if (typ=="cu_70"){
    warning(paste("Error reading cu_70 from github"))
    tmp <- read.csv(file = "./data/backup_cu70.csv", stringsAsFactors=F)
  }else if (typ=="cu_80"){
    warning(paste("Error reading cu_80 from github"))
    tmp <- read.csv(file = "./data/backup_cu80.csv", stringsAsFactors=F)
  }else if (typ=="ihme"){
    warning(paste("Error reading ihme from github"))
    tmp <- read.csv(file = "./data/backup_ihme.csv", stringsAsFactors=F)
  }else if (typ=="mobs"){
    warning(paste("Error reading mobs from github"))
    tmp <- read.csv(file = "./data/backup_mobs.csv", stringsAsFactors=F)
  }else if (typ=="geneva"){
    warning(paste("Error reading geneva from github"))
    tmp <- read.csv(file = "./data/backup_geneva.csv", stringsAsFactors=F)
  }else if (typ=="austin"){
    warning(paste("Error reading austin from github"))
    tmp <- read.csv(file = "./data/backup_utaustin.csv", stringsAsFactors=F)
  }else if (typ=="yyg"){
    warning(paste("Error reading yyg from github"))
    tmp <- read.csv(file = "./data/backup_yyg.csv", row.names=F)
  }
  tmp
}

pull_descartes_data <- function(is_local){
  tmp <- read.csv("https://raw.githubusercontent.com/descarteslabs/DL-COVID-19/master/DL-us-m50_index.csv", stringsAsFactors = FALSE)
  if (is_local){
    write.csv(x = tmp, file = "./data/backup_descartes_mobility_data.csv", row.names=F)
  }
  return(tmp)
}

get_descartes_mobility_data <- function(){
  is_local <- Sys.getenv('SHINY_PORT') == ""
  
  descartes.dat <- tryCatch(expr = {pull_descartes_data(is_local = is_local)},
                            error = function(e){use_backup_data(typ = "descartes")})
  
  descartes.dat$admin2 <- ifelse(descartes.dat$admin1 == 'Washington, D.C.', 'Washington',descartes.dat$admin2)
  descartes.dat$admin1 <- ifelse(descartes.dat$admin1 == 'Washington, D.C.', 'District of Columbia',descartes.dat$admin1)
  
  temp = descartes.dat[descartes.dat$admin1 == 'District of Columbia',]
  temp$admin2 = ""
  descartes.dat <- rbind(descartes.dat, temp)
  
  ret.dat <- descartes.dat %>%
    rename(state = admin1, county=admin2) %>%
    select(-country_code, -admin_level, -fips) %>%
    left_join(STATE_NAMES, by = c("state"="full")) %>%
    mutate(mState.Providence = ifelse(trimws(county)=="", state, paste(county, abbr, sep=", ")), 
           Country.Region = "United States") %>% 
    select(-abbr, -state, -county) %>%
    gather(key = "date", value = "percent_off_norm_maxMobility", -mState.Providence, -Country.Region) %>%
    mutate(date = str_remove(as.character(date), "X"),
           date = as.Date(date, tryFormats = "%Y.%m.%d")
    )
    
  return(ret.dat)
}

get_def_param = function(param_name, metric=NULL, def_params=NULL){
  if(is.null(def_params)) def_params = read.csv('data/database/default_params.csv', stringsAsFactors = F)
  param_row = def_params[which(def_params$param == param_name),]
  if(!is.null(metric)){
    assert_that(metric %in% c('min','max','step','substep'))
    val_col = metric
  }else{
    val_col = 'value'
  }
  if(param_row$ui_type=='date'){
    if(val_col=='max'){
      param_row[,val_col] = as.Date(param_row[,'min'],  tryFormats=c("%m/%d/%y", "%Y-%m-%d")) + param_row[,'max']
    }else{
      param_row[,val_col] = as.Date(param_row[,val_col],  tryFormats=c("%m/%d/%y", "%Y-%m-%d"))
    }
  }else{
    param_row[,val_col] = as.numeric(param_row[,val_col])
  }
  return(param_row[val_col][[1]])
}

get_cdc_model_comp_data <- function(){
  # var to detect if running locally or on server
  is_local <- Sys.getenv('SHINY_PORT') == ""
  
  elig_quantiles <- c(NA, 0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)
  
  cu_80 <- tryCatch(expr = {pull_cdc_models_github(typ="cu_80", is_local=is_local)},
                    error = function(e){use_backup_data(typ="cu_80")}) %>%
    filter(quantile %in% elig_quantiles) %>%
    mutate(type.quantile = ifelse(type=="point", "point", paste("quant", quantile, sep="_"))) %>%
    select(location_name=contains("location"), forecast_date,target_type=target, target_date = target_end_date, type.quantile, value) %>%
    mutate(fc.name = "cu_80") %>%
    filter(!str_detect(target_type, pattern = "wk"))
  ihme <- tryCatch(expr = {pull_cdc_models_github(typ="ihme", is_local=is_local)},
                    error = function(e){use_backup_data(typ="ihme")}) %>%
    filter(quantile %in% elig_quantiles) %>%
    mutate(type.quantile = ifelse(type=="point", "point", paste("quant", quantile, sep="_"))) %>%
    select(location_name=contains("location"), forecast_date, target_type=target,target_date = target_end_date, type.quantile, value) %>%
    mutate(fc.name = "ihme") %>%
    filter(!str_detect(target_type, pattern = "wk"))
  mobs <- tryCatch(expr = {pull_cdc_models_github(typ="mobs", is_local=is_local)},
                   error = function(e){use_backup_data(typ="mobs")}) %>%
    filter(quantile %in% elig_quantiles) %>%
    mutate(type.quantile = ifelse(type=="point", "point", paste("quant", quantile, sep="_"))) %>%
    select(location_name=contains("location"), forecast_date,target_type=target, target_date = target_end_date, type.quantile, value) %>%
    mutate(fc.name = "mobs") %>%
    filter(str_detect(target_type, pattern = "wk"))
  geneva <- tryCatch(expr = {pull_cdc_models_github(typ="geneva", is_local=is_local)},
                   error = function(e){use_backup_data(typ="geneva")}) %>%
    filter(quantile %in% elig_quantiles) %>%
    mutate(type.quantile = ifelse(type=="point", "point", paste("quant", quantile, sep="_"))) %>%
    select(location_name=contains("location"), forecast_date,target_type=target, target_date = target_end_date, type.quantile, value) %>%
    mutate(fc.name = "geneva")
  austin <- tryCatch(expr = {pull_cdc_models_github(typ="austin", is_local=is_local)},
                     error = function(e){use_backup_data(typ="austin")}) %>%
    filter(quantile %in% elig_quantiles) %>%
    mutate(type.quantile = ifelse(type=="point", "point", paste("quant", quantile, sep="_"))) %>%
    select(location_name=contains("location"), forecast_date, target_type=target,target_date = target_end_date, type.quantile, value) %>%
    mutate(fc.name = "austin") %>%
    filter(str_detect(target_type, pattern = "wk"))
  yyg <- tryCatch(expr = {pull_cdc_models_github(typ="yyg", is_local=is_local)},
                     error = function(e){use_backup_data(typ="yyg")}) %>%
    filter(quantile %in% elig_quantiles) %>%
    mutate(type.quantile = ifelse(type=="point", "point", paste("quant", quantile, sep="_"))) %>%
    select(location_name=contains("location"), forecast_date, target_type=target, target_date = target_end_date, type.quantile, value) %>%
    mutate(fc.name = "yyg") %>%
    filter(str_detect(target_type, pattern = "wk"))
  
  fips.key <- maps::state.fips %>%
    select(fips, abb) %>%
    distinct() %>%
    left_join(STATE_NAMES, by = c("abb"="abbr")) %>%
    mutate(fips = ifelse(str_length(as.character(fips))==1, paste0("0", fips), as.character(fips))) %>%
    select(fips, full)
  
  ret.dat <- cu_80 %>%
    union_all(ihme) %>%
    union_all(mobs) %>%
    union_all(geneva) %>%
    union_all(austin) %>%
    union_all(yyg) %>%
    left_join(fips.key, by = c("location_name"="fips")) %>%
    mutate(location_name = ifelse(location_name == "US" | is.na(full), "United States", full),
           target_pop = ifelse(str_detect(target_type, "death"), "death", "hosp"),
           target_type = ifelse(str_detect(target_type, "inc"), "delta", "cumulative")) %>%
    select(-full) %>%
    rename(mState.Providence = location_name) 
  
  return(ret.dat)
}


pull_cdc_models_github <- function(typ, is_local){
  
  if (typ=="cu_60"){
    tmp <- read.csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed/CU-60contact/2020-04-26-CU-60contact.csv", stringsAsFactors = FALSE)
    if (is_local){
      write.csv(x = tmp, file = "./data/backup_cu60.csv", row.names=F)
    }
  }else if (typ=="cu_70"){
    tmp <- read.csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed/CU-70contact/2020-04-26-CU-70contact.csv", stringsAsFactors = FALSE)
    if (is_local){
      write.csv(x = tmp, file = "./data/backup_cu70.csv", row.names=F)
    }
  }else if (typ=="cu_80"){
    tmp <- read.csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed/CU-select/2020-07-05-CU-select.csv", stringsAsFactors = FALSE)
    if (is_local){
      write.csv(x = tmp, file = "./data/backup_cu80.csv", row.names=F)
    }
  }else if (typ=="ihme"){
    tmp <- read.csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed/IHME-CurveFit/2020-07-06-IHME-CurveFit.csv", stringsAsFactors = FALSE)
    tmp2 <- read.csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv", stringsAsFactors = FALSE)
    tmp <- tmp %>% left_join(tmp2, by = "location") %>% select(-location)
    if (is_local){
      write.csv(x = tmp, file = "./data/backup_ihme.csv", row.names=F)
    }
  }else if (typ=="mobs"){
    tmp <- read.csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed/MOBS-GLEAM_COVID/2020-07-13-MOBS-GLEAM_COVID.csv", stringsAsFactors = FALSE)
    if (is_local){
      write.csv(x = tmp, file = "./data/backup_mobs.csv", row.names=F)
    }
  }else if (typ=="geneva"){
    tmp <- read.csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed/Geneva-DetGrowth/2020-07-12-Geneva-DetGrowth.csv", stringsAsFactors = FALSE)
    if (is_local){
      write.csv(x = tmp, file = "./data/backup_geneva.csv", row.names=F)
    }
  }else if (typ=="austin"){
    tmp <- read.csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed/UT-Mobility/2020-07-13-UT-Mobility.csv", stringsAsFactors = FALSE)
    if (is_local){
      write.csv(x = tmp, file = "./data/backup_utaustin.csv", row.names=F)
    }
  }else if (typ=="yyg"){
    tmp <- read.csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed/YYG-ParamSearch/2020-07-15-YYG-ParamSearch.csv", stringsAsFactors = FALSE)
    if (is_local){
      write.csv(x = tmp, file = "./data/backup_yyg.csv", row.names=F)
    }
  }
  
  return(tmp)
}


get_mexican_dat <- function(){
  
  # var to detect if running locally or on server
  is_local <- Sys.getenv('SHINY_PORT') == ""
  
  base.cases.url <- "https://coronavirus.gob.mx/datos/Downloads/Files/Casos_Diarios_Estado_Nacional_Confirmados_"
  base.deaths.url <- "https://coronavirus.gob.mx/datos/Downloads/Files/Casos_Diarios_Estado_Nacional_Defunciones_"
  
  init.date <- format(Sys.Date()-days(1), "%Y%m%d.csv")
  fallback.date <- format(Sys.Date()-days(2), "%Y%m%d.csv")
  
  backup.cases <- read.csv("./data/mex_raw/mex_cases_backup.csv")
  backup.deaths <- read.csv("./data/mex_raw/mex_deaths_backup.csv")
  
  mex.cases.1 <- tryCatch(expr = suppressWarnings(read.csv(paste0(base.cases.url, init.date))),
                        error = function(x) {return(backup.cases)})
  mex.cases.2 <- tryCatch(expr = suppressWarnings(read.csv(paste0(base.cases.url, fallback.date))),
                          error = function(x) {return(backup.cases)})
  
  if (ncol(mex.cases.1)>=ncol(mex.cases.2)){
    raw.mex.cases <- mex.cases.1
  }else{
    raw.mex.cases <- mex.cases.2
  }
  
  if (is_local & (ncol(raw.mex.cases)>ncol(backup.cases))) {
    write.csv(raw.mex.cases, file = "./data/mex_raw/mex_cases_backup.csv")
  }
  
  mex.deaths.1 <- tryCatch(expr = suppressWarnings(read.csv(paste0(base.deaths.url, init.date))),
                          error = function(x) {return(backup.deaths)})
  mex.deaths.2 <- tryCatch(expr = suppressWarnings(read.csv(paste0(base.deaths.url, fallback.date))),
                          error = function(x) {return(backup.deaths)})
  
  if (ncol(mex.deaths.1)>=ncol(mex.deaths.2)){
    raw.mex.deaths <- mex.deaths.1 
  }else{
    raw.mex.deaths <- mex.deaths.2
  }
  
  if (is_local & ncol(raw.mex.deaths)>ncol(backup.deaths)){
    write.csv(raw.mex.deaths, file = "./data/mex_raw/mex_deaths_backup.csv", row.names = FALSE)
  }
  
  mex.cases <- raw.mex.cases %>%
    select(cve_ent, poblacion, nombre, matches(match = "(X\\d+\\.\\d+\\.\\d+|X\\d+-\\d+-\\d+)")) %>%
    rename(population=poblacion, mState.Province = nombre) %>%
    select(-cve_ent) %>%
    gather(key = "date", value = "case_count", -population, - mState.Province) %>%
    filter(mState.Province != "Nacional") %>%
    mutate(mState.Province = sapply(FUN = simpleCap, X = tolower(mState.Province)),
           date = as.Date(date, tryFormats = c("X%d-%m-%Y", "X%d.%m.%Y")),
           Country.Region = "Mexico",
           mState.Province = as.character(ifelse(mState.Province=="Mexico", "Mexico (State)", mState.Province))
           ) %>%
    group_by(mState.Province) %>%
    mutate(case_count = cumsum(case_count)) %>%
    ungroup()
    
  mex.deaths <- raw.mex.deaths %>%
    select(cve_ent, poblacion, nombre, matches(match = "(X\\d+\\.\\d+\\.\\d+|X\\d+-\\d+-\\d+)")) %>%
    rename(population=poblacion, mState.Province = nombre) %>%
    select(-cve_ent, -population) %>%
    gather(key = "date", value = "death_count", -mState.Province) %>%
    filter(mState.Province != "Nacional") %>%
    mutate(mState.Province = sapply(FUN = simpleCap, X = tolower(mState.Province)),
           date = as.Date(date, tryFormats = c("X%d-%m-%Y", "X%d.%m.%Y")),
           Country.Region = "Mexico",
           mState.Province = as.character(ifelse(mState.Province=="Mexico", "Mexico (State)", mState.Province))
    ) %>%
    group_by(mState.Province) %>%
    mutate(death_count = cumsum(death_count)) %>%
    ungroup()
  
  ret.dat <- mex.cases %>%
    left_join(mex.deaths, by = c("mState.Province", "date", "Country.Region")) %>%
    mutate(death_count = ifelse(is.na(death_count), 0, death_count),
           mState.Providence = mState.Province,
           recovered_count = NA) %>%
    group_by(mState.Providence) %>%
    mutate(
      firstDeathDate = suppressWarnings(min(date[death_count>0], na.rm = TRUE)),
      firstDeathDate = as.Date(firstDeathDate),
      firstCaseDate = suppressWarnings(min(date[case_count>0], na.rm = TRUE)),
      firstCaseDate = as.Date(firstCaseDate)
    ) %>%
    ungroup() %>%
    #mutate(mState.Providence = paste(mState.Providence, "MX", sep = ", ")) %>%
    select(mState.Providence, Country.Region, date, case_count, death_count, recovered_count, firstDeathDate, firstCaseDate, population)
    
  return(ret.dat)
}

get_international_jhu_data <- function(){
  
  # var to detect if running locally or on server
  is_local <- Sys.getenv('SHINY_PORT') == ""
  
  jhu.global.cases <- tryCatch(expr = {pull_jhu_data_sm(typ="cases", is_local = is_local)},
                               error = function(e){use_backup_data(typ = "cases")})
  jhu.global.deaths <- tryCatch(expr= {pull_jhu_data_sm(typ="deaths", is_local = is_local)},
                                error = function(e){use_backup_data(typ = "deaths")})
  jhu.global.recovered <- tryCatch(expr={pull_jhu_data_sm(typ="recovered", is_local = is_local)},
                                   error = function(e){use_backup_data(typ = "recovered")})
  
  ret.dat <- jhu.global.cases %>%
    gather(key="date", value="case_count", -Province.State,-Country.Region, -Lat, -Long) %>%
    mutate(date = str_remove(as.character(date), "X"),
             date = as.Date(date, tryFormats = "%m.%d.%y")
    ) %>%
    mutate(Country.Region = ifelse(Country.Region == "Burma", "Myanmar", Country.Region), 
           Country.Region = ifelse(Country.Region == "Kosovo", "Serbia", Country.Region), 
           Country.Region = ifelse(Country.Region == "Taiwan*", "Taiwan", Country.Region), 
           Country.Region = ifelse(Country.Region == "West Bank and Gaza", "State of Palestine", Country.Region), 
    ) %>%
    group_by(Country.Region, date) %>%
    summarise(case_count=sum(case_count, na.rm = TRUE)) %>%
    ungroup() %>% 
    left_join(
      jhu.global.deaths %>%
        gather(key="date", value="death_count", -Province.State,-Country.Region, -Lat, -Long) %>%
        mutate(date = str_remove(as.character(date), "X"),
               date = as.Date(date, tryFormats = "%m.%d.%y")
        ) %>%
        group_by(Country.Region, date) %>%
        summarise(death_count=sum(death_count, na.rm = TRUE)) %>%
        ungroup(),
      by = c("Country.Region", "date")
    ) %>%
    left_join(
      jhu.global.recovered %>%
           gather(key="date", value="recovered_count", -Province.State,-Country.Region, -Lat, -Long) %>%
           mutate(date = str_remove(as.character(date), "X"),
                  date = as.Date(date, tryFormats = "%m.%d.%y")
           ) %>%
           group_by(Country.Region, date) %>%
           summarise(recovered_count=sum(recovered_count, na.rm = TRUE)) %>%
           ungroup(),
         by = c("Country.Region", "date")
    ) %>%
    mutate(Country.Region = ifelse(Country.Region=="US", "United States", Country.Region)) %>%
    group_by(Country.Region) %>%
    mutate(
      mState.Providence = Country.Region,
      #Country.Region = "National",
      firstDeathDate = suppressWarnings(min(date[death_count>0], na.rm = TRUE)),
      firstDeathDate = as.Date(firstDeathDate),
      firstCaseDate = suppressWarnings(min(date[case_count>0], na.rm = TRUE)),
      firstCaseDate = as.Date(firstCaseDate)
    ) %>%
    ungroup() %>%
    select(mState.Providence, Country.Region, date, case_count, death_count, recovered_count, firstDeathDate, firstCaseDate) %>%
    group_by(mState.Providence, Country.Region) %>%
    filter(max(case_count, na.rm = TRUE)>10) %>% #impose limit on which countries to include/only those with at least 10 cases
    ungroup() 
  
  return(ret.dat)
    
}

get_world_population_data <- function(){
  
  ret.dat <- countypop %>%
      mutate(countyState=paste(county, abbr, sep=', ')) %>%
      select(mState.Providence =countyState,population=pop_2015) %>%
      mutate(Country.Region = "United States") %>%
      mutate(mState.Providence = ifelse(mState.Providence == "District of Columbia, DC", "Washington, DC", mState.Providence),
             mState.Providence = ifelse(mState.Providence == "Anchorage Municipality, AK", "Municipality of Anchorage, AK", mState.Providence),
             mState.Providence = ifelse(mState.Providence == "DoÐa Ana County, NM", "Dona Ana County, NM", mState.Providence)
             ) %>%
      select(mState.Providence, Country.Region, population) %>%
    union_all(
      statepop %>%
        select(mState.Providence=full,population=pop_2015) %>%
        mutate(Country.Region = "United States") %>%
        select(mState.Providence, Country.Region, population)
    ) %>%
    union_all(
      read.csv("./data/world_population.csv", stringsAsFactors = FALSE) %>%
        mutate(Country.Region = ifelse(Country.Region == "Congo", "Congo (Brazzaville)", Country.Region),
               Country.Region = ifelse(Country.Region == "DR Congo", "Congo (Kinshasa)", Country.Region),
               Country.Region = ifelse(Country.Region == "C\xf4te d'Ivoire", "Cote d'Ivoire", Country.Region),
               Country.Region = ifelse(Country.Region == "South Korea", "Korea, South", Country.Region),
               Country.Region = ifelse(Country.Region == "North Korea", "Korea, North", Country.Region)
        ) %>%
        mutate(mState.Providence = Country.Region) %>%
        select(mState.Providence, Country.Region, population)
    ) %>%
    union_all(
      #get the names from cross walk
      read.csv("./data/msa-crosswalk.csv", col.names = c("countyFIPS", "msa_code", "msa_name"), stringsAsFactors = FALSE) %>%
        left_join(
          countypop %>%
            mutate(fips.num = as.numeric(fips)) %>%
            select(fips.num, pop_2015),
          by = c("countyFIPS"="fips.num")
        ) %>%
        group_by(msa_code, msa_name) %>%
        summarise(msa.pop.est = sum(pop_2015, na.rm = TRUE)) %>%
        ungroup() %>%
      #read.csv("./data/mbsa-aggreagate.csv", stringsAsFactors = FALSE) %>%
        mutate(mState.Providence=msa_name,
               population= msa.pop.est, #MSA_POPESTIMATE2019,
               Country.Region="United States") %>%
        select(mState.Providence, Country.Region, population) %>%
        filter(population>0) #this is dangerous
    ) %>%
    distinct()
  
  # Resolve City capitalization issues
  ret.dat = data.frame(lapply(ret.dat, function(x) {gsub("city", "City", x)}))
  
  return(ret.dat)
}


# get_state_ode_params <- function(){
#   
#   #ret.dat <- read.csv("./data/rate_estimates_by_state_old.csv", stringsAsFactors = FALSE) %>%
#   ret.dat <- read.csv("./data/rate_estimates_by_state.updated_4_1_20.csv", stringsAsFactors = FALSE) %>%
#     select(state, est.R0, est.r) %>%
#     left_join(STATE_NAMES, by = c("state"="abbr")) %>%
#     mutate(Country.Region = "United States",
#            full = ifelse(is.na(full), "Disctrict of Columbia", full)) %>%
#     select(mState.Providence=full, Country.Region, est.R0)
#   
#   return(ret.dat)
#   
# }

get_daily_state_testing_data <- function(){
  testing_df <- read_data_from_file('data/daily_state_testing.csv', date_cols=c('date')) %>%
    group_by(state) %>%
    arrange(date) %>%
    mutate(tot_positive = positive,
           daily_positive = positive - lag(positive, n=1, default=0),
           tot_negative = negative,
           daily_negative = negative - lag(negative, n=1, default=0),
           tot_pending = pending,
           daily_pending = pending - lag(pending, n=1, default=0),
           tot_hospitalized = hospitalizedCumulative,
           daily_hospitalized = hospitalizedCumulative - lag(hospitalizedCumulative, n=1, default=0),
           tot_icu = inIcuCumulative,
           daily_icu = inIcuCumulative - lag(inIcuCumulative, n=1, default=0),
           tot_ventilator = onVentilatorCumulative,
           daily_ventilator = onVentilatorCumulative - lag(onVentilatorCumulative, n=1, default=0),
           tot_recovered = recovered,
           daily_recovered = recovered - lag(recovered, n=1, default=0)
    ) %>%
    ungroup() %>%
    arrange(state, date) %>%
    select(state, date, tot_positive, daily_positive, tot_negative, daily_negative, tot_pending, daily_pending, tot_hospitalized,
           daily_hospitalized, tot_icu, daily_icu, tot_ventilator, daily_ventilator, tot_recovered, daily_recovered)
  return(testing_df)
}

# get_county_ode_params <- function(){
#   
#   state_params <- STATE_ODE_PARAMS
#   
#   ret.dat <- read.csv("./data/rate_estimates_by_county.csv", stringsAsFactors = FALSE) %>%
#     select(state, county, county.R0 = est.R0, est.r) %>%
#     mutate(county.R0 = as.numeric(county.R0)) %>%
#     left_join(STATE_NAMES, by = c("state"="abbr")) %>%
#     left_join(select(state_params, mState.Providence, state.R0 = est.R0), 
#               by = c("full"="mState.Providence")) %>%
#     mutate(
#       mState.Providence = paste(county, state, sep=", "),
#       est.R0 = ifelse(is.na(county.R0), state.R0,
#                       ifelse(county.R0>10 | county.R0 < 0, state.R0, county.R0)),
#       Country.Region = "United States") %>%
#     select(mState.Providence, Country.Region, est.R0)
#   
#   return(ret.dat)
#   
# }

county_to_state_counts <- function(cty_df, state, char_cols = c(1,2,4)){
  cty_sub = cty_df[,-c(char_cols)] %>% filter(State==state)
  state_df <- aggregate(. ~  State, data = cty_sub, sum)
  state_long <- gather(state_df, date, value, -State) %>%
    strip_dates(date_col='date') 
  return(state_long)
}

strip_dates <- function(df, date_col='date'){
  dtest <- df[1,date_col[1]]
  if( grepl('[0-9]+\\-[aA-zZ]{3}$', dtest)){
    # format: 19-Mar - need to add year (2020)
    df[,date_col] = paste0(df[,date_col], '-20')
    date_pat = '%d-%b-%y'
  }else if (grepl("[0-9]+\\/[0-9]+\\/[0-9]{4}$" ,dtest)){
    #ft mm/dd/YYYY
    date_pat <- '%m/%d/%Y'
  }else if (grepl("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}$" ,dtest)){
    #ft YYYY-mm-dd
    date_pat <- '%Y-%m-%d'
  }else if(grepl('X[0-9]+\\.[0-9]+\\.[0-9]{4}$', dtest)){
    # format: X3.20.2020 
    date_pat= 'X%m.%d.%Y'
  }else if(grepl('X[0-9]+\\.[0-9]+\\.[0-9]{2}$', dtest)){
    # format: X3.20.20 
    date_pat = 'X%m.%d.%y'
  }else{
    print('Could not find custom patterns.')
  }
  df[,date_col] = as.Date(df[,date_col], format=date_pat)
  #tmp_dates = as.Date(sapply(df$date, function(x){as.character(as.Date(x, format=date_pat))}))
  return(df)
}

#=============Doubling time======================#
# assumes count df is daily counts of cases
calc_doubling_time <- function(count_df, ct_col='case_count', n=NULL){
  count_df <- data.frame(count_df[as.vector(count_df[,ct_col] > 0),])
  # calculate average growth rate
  count_df$growth_rate <- (count_df[,ct_col] - lag(count_df[,ct_col], n=1))/lag(count_df[,ct_col], n=1)
  count_df <- count_df[which(!is.na(count_df$growth_rate)),]
  #growth_rates <- count_df[c('date', 'growth_rate')] %>% filter(date > first_inf_date)
  # filter to last n days if necessary
  if(!is.null(n)){
    count_df <- count_df %>%
      arrange(date) %>%
      top_n(n = n, wt=as.Date(date))
  } 
  growth_rate <- mean(count_df$growth_rate, na.rm=T)
  # calculate doubling time in days
  doubling_time <- 70 / (100*growth_rate)
  return(doubling_time)
}

get_state_counties <- function(state, just_names=F){
  if(nchar(state) > 2){
    state = state_lookup(state)
  }
  county_df <- us_map('counties') %>% filter(abbr==state)
  county_df$county <- gsub('County$', '', county_df$county)
  if(just_names){
    return(county_df$county)
  }else{
    return(county_df)
  }
}

get_eligible_counties <- function(metric='deaths', cutoff=14){
  metric = tolower(metric)
  assert_that(metric %in% c('deaths','death_days','cases','case_days'))
  cty_df = COUNTY_INF_STATS %>% filter(!str_detect(mState.Providence, 'Statewide')) %>%
    group_by(mState.Providence) %>%
    filter(!grepl('Statewide', mState.Providence)) %>%
    summarise(
      case_days = length(which(case_count > 0)),
      death_days = length(which(death_count > 0)),
      cases = max(case_count, na.rm=T),
      deaths = max(death_count, na.rm=T)
    )
  # right now just focusing on deaths. But could filter by case days if modeling cases.
  elig_counties <- cty_df[which(cty_df[,metric] >= cutoff),]
  return(elig_counties)
}


clean_hospital_data <- function(hospital_df, export=FALSE){
  hospital_df$HOSPITAL_NAME <- sapply(hospital_df$HOSPITAL_NAME, function(x){gsub('\\(.*?\\)','',x)})
  hospital_df$HOSPITAL_NAME <- sapply(hospital_df$HOSPITAL_NAME, function(x){gsub('Center$','Ctr',x)})
  hospital_df$HOSPITAL_NAME <- sapply(hospital_df$HOSPITAL_NAME, function(x){gsub('System','Sys',x)})
  hospital_df$text_tip <- paste0('<b>', hospital_df$HOSPITAL_NAME,'</b>',
                                 '<br>', hospital_df$County, 
                                 '<br>Num Beds: ', hospital_df$NUM_STAFFED_BEDS,
                                 '<br>Num ICU Beds: ', hospital_df$NUM_ICU_BEDS,
                                 '<br>Bed Util: ', round(hospital_df$BED_UTILIZATION,2),
                                 '<br>Pot. Bed Increases: ', hospital_df$Potential_Increase_In_Bed_Capac
  )
  # add AVAILABLE ICU bed and bed counts
  hospital_df$AVAIL_BEDS <- round(hospital_df$NUM_STAFFED_BEDS * (1-hospital_df$BED_UTILIZATION),0)
  
  hospital_df$AVAIL_ICU_BEDS <- round(hospital_df$NUM_ICU_BEDS * (1-hospital_df$BED_UTILIZATION), 0)
  if(export){
    write.csv(hospital_df, file='./data/hospital_icu_bed_data.csv')
  }
  
  # TODO: check with team on best way to handle NA data (delete, or imputation?)
  #hospital_df <- hospital_df %>% filter(!is.na(NUM_STAFFED_BEDS))
  return(hospital_df)
}

state_lookup <- function(state){
  if(nchar(state) == 2){
    # abbreviation -> need state name
    return(STATE_LOOKUP_ABBR[[state]])
  }else{
    # state name -> need abbreviation
    if(!startsWith(state, 'District')) state <- simpleCap(tolower(state))
    return(STATE_LOOKUP_NAME[[state]])
  }
}

# pulled from https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

get_hosp_data <- function(){
  HospDataAll=read_sheet("https://docs.google.com/spreadsheets/d/1zZKKnZ47lqfmUGYDQuWNnzKnh-IDMy15LBaRmrBcjqE/edit#gid=1585003222",sheet="Hospital capacity")
  HospDataAll=na.omit(HospDataAll)
  hdata = as.data.frame(t(data.frame(HospDataAll[,c("Name","Value")], row.names="Name")))
  
  return(hdata)
}

get_state_tracking_stats = function(testing_df=NULL){
  if(is.null(testing_df)) testing_df = DAILY_STATE_TESTING
  hosp_cts = testing_df %>%
    group_by(state) %>%
    summarise('hosps' = sum(daily_hospitalized, na.rm=T),
              'n_hosp' = length(which(!is.na(daily_hospitalized))), 
              'tot_pos' = max(tot_positive, na.rm=T), 
              'tot_negative' = max(tot_negative, na.rm=T), 
              'n_test' = length(which(!is.na(tot_positive)))) %>%
    mutate(
      'hosp_rate' = round(hosps/tot_pos, 2),
      'pos_rate' = round(tot_pos / (tot_pos + tot_negative), 2), 
      'state_name' = sapply(state, state_lookup)
    )
  return(hosp_cts)
}

get_covid_clinical_params <- function(){
  ret.dat <- read.csv("./data/COVID19_parameters_updated.csv", stringsAsFactors = FALSE,
                      col.names = c("Quantity", "Statistic", "Name", "Value", "Units", "Source", "X", "Location", "Date", "Notes", "X.1",
                                    "RTMod", "X.2", "X.3")) %>%
    filter(!is.na(Name), trimws(Name)!="") %>%
    select(metric=Quantity, var.name=Name, value=Value, units=Units) %>%
    mutate(value = as.numeric(str_remove(value, "%")))
  
  return(ret.dat)
}

generate_ode_intervention_df <- function(selected.interventions, npi.simulation.times.df, selected.state, model.start.date, abs.max=1, sim.duration=365){
  
  active.interentions <- npi.simulation.times.df %>%
    filter(is.na(state) | state==selected.state) %>%
    filter(policy_long %in% selected.interventions) %>%
    mutate(init_date = as.Date(model.start.date),
           interv_days_start = as.numeric(as.Date(exp_start_date) - as.Date(init_date)),
           interv_days_end = as.numeric(as.Date(exp_end_date) - as.Date(init_date))
    ) %>%
    distinct()
  
  all.intervs.df <- data.frame()
  
  if (all(!is.null(active.interentions) & nrow(active.interentions) > 0) ){
    for (i in 1:nrow(active.interentions)){
      inter.sim.prds <- data.frame(sim.time = seq(from=as.numeric(active.interentions[i,"interv_days_start"]), to = as.numeric(active.interentions[i,"interv_days_end"]), by = 1))
      inter.sim.prds$policy_long <- as.character(active.interentions[i, "policy_long"])
      inter.sim.prds$Prop_Reduction_Mild_Trans <- as.numeric(active.interentions[i, "Prop_Reduction_Mild_Trans"])
      inter.sim.prds$Prop_Reduction_Severe_Trans <- as.numeric(active.interentions[i, "Prop_Reduction_Severe_Trans"])
      inter.sim.prds$Prop_Reduction_Death <- as.numeric(active.interentions[i, "Prop_Reduction_Death"])
      
      all.intervs.df <- rbind.data.frame(all.intervs.df, inter.sim.prds, stringsAsFactors = FALSE)
    }
    
    # should return one row per active time period of intervention
    #KRF 3/31/20 THIS IS HOW WE PASS THE INTERVENTION IMPACT TO THE SIMULATION
    all.intervs.df <- all.intervs.df %>%
      group_by(sim.time) %>%
      summarise(Prop_Reduction_Mild_Trans = sum(Prop_Reduction_Mild_Trans, na.rm=TRUE),
                #ifelse(any(str_detect(policy_long, pattern = "Stay At Home")), 
                                            #       unique(Prop_Reduction_Mild_Trans[policy_long=="Stay At Home Order"]),
                                                   #ifelse(any(str_detect(policy_long, pattern = "Business Closures")), 
                                                   #       unique(Prop_Reduction_Mild_Trans[policy_long=="Non-Essential Business Closures"]),
                                                          #)
      #),
      Prop_Reduction_Mild_Trans = ifelse(Prop_Reduction_Mild_Trans>abs.max, abs.max, Prop_Reduction_Mild_Trans),
      #intervention.reduction.mapping(policy_long, Prop_Reduction_Mild_Trans),
      Prop_Reduction_Severe_Trans = max(Prop_Reduction_Severe_Trans,na.rm = TRUE),
      Prop_Reduction_Death = max(Prop_Reduction_Death,na.rm = TRUE)
      ) %>%
      ungroup() #%>%
    
    #for each row, check if earlier was the same
    sim.period <- c(1, rep(NA, nrow(all.intervs.df)-1))
    current.period <- 1
    for (i in 2:nrow(all.intervs.df)){
      if (all.intervs.df[i, "Prop_Reduction_Mild_Trans"] == all.intervs.df[i-1, "Prop_Reduction_Mild_Trans"] &
          all.intervs.df[i, "Prop_Reduction_Severe_Trans"] == all.intervs.df[i-1, "Prop_Reduction_Severe_Trans"] &
          all.intervs.df[i, "Prop_Reduction_Death"] == all.intervs.df[i-1, "Prop_Reduction_Death"]){
        sim.period[i] <- current.period
      }else{
        current.period <- current.period + 1
        sim.period[i] <- current.period
      }
    }
    all.intervs.df$sim.period <- sim.period
    
    all.intervs.df <- all.intervs.df %>%
      group_by(Prop_Reduction_Mild_Trans, Prop_Reduction_Severe_Trans, Prop_Reduction_Death, sim.period) %>%
      summarise(start_time = min(sim.time,na.rm=TRUE), 
                end_time = max(sim.time,na.rm=TRUE)) %>%
      ungroup() %>%
      distinct() %>%
      arrange(start_time) %>%
      mutate(firstCaseDate = as.Date(model.start.date),
             end_time=end_time+1)
    
    sim.end.time <- as.numeric(max(npi.simulation.times.df$exp_start_date[npi.simulation.times.df$policy_long %in% selected.interventions]+days(sim.duration), 
                                   na.rm = TRUE)-model.start.date)
    if (max(all.intervs.df$end_time, na.rm=TRUE) < sim.end.time){
      #this indicates a period of no-interventions at the end of the simulations (eg return to normalcy)
      
      final.row <- c(0, 0, 0, max(all.intervs.df$sim.period, na.rm=TRUE)+1, max(all.intervs.df$end_time, na.rm=TRUE), sim.end.time, format(Sys.Date(), "%Y-%m-%d"))
      
      all.intervs.df <- rbind(all.intervs.df, final.row) %>%
        type.convert() %>%
        mutate(firstCaseDate = as.Date(model.start.date, origin = "1970-01-01"))
        
    }
      
      
      
  }else{
    all.intervs.df <- data.frame(matrix(ncol=8,nrow=0, 
                                        dimnames=list(NULL, c("Prop_Reduction_Mild_Trans", "Prop_Reduction_Severe_Trans", 
                                                              "Prop_Reduction_Death", "sim.period", "start_time", "end_time", 
                                                              "firstCaseDate", "end_time"))))
  }
  
  return(all.intervs.df)
}

merge_beta1_intervention_df <- function(intervention.df, b1.df, state, b1.proj.method = "max", sah.effectiveness = 0.7){
  
  
  if (b1.proj.method == "max"){
    proj.beta1.val <- max(b1.df$b1, na.rm=TRUE) #last 10 periods ~equals last 20 days
  }else if (b1.proj.method == "20 day"){
    proj.beta1.val <- mean(tail(b1.df$b1, 10), na.rm=TRUE) #last 10 periods ~equals last 20 days
  }else if (b1.proj.method == "last"){
    proj.beta1.val <- mean(tail(b1.df$b1, 5), na.rm=TRUE)/(1-sah.effectiveness)
  }else if (b1.proj.method == "min"){
    #min method
    max.val <- max(b1.df$b1, na.rm=TRUE)
    proj.beta1.val <- min(tail(b1.df$b1[b1.df$b1>0], 20), na.rm=TRUE)/(1-sah.effectiveness)  #last 20 periods ~equals last 40 days

    proj.beta1.val <- min(proj.beta1.val, max.val, na.rm=TRUE)
  }else if (b1.proj.method == "minmean"){
    #minmean method
    max.val <- max(b1.df$b1, na.rm=TRUE)
    proj.beta1.val <- mean(tail(b1.df$b1[b1.df$b1>0], 10), na.rm=TRUE)/(1-sah.effectiveness)  #last 20 periods ~equals last 40 days
    
    proj.beta1.val <- min(proj.beta1.val, max.val, na.rm=TRUE)
  }
  
  # this returns the second highest b1 value (trying to avoid erroneous super b1 during init period of high infectivity)
  no.interv.max.b1 <- max(b1.df$b1[b1.df$b1<max(b1.df$b1,na.rm=TRUE)], na.rm = TRUE)
  
  proj.row <- c(state, "proj",
                max(b1.df$end_time),
                as.numeric(max(intervention.df$end_time)),
                format(as.Date(max(b1.df$start_date)+ddays(2), origin="1970-01-01"), "%Y-%m-%d"),
                proj.beta1.val)
  
  b1.df.all <- rbind(b1.df, proj.row) %>%
    mutate(start_time = as.numeric(start_time),
           end_time = as.numeric(end_time))
  
  ret.df <- data.frame(sim.time= seq(from = min(b1.df.all$start_time,na.rm = TRUE), 
                                     to = max(b1.df.all$end_time, na.rm = TRUE), by = 1)) %>%
    left_join(mutate(b1.df.all, beta.period = 1:n()), 
              by = c("sim.time" = "start_time")) %>%
    select(sim.time, b1, beta.period, beta.step = period) %>%
    fill(b1, beta.period, beta.step) %>%
    left_join(
      intervention.df %>%
        select(1:3, start_time, firstCaseDate, sim.period),
      by = c("sim.time" = "start_time")
    ) %>%
    fill(Prop_Reduction_Mild_Trans, Prop_Reduction_Severe_Trans, Prop_Reduction_Death, firstCaseDate, sim.period) %>%
    fill(firstCaseDate, .direction = c("up")) %>%
    mutate(sim.period = ifelse(is.na(sim.period), 0, sim.period)) %>%
    group_by(beta.period, beta.step, sim.period) %>%
    summarise(
      Prop_Reduction_Mild_Trans = unique(Prop_Reduction_Mild_Trans),
      Prop_Reduction_Severe_Trans = unique(Prop_Reduction_Severe_Trans),
      Prop_Reduction_Death = unique(Prop_Reduction_Death),
      #sim.period2 = paste(beta.period, sim.period, sep="_"),
      start_time = min(sim.time, na.rm=TRUE),
      end_time = max(sim.time, na.rm = TRUE),
      firstCaseDate = unique(firstCaseDate),
      b1 = unique(b1)
    ) %>%
    ungroup() %>%
    select(-beta.period, -sim.period) %>%
    mutate(sim.period = 1:n(),
           end_time = end_time+1,
           start_time = ifelse(start_time == 0, 1, start_time),
           end_time=as.numeric(end_time),
           start_time = as.numeric(start_time),
           b1 = as.numeric(b1),
           Prop_Reduction_Mild_Trans = ifelse(is.na(Prop_Reduction_Mild_Trans), 0, Prop_Reduction_Mild_Trans),
           Prop_Reduction_Severe_Trans = ifelse(is.na(Prop_Reduction_Severe_Trans), 0, Prop_Reduction_Severe_Trans),
           Prop_Reduction_Death = ifelse(is.na(Prop_Reduction_Death), 0, Prop_Reduction_Death),
           b1 = ifelse(Prop_Reduction_Mild_Trans == 0 & beta.step == "proj", no.interv.max.b1, b1)
           ) %>%
    filter(start_time != end_time)
  
  return(ret.df)
}

update_moving_beta_db <- function(county_dir = NULL, out=NULL, export_res=T, prnt=F, overwrite_newer_data=F){
  if(is.null(county_dir)) county_dir = 'tuning_results/region_betas/'
  if(is.null(out)) out = 'data/database/new_region_fits.csv'
  old_df = read_data_from_file('data/database/new_region_fits.csv', date_cols=c('date'))
  # combine county results and update moving_beta_data
  comb_df <- data.frame()
  for(file in paste0(county_dir, list.files(county_dir))){
    region_betas = read_data_from_file(file, date_cols='date')
    region = unique(region_betas$region)
    # name validation (make sure region in data mathces region in filename)
    region_match = sapply(region, function(x){grepl(x, file)})
    if(any(region_match)){
      if(length(region) > 1){
        true_region = names(which(region_match))
        print(paste0('Found multiple regions in ', true_region, ' file. Filtering to just the right region.'))
        region_betas = region_betas %>% filter(region==true_region)
        write.csv(region_betas, file)
      }
      if(prnt) print(paste0(file,' - ', region, ' - ', nrow(region_betas)))
      if(nrow(comb_df) > 0){
        cols = intersect(colnames(comb_df), colnames(region_betas))
      }else{
        cols = c('region', 'time', 'date', 'period', 'b1')
      }
      if(overwrite_newer_data){
        comb_df <- rbind.data.frame(comb_df, region_betas[,cols])
      }else{
        # only overwrite if new data is more current than existing data
        existing_reg_df = old_df %>% filter(region==region)
        if (max(region_betas$date, na.rm=T) >= max(existing_reg_df$date, na.rm=T)){
          comb_df <- rbind.data.frame(comb_df, region_betas[,cols])
        }
      }
    }else{
      print(paste0('found corrupte file: ', file, '. SKIPPING'))
    }
  }
  # merge county results with remaining regions in MOVING_BETA_DF
  rem_regions = setdiff(unique(old_df$region), unique(comb_df$region))
  comb_df = rbind.data.frame(comb_df, old_df %>% filter(region %in% rem_regions)) %>%
    distinct()
  if(export_res)   write.csv(comb_df, file=out, row.names=F)
  return(comb_df)
}

# update county tuning params with latest params in tuning_results
update_county_tuning_params <- function(tuning_dir='tuning_results', only_moving_window_results=T){
  old_params = COUNTY_TUNING_PARAMS
  
  # read in all files for counties with same param count as counties in county_tuning_params file.
  param_cols = setdiff(colnames(old_params), c('tuning_date', 'loss', 'region'))
  all_files = list.files(tuning_dir)
  isCounty = sapply(all_files, detect_county)
  all_county_files = all_files[isCounty]
  
  # if only_moving_window_results, only include files generated from county tuning process
  if(only_moving_window_results){
    elig_county_files = all_county_files[which(grepl('params_moving_window', all_county_files))]
  }else{
    elig_county_files = all_county_files
  }
  elig_counties = gsub('\\_[0-9]+\\_params.*.csv', '', elig_county_files)
  
  if(length(elig_counties) > 0){
    old_params = old_params %>% filter(!region %in% elig_counties)
    dfs = lapply(elig_county_files, function(f){read.csv(paste(tuning_dir, f,sep='/'), stringsAsFactors=F)})
    new_params = bind_rows(dfs)
    new_params = new_params %>%
      mutate(region = elig_counties) %>%
      drop_na() %>% distinct() %>%
      # if initial.infected excluded, assume equal to 1 
      mutate(initial.infected = ifelse('initial.infected' %in% colnames(new_params), initial.infected, 1))
    
    if('start_date' %in% colnames(new_params)) new_params$start_date = as.Date(new_params$start_date)
    
    # add state column if not already there
    if(sum(grepl('state', colnames(new_params)))==0){
      new_params$state = sapply(new_params$region, function(x){str_split(x, ', ')[[1]][[2]]})
    }
    
    # create NA columns for any remamnining missing variables in new params
    for(missing_param in setdiff(colnames(old_params), colnames(new_params))){
      new_params[missing_param] = NA
    }
    
    # merge new and old params 
    all_params = bind_rows(old_params, new_params[,colnames(old_params)])
    
    # fix b1s - make sure min is 0
    if('b1' %in% colnames(all_params)) all_params$b1 = sapply(all_params$b1, function(x){max(x,0)})
    # export results
    write.csv(all_params, 'data/database/all_counties_optimal_params.csv', row.names=F)
  }else{
    warning('No eligible counties params - skipping update.')
  }
}

get_state_tuning_params <- function(def_npi_weights){
  tuning_df <- read.csv('data/database/all_states_optimal_params.csv', stringsAsFactors = F)
  if('start_date' %in% colnames(tuning_df)) tuning_df$start_date = as.Date(tuning_df$start_date)
  if('tuning_date' %in% colnames(tuning_df)) tuning_df$tuning_date = as.Date(tuning_df$tuning_date)
  # fill in NA NPI weights with default weights
  npi_param_name_mapper <- c("npi.sc.prop.mild"= "State-Mandated School Closures",
                             "npi.nbc.prop.mild"="Non-Essential Business Closures",
                             "npi.bar.prop.mild"="Bar/Restaurant Limits",
                             "npi.lgb.prop.mild"="Large Gatherings Ban",
                             "npi.mqt.prop.mild"="Mandatory Quarantine for Travelers",
                             "npi.sah.prop.mild"="Stay At Home Order",
                             "npi.rev.prop.mild"="Reopening Plan Reversal")
  mapper = hash::hash(keys=names(npi_param_name_mapper), values=as.vector(npi_param_name_mapper))
  
  for(col in colnames(tuning_df)){
    match_res = mapper[[col]]
    if(!is.null(match_res)){
      npi_name = match_res
      def_npi_weight = def_npi_weights[[npi_name]]
      tuning_df[which(is.na(tuning_df[,col])),col] = def_npi_weight
    }
  }
  return(tuning_df)
}

get_county_mapping <- function(){
  
  ret.dat <- COUNTY_INF_STATS %>%
    select(mState.Providence) %>%
    distinct() %>%
    separate(col = mState.Providence, into = c("County.Pretty", "abbr"), sep = ", ", remove = TRUE) %>%
    select(County.Pretty, abbr) %>%
    mutate(county.just.name = trimws(str_remove(County.Pretty, pattern = "County")),
           county.just.name = trimws(str_remove(county.just.name, pattern = "Parish")),
           county.just.name = trimws(str_remove(county.just.name, pattern = "City and Borough of")),
           county.just.name = trimws(str_remove(county.just.name, pattern = "City and Borough")),
           county.just.name = trimws(str_remove(county.just.name, pattern = "Borough")),
           county.just.name = trimws(str_remove(county.just.name, pattern = "Municipality of")),
           county.just.name = trimws(str_remove(county.just.name, pattern = "Municipality")),
           county.just.name = trimws(str_remove(county.just.name, pattern = "Census Area")),
           county.ugly = str_remove(county.just.name, pattern = "'"),
           county.ugly = str_remove(county.ugly, pattern = "\\."),
           county.ugly = capitalize(tolower(county.ugly))) %>%
    left_join(
      STATE_NAMES, by = c('abbr')
    ) %>%
    mutate(
      county.ugly.fipskey = ifelse(county.ugly == "Dekalb", "De kalb", 
                                   ifelse(county.ugly == "Desoto", "De soto",
                                          ifelse(county.ugly == "Dupage", "Du page",
                                                 ifelse(county.ugly == "Lasalle", "La salle",
                                                        ifelse(county.ugly == "Laporte", "La porte", 
                                                               ifelse(county.ugly == "Lamoure", "La moure",
                                                                      ifelse(county.ugly == "Dewitt", "De witt", county.ugly))))))),
      polyname=tolower(paste(full, county.ugly.fipskey, sep=","))) %>%
      right_join(maps::county.fips %>%
                    rbind(read.csv("data/additional_fips.csv")) %>%
                    mutate(fips_str=as.character(fips),
                          polyname = ifelse(polyname=="south dakota,shannon", "south dakota,oglala lakota", polyname),
                          polyname = ifelse(polyname=="montana,yellowstone national", "montana,yellowstone", polyname),
                          polyname = ifelse(polyname=="virginia,mathews", "virginia,matthews", polyname),
                          polyname = ifelse(polyname=="virginia,suffolk", "virginia,suffolk city", polyname),
                          polyname = ifelse(polyname=="virginia,hampton", "virginia,hampton city", polyname),
                          polyname = ifelse(polyname=="virginia,newport news", "virginia,newport news city", polyname),
                          polyname = ifelse(polyname=="virginia,virginia beach", "virginia,virginia beach city", polyname),
                          polyname = ifelse(polyname=="virginia,norfolk", "virginia,norfolk city", polyname)
                          ) %>%
                   separate(col=polyname, into=c("polyname_pretty", "poly_part"), sep= ":", remove=FALSE, extra="warn", fill="right"),
                by = c("polyname"="polyname_pretty")) %>%
      select(County.Pretty, abbr, county.just.name, county.ugly, polyname, fips, full)
  
  return(ret.dat)
}

repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)        # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
}      

get_state_chime_data <- function(){
  
  ret.dat <- read.csv("./data/chime_state.csv", stringsAsFactors = FALSE) %>%
    select(date, state, 
           I2_I3_hosp = Projected_Census_hospitalized,
           I2_I3_icu = Projected_Census_Icu, 
           I2_I3_vent = Projected_Census_ventilated,
           #intervention.date = Date.of.Intervention, 
           #intervention.effectiveness = Social.Distancing.Intervention.Effectiveness,
           I1_I2_I3 = SIR_infected,
           S = SIR_susceptible,
           R = SIR_recovered) %>%
    #filter(intervention.effectiveness == 85) %>%
    #group_by(county.fips) %>%
    #mutate(
    #  I2_I3_hosp = repeat.before(I2_I3_hosp),
    #  I2_I3_icu = repeat.before(I2_I3_icu),
    #  I2_I3_vent = repeat.before(I2_I3_vent)) %>%
    #ungroup() %>%
    mutate(date = as.Date(paste0(date, ", 2020"), tryFormats="%b, %d, %Y"),
           #intervention.date = as.Date(intervention.date, tryFormats="%m/%d/%y"),
           I3 = floor((I2_I3_vent + I2_I3_icu)/2), #TODO: This can be done better...revisit
           I2 = I2_I3_hosp - I3,
           I1 = I1_I2_I3 - I2 - I3) %>%
    group_by(state, date) %>%
    summarise(
      S = sum(S, na.rm = TRUE),
      I1 = sum(I1, na.rm = TRUE),
      I2 = sum(I2, na.rm = TRUE),
      I3 = sum(I3, na.rm = TRUE),
      R = sum(R, na.rm=TRUE)#,
     # population = sum(population,na.rm=TRUE) # population isn't correct...doesn't sum to anywhere near state totals
    ) %>%
    ungroup()
  
  return(ret.dat)
}

get_state_ihme_data <- function(){
  ret.dat <- read.csv("./data/ihme_state.csv", stringsAsFactors = FALSE) %>%
    select(state=location_name, date, I2_I3 = allbed_mean, I2_I3_icu = ICUbed_mean, I2_I3_vent=InvVen_mean, D = totdea_mean) %>%
    mutate(date = as.Date(date),
           I3 = floor((I2_I3_icu + I2_I3_vent)/2),
           I2 = I2_I3 - I3,
           state = ifelse(str_detect(state, pattern = ", WA"), "Washington", state)) %>%
    group_by(state, date) %>%
    summarise(D = sum(D, na.rm = TRUE),
              I2 = sum(I2, na.rm = TRUE),
              I3 = sum(I3, na.rm = TRUE)) %>%
    ungroup()
  
  return(ret.dat)
}

get_model_comp_descriptions <- function(){
  ret.dat <- read.csv("./descriptions/modelDescriptions_v1.csv", stringsAsFactors = FALSE, col.names = c("Model", "Assumptions", "Strengths", "Limitations", "model_var_name")) %>%
    filter(!is.na(Model),
           trimws(Model) != "") 
  
  return(ret.dat)
}

iter_excel_sheets <- function(dat.path, sheet.name){
  dat <- read_xlsx(path=dat.path, sheet = sheet.name) %>%
    mutate(region=sheet.name)
  return(dat)
}

get_gaussian_dat <- function(pth = "./data/04092020_gaussianProj.xlsx"){
  sheets <- excel_sheets(pth)
  excel.dat <- plyr::ldply(.data = sheets, .fun = iter_excel_sheets, dat.path=pth)
  
  ret.dat <- excel.dat %>%
    mutate(date = as.Date(Date, tryFormats="%m/%d/%y")) %>%
    select(date, state_abbr = region, step=N, deaths_lowerCL=LCL, deaths_mean  = `Mean`, deaths_upperCL=UCL) %>%
    left_join(STATE_NAMES, by = c("state_abbr"="abbr")) %>%
    mutate(deaths_lowerCL = ifelse(deaths_lowerCL==0 & date<Sys.Date(), deaths_mean, deaths_lowerCL),
           deaths_upperCL = ifelse(deaths_upperCL==0 & date<Sys.Date(), deaths_mean, deaths_upperCL),
           deaths_lowerCL = ifelse(deaths_lowerCL<0, 0, deaths_lowerCL),
           full = ifelse(
             state_abbr=="USA", "United States",
             ifelse(state_abbr=="DC", "District of Columbia", full)))
  
  return(ret.dat)
}

get_gaussian_dat2 <- function(folder.pth = "./data/gaussianProj/2020_04_16/"){
  
  state.codes <- maps::state.fips %>%
    select(state_fips = fips, state_abbr=abb) %>%
    distinct() %>%
    left_join(STATE_NAMES, by = c("state_abbr"="abbr")) %>%
    select(fips = state_fips, full, state_abbr) %>%
    mutate(full = ifelse(fips==11, "District of Columbia", full),
           fips = as.character(fips)) %>%
    union_all(
      data.frame(fips=c("2", "15", "US"), 
                 full = c("Alaska", "Hawaii", "United States"),
                 state_abbr = c("AK", "HI", "US"),
                 stringsAsFactors = FALSE)
    ) 
  
  county.codes <- USFACTS_CONFIRMED %>% 
    select(countyFIPS, County.Name, State) %>%
    mutate(mState.Providence = paste(County.Name, State, sep = ", "),
           countyFIPS = as.character(countyFIPS)) %>%
    select(fips = countyFIPS, full = mState.Providence, state_abbr=State) %>%
    filter(fips != "1", fips != "0")
  
  all.codes <- county.codes %>%
    union_all(state.codes)
  
  gp.cases <- data.frame(fread(file.path(folder.pth, "cases.csv"), stringsAsFactors = FALSE)) %>%
    mutate(date = as.Date(Date, tryFormats="%m/%d/%y")) %>%
    select(date, fips=FIPS, step=N, cases_lowerCL=LCL, cases_mean = `Mean`, cases_upperCL=UCL) %>%
    mutate(cases_lowerCL = ifelse(cases_lowerCL==0 & date<Sys.Date(), cases_mean, cases_lowerCL),
           cases_upperCL = ifelse(cases_upperCL==0 & date<Sys.Date(), cases_mean, cases_upperCL),
           cases_lowerCL = ifelse(cases_lowerCL<0, 0, cases_lowerCL)) %>%
    select(-step) %>%
    filter(fips!="0")
  
  gp.deaths <- data.frame(fread(file.path(folder.pth, "deaths.csv"), stringsAsFactors = FALSE)) %>%
    mutate(date = as.Date(Date, tryFormats="%m/%d/%y")) %>%
    select(date, fips=FIPS, deaths_lowerCL=LCL, deaths_mean = `Mean`, deaths_upperCL=UCL) %>%
    mutate(deaths_lowerCL = ifelse(deaths_lowerCL==0 & date<Sys.Date(), deaths_mean, deaths_lowerCL),
           deaths_upperCL = ifelse(deaths_upperCL==0 & date<Sys.Date(), deaths_mean, deaths_upperCL),
           deaths_lowerCL = ifelse(deaths_lowerCL<0, 0, deaths_lowerCL)) %>%
    filter(fips!="0")
  
  gp.all <- gp.cases %>%
    left_join(gp.deaths, by = c("date", "fips")) %>%
    left_join(all.codes, by = c("fips"="fips")) %>%
    replace_na(list(deaths_lowerCL=0, deaths_mean=0, deaths_upperCL=0))
  
  return(gp.all)
}

make_ts_dat_for_paul <- function(){
  ret.dat <- STATE_INF_STATS %>%
    mutate(state.region.key = paste(mState.Providence, Country.Region, sep="___")) %>%
    group_by(mState.Providence) %>%
    mutate(firstCaseDate = min(date[case_count>0],na.rm = TRUE),
           first100CasesDate = suppressWarnings(min(date[case_count>=100],na.rm=TRUE)), #TODO eventually shift up to 100 when first 25 is universal
           daysFromFirstDeath = as.numeric(as.Date(date)- as.Date(firstDeathDate)),
           daysFromFirstCase = as.numeric(as.Date(date) - as.Date(firstCaseDate)),
           daysFromFirst100Cases = as.numeric(as.Date(date) - as.Date(first100CasesDate)),
           date = as.Date(date)
    ) %>%
    ungroup() %>%
    left_join(WORLD_POP, by=c("mState.Providence", "Country.Region")) %>%
    select(state=mState.Providence, date, Cases=case_count, Deaths=death_count, Recovered=recovered_count,
           firstCaseDate, firstDeathDate, first100CasesDate,
           daysFromFirstDeath, daysFromFirstCase,daysFromFirst100Cases, population,
           state.region.key) %>%
    filter(date >= as.Date("2020-01-22")) %>%
    group_by(state) %>% # grouping to create delta/diff vars
    arrange(as.Date(date)) %>%
    mutate(
      Cases_smoothed.delta = Cases - lag(Cases, n = 3, default = 0),
      Deaths_smoothed.delta = Deaths - lag(Deaths, n = 3, default = 0),
      Recovered_smoothed.delta = Recovered - lag(Recovered, n = 3, default = 0),
      Cases_log10 = log10(Cases),
      Deaths_log10 = log10(Deaths),
      Recovered_log10 = log10(Recovered),
      Cases_per.million = (Cases/population)*100000,
      Deaths_per.million = (Deaths/population)*100000,
      Recovered_per.million = (Recovered/population)*100000,

      Cases_daily.rate = ifelse(lag(Cases, n = 1, default = 1) == 0, 0,
                                (Cases - lag(Cases, n = 1, default = 0))/lag(Cases, n = 1, default = 1)),
      Deaths_daily.rate = ifelse(lag(Deaths, n = 1, default = 1)==0, 0,
                                 (Deaths - lag(Deaths, n = 1, default = 0))/lag(Deaths, n = 1, default = 1)),
      Recovered_daily.rate = ifelse(lag(Recovered, n = 1, default = 1)==0, 0,
                                    (Recovered - lag(Recovered, n = 1, default = 0))/lag(Recovered, n = 1, default = 1)),

      Cases_daily.rate2 = ifelse(lag(Cases, n = 2, default = 1) == 0, 0,
                                 (lag(Cases, n = 1, default = 0) - lag(Cases, n = 2, default = 0))/lag(Cases, n = 2, default = 1)),
      Deaths_daily.rate2 = ifelse(lag(Deaths, n = 2, default = 1)==0, 0,
                                  (lag(Deaths, n = 1, default = 0) - lag(Deaths, n = 2, default = 0))/lag(Deaths, n = 2, default = 1)),
      Recovered_daily.rate2 = ifelse(lag(Recovered, n = 2, default = 1), 0,
                                     (lag(Recovered, n = 1, default = 0) - lag(Recovered, n = 2, default = 0))/lag(Recovered, n = 2, default = 1)),

      Cases_daily.rate3 = ifelse(lag(Cases, n = 3, default = 1) == 0, 0,
                                 (lag(Cases, n = 2, default = 0) - lag(Cases, n = 3, default = 0))/lag(Cases, n = 3, default = 1)),

      Deaths_daily.rate3 = ifelse(lag(Deaths, n = 3, default = 1)==0, 0,
                                  (lag(Deaths, n = 2, default = 0) - lag(Deaths, n = 3, default = 0))/lag(Deaths, n = 3, default = 1)),

      Recovered_daily.rate3 = ifelse(lag(Recovered, n = 3, default = 1)==0, 0,
                                     (lag(Recovered, n = 2, default = 0) - lag(Recovered, n = 3, default = 0))/lag(Recovered, n = 3, default = 1)),

      Cases_daily.rate4 = ifelse(lag(Cases, n = 4, default = 1) == 0, 0,
                                 (lag(Cases, n = 3, default = 0) - lag(Cases, n = 4, default = 0))/lag(Cases, n = 4, default = 1)),

      Deaths_daily.rate4 = ifelse(lag(Deaths, n = 4, default = 1)==0, 0,
                                  (lag(Deaths, n = 3, default = 0) - lag(Deaths, n = 4, default = 0))/lag(Deaths, n = 4, default = 1)),

      Recovered_daily.rate4 = ifelse(lag(Recovered, n = 4, default = 1)==0, 0,
                                     (lag(Recovered, n = 3, default = 0) - lag(Recovered, n = 4, default = 0))/lag(Recovered, n = 4, default = 1)),

      Cases_daily.rate5 = ifelse(lag(Cases, n = 5, default = 1) == 0, 0,
                                 (lag(Cases, n = 4, default = 0) - lag(Cases, n = 5, default = 0))/lag(Cases, n = 5, default = 1)),

      Deaths_daily.rate5 = ifelse(lag(Deaths, n = 5, default = 1)==0, 0,
                                  (lag(Deaths, n = 4, default = 0) - lag(Deaths, n = 5, default = 0))/lag(Deaths, n = 5, default = 1)),

      Recovered_daily.rate5 = ifelse(lag(Recovered, n = 5, default = 1)==0, 0,
                                     (lag(Recovered, n = 4, default = 0) - lag(Recovered, n = 5, default = 0))/lag(Recovered, n = 5, default = 1)),

      Cases_smoothing_denom = (Cases_daily.rate > 0) + (Cases_daily.rate2 > 0) + (Cases_daily.rate3 > 0) + (Cases_daily.rate4 > 0) + (Cases_daily.rate5 > 0),
      Deaths_smoothing_denom = (Deaths_daily.rate > 0) + (Deaths_daily.rate2 > 0) + (Deaths_daily.rate3 > 0) + (Deaths_daily.rate4 > 0) + (Deaths_daily.rate5 > 0),
      Recovered_smoothing_denom = (Recovered_daily.rate > 0) + (Recovered_daily.rate2 > 0) + (Recovered_daily.rate3 > 0) + (Recovered_daily.rate4 > 0) + (Recovered_daily.rate5 > 0),

      Cases_smoothed.rate = (Cases_daily.rate + Cases_daily.rate2 + Cases_daily.rate3 + Cases_daily.rate4 + Cases_daily.rate5)/Cases_smoothing_denom,
      Deaths_smoothed.rate = (Deaths_daily.rate + Deaths_daily.rate2 + Deaths_daily.rate3 + Deaths_daily.rate4 + Deaths_daily.rate5)/Deaths_smoothing_denom,
      Recovered_smoothed.rate = (Recovered_daily.rate + Recovered_daily.rate2 + Recovered_daily.rate3 + Recovered_daily.rate4 + Recovered_daily.rate5)/Recovered_smoothing_denom,
      Cases_doubling.days = ifelse(Cases_smoothed.rate == Inf | Cases_smoothed.rate == -Inf, NA, 70/(100*Cases_smoothed.rate)),
      Deaths_doubling.days = ifelse(Deaths_smoothed.rate == Inf | Deaths_smoothed.rate == -Inf, NA, 70/(100*Deaths_smoothed.rate)),
      Recovered_doubling.days = ifelse(Recovered_smoothed.rate == Inf | Recovered_smoothed.rate == -Inf, NA, 70/(100*Recovered_smoothed.rate))
    ) %>%
    ungroup() %>%
    select(state, date, Cases_smoothed.rate, Deaths_smoothed.rate, first100CasesDate) %>%
    filter(as.Date(date) >= first100CasesDate)

  ret.dat.cases <- ret.dat %>%
    select(state, date, Cases_smoothed.rate) %>%
    spread(key = date, value = Cases_smoothed.rate, fill = 0) %>%
    filter(state %in% c("Maryland", "New Jersey", "New York", "Florida", "Indiana", "Nebraska"))

  ret.dat.deaths <- ret.dat %>%
    select(state, date, Deaths_smoothed.rate) %>%
    spread(key = date, value = Deaths_smoothed.rate, fill = 0) %>%
    filter(state %in% c("Maryland", "New Jersey", "New York", "Florida", "Indiana", "Nebraska"))

  return(list(cases=ret.dat.cases, deaths = ret.dat.deaths))
}

### Regional Similarity Data Setup
region_similarity_vectors <- function(mindays = 14, mincases = 20, mincases_permil = 300){
  
  reg_data_full = rbind(rbind(rbind(COUNTY_INF_STATS, MBSA_INF_STATS), COUNTRY_INF_STATS), STATE_INF_STATS[,-9]) 
  reg_data_full$region_type = c(rep("County", nrow(COUNTY_INF_STATS)),
                           rep("Metro Area", nrow(MBSA_INF_STATS)),
                           rep("Country", nrow(COUNTRY_INF_STATS)),
                           rep("State", nrow(STATE_INF_STATS)))
  reg_data = reg_data_full  %>%
    filter(str_detect(mState.Providence, "Ship", negate = TRUE)) %>%
    group_by(mState.Providence, region_type) %>%
    mutate(datefilter = suppressWarnings(min(date[case_count>=20], na.rm = TRUE)),
           ndays = sum(case_count>=mincases)) %>%
    filter(date > datefilter & !is.na(datefilter) &
             ndays >= mindays &
             !(mState.Providence == "Dutchess County, NY" & region_type == "Metro Area") &
             !(mState.Providence == "Orange County, CA" & region_type == "Metro Area")
    ) %>%
    select(mState.Providence, region_type, date, case_count) %>%
    arrange(mState.Providence, date) %>%
    mutate(identifier = paste(mState.Providence, region_type))
  
  ## Population-based filtering
  reg_data_pop = reg_data_full %>%
    mutate(Country.Region = ifelse(region_type == "Country", mState.Providence, "United States")) %>%
    left_join(WORLD_POP) %>%
    mutate(population = as.numeric(population)) %>%
    mutate(case_count_per_mil = case_count/population*100000) %>%
    group_by(mState.Providence, region_type) %>%
    mutate(datefilter = suppressWarnings(min(date[case_count_per_mil>=mincases_permil], na.rm = TRUE)),
           ndays = sum(case_count_per_mil>=mincases_permil)) %>%
    filter(date > datefilter & !is.na(datefilter) &
             ndays >= mindays &
             !(mState.Providence == "Dutchess County, NY" & region_type == "Metro Area") &
             !(mState.Providence == "Orange County, CA" & region_type == "Metro Area")
    ) %>%
    select(mState.Providence, region_type, date, case_count_per_mil) %>%
    arrange(mState.Providence, date) %>%
    mutate(identifier = paste(mState.Providence, region_type))
    
  reg_data <- reg_data %>% left_join(reg_data_pop)
  
  region_list = unique(reg_data$identifier)
  SIM_REGION_LIST = reg_data[!duplicated(reg_data[c(1,2,5)]),c(1,2,5)] 
  state_map = USFACTS_CONFIRMED %>%
    select(county = County.Name, state=State) %>%
    mutate(county.state = paste(county, state, sep=", "))
  SIM_REGION_LIST = SIM_REGION_LIST %>% 
    left_join(state_map, by = c("mState.Providence" = "county.state")) %>%
    filter(str_detect(mState.Providence, "Ship", negate = TRUE)) 
  nRegions = length(region_list)

  REGIONAL_VECTORS = vector("list", nRegions)
  REGIONAL_VECTORS_PERMIL = vector("list", nRegions)
  for (i in 1:(nRegions)) {
    filtered_data = reg_data %>% filter(identifier == region_list[i])
    REGIONAL_VECTORS[[i]] = filtered_data$case_count
    REGIONAL_VECTORS_PERMIL[[i]] = filtered_data$case_count_per_mil[!is.na(filtered_data$case_count_per_mil)]
    #if (length(REGIONAL_VECTORS_PERMIL[[i]]) == 0) {REGIONAL_VECTORS_PERMIL[[i]] = NA}
  }
  SIM_REGION_LIST$data_len = lengths(REGIONAL_VECTORS)
  save(REGIONAL_VECTORS, REGIONAL_VECTORS_PERMIL, SIM_REGION_LIST, file="data/database/regional_sim_data.RData")
}

prep_hrsa_demog_similarity_df <- function(){
   init.dat <- read.csv(file="./data/measures_county-aggregated.csv", stringsAsFactors = FALSE)
   init.dat <- init.dat[, !names(init.dat) %in% HRSA_DEMOG_CATS$Column[HRSA_DEMOG_CATS$Category=="EXCLUDE"]]

   data_complete <- init.dat[complete.cases(init.dat),]
   fips <- data_complete$County.FIPS
   data_complete <- data_complete %>% select(-County.FIPS) #remove rows that have NAs
   data_complete_zvar <- data_complete[, which(apply(data_complete, 2, var) != 0)] #remove zero variance columns

   data_complete_zvar$fips <- fips

   return(data_complete_zvar)
}

prep_hrsa_demog_similarity_pct_df <- function(){
  init.dat <- read.csv(file="./data/measure_pct_county-aggregated.csv", stringsAsFactors = FALSE)
  init.dat <- init.dat[, !names(init.dat) %in% HRSA_DEMOG_CATS$Column[!is.na(HRSA_DEMOG_CATS$Category)]]

  data_complete <- init.dat[complete.cases(init.dat),]
  fips <- data_complete$County.FIPS
  data_complete <- data_complete %>% select(-County.FIPS) #remove rows that have NAs
  data_complete_zvar <- data_complete[, which(apply(data_complete, 2, var) != 0)] #remove zero variance columns

  data_complete_zvar$fips <- fips

  return(data_complete_zvar)
}

build_demog_adj_mat <- function(pct_sim_dat){

  dists_mat <- lapply(1:ncol(pct_sim_dat), function(x){
    t <- stats::dist(pct_sim_dat[,x, drop=F], method = "manhattan", diag = T, upper = T)
    d <- as.data.frame(as.matrix(t))
    rownames(d) <- labels(t)
    colnames(d) <- labels(t)
    return(d)
  })

  return(dists_mat)
}

hospital_county <- function () {
  zip_to_fips = read.csv("data/zip_to_fips.csv", colClasses=c('character'))  %>% mutate(Zip = as.integer(zip)) %>% select(Zip, fips)
  
  ret.dat <- HOSPITALS %>% 
    select(Zip, NUM_STAFFED_BEDS, NUM_ICU_BEDS) %>% 
    left_join(zip_to_fips, by='Zip') %>%
    group_by(fips) %>%
    dplyr::summarize(staffed_beds = sum(NUM_STAFFED_BEDS, na.rm = T), icu_beds = sum(NUM_ICU_BEDS, na.rm = T)) %>%
    filter(!is.na(fips)) %>% 
    mutate(fips = as.character(fips))
  
  return(ret.dat)
}

get_similarity_table_view <- function () {
  land_area = read.csv("data/uscensus_county_land_area.csv", colClasses=c('character', 'character', 'numeric', 'numeric')) %>% 
    mutate(fips = as.integer(county)) %>% select(fips, LandSQMI)
  
  zip_to_fips = read.csv("data/zip_to_fips.csv", colClasses=c('character'))  %>% mutate(Zip = as.integer(zip)) %>% select(Zip, fips)
  
  hrsa = read.csv("./data/hrsa_similarity_raw.csv", sep = ",") %>% select(fips, Percent.of.pop.65.)
  
  nursing <- read.csv("data/Datasets_Skilled_Nursing_Provider_Info.csv") %>% 
    select(Zip = Provider.Zip.Code, Nursing_Pop = Average.Number.of.Residents.Per.Day) %>% 
    left_join(zip_to_fips, by='Zip') %>%
    group_by(fips) %>%
    dplyr::summarize(Total_Nursing_Pop = sum(Nursing_Pop, na.rm = T)) %>%
    filter(!is.na(fips)) %>% 
    mutate(fips = as.integer(fips))
  
  HOSPITALS_COUNTY = HOSPITALS_COUNTY %>% mutate(fips = as.integer(fips))
  
  ret.dat <- countypop %>%
    mutate(countyState=paste(county, abbr, sep=', ')) %>%
    select(fips, mState.Providence =countyState,population=pop_2015) %>%
    mutate(Country.Region = "United States") %>%
    mutate(mState.Providence = ifelse(mState.Providence == "District of Columbia, DC", "Washington, DC", mState.Providence),
           mState.Providence = ifelse(mState.Providence == "Anchorage Municipality, AK", "Municipality of Anchorage, AK", mState.Providence),
           mState.Providence = ifelse(mState.Providence == "DoÐa Ana County, NM", "Dona Ana County, NM", mState.Providence)
    ) %>% 
    mutate(fips = as.integer(fips)) %>%
    left_join(land_area, by='fips') %>%
    mutate(pop_density = population/LandSQMI) %>%
    left_join(HOSPITALS_COUNTY, by='fips') %>% 
    mutate(staffed_beds_perM = staffed_beds/population*100000, icu_beds_perM = icu_beds/population*100000) %>%
    left_join(nursing, by='fips') %>% 
    mutate(nursing_pop_percent = round(Total_Nursing_Pop/population*100,2)) %>% 
    left_join(hrsa, by='fips') %>%
    select(fips, mState.Providence, population, pop_density, staffed_beds_perM, icu_beds_perM, nursing_pop_percent, pop_65_percent = Percent.of.pop.65.)
  
  ret.dat = data.frame(lapply(ret.dat, function(x) {gsub("city", "City", x)}))
  
  return(ret.dat)
}

prank<-function(x){
  r<-rank(x)/sum(!is.na(x))
  r[is.na(x)]<-NA
  r*100
}

hrsa_similarity_percentiles <- function() {
  addl_data <- SIMILARITY_TABLE_VIEW %>% 
    select(fips, pop_density, staffed_beds_perM, icu_beds_perM, nursing_pop_percent) %>% mutate(fips = as.integer(fips))
  
  for (i in 2:ncol(addl_data)){
    addl_data[,i] <- prank(addl_data[,i])
  }
  
  ret.dat <- read.csv("./data/hrsa_similarity_pct.csv", sep = ",")  %>%
    left_join(addl_data)
  
  return(ret.dat)
}


get_ppe_preds <- function(region_list=NULL, case_pth = 'data/predicted_cases.csv', death_pth='data/predicted_deaths.csv'){
  preds = read.csv(case_pth, stringsAsFactors = F) %>%
    mutate(county_name_full = paste0(County.Name, ', ', State),
           date = as.Date(dates_str, tryFormats=c('%m/%d/%Y')))

  death_preds = read.csv(death_pth, stringsAsFactors = F) %>%
    mutate(county_name_full = paste0(County.Name, ', ', State),
           region_name = ifelse(nchar(County.Name)==2, State, county_name_full),
           date = as.Date(dates_str, tryFormats=c('%m/%d/%Y')))
}

most_recent_weekly_testing_data <- function(state_name = NULL) {
  df = DAILY_STATE_TESTING
  if(!is.null(state_name)){
    state_abbr = state_lookup(state_name)
    df <- filter(df, state==state_abbr)
  }
  
  # weekly plot
  weekly_df <- df %>% mutate(week=strftime(date, format="%V")) %>%
    group_by(week) %>%
    dplyr::summarize(
      total_positive=sum(daily_positive, na.rm=T),
      total_negative=sum(daily_negative, na.rm=T),
      positive_rate= 100*round(sum(daily_positive, na.rm=T) /  (sum(daily_positive, na.rm=T) +sum(daily_negative, na.rm=T)), 3),
      avg_pos_per_day = total_positive / 7,
      avg_neg_per_day = total_negative / 7,
      avg_tests_per_day = avg_pos_per_day + avg_neg_per_day,
      total_hospitalized = sum(daily_hospitalized, na.rm=T), 
      start_date=min(date)
    ) %>% 
    ungroup() %>%
    filter(start_date == max(start_date))
  
  return(weekly_df)
}

get_nhsn_hospitalization_data <- function(){

  ret.dat <- read.csv(file = "data/covid19-NatEst.csv", stringsAsFactors = FALSE) %>%
    .[-1,] %>%
    select(state_abb=state, full=statename, date=collectionDate,
           hosp_beds_occupied = InpatBeds_Occ_AnyPat_Est,
           hosp_beds_occupied_lower95 = InpatBeds_Occ_AnyPat_LoCI,
           hosp_beds_occupied_upper95 = InpatBeds_Occ_AnyPat_UpCI,
           hosp_bed_avail = InpatBeds_Occ_AnyPat_Est_Avail,

           hosp_covid_cases = InpatBeds_Occ_COVID_Est,
           hosp_covid_case_lower95 = InpatBeds_Occ_COVID_LoCI,
           hosp_covid_case_upper95 = InpatBeds_Occ_COVID_UpCI) %>%
    mutate(
      date.part.1 = str_sub(as.character(date), end = 2),
      date.part.2 = tolower(str_sub(as.character(date), start = 3, end = 5)),
      date.part.2 = mapply(simpleCap, date.part.2),
      date.part.3 = str_sub(as.character(date), start = 6),
      all.date.parts = paste(date.part.1, date.part.2, date.part.3, sep="-")) %>%
    mutate(date = as.Date(all.date.parts, tryFormats=c("%d-%b-%Y"))) %>%
    select(-all.date.parts) %>%
    type.convert() %>%
    mutate(tot_hosp_beds = hosp_beds_occupied+hosp_bed_avail,
           state.region.key = ifelse(state_abb=="US", paste(full, full, sep = "___"),
                                     paste(full, "United States", sep = "___")))

  return(ret.dat)
}

politeList <- function(vec){
  if (length(vec) == 1){
    return(vec[1])
  }else if (length(vec) == 2){
    return(paste(vec, collapse = " and "))
  }else{
    return(paste(paste(vec[1:(length(vec)-1)], collapse = ", "), vec[length(vec)], sep = ", and "))
  }
}

get_cdc_age_rates <- function(){
  cdc_dat = read.csv('https://data.cdc.gov/api/views/vbim-akqf/rows.csv?accessType=DOWNLOAD', stringsAsFactors = F)
  cdc_dat$cdc_report_dt = as.Date(cdc_dat$cdc_report_dt)
  cdc_dat$pos_spec_dt = as.Date(cdc_dat$pos_spec_dt, format = "%Y/%m/%d")
  cdc_dat$onset_dt = as.Date(ifelse(cdc_dat$onset_dt == "", NA, as.character(cdc_dat$onset_dt)))
  
  cdc.cases = cdc_dat %>% 
    filter(current_status == 'Laboratory-confirmed case')  %>% 
    mutate(min_date = pmin(cdc_report_dt, pos_spec_dt, na.rm = T)) %>% 
    filter(!is.na(age_group) & age_group != "Unknown") %>%
    mutate(week = floor_date(min_date, unit = "week", week_start = 7))
  
  # split groups into 5 year groups
  cdc.cases = cdc.cases %>% 
    group_by(week, age_group) %>% 
    summarise(case_count = n())
  total_cases_week = cdc.cases %>% ungroup() %>% group_by(week) %>% summarise(case_week = sum(case_count, na.rm = T))
  
  cdc.cases = cdc.cases %>%
    mutate(
      age_group2 = ifelse(
        age_group == "0 - 9 Years", "0-4 Years,5-9 Years", ifelse(
          age_group == "10 - 19 Years", "10-14 Years,15-19 Years", ifelse(
            age_group == "20 - 29 Years", "20-24 Years,25-29 Years", ifelse(
              age_group == "30 - 39 Years", "30-34 Years,35-39 Years", ifelse(
                age_group == "40 - 49 Years", "40-44 Years,45-49 Years", ifelse(
                  age_group == "50 - 59 Years", "50-54 Years,55-59 Years", ifelse(
                    age_group == "60 - 69 Years", "60-64 Years,65-69 Years", ifelse(
                      age_group == "70 - 79 Years", "70-74 Years,75-79 Years", "80-84 Years,85+ Years"
                    )
                  )
                )
              )
            )
          )  
        )
      ),
      age_group_small= strsplit(age_group2, ",")
    ) %>% unnest(age_group_small) %>%
    mutate(
      cases_small = case_count/2
    ) %>% 
    group_by(week, age_group_small) %>% 
    summarise(cases = sum(cases_small, na.rm = T)) %>% 
    left_join(total_cases_week) %>% 
    ungroup() %>% 
    mutate(pct_cases = cases/case_week, date = week) 
  
  cdc.cases = cdc.cases %>% select(date, age_group_small, pct_cases_raw = pct_cases) %>% filter(date >= as.Date('2020/3/15'))
  
  cdc.cases.smooth <- data.frame(age_group_small = character(), date = Date(), pct_cases_smooth = numeric())
  
  for (i in 1:length(unique(cdc.cases$age_group_small))) {
    age_group = unique(cdc.cases$age_group_small)[i]
    if (age_group != "0-4 Years") {
      temp_dat = cdc.cases %>% filter(age_group_small == age_group) %>% mutate(week_num = week(date)) %>% arrange(week_num)
      loess = predict(loess(pct_cases_raw ~ week_num, temp_dat, span = 0.5))
      temp_dat$pct_cases_smooth = loess
      cdc.cases.smooth = rbind(cdc.cases.smooth, temp_dat %>% select(age_group_small, date, pct_cases_smooth))
    }
  }
  cdc.cases.smooth = rbind(cdc.cases.smooth, 
                           cdc.cases.smooth %>% group_by(date) %>% dplyr::summarize(pct_cases_smooth = 1 - sum(pct_cases_smooth), .groups = 'keep') %>% mutate(age_group_small = "0-4 Years"))
  
  cdc_case_5yr = cdc.cases.smooth %>% select(age_group_small, date, pct_cases = pct_cases_smooth) %>%
    left_join(cdc.cases)
  
  cdc_dat2 = read.csv('https://data.cdc.gov/api/views/vsak-wrfu/rows.csv?accessType=DOWNLOAD', stringsAsFactors = T)
  
  total_deaths_week = cdc_dat2 %>% filter(Sex == "All Sex") %>%
    mutate(date = as.Date(Week.ending.Date, format = '%m/%d/%Y') - 6) %>% 
    group_by(date) %>% summarise(death_week = sum(COVID.19.Deaths, na.rm = T))
  
  cdc_death_5yr = cdc_dat2 %>% filter(Sex == "All Sex") %>%
    mutate(date = as.Date(Week.ending.Date, format = '%m/%d/%Y') - 6) %>%
    mutate(
      age_group2 = ifelse(
        Age.Group %in% c("Under 1 year", "1-4 years"), "0-4 Years", ifelse(
          Age.Group == "5-14 years", "5-9 Years,10-14 Years", ifelse(
            Age.Group == "15-24 years", "15-19 Years,20-24 Years", ifelse(
              Age.Group == "25-34 years", "25-29 Years,30-34 Years", ifelse(
                Age.Group == "35-44 years", "35-39 Years,40-44 Years", ifelse(
                  Age.Group == "45-54 years", "45-49 Years,50-54 Years", ifelse(
                    Age.Group == "55-64 years", "55-59 Years,60-64 Years", ifelse(
                      Age.Group == "65-74 years", "65-69 Years,70-74 Years", ifelse(
                        Age.Group == "75-84 years", "75-79 Years,80-84 Years", "85+ Years"
                      )
                    )
                  )
                )
              )
            )
          )  
        )
      ),
      age_group_small= strsplit(age_group2, ",")
    ) %>% unnest(age_group_small) %>%
    mutate(
      deaths_small = ifelse(age_group_small %in% c("0-4 Years", "85+ Years"), COVID.19.Deaths,COVID.19.Deaths/2)
    ) %>% 
    group_by(date, age_group_small) %>% 
    summarise(deaths = sum(deaths_small, na.rm = T), .groups = 'keep') %>% 
    left_join(total_deaths_week) %>% 
    ungroup() %>% 
    mutate(pct_deaths_raw = deaths/death_week)
  
  cdc_death_smooth <- data.frame(age_group_small = character(), date = Date(), pct_deaths_smooth = numeric())
  
  for (i in 1:length(unique(cdc_death_5yr$age_group_small))) {
    age_group = unique(cdc_death_5yr$age_group_small)[i]
    if (age_group != "0-4 Years") {
      temp_dat = cdc_death_5yr %>% filter(age_group_small == age_group) %>% mutate(week_num = week(date)) %>% arrange(week_num) %>% filter(week_num > 10)
      loess = predict(loess(pct_deaths_raw ~ week_num, temp_dat, span = 0.5))
      temp_dat$pct_deaths_smooth = loess
      cdc_death_smooth = rbind(cdc_death_smooth, temp_dat %>% select(age_group_small, date, pct_deaths_smooth))
    }
  }
  cdc_death_smooth = rbind(cdc_death_smooth, 
                           cdc_death_smooth %>% group_by(date) %>% summarise(pct_deaths_smooth = 1 - sum(pct_deaths_smooth), .groups = 'keep') %>% mutate(age_group_small = "0-4 Years"))
  
  cdc_death_5yr = cdc_death_smooth %>% select(age_group_small, date, pct_deaths = pct_deaths_smooth) %>% left_join(cdc_death_5yr)
  
  recent_case_ratio = cdc_case_5yr %>% filter(date == max(date)) %>% select(age_group_small, pct_cases)
  week_list = unique(cdc_case_5yr$date[cdc_case_5yr$date>as.Date("2020/03/14")])
  # add data through July assuming age ratios remain constant
  new_dates = as.data.frame(seq(max(week_list)+7, max(cdc_death_5yr$date), by = 7))
  new_dates = new_dates[[1]]
  
  recent_case_ratio_full = data.frame(date = as.Date(character()), age_group_small = character(), pct_cases = numeric(), pct_cases_raw = NULL)
  for (i in 1:length(new_dates)) {
    temp = recent_case_ratio
    temp$date = rep(new_dates[i], nrow(recent_case_ratio))
    temp$pct_cases_raw = rep(NA, nrow(recent_case_ratio))
    recent_case_ratio_full = rbind(recent_case_ratio_full, temp)
  }
  
  ret.dat = cdc_death_5yr %>% left_join(cdc_case_5yr %>% rbind(recent_case_ratio_full)) %>%
    select(week = date, age_group_small, pct_cases, pct_deaths, pct_cases_raw, pct_deaths_raw)
  ret.dat$cases_update_date = max(week_list)
  
  return(ret.dat)
}
