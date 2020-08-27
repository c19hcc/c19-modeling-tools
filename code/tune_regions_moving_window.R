##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: 
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

# TUNE ALL REGIONS (STATES AND COUNTIES)

source('code/tuning_moving_window.R')
#source('database.R')
library(foreach)

pbo = pboptions(type="txt")

main = function(){
  args <- commandArgs(trailingOnly = TRUE)
  if(length(args) > 0){
    parallel <- args[1]
  }else{
    parallel = FALSE
  }
  print(paste0('PARALLEL: ', parallel))
  if(parallel){
    print('building cluster...')
    CLUSTER = build_cluster()
  }
  trim_days=8
  print('#=============TUNING STATES===============#')
  res = tune_region_list_moving_window(STATE_NAMES$full, parallel=parallel, fname='data/database/new_state_fits.csv',
                                       baseline_tuning=F, just_new_days=F, window=20, trim_beta_days=trim_days)
  print('#=============RE-TUNING EXISTING COUNTIES===============#')
  prev_county_names = unique(COUNTY_TUNING_PARAMS$region)
  # REMOVE WHEN DONE TESTING********
  #prev_ids = sample(c(1:length(prev_county_names)), 9)
  #prev_county_names = prev_county_names[prev_ids]
  #county_names = setdiff(COUNTY_TUNING_PARAMS$region, unique(MOVING_BETA_DATA$region))
  res = tune_region_list_moving_window(prev_county_names, parallel=parallel, fname='data/database/new_county_fits.csv',
                                       baseline_tuning=F, window=20, just_new_days=F, trim_beta_days=trim_days)
  all_elig_counties = get_eligible_counties(metric='deaths', cutoff=15) %>% .$mState.Providence
  new_counties = setdiff(all_elig_counties, prev_county_names)
  # ****REMOVE WHEN DONE TESTING*******
 # new_counties = new_counties[sample(c(1:length(new_counties)),2)]
  print(paste0('#=============TUNING ', length(new_counties), ' NEW COUNTIES===============#'))
  res = tune_region_list_moving_window(new_counties, parallel=parallel, fname='data/database/new_county_fits.csv',
                                       baseline_tuning=T, window=20, just_new_days=F, fixed_params=list('initial.infected'=1), 
                                       free_params=c('b1', 'start_date'), trim_beta_days=trim_days)
  
  print('adding new county params to all_counties_optimal_params.csv...')
  update_county_tuning_params()
  
  print('#=============UPDATING FIT DB==============#')
  res = update_moving_beta_db(export_res=T)
  
  
  if(parallel) stopCluster(CLUSTER)
}

main()
