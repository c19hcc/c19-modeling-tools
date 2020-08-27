##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: 
## Date: June 2020
## Developers: Allison Hill, Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner, Eric Neumann
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

library(assertthat)
library(parallel)
library(doParallel)
library(dplyr)
library(reshape2)
set.seed(23)
library(pbapply)
library(plotly)
library(ggplot2)
library(Hmisc)
#op <- pboptions(type="text") # default

source('code/ode_functions.R')
source('database.R')
source('code/dataProcessing.R')


#========GLOBAL PARAMS==========#
#STATE_R0 <- get_state_ode_params() # gets estimated R0 for each state
DEF_CLINICAL_PARAMS <- get_covid_clinical_params() # gets all default clinical params
HDATA = read.csv('data/hdata_local.csv', stringsAsFactors = F)
DEF_PARAMS = read.csv('data/database/default_params.csv', stringsAsFactors = F)

metric_key = list('deaths'='death_count','cases'='case_count')

# for testing
# state_name='California'
# start_date = '2020/03/01'
# fixed_params <- list('start_date'='2020/03/01', 'initial.infected'=2, 'FracAsympto'=.2, 'b21'=0, 'b31'=0)
# manual tuning results: b1 - .43, initial.infected: 2, start_date: 2020/03/01
# free_params <- c('b1', 'b21', 'b31', 'ProbDeath', 'DoublingTime')
# loss_fn=RMSE
# metric='Deaths'

# parallelization code
build_cluster <- function(n=NULL){
  if(is.null(n)) n = detectCores()
  cluster <- makeCluster(n)
  registerDoParallel(cluster)
  
  # export necessary packages to cluster
  clusterEvalQ(cluster, library(lubridate))
  clusterEvalQ(cluster, library(dplyr))
  clusterEvalQ(cluster, library(tidyr))
  clusterEvalQ(cluster, library(stringr))
  clusterEvalQ(cluster, library(reshape2))
  clusterEvalQ(cluster, library(Hmisc))
  clusterEvalQ(cluster, library(assertthat))
  
  clusterEvalQ(cluster, source('database.R'))
  clusterEvalQ(cluster, source('code/ode_functions.R'))
  clusterEvalQ(cluster, source('code/optimizer.R'))
  clusterEvalQ(cluster, source('code/dataProcessing.R'))
  
  # export functions
  clusterExport(cluster,list('run_model_fixed_params',
                              'run_model',
                              'run_model_lite',
                              'run_model_and_eval',
                              'calc_model_error',
                              'get_region_plot_ts',
                              'get_state_infection_stats',
                              'generate_state_npi_df', 
                              'get_npi.intervention.dat', 
                              'get_npi.interv.sim.dat',
                              'get_full.npi.react.df',
                              'generate_ode_intervention_df',
                              'get_def_npi_params', 
                              'get_all_region_params', 
                              'get_state_hosp_data',
                              'get_county_hosp_data',
                              'gen_region_constraints',
                              'RMSE',
                              'RMSE_parzen',
                              'RMSE_hann',
                              'RMSE_triangle',
                              'sineWindow',
                              'gParzenW', 
                              'gTriW', 
                              'gHannW', 
                              'gSineW'))
  # export global variables
  clusterExport(cluster,list('DEF_CLINICAL_PARAMS',
                              'HDATA',
                              'metric_key', 
                              'DEF_PARAMS',
                              'STATE_NAMES', 
                             'COUNTY_NAMES'),envir=globalenv())
  return(cluster)
}


tune_region_model <- function(region_name, metric='Deaths', fixed_params=NULL, pre_NPI=T,npi_lag=14, parallel=F,
                             free_params=NULL, start_date=NULL, loss_fn=RMSE, exprt=T, entire_dur=F, finetune=F){
  
    assert_that(tolower(metric) %in% c('deaths', 'cases'))
    
    if(parallel){
      if(!exists('CLUSTER')){
        stop('Could not find global CLUSTER variable. Please run CLUSTER = build_cluster() 
              first to run this function in parallel.')
      }
    }else{CLUSTER = NULL}
    
    # enumearte combinations of parameters to try
    constraints <- gen_region_constraints(region_name, metric, pre_NPI=pre_NPI)
    # if start date provided, override default start date & rm from free_params if necessary
    if(!is.null(start_date)){
      if(is.null(fixed_params)) fixed_params = list()
      fixed_params[['start_date']] = start_date
      if('start_date' %in% free_params) free_params = free_params[which(free_params != 'start_date')]
    }
    # if just one free param, make sure combinations are named accordingly
    param_comb_df <- get_param_combs(free_params=free_params, fixed_params=fixed_params, constraints=constraints, finetune=finetune)
    
    if(length(free_params)==1){
      pname = free_params[1]
      param_combs = vector('list', nrow(param_comb_df))
      for(i in 1:nrow(param_comb_df)){
        comb = c(param_comb_df[i,])
        names(comb) = pname
        param_combs[[i]] = comb
      }
    }else{
      param_combs <- lapply(1:nrow(param_comb_df), function(i){param_comb_df[i,]})
    }
    
    if(parallel) clusterExport(CLUSTER, list('region_name'), envir=environment())
    param_comb_df$loss <- unlist(pblapply(param_combs, function(params){run_model_fixed_params(region_name=region_name, 
                                                                                               fixed_params = params,
                                                                                               loss_fn=loss_fn,
                                                                                               plt=F,
                                                                                               pre_NPI=pre_NPI,
                                                                                               npi_lag=npi_lag, 
                                                                                               entire_dur=entire_dur)},
                                          cl=CLUSTER))
    
    # param_comb_df$loss2 <- loss_col
    optimal_params = param_comb_df %>% top_n(-1, loss)
    print(paste0("OPTIMAL PARAMS FOR ", toupper(region_name), " (tuning for ", metric,")"))
    print(optimal_params)
    
  if(exprt) write.csv(param_comb_df, file=paste0('tuning_results/', region_name, '_', ncol(param_comb_df)-1, '_params.csv'),
                            row.names=F)
  # if there's a tie, return the first set of parameters
  return(optimal_params[1,])
}
## 2/14/20	0.75	0.1	0	2	7.700780594	Connecticut
# run model with a set of custom parameters. 
run_model_fixed_params <- function(region_name, fixed_params, metric='Deaths', start_date=NULL, loss_fn=RMSE, plt=F, pre_NPI=F, npi_lag=7, entire_dur=T, debug=F){
  
  if(debug) print(fixed_params)
  
  isCounty = grepl('[cC]ounty', region_name)
  # pull in real state data
  region_df <- get_region_plot_ts(region_name, metric)

  metric = metric_key[[tolower(metric)]]

  # get all non-resource starting parameters
  all_params <- suppressWarnings(get_all_region_params(region_name, fixed_params))
  
  # if no start date provided, fix start date at 10 days before first case. Otherwise, override default start date
  if(is.null(start_date)){
    if('start_date' %in% names(fixed_params)){
      start_date = fixed_params$start_date
    }else{
      start_date = as.Date(all_params$start_date)
    }
  }else{
    all_params$start_date = as.Date(start_date)
  }
  
  # call to Kyle's function to pull NPI data
  if(isCounty){
    state_abbr = strsplit(region_name, ', ')[[1]][[2]]
    state_name = state_lookup(state_abbr)
  }else{
    state_name = region_name
  }
  interv_data <- generate_state_npi_df(state_name, return_npis=T, param_list=fixed_params, mod.start.date=start_date, entire_dur=entire_dur)
  intervention_df = interv_data$timeline_df
  state_intervs = interv_data$state_npis %>% filter(!is.na(start_date))
  if(debug){
    print('state intervention df: ')
    print(state_intervs)
    
    print('timeline: ')
    print(intervention_df)
  }

  # if pre_NPI is TRUE, filter data to only include up to npi_lag days after first significant NPI
  cutoff=NULL
  if(pre_NPI){
    # first check for SAH order. Then Non-essential Business Closure. Then School closures. Otherwise, don't need to filter
    if('H' %in% state_intervs$policy){
      cutoff = state_intervs %>% filter(policy == 'H') %>% .$start_date
    }else if('B' %in% state_intervs$policy){
      cutoff = state_intervs %>% filter(policy == 'B') %>% .$start_date
    }

    if(!is.null(cutoff) && !is.na(cutoff)){
      # filter to data up to npi_lag days after most stringent NPI
      region_df = region_df %>% filter(date < as.Date(cutoff) + npi_lag)
    }
  }
  
  # hospital capacity resources
  if(grepl('[cC]ounty', region_name)){
    hosp_cap_data = get_county_hosp_data(region_name, HDATA)
  }else{
    hosp_cap_data = get_state_hosp_data(region_name, HDATA)
  }
  
  # run model
  results <- run_model(all_params,
                             hops.cap.dat=hosp_cap_data,
                             intervention.df= intervention_df,
                             graph.start.date=start_date)
  
  # if plt=True, generate intervention plot
  if(plt) print(draw_intervention_plot(all_params, hosp_cap_data, intervention_df, results, VarShowCap='Deaths',  graph.start.date=start_date, interv.real.overlay=T))
  
  # combine pre-NPI data from baseline with post-NPI data from intervention
  all_results = bind_rows((results$Baseline %>%
                             filter(time < min(results$Intervention$time)) %>%
                             mutate(Intervention='Do Nothing')),
                          results$Intervention %>% mutate(Intervention='NPI Strategy'))
  
  # calculate error
  error <- calc_model_error(actual_df=region_df, pred_df=all_results, loss_fn=loss_fn, start_date=start_date, plt=plt)
  return(error)
}

detect_county <- function(region_name){
  return((grepl('[cC]ounty', region_name)) || (grepl(',', region_name)))
}

# method to generate state-level constraints for model start date and potentially other parameters in the future
gen_region_constraints <- function(region_name, metric='Deaths', pre_NPI=F){
  isCounty = detect_county(region_name)
  if(isCounty){
    inf_stats = get_county_infection_stats(region_name)
    state_name = state_lookup(strsplit(region_name, ', ')[[1]][[2]])
  }else{
    inf_stats = get_state_infection_stats(region_name)
  }
  constraints = list()
  # if first infection date present, and tuning for cases, ensure model can't start after first known infection date
  if(tolower(metric)=='cases' && 'fi_date' %in% names(inf_stats)){
    constraints[['start_date']] = c('max'=inf_stats[['fi_date']])
  }else if(tolower(metric)=='deaths' && 'fd_date' %in% names(inf_stats)){
    constraints[['start_date']] = c('max'=inf_stats[['fd_date']])
  }
  # if pre_NPI, ensure that start date doesn't exceed date of first NPI
  if(pre_NPI){
    state_npis = get_state_npis(state_name) 
    state_npis$start_date =as.Date(state_npis[,'Start Date'], format='%m/%d/%y')
    first_npi_date = min(state_npis$start_date, na.rm=T)
    if('start_date' %in% names(constraints)){
      constraints[['start_date']][['max']] = min(constraints$start_date[['max']], first_npi_date)
    }else{
      constraints[['start_date']][['max']] = first_npi_date
    }
  }
  return(constraints)
}

gen_region_constraints_strict <- function(state_name){
  inf_stats = get_state_infection_stats(state_name)
  constraints = list()
  # if first infection date present, ensure model can't start after first known infection date
    constraints[['start_date']] = c('max'=inf_stats[['fi_date']])
  return(constraints)
}
# return a dataframe listing all combinations of parameter values for chosen params
# constraints: allow you to override the default limits of a chosen prameter. You can specify a lower-bound (min), upper bound (max), or both. 
# e.g constraints = list('start_date'=list('max'='2020/03/17' )) will ensure that no start dates after 2020/03/17 will be used in combinations
get_param_combs <- function(free_params=NULL, fixed_params=NULL, constraints=list(), finetune=F){
  assert_that((!is.null(free_params) | !is.null(fixed_params)))
  
  if(finetune){
    param_list = names(fixed_params)
    step_col='substep'
  }else{
    param_list = free_params
    step_col = 'step'
  }
  
  param_combs = list()
  for(param in param_list){
    param_row = DEF_PARAMS[which(DEF_PARAMS$param==param),]
    if(param_row$value != -1){
      if(finetune){
        # if this is a parameter it makes sense to tune for, set bounds. Otherwise, fix and add to param_combs 
        if(param_row[[step_col]] != -1){
          midpoint = fixed_params[[param]]
          if(param_row$ui_typ=='date'){midpoint = as.Date(midpoint)}
          num_points = ceil(param_row$step/param_row$substep/2)
          start = midpoint -  num_points * param_row$substep
          end = midpoint + num_points * param_row$substep
          param_combs[[param]] = seq(start, end, by=param_row[[step_col]])
        }else{
          param_combs[[param]] = fixed_params[[param]]
        }
      }else{
        if(param_row$ui_type=='date'){
          start = as.Date(param_row$min, format='%m/%d/%y')
          end = start + param_row$max
        }else{
          start = as.numeric(param_row$min)
          end = as.numeric(param_row$max)
        }
        # check for constraints - if any specified for param, constrain start and/or end accordingly
        if(param %in% names(constraints)){
          # if param is a date type, convert bounds to Dates
          if(grepl('[dD]ate', param)){ 
            const_types = names(constraints[[param]])
            constraints[[param]] = lapply(constraints[[param]], as.Date)
            names(constraints[[param]]) = const_types}
          # check for lower bound constraint
          if('min' %in% names(constraints[[param]])) start = max(start, constraints[[param]][['min']])
          if('max' %in% names(constraints[[param]])) end = min(end, constraints[[param]][['max']])
        }
        param_combs[[param]] = seq(start, end, by=param_row[[step_col]])
      }

    }
  }
  # add fixed params to grid (assumes non-finetuning)
  if(!finetune && !is.null(fixed_params)){
    for(param in names(fixed_params)){
      param_combs[param] = fixed_params[param]
    }
  }
  return(expand.grid(param_combs))
}


# function to return ALL parameters needed to run model for a given region (state, or county, state_abbr)
get_all_region_params <- function(region_name,  fixed_params=NULL, start_date=NULL, N=0){
  # start with default values from param table, then override with state params
  param_list = suppressWarnings(as.list(as.numeric(DEF_PARAMS$value)))
  names(param_list) = DEF_PARAMS$param
  # County or state?
  isCounty = detect_county(region_name)
  if(isCounty && !grepl(', [aA-zZ]{2}$', tolower(region_name))){
    stop('Detected county, but invalid format. Please specify county as county_name, state_abbr
         (e.g Fairfax County, VA)')
  }else if(isCounty){
    state_name = state_lookup(strsplit(region_name, ', ')[[1]][[2]])
  }else{
    state_name = region_name
  }
  # Population params
  if(isCounty) region_stats = get_county_infection_stats(region_name)
  else region_stats = get_state_infection_stats(region_name)
  param_list$Tmax = 180 # days
  param_list$population.size = region_stats$pop
  # if no start date given, start model N days before initial infection
  if(!is.null(start_date)){
    param_list$start_date = as.Date(start_date)
  }else{param_list$start_date = region_stats$fi_date - N}
  param_list$initial.infected = region_stats$fi_ct
  
  # Transmission params
  # pull Eric's state-level estimate for R0
  # if(!isCounty){
  #   param_list$R0 = STATE_ODE_PARAMS %>% filter(mState.Providence==region_name) %>% .$est.R0
  #   # using Eric's method to calculate b1 from R0
  #   param_list$b1 = param_list$R0 /param_list$DurMildInf # mild infections
  # }
  # param_list$b21 = 0 # severe infections
  # param_list$b31 = 0 # critical infections
  
  # pull in optimal state params from tuning file
  if(isCounty){
    tuning_df = COUNTY_TUNING_PARAMS
    param_cols = setdiff(colnames(tuning_df), c('loss','county'))
    tuning_params = tuning_df %>% filter(region==region_name) %>% select(!!!param_cols)
  }else{
    tuning_df = STATE_TUNING_PARAMS
    param_cols = setdiff(colnames(tuning_df), c('loss','state'))
    tuning_params = tuning_df %>% filter(state==region_name) %>% select(!!!param_cols)
  }
  if(nrow(tuning_params) > 0){
    for(tparam in names(tuning_params)) if(!is.na(param_list[tparam])) param_list[tparam] = tuning_params[tparam]
  }
  
  # Hospital resource params:
  if(isCounty) hosp_params = get_county_hosp_data(region_name, HDATA) else hosp_params <- get_state_hosp_data(region_name, HDATA)
  for(hparam in names(hosp_params)){
    if(grepl('Occ', hparam)) hosp_params[hparam] = 100*hosp_params[hparam]
    param_list[hparam] = hosp_params[hparam]
  }
  # set icu occupancy equal to chosen # of occupied beds / # of total beds (from sliders)
  param_list[['ICUBedOcc']] = param_list[['TotICUHospBedsOcc']] / param_list[['TotICUHospBedsAvail']]
  param_list[['HospBedOcc']] = param_list[['TotHospBedsOcc']] / param_list[['TotHospBedsAvail']]
  param_list[['ICUBedper']] = param_list[['TotICUHospBedsAvail']] / region_stats[['pop']] * 1000
  param_list[['HospBedper']] = param_list[['TotHospBedsAvail']] / region_stats[['pop']] * 1000
  
  
  # override any fixed params in param list with fixed param values
  if(!is.null(fixed_params)){
    for(param in names(fixed_params)){
      param_list[param] = fixed_params[param]
    }
  }
  
  # params for plotting
  param_list$inter_state <- state_name
  param_list$interv.real.overlay = T
  param_list$interv.mobility.overlay = F
  param_list$interv.per.capita = F
  param_list$VarShowCap='Deaths'
  return(param_list)
}

calc_model_error <- function(actual_df, pred_df, loss_fn=RMSE,  start_date=NULL, plt=F){
  
  metric <- unique(actual_df$count_type)
  if(!is.null(start_date)) start_date = as.Date(start_date)
  # ensure actual data's dates are Date types
  actual_df <- actual_df %>% mutate(date=as.Date(date))
  # calculate total cases column in case its needed
  pred_df <- pred_df %>% mutate(
    case_count = I1+I2+I3,
    death_count = D
  )
  # if no start date given, assume first prediction corresponds to first date in observed data
  if(is.null(start_date)){
    start_date = min(actual_df$date)
  }else if(start_date < min(actual_df$date, na.rm=T)){
    # if start date occurs before first date of real data, pad real data with zeros for alignment
    actual_df = pad_covid_data(actual_df, start_date)
  
  }else{ # otherwise, filter actual data to make sure first date is no earlier than start date
    start_date <- as.Date(start_date)
    actual_df <- actual_df %>% filter(date >= as.Date(start_date))
  }
  
  # add dates to model results, defining t=0 as start_date
  pred_df[,'date'] = start_date + pred_df[,'time']
  metric_preds = pred_df[,c('date', metric)] %>% rename('pred' = metric) # TODO: add a date column
  metric_actual = actual_df[,c('date', 'value')] %>% rename('actual' = value) # TODO: convert date to days since first case/death
  comb_metric <- metric_actual %>% inner_join(metric_preds, by='date')
  error <- loss_fn(comb_metric$pred, comb_metric$actual)
  if(plt){
    p <- ggplot(comb_metric, aes(x=date, y=actual, color='actual')) + geom_point() +
      geom_line(aes(x=date, y=pred, color='predicted')) +
      geom_point() + 
      labs(y=ifelse(metric=='death_count', 'Death Count', 'Case Count')) + 
      annotate("text", x = max(comb_metric$date, na.rm=T)-7, y = quantile(comb_metric$actual, .75, na.rm=T) + 1,
               label=paste0("RMSE: ", round(error,2)))
    print(ggplotly(p))
  }
  return(error)
}

# pad covid df from a start date to the earliest date in the df with 0s
pad_covid_data <- function(df, start_date, date_col='date'){
  day_diff =  min(df[,date_col], na.rm=T) - as.Date(start_date)
  padding = df[rep(1,day_diff),] %>%
    mutate(value=0, date = date - (day_diff):1)
  df = data.frame(rbind(padding, df))
  rownames(df) = 1:nrow(df)
  return(df)
}


run_model_and_eval <- function(input, Tmax, eval_df=NULL, start_date=NULL, offset=NULL, y0=NULL, hdata=NULL, plt=F, loss_fn=RMSE){
  model_res = run_model_lite(input=input, Tmax=Tmax, offset=offset, y0=y0, hdata=hdata)
  if(is.null(start_date)) start_date = input$start_date
  # NOTE - need to generalize this for counties
  if(is.null(eval_df)) eval_df = get_region_plot_ts(input$inter_state)
  error = calc_model_error(pred_df = model_res, actual_df=eval_df, loss_fn=loss_fn, start_date=start_date, plt=plt)
  return(error)
}

run_model_lite <- function(input, Tmax, offset=NULL, y0=NULL, hdata=NULL, pModel=NULL){
  if(is.null(hdata)) hdata = get_hosp_data()
  # calculate model params based on input
  #if(is.null(pModel)){
  ParamStruct=GetModelParams(input, hdata=hdata)
  pModel=ParamStruct$pModel
  #print(paste0('b1: ', pModel['b1']))
  N = input$population.size
  
  if(is.null(y0)){
    # set initial conditions
    I2.val = input$initial.infected
    I1.val = I2.val * 10 #this is completely arbitrary
    E0= I1.val * 5 * input$IncubPeriod
    S0 = N - E0 - I2.val - I1.val
    y0 = c(S=S0, E=E0, I0=0, I1=I1.val, I2=I2.val, I3=0, R=0, D=0)
  }
  res = GetSpread_SEIR(pModel,N,Tmax,y0)
  if(!is.null(offset)) res$time = res$time + offset
  return(res)
}


RMSE <- function(preds, actual, window_fn=NULL, normalize=F){
  assertthat::are_equal(length(preds),length(actual))
  n = length(preds)
  diffs = (preds - actual)
  if(normalize) diffs = diffs / actual
  if(!is.null(window_fn)) diffs = window_fn(diffs)
  return(sqrt(sum(diffs^2)/n))
  #return(sum(diffs^2)/n)
}

RMSE_parzen = function(preds, actual){
  return(RMSE(preds, actual, window_fn=parzenWindow))
}

RMSE_hann = function(preds, actual){
  return(RMSE(preds, actual, window_fn=hannWindow))
}

RMSE_triangle = function(preds, actual){
  return(RMSE(preds, actual, window_fn=triWindow))
}

RMSE_sine = function(preds, actual){
  return(RMSE(preds, actual, window_fn=sineWindow))
}

get_region_hosp_data <- function(region_name, hdata=NULL){
  isCounty = detect_county(region_name)
  if(isCounty) return(get_county_hosp_data(county_full=region_name, hdata=hdata))
  else return(get_state_hosp_data(state_name=region_name, hdata=hdata))
}
# county_full = county_name, state_abbr (e.g. Fairfax County, VA)
get_county_hosp_data <- function(county_full, hdata=NULL){
  if(is.null(hdata)){
    hdata = get_hosp_data()
  }
  resources = get_county_resource_stats(HOSPITALS, county_full)
  for(resource in names(resources)){
    hdata[[resource]] = resources[[resource]]
  }
  return(hdata)
}

get_state_hosp_data <- function(state_name, hdata=NULL){
  if(is.null(hdata)){
    hdata = get_hosp_data()
  }
  if(nchar(state_name) > 2){
    state_name = state_lookup(state_name)
  }
  resources = get_state_resource_stats(HOSPITALS, state_name)
  for(resource in names(resources)){
    hdata[[resource]] = resources[[resource]]
  }
  return(hdata)
}


#========NPI FUNCTIONS=============

####NPI data structure pipeline####
#npi.intervention.dat %>% intervention.checks %>% npi.check.dates.dat %>% interv.sim.dat %>%
#    full.npi.react.df %>% generate_ode_intervention_df()
## example
# all.intervs.df <- generate_ode_intervention_df(selected.interventions = selected.interv, 
#                                                npi.simulation.times.df = npi.simulation.times.dat,
#                                                selected.state = selected.st,
#                                                model.start.date = graph.start.date)
# draw_new_intervention_plotly(input, hops.cap.dat = values$hdata(), intervention.df = all.intervs.df, graph.start.date = graph.start.date)

generate_state_npi_df <- function(state_name, mod.start.date=NULL, param_list=NULL, chosen.intervs.df=NULL, return_npis=F, entire_dur=F){
  if(is.null(mod.start.date)) mod.start.date = suppressWarnings(get_all_region_params(state_name) %>% .$start_date)
  if(nchar(state_name) == 2) state_name = state_lookup(state_name)
  # get dataframe of all NPIs with associated weights 
  npi.intervention.dat = get_npi.intervention.dat(param_list, for_entire_duration = entire_dur)
  
  # default to state interventions. If chosen interventions provided, override with those
  if(is.null(chosen.intervs.df)) chosen.intervs.df = npi.intervention.dat %>% filter(state==state_name, !is.na(start_date)) 

  # if no start for simulation provided, use 10 days before first infection
  # get start and end dates for these interventions
  chosen.intervs.dates.dat <- chosen.intervs.df %>% select(state, policy_long, policy_var, exp_start_date, exp_end_date)
  #chosen.intervs.names <- chosen.intervs.dates.dat['policy_var'][[1]]
  chosen.intervs.names <- chosen.intervs.dates.dat['policy_long'][[1]]
  
  npi.interv.sim.dat = get_npi.interv.sim.dat(npi.intervention.dat, chosen.intervs.dates.dat)
  
  full.npi.react.df = get_full.npi.react.df(npi.intervention.dat, npi.interv.sim.dat)
  
  state.intervention.df <- generate_ode_intervention_df(selected.interventions = chosen.intervs.names,
                                                        npi.simulation.times.df = full.npi.react.df,
                                                        selected.state = state_name,
                                                        model.start.date = as.Date(mod.start.date))
  if(return_npis){return(list('timeline_df'=state.intervention.df, 'state_npis'=chosen.intervs.df))}
  else{return(state.intervention.df)}
}

get_npi.intervention.dat <- function(fixed_params=NULL, Tmax = 180, for_entire_duration=F, def_duration=30*6){
  # get default NPI params
  npi_params = get_def_npi_params() %>% rename(policy_long=Policy) 
  # if any NPI weights specified in fixed_params, update weights in default npi table
  if(!is.null(fixed_params)){
    fixed_npis = intersect(names(fixed_params), npi_params$policy_var)
    for(npi in fixed_npis){
      if(length(fixed_params[[npi]]) > 0){npi_params[which(npi_params$policy_var==npi), 'Prop_Reduction_Mild_Trans'] = fixed_params[[npi]]}
    }
  }
 npi_df= NPI_DATA %>% filter(policy_long %in% CURRENT_NPIS) %>%
    select(-Prop_Reduction_Mild_Trans, -Prop_Reduction_Severe_Trans, -Prop_Reduction_Death) %>%
    left_join(npi_params, by='policy_long') %>%
    mutate(exp_start_date = if_else(is.na(start_date), Sys.Date(), as.Date(start_date)),
           exp_start_date = as.Date(exp_start_date))
 
  # if for_entire_duration is true, set end dates to end of model.Otherwise, run for 4 months
  if(for_entire_duration){duration = Tmax-1}
  else{duration = def_duration}
 
 npi_df = npi_df %>%
   mutate(exp_end_date = ifelse(is.na(end_date), (start_date+duration), end_date), 
          exp_end_date = as.Date(exp_end_date, origin='1970-01-01'))
 
  return(npi_df)
}

# get_npi.intervention.dat <- function(fixed_params=NULL, Tmax = 180, for_entire_duration=F){
#   # get default NPI params
#   npi_params = get_def_npi_params() %>% rename(policy_long=Policy) 
#   # if any NPI weights specified in fixed_params, update weights in default npi table
#   if(!is.null(fixed_params)){
#     fixed_npis = intersect(names(fixed_params), npi_params$policy_var)
#     for(npi in fixed_npis){
#       if(length(fixed_params[[npi]]) > 0){npi_params[which(npi_params$policy_var==npi), 'Prop_Reduction_Mild_Trans'] = fixed_params[[npi]]}
#     }
#   }
#   npi_df <- NPI_DATA %>%
#     filter(policy_long %in% CURRENT_NPIS) %>%
#     mutate(exp_start_date = if_else(is.na(start_date), Sys.Date(), as.Date(start_date)),
#            exp_start_date = as.Date(exp_start_date),
#            exp_end_date = if_else(is.na(end_date),  exp_start_date + days(Tmax-1), as.Date(end_date)),
#            exp_end_date = as.Date(exp_end_date)) %>%
#     group_by(state, Country.Region, state_abb) %>%
#     mutate(SAH_value = ifelse("Stay At Home Order" %in% policy_long, Prop_Reduction_Mild_Trans[policy_long=="Stay At Home Order"],0.4)) %>%
#     #Scaled_Prop_Reduction_Mild_Trans = Prop_Reduction_Mild_Trans*SAH_value/(sum(Prop_Reduction_Mild_Trans,na.rm = TRUE)-SAH_value),
#     #Prop_Reduction_Mild_Trans = ifelse(policy_long=="Stay At Home Order", SAH_value, Scaled_Prop_Reduction_Mild_Trans)) %>%
#     ungroup()
#   
#   # if for_entire_duration is true, set end dates to end of model.Otherwise, run for 4 months
#   if(for_entire_duration){duration = Tmax-1}
#   else{duration = 30*4}
#   npi_df[,'exp_end_date'] = npi_df[,'exp_start_date'] + duration
#   return(npi_df)
# }

  


get_npi.interv.sim.dat <- function(npi.intervention.dat, chosen.intervs.dates.dat=NULL){
  if (is.null(chosen.intervs.dates.dat)){
    ret.dat <- npi.intervention.dat
  }else{
    ret.dat <- chosen.intervs.dates.dat %>% #$intervention.timeline_data %>%
      select(state, policy_long, exp_start_date2=exp_start_date, exp_end_date2=exp_end_date) %>%
      full_join(npi.intervention.dat, by=c("state", "policy_long")) %>%
      mutate(exp_start_date = if_else(is.na(exp_start_date2), as.Date(exp_start_date), as.Date(exp_start_date2) ),
             exp_end_date = if_else(is.na(exp_end_date2), as.Date(exp_end_date), as.Date(exp_end_date2))) %>%
      select(state, policy, policy_long, start_date, exp_start_date,
             exp_end_date, Prop_Reduction_Mild_Trans, Prop_Reduction_Severe_Trans,
             Prop_Reduction_Death) %>%
      distinct()
  }
  return(ret.dat)
}


get_full.npi.react.df <- function(npi.intervention.dat, npi.interv.sim.dat){
  
  #custom interventions/NPIs from here with proportions
  int.eff.df <- npi.intervention.dat %>%
    select(state, policy, policy_long, start_date, Prop_Reduction_Mild_Trans, Prop_Reduction_Severe_Trans, Prop_Reduction_Death)
  
  interv.dates.df <- npi.interv.sim.dat %>%
    select(state, policy, policy_long, exp_start_date, exp_end_date)
  
  full.npi.int.df <- int.eff.df %>%
    full_join(interv.dates.df, by = c("state", "policy", "policy_long"))  %>%
    select(state, policy, policy_long, start_date, exp_start_date,
           exp_end_date, Prop_Reduction_Mild_Trans, Prop_Reduction_Severe_Trans,
           Prop_Reduction_Death) %>%
    distinct() %>%
    group_by(policy_long) %>%
    mutate(Prop_Reduction_Mild_Trans = ifelse(is.na(Prop_Reduction_Mild_Trans), mean(Prop_Reduction_Mild_Trans,na.rm=TRUE), Prop_Reduction_Mild_Trans),
           Prop_Reduction_Severe_Trans = ifelse(is.na(Prop_Reduction_Severe_Trans), mean(Prop_Reduction_Severe_Trans,na.rm=TRUE), Prop_Reduction_Severe_Trans),
           Prop_Reduction_Death = ifelse(is.na(Prop_Reduction_Death), mean(Prop_Reduction_Death,na.rm=TRUE), Prop_Reduction_Death)) %>%
    ungroup() %>%
    filter(!is.na(state))
  
  #ready to send over for inputs
  return(full.npi.int.df)
}

get_def_npi_params <- function(){
  npi.table <- NPI_DATA %>%
    filter(policy_long %in% CURRENT_NPIS) %>%
    mutate(policy_var = NPI_param_name_mapper[policy_long]) %>%
    select(Policy=policy_long, policy_var, Prop_Reduction_Mild_Trans, Prop_Reduction_Severe_Trans, Prop_Reduction_Death) %>%
    distinct() %>%
    arrange(Policy)
  return(npi.table)
}

get_state_npis <- function(state_name){
  state_npis <- NPI_DATA %>% filter(state==state_name) %>%
    arrange(start_date) %>% 
    mutate(Start_Date = as.Date(start_date),
           End_Date = as.Date(end_date),
    ) %>% #is there a way to format this?
    select(`Start Date` = Start_Date, `Policy` = policy_long, `Code` = policy, 
           `End Date` = End_Date) %>%
    mutate(policy_var=NPI_param_name_mapper[`Policy`])
  return(state_npis)
}


# intervention.checks <- function(input){
#   checks <- c(input$interv_bar, input$interv_lgb, input$interv_nbc, input$interv_sc, input$interv_sah)
#   names(checks) <- c( "Bar/Restaurant Limits", "Large Gatherings Ban","Non-Essential Business Closures",
#                       "School Closures", "Stay At Home Order") 
#   #only return true vals
#   checks <- checks[checks==TRUE]
#   ret.names <- names(checks)
#   return(ret.names)
# }
# 
# # builds a df of chosen NPIs for a given state, their start dates and end dates. For now, 
# # fixing start dates to as they appear in NPI data and setting end dates to 4 months later
# get_npi.checks.dates.dat <- function(state, active.interventions){
#   # TODO: change this variable once we've introduced mandatory quarantine for travelers
#   available.npis <- setdiff(CURRENT_NPIS, c('Mandatory Quarantine for Travelers')) %>% sort()
#   interv.dates <- data.frame(
#     state = rep(state, length(available.npis)),
#     policy_long = available.npis,
#     exp_start_date = sapply(available.npis, function(x){}),
#     exp_end_date = c(input$interv_bar_date[2], input$interv_lgb_date[2], input$interv_nbc_date[2], input$interv_sc_date[2], input$interv_sah_date[2])
#   ) %>% #TODO KRF DATES
#     filter(policy_long %in% active.interventions)
#   
#   return(interv.dates)
# }
#===========Hospital Resource functions================#

build_DEF_PARAMS <- function(exprt=T){
  clin.params <-read.csv('data/COVID19_parameters_updated.CSV') %>%
    select(param=Name, sourceVal = Value, Units) %>%
    mutate(param=as.character(param)) %>%
    filter(nchar(param)>0)
  default_params <- get_all_region_params('Massachusetts')
  param_df <- data.frame(cbind(unlist(names(input)), unlist(as.vector(input)))) %>%
    rename(param=X1, value=X2) %>%
    mutate(param = as.character(param), value = as.numeric(as.character(value)))
  param_df <- param_df %>% left_join(clin.params)
  if(exprt){write.csv(param_df, file='data/optimization_default_params.csv', row.names=F)}
}

#=============Plotting Functions=========================#
plot_model_curves <- function(model_results){
  model_results <- model_results %>%
    mutate(Itot=I1+I2+I3) %>%
    select(time, S, E, I1, I2, I3, Itot, D)
  plt <- ggplot(model_results, aes(x=time, y=S)) +
    geom_line(aes(color='Susceptible')) +
    geom_line(aes(x=time, y=E, color='Exposed')) +
    geom_line(aes(x=time, y=I1, color='I1')) +
    geom_line(aes(x=time, y=I2, color='I2')) +
    geom_line(aes(x=time, y=I3, color='I3')) +
    geom_line(aes(x=time, y=Itot, color='All Cases')) +
    geom_line(aes(x=time, y=D, color='Deaths')) +
    labs(x='time (days)', y='Persons')
  return(ggplotly(plt))
}


get_region_plot_ts <- function(region_name, metric='Deaths', start_date=NULL){
  isCounty = grepl('[cC]ounty', region_name)
  if(isCounty) region_df = COUNTY_INF_STATS else region_df = STATE_INF_STATS
  res <- region_df %>%
    filter(mState.Providence == region_name,
           date >= as.Date(firstCaseDate)-days(1)) %>%
    arrange(date) %>%
    mutate(case_diff = case_count - lag(case_count, n = 1),
           death_diff = death_count - lag(death_count, n=1)) %>%
    select(mState.Providence, Country.Region, date, case_count, death_count, case_diff, death_diff) %>%
    gather(key = "count_type", value = "value", case_count, death_count, case_diff, death_diff) %>%
    filter(count_type == ifelse(metric=="Deaths", "death_count", "case_count")) %>%
    distinct()
  
  if(!is.null(start_date) && start_date < min(res$date)){
    res = pad_covid_data(res, start_date, date_col='date')
  }
  return(res)
}
plt.opts <- c("I3bed" = "Critical Infections", 
              "I3mv" = "Critical Infections", 
              "Hosp" = "Cases Anticipated Needing Hospitalization", 
              "CasesCap" = "All Cases", 
              "Deaths" = "Deaths")



###### WINDOWS
## pass array of differences to each function
## this is computed BEFORE the values are squared and summed !

## Rectangular Window
idWindow <- function (v) {
  v
}

## Sine Window
gSineW = NULL ## global calculated  window array for optimization
sineWindow <- function(v) {
  len = length(v)
  if(is.null(gSineW) | len!=length(gSineW)) {
    #print("Using Sine Window...")
    SineW = sin(seq(0,pi,length.out = len))
    assign("gSineW", SineW, envir = .GlobalEnv)
  }
  v*gSineW
}
res = sineWindow(rep(1,100))

## Triangular Window
gTriW = NULL  ## global calculated  window array for optimization
triWindow <- function(v) {
  len = length(v)
  if(is.null(gTriW) | len!=length(gTriW)) {
    TriW = c(seq(0,1-1/(len/2),length.out = len/2), seq(1,0,length.out = len-len/2))
    assign("gTriW", TriW, envir = .GlobalEnv)
  }
  v*gTriW
}
res = triWindow(rep(1, 100))


## Parzen Window
gParzenW = NULL  ## global calculated  window array for optimization

parzenWindow <- function(v) {
  len = length(v)
  if(is.null(gParzenW) | len!=length(gParzenW)) {
    #print("Using Parzen Window...")
    N = len
    L = N+1
    
    w0 <- function(x) {
      if(abs(x) < L/4) {
        z = 1 - 6*(x/(L/2))^2 * (1 - abs(x)/(L/2))
      }else {
        z = 2*(1 - abs(x)/(L/2))^3
      }
      z
    }
    n = seq(0,N-1) - N/2
    ParzenW = sapply(n,FUN = w0) # w0(n - N/2)
    assign("gParzenW", ParzenW, envir = .GlobalEnv)
  }
  v*gParzenW
}
res = parzenWindow(rep(1, 100))


## Hann Window
gHannW = NULL  ## global calculated  window array for optimization
hannWindow <- function(v) {
  len = length(v)
  if(is.null(gHannW) | len!=length(gHannW)) {
    #print("Using Hann Window...")
    HannW = sin(seq(0,pi,length.out = len))^2
    assign("gHannW", HannW, envir = .GlobalEnv)
  }
  v*gHannW
}
res = hannWindow(rep(1, 100))


window_func <- function (v) {
  hannWindow(v)
}
