# Sys.setenv(PATH= paste0('/Users/babraham/anaconda3/bin:',Sys.getenv('PATH')))
# remove.packages('reticulate')
# install.packages('reticulate')
# Sys.getenv('PATH')
# library(reticulate)
# setwd('/Users/babraham/Documents/HealthLab/Covid19/npi-modeling-dashboard')
# source_python('code/short_model_v2.py')
source('code/tuning_moving_window.R')
source('code/plotting_functions.R')

run_ens_model = function(region_name, n_days=30, start_date=NULL, ppe=T, all.intervs.df=NULL, hc_data=NULL, beta_ts=NULL, plt=F, just_ens_data=F){
  #print(paste0('getting forecasts for ', region_name))
  region_input = get_all_region_params(region_name)
  sim.duration = region_input$Tmax
  isCounty = detect_county(region_name)
  if(isCounty){
    state = str_split(region_name, ', ')[[1]][[2]]
    if(nchar(state)==2) state = state_lookup(state)
  }
  
  if(is.null(start_date)) start_date=region_input$start_date
  
  if(is.null(hc_data))  hc_data <- get_region_hosp_data(region_name, HDATA)
  
  if(is.null(all.intervs.df)){
    full.npi.react.df <- NPI_DATA %>%
      filter(policy_long %in% CURRENT_NPIS, state == state) %>%
      mutate(exp_start_date = if_else(is.na(start_date), Sys.Date(), as.Date(start_date)),
             exp_start_date = as.Date(exp_start_date),
             exp_end_date = if_else(is.na(end_date),  exp_start_date+days(sim.duration-1), as.Date(end_date)),
             exp_end_date = as.Date(exp_end_date))
    
    selected.interv <- full.npi.react.df %>%
      filter(!is.na(start_date)) %>%
      select(policy_long) %>%
      distinct() %>%
      .$policy_long %>%
      unique()
    
    all.intervs.df <- generate_ode_intervention_df(selected.interventions = selected.interv,
                                                   npi.simulation.times.df = full.npi.react.df,
                                                   selected.state = state,
                                                   model.start.date = start_date,
                                                   sim.duration = sim.duration) #abs.max = 0.7)
  }

  # abs.max = input$npi.sah.prop.mild)
  
  if(is.null(beta_ts)) beta_ts = MOVING_BETA_DATA
  beta1.df <- gen_beta_df(region_name = region_name, beta_ts = beta_ts)
  
  updated.intervs.df <- merge_beta1_intervention_df(intervention.df = all.intervs.df, b1.df = beta1.df, state = state, b1.proj.method = "min")#, sah.effectiveness = input$npi.sah.prop.mild)
  
  forecast_period=n_days #days
  
  # genereate baseline results (no interventions/changing b1s)
  baseline_results = run_model_lite(region_input, sim.duration, offset=NULL, hdata=hc_data) %>%
    mutate(period='baseline', 'b1'=region_input$b1, 'date'=time + start_date)
  
  # generate beta-fitted portion of curve (same for NPI and mobility models)
  int_results <- gen_region_seir_df(region_name, hdata=HDATA, b1_timelines=beta1.df) %>%
    mutate(date=time+as.Date(start_date))
  
  forecast_start = max(int_results$date, na.rm=T)
  forecast_end = forecast_start + forecast_period
  
  # generate NPI projections based on ending conditions of beta-fitted curve
  all.intervs.proj.df = updated.intervs.df %>% filter(beta.step=='proj')
  P = tail(int_results, 1)
  y0 = c(S=P$S, E=P$E, I0=P$I0, I1=P$I1, I2=P$I2, I3=P$I3, R=P$R, D=P$D)
  npi_projs <- run_model(region_input, hops.cap.dat = hc_data, intervention.df=all.intervs.proj.df,
                         graph.start.date=as.Date(start_date, tryFormats=c("%m/%d/%Y", "%Y-%m-%d")),
                         beta1.override = TRUE, interv_y0=y0)
  
  # add necessary columns to projections for merging
  npi_projs$Intervention = npi_projs$Intervention %>% mutate(date=time+start_date, period='proj')
  npi_projs$Baseline = npi_projs$Baseline %>% mutate(date=time+start_date, period='proj')
  
  # average predictions
  weights = list('mobility'=1/3, 'npi'=1/3, 'ppe'=1/3)
  
  # ensemble death predictions from ppe, mobility, and NPI models
  pred_list = list()
  
  # mobility
  pred_list[['mobility']] = suppressWarnings(run_model_with_forecasts(region_name, input=region_input, withTime=T)) %>%
    # select(-b1_lower, -b1_upper) %>%
    filter(date <= forecast_end) %>%
    distinct()
  # npi
  pred_list[['npi']] = rbind.data.frame(int_results, npi_projs$Intervention) %>%
    filter(date <= forecast_end) %>%
    distinct()
  # ppe
  if(ppe){
    ppe_res = gen_ppe_preds(region_name)
    if(!is.null(ppe_res)){
      ppe_res = ppe_res %>% rename(D = deaths_pred)
      pred_list[['ppe']] = pred_list[['npi']]  %>%
        select(date, time) %>%
        left_join(ppe_res, by=c('date')) %>%
        mutate(b1=-1, period=-1, R=0) %>%
        filter(date >= start_date, date <= forecast_end) %>%
        distinct()
    }
  }
 
  # combine predictions in a weighted fashion, using weights defined above
  ensemble_cols = setdiff(colnames(baseline_results), c('period','date','time','b1', 'b1_lower', 'b1_upper'))
  ensemble_df = data.frame(zeros(length(unique(pred_list$npi$date)), length(ensemble_cols)))
  colnames(ensemble_df) = ensemble_cols
  weight_df = data.frame(ensemble_df)
  
  for(model in names(pred_list)){
    model_preds = pred_list[[model]]
    model_cols = setdiff(colnames(model_preds), c('b1_lower', 'b1_upper'))
    model_preds = model_preds[,model_cols] %>% filter(date >= start_date, date <= forecast_end) %>%
      select(-b1, -period) %>% distinct()
    update_cols = intersect(ensemble_cols, colnames(drop_na(model_preds)))
    tryCatch({
      ensemble_df[,update_cols] = suppressWarnings(ensemble_df[,update_cols] + weights[[model]] * model_preds[,update_cols])
      weight_df[,update_cols] = weight_df[,update_cols] + weights[[model]]
    }, error=function(e){print(paste0('error combining predictions from ', model, 'nrow ens_df: ', nrow(ensemble_df), ' | nrow model_df: ', nrow(model_preds)))})
  }
  
  ensemble_df = ensemble_df * 1/weight_df
  
  int_results_columns <- names(int_results)
  
  pred_list$ensemble = ensemble_df %>%
    mutate(time = c(1:nrow(ensemble_df)),
           date = time + start_date - 1,
           period = ifelse(date <= forecast_start, 'fit', 'forecast')) %>%
    left_join(distinct(pred_list$mobility[,c('date','b1')]), by='date') %>%
    select(!!!int_results_columns)
  
  pred_list$Baseline = baseline_results
  
  if(plt){
       print(suppressWarnings(draw_intervention_plot(input=region_input, hops.cap.dat=hc_data, intervention.df=updated.intervs.df, plotCounty=isCounty, graph.start.date=start_date, 
                                                  mod_results=pred_list, VarShowCap = c('Deaths'), interv.real.overlay = T)))
  }
  
  if(just_ens_data) return(pred_list$ensemble)
  
  return(pred_list)
}



# ======= APPROACH 1: Sum of State Predictions =================# 
state_names = STATE_NAMES$full
n_days=28
state_res = pblapply(state_names, function(state){res=run_ens_model(state, plt=F, ppe=F, just_ens_data = T)})
names(state_res) = state_names
ens_start_date = as.Date(max(sapply(state_res, function(x){min(x$date,na.rm=TRUE)})), origin='1970-01-01')
ens_end_date = as.Date(min(sapply(state_res, function(x){max(x$date,na.rm=TRUE)})), origin='1970-01-01')
forecast_start_date = ens_end_date - (n_days - 1)
ens_rowct = as.integer(ens_end_date - ens_start_date + 1)
non_agg_cols = c('time','date','b1','period')
agg_cols = setdiff(colnames(state_res[[1]]), non_agg_cols)

agg_df = data.frame(zeros(ens_rowct, length(agg_cols)))
colnames(agg_df) = agg_cols

for(state in state_names){
  state_preds = state_res[[state]] %>%
    filter(date >= ens_start_date, date <= ens_end_date) %>% 
    select(!!! ens_cols) %>% 
    distinct()
  ens_df = ens_df + state_preds[,ens_cols]
}

ens_df = agg_df %>%
  mutate(
    time = c(0:(ens_rowct-1)),
    date =  time + ens_start_date, 
    period = ifelse(date <= forecast_start_date, 'fit', 'forecast')
  )

nat_ts = get_national_time_series() %>%
  filter(date >= ens_start_date, date <= ens_end_date)

ggplot(nat_ts, aes(x=date, y=death_count)) + geom_point(color='red') + 
  geom_line(data=ens_df, aes(s=date, y=D, color=period), size=1.5) +
  labs(x='Date', y='Cumulative Deaths', color='Period') + 
  ggtitle(paste0('National Death Forecast through ', format(ens_end_date, format='%B %d, %Y'))) + 
  theme_bw()

#===============APPROACH 2: BUILD AND TUNE A NATIONAL SEIR MODEL==================#
national_ts = get_national_time_series()


CLUSTER = build_cluster()
