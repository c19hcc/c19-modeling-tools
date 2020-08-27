##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: 
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

source('code/optimizer.R')
source('code/mobility_forecasting.R')

library(foreach)

tune_region_model_moving_window <- function(region_name, metric='Deaths', npi_lag=14, free_params=NULL, fixed_params=NULL, win=20,stp=2, just_new_days=F,
                                            loss_fn=RMSE, parallel=F, Tmax=180, export_data=F, baseline_tuning=F, smooth=F, trim_beta_days=0, plt=F){
  ti = Sys.time()
  print(paste0('TUNING ', toupper(region_name), ' MODEL'))
  
  # get all state input parameters
  input = suppressWarnings(get_all_region_params(region_name, fixed_params=fixed_params))
  
  # get state hospital data
  hosp_cap_data = get_region_hosp_data(region_name, HDATA)
  
  # get region intervention data (if couty, get state's interventions)
  isCounty = detect_county(region_name)
  if(isCounty) state_name = state_lookup(strsplit(region_name, ', ')[[1]][[2]]) else state_name = region_name
  intervention_data = generate_state_npi_df(state_name, return_npis=T)
  state_npis = intervention_data$state_npis
  intervention_df = intervention_data$timeline_df
  
  # get state counts for chosen metric (either cases or deaths)
  region_df <- get_region_plot_ts(region_name, metric)
  
  # if smooth=True, smooth raw time series with splines
  if(smooth){
    print('smoothing data...')
    ss = smooth.spline(x=region_df$date, y=region_df$value, spar=.5)
    region_df = cbind.data.frame(region_df, smoothed=ss$y) %>%
      rename(value_raw = value) %>%
      mutate(value = ifelse(smoothed > 0, smoothed, 0)) #%>%
    #rename(value=smoothed)
  }
  
  # calculate derivatives of time series for later use
  region_df <- region_df %>% mutate(
    d1 = value - lag(value, n=1, default=0), 
    d2 = (d1 - lag(d1, n=1, default=0)), 
    d3 = (d2 - lag(d2, n=1, default=0)), 
    #window = sapply(d2, function(x){round(max(sqrt(1-x/max(abs(d2)))*win, stp*4))}), 
    #window = lag(window, n=10, default=win), 
    window = win
  )
  
  # if parallel=T, point function to CLUSTER variable
  if(parallel){
    if(!exists('CLUSTER')){
      stop('Could not find global CLUSTER variable. Please run CLUSTER = build_clusters()
              first to run this function in parallel.')
    }
  }else{
    CLUSTER = NULL
  }
  
  # figure out baseline cutoff date based on first npi date, first date of data, and model start date
  start_date_offset = 0
  # if start date before first date of real data, offset start time by difference
  if(min(region_df$date, na.rm=T) - input$start_date > 0){
    start_date_offset = min(region_df$date, na.rm=T) - input$start_date
  }
  
  # if NPIs, figure out first NPI date and use as cutoff. 
  npi_cutoff = 0
  if(nrow(state_npis) > 0){
    npi_cutoff = min(state_npis$start_date, na.rm=T) -min(region_df$date, na.rm=T) + npi_lag
  }
  
  if(isCounty){
    tuning_df = COUNTY_TUNING_PARAMS
    reg_col = 'region'
    
  }else{
    tuning_df = STATE_TUNING_PARAMS
    reg_col = 'state'
  }
  # if just_new_days = TRUE, start tuning where old tuning left off. Otherwise, re-tune the entire curve.
  if(just_new_days){
    # get region's previous b1 values and re-generate model results up to last date of tuning.
    region_betas = gen_beta_df(region_name=region_name, beta_ts=MOVING_BETA_DATA) %>%
      filter(start_time != end_time)
    # generate model results based on beta timelines
    baseline_res = gen_region_seir_df(region_name, b1_timelines=region_betas) %>%
      filter(time <= max(time, na.rm=T) -win + 1)
    # set Tmax_bline to last day of results with complete window lookahead
    Tmax_bline = tail(baseline_res$time, 1)
    # set tuning params equal to default values for region
    finetuned_params = tuning_df[which(tuning_df[,reg_col]==region_name),-c(1)]
    bline_params = finetuned_params %>% select(-loss)
  }else{
    # run model on at most 3 days of real data
    Tmax_bline = as.integer(start_date_offset + min(3, npi_cutoff))
    
    # if baseling_tuning = True, tune model to get best starting conditions. Othersie, use defaults
    if(baseline_tuning){
      assert_that(tolower(metric) %in% c('deaths', 'cases'))
      
      # STEP 1: fit baseline model up to npi_cutoff days after the first NPI was implemented
      if(parallel) clusterExport(CLUSTER,list('region_name', 'loss_fn', 'free_params', 'npi_lag', 'fixed_params'), envir=environment())
      baseline_params = tune_region_model(region_name, metric=metric, npi_lag=npi_lag, pre_NPI=T, free_params=free_params,
                                          fixed_params=fixed_params, exprt=export_data, finetune=F, parallel=F)
      
      # fine-tune baseline fit
      new_fixed_params = baseline_params %>% select(-loss)
      print(paste0('new fp: ', new_fixed_params))
      if(parallel)  clusterExport(CLUSTER, list('new_fixed_params'), envir=environment())
      finetuned_params = tune_region_model(region_name, metric=metric, npi_lag=npi_lag, pre_NPI=T, free_params=free_params,
                                           fixed_params=new_fixed_params, exprt=export_data, finetune=T, parallel=parallel)
      
    }else{
      finetuned_params = tuning_df[which(tuning_df[,reg_col]==region_name),-c(1)] 
    }
    
    # override any tuned params with fixed params if necessary
    if(!is.null(fixed_params)){
      common = intersect(names(fixed_params), names(finetuned_params))
      finetuned_params[,common] = fixed_params[common]
    }
    
    # STEP 2: run model with best params and store results
    bline_params = finetuned_params %>% select(-loss)
    for(param in names(bline_params)){input[param]=bline_params[param]}   
    
    baseline_res = run_model_lite(input, Tmax_bline,  hdata=hosp_cap_data)
  }
  
  # STEP 3: fit the model incrementally STP days at a time by looking at window of size WIN
  step = stp; window=win
  # 3.b figure out how many steps correspond to actual count data.
  start = Tmax_bline
  end = as.integer(max(region_df$date, na.rm=T) - input$start_date)
  # if trim_beta_days > 0, exclude last trim_beta_days # of days from beta results
  if(trim_beta_days > 0) end = end - trim_beta_days
  
  intvls = seq(from=start, to=end, by=step)
  #intvl_windows = sapply(intvls, function(x){max(1, round((1-x/max(intvls))^(1/2)*window))})
  
  # if no trimming, add extra window at end to deal with remaider 
  if(max(intvls) < end) intvls <- c(intvls, end)
  # tune w.r.t b1 for each interval
  print('INITIAL CONDITIONS')
  print(finetuned_params)
  drop_cols = c('b1', 'tuning_date', 'loss')
  fp = finetuned_params[,setdiff(colnames(finetuned_params), drop_cols)]
  param_combs = get_param_combs(free_params=c('b1'), fixed_params=fp)
  
  # generate param combo lists
  non_tuned_input = input[setdiff(names(input), names(param_combs))]
  input_lists = lapply(1:nrow(param_combs), function(i){c(non_tuned_input, param_combs[i,])})
  
  # iterate through each time interval, fitting b1 for the corresponding time window
  final_df = baseline_res 
  if(!'period' %in% colnames(final_df)) final_df$period = 'baseline'
  if(!'b1' %in% colnames(final_df)) final_df$b1 = bline_params$b1
  
  if(parallel) clusterExport(CLUSTER, list('region_df', 'hosp_cap_data', 'loss_fn'), envir=environment())
  
  for(i in 1:(length(intvls)-1)){
    # start day after previous run ended
    t_start  = intvls[i]
    t_end = intvls[i+1]
    
    # if possible, use calculated window size for corresponding data range. Otherwise, resort to default
    #window_ids = as.integer(t_start:t_end - start_date_offset)
    #empirical_win = round(mean(region_df[window_ids,'window'], na.rm=T))
    #if(!is.na(empirical_win)) window = empirical_win else window=win
    window=win
    print(paste0('------window ', i, ': ',window, ' ---------------'))
    
    # set initial conditions for run
    P = tail(final_df, 1)
    y0 = c(S=P$S, E=P$E, I0=P$I0, I1=P$I1, I2=P$I2, I3=P$I3, R=P$R, D=P$D)
    
    print(paste0('===',t_start+input$start_date,'->',format(t_end+input$start_date, format='%m-%d'), '==='))
    # run model and calc error for each combination of b1
    if(parallel) clusterExport(CLUSTER, list('P', 'y0', 'window'), envir=environment())
    param_combs$loss <- unlist(pblapply(input_lists, function(inp){run_model_and_eval(inp, Tmax=window+1,
                                                                                      eval_df=region_df, 
                                                                                      y0=y0, offset=P$time,
                                                                                      hdata = hosp_cap_data, 
                                                                                      loss_fn=loss_fn)},cl=CLUSTER))
    # choose params that minimize loss
    rough_params = param_combs %>% top_n(-1, loss)
    
    # fine-tune these parameters for all but last interval
    if(i < length(intvls)-1){
      rough_fp = rough_params %>% select(-b1, -loss)
      if(rough_params$b1==0){
        b1 = seq(from=.0,to=.04,by=.01)
      }else{
        b1 = seq(from=rough_params$b1-.03, to=rough_params$b1+.03, by=.01)
      }
      if(ncol(rough_fp)==1){
        param_list = c(rough_fp[rep(1,length(b1)),])
        param_name = names(rough_fp)[[1]]
        finetuned_combs = data.frame(b1)
        finetuned_combs[,param_name] = param_list
      }else{
        finetuned_combs = data.frame(cbind(b1), rough_fp[rep(1,length(b1)),])
      }
      rownames(finetuned_combs) <- c(1:nrow(finetuned_combs))
      finetuned_lists = lapply(1:nrow(finetuned_combs), function(i){c(non_tuned_input, finetuned_combs[i,])})
      finetuned_combs$loss <- unlist(lapply(finetuned_lists, function(inp){run_model_and_eval(inp, Tmax=window+1,
                                                                                              eval_df=region_df, 
                                                                                              y0=y0, offset=P$time,
                                                                                              hdata = hosp_cap_data)} ))
      optimal_params = finetuned_combs %>% top_n(-1, loss)
    }else{
      optimal_params = rough_params
    }
    
    # run model with optimal params and store output
    window_params = input
    for(param in names(optimal_params)){window_params[param] = optimal_params[param]}
    if(just_new_days){step_idx = as.integer(str_extract(tail(final_df$period, 1), '[0-9]+'))+1}
    else{step_idx=i}
    window_df = run_model_lite(window_params, window, offset = t_start, y0=y0, hdata=hosp_cap_data) %>%
      mutate(period=paste0('step',step_idx), b1=window_params$b1) %>%
      filter(time >= t_start, time <=t_start+step)
    
    final_df = bind_rows(final_df, window_df)
  }
  final_df = final_df %>% mutate(date=time+input$start_date)
  dt = Sys.time() - ti
  print(paste0('Total time: ', as.numeric(dt), attr(dt, 'units')))
  # if smoothed, revert back to original time series for comparison
  if(plt){
    if(smooth) region_df = get_region_plot_ts(region_name, metric, input$start_date)
    print(ggplotly(ggplot(data=region_df %>% filter(date >=input$start_date), aes(x=date,y=value), color='red') + 
                     geom_line() +
                     geom_line(data=final_df, aes(x=date, y=D, color=as.factor(period)))
    ))
    print(draw_beta_death_plot(final_df,region_df))
    # get initial conditions from last day of baseline run
  }
 
  calc_model_error(actual_df=region_df, pred_df=final_df, start_date=input$start_date, plt=plt)
  
  # step 5: if smooth=T, apply b1 smoothing (splines) and re-calculate predictions with interpolated values
  #if(smooth) final_df = smooth_results(final_df)
  
  return(final_df)
}


smooth_results <- function(res_df){
  b1_splined <- data.frame(
    with(res_df,  spline(x=time, y=b1, method='fmm'))
  ) %>% rename(time=x, b1_smooth=y) %>%
    mutate(b1_smooth = sapply(b1_smooth, function(x){max(0,x)}))
  
  b1_splined = b1_splined %>% 
    mutate(t_int = floor(time)) %>%
    group_by(t_int) %>%
    summarise(b1_smooth=round(mean(b1_smooth), 3)) 
  
  b1_splined$group = rep(NA,nrow(b1_splined))
  group_idx = 1
  for(i in 1:(nrow(b1_splined)-1)){
    if(b1_splined[i+1,'b1_smooth'] - b1_splined[i,'b1_smooth'] != 0) group_idx = group_idx+1
    b1_splined[i, 'group'] = group_idx
  }
  b1_splined[which(is.na(b1_splined$group)), 'group'] = max(b1_splined$group, na.rm=T)
  

  return(final_smoothed)
}

gen_region_seir_df <- function(region_name, Tmax=180, hdata=HDATA, no_preds=T, b1_timelines=NULL, trim_beta_days=0){
  if(is.null(b1_timelines)) b1_timelines = gen_beta_df(region_name)
  input = suppressWarnings(get_all_region_params(region_name))
  hosp_cap_data = get_region_hosp_data(region_name,hdata=hdata)
  final_df = data.frame()
  for(grp in b1_timelines$period){
    tmp_input = input
    grp_rec = b1_timelines %>% filter(period==grp)
    tmp_input$b1 = grp_rec$b1
    T_dur = max(1, grp_rec$end_time - grp_rec$start_time)
    if(grp =='baseline'){
      y0=NULL
    }else{
      P = tail(final_df, 1)
      y0 = c(S=P$S, E=P$E, I0=P$I0, I1=P$I1, I2=P$I2, I3=P$I3, R=P$R, D=P$D)
    }
    group_res = run_model_lite(tmp_input, Tmax=T_dur, y0=y0,
                               hdata=hosp_cap_data, offset=grp_rec$start_time) %>%
      mutate(b1 = grp_rec$b1, period=grp)
    final_df = rbind.data.frame(final_df, group_res) 
  }
  final_df = final_df %>% mutate(date = time + input$start_date)
  if(no_preds) final_df = final_df %>% filter(period != 'final')
  if(trim_beta_days > 0) final_df = final_df %>% filter(time <= max(time, na.rm=T) - trim_beta_days)
  return(final_df)
}

gen_beta_df <- function(region_name=NULL, beta_ts = NULL, beta_fname = 'data/database/new_region_fits.csv'){
  if(is.null(beta_ts)){
    beta_ts = read_data_from_file(beta_fname, date_cols=c('date'))
  }else if(!('region' %in% colnames(beta_ts))){
    # if no region column, in beta_ts, assume all all betas in beta_ts are for chosen region
    beta_ts$region = region_name
  }
  #isCounty = detect_county(region_name)
  beta_df <- beta_ts %>%
    select(region, b1, period, date, time) %>%
    rename(tmp_b1=b1) %>%
    group_by(region, period) %>%
    summarise(
      start_time = min(time),
      end_time = max(time),
      start_date = min(date), 
      b1 = mean(tmp_b1, na.rm=T)
    ) %>%
    ungroup() %>%
    arrange(region, start_time)
  if(!is.null(region_name)) beta_df = beta_df %>% filter(region==region_name)

  return(beta_df)
}

draw_beta_death_plot <- function(model_res, state_df, plt.height=NULL){
  
  plt_cols <- c("#882255", "#44AA99")
  plt_line_names <- c("Est. &#946;<sub>1</sub>", "Cumulative Deaths")
  names(plt_cols) <- plt_line_names
  state_name = unique(state_df$mState.Providence)[1]
  ret.plt <- plot_ly(colors = plt_cols, height = plt.height)
  
  # b1.dat <- BETA1_TRACKING %>%
  #   filter(state == sel.abb[1]) %>%
  #   mutate(value = ifelse(b1>2, 2, b1),
  #          date = as.Date(start.middate),
  #          metric = "Est. &#946;<sub>1</sub>") %>%
  #   filter(as.Date(date) >= as.Date(graph.start.date))
  
  yaxis1.ops <- list(
    tickfont = list(color="#882255"),
    title = "Est &#946;<sub>1</sub>",
    nticks = 5,
    range = c(0, max(model_res$b1, na.rm=TRUE)*1.05)
  )
  
  yaxis2.opts <- list(
    overlaying = "y",
    #tickfont = list(color="#44AA99"),
    side = "right",
    title = "Death Counts",
    showgrid = FALSE,
    range = c(0, max(state_df$value,na.rm=TRUE)*1.05)
  )
  plot.title <- paste0("Model Fit and b1 Changes for ", state_name)
  current.day <- Sys.Date()
  
  ret.plt <- ret.plt %>%
    add_trace(data = model_res, x=~date, y=~b1, color='B1', type='scatter', 
              mode='lines', line=list(color="#882255",dash='solid',width = 2)) %>%
    add_trace(data = state_df, x=~date, y=~value, color='Actual Deaths', type='scatter', 
              mode='lines', line=list(color="red",dash='solid',width = 2), 
              yaxis = "y2") %>%
    add_trace(data = model_res, x=~date, y=~D, color='Estimated Deaths', type='scatter', 
              mode='lines', line=list(color="#44AA99",dash='solid',width = 2), 
              yaxis = "y2") %>%
    
    layout(title = list(text=plot.title,x=0,xref="paper"),
           hovermode = "x unified",
           xaxis=list(title="", range = c(min(state_df$date), max(state_df$date))), 
           yaxis=yaxis1.ops,
           yaxis2=yaxis2.opts,
           margin=list(l=45, r=200, autoexpand=FALSE),
           legend = list(x=1.08))
  return(ret.plt)
  
}

# generate seir predictions by three methods:
# 1. using last beta value (basic),
# 2. using mean of last n beta values (mean), or
# 3. simulation, varying b1 and a0 (montecarlo), 

gen_seir_preds_and_cis = function(region_name, Tmax=30, method='montecarlo', plt=F, n=20,
                                  iters=1000, model_res=NULL, input=NULL, hosp_cap_data=NULL){
  # get all params if called offline
  if(is.null(input)) input = suppressWarnings(get_all_region_params(region_name))
  
  # get hosp capacity params if none provided
  if(is.null(hosp_cap_data)) hosp_cap_data = get_region_hosp_data(region_name, HDATA)

  # get model results if not given already
  model_res = gen_region_seir_df(region_name, HDATA, no_preds=T) %>%
    mutate(date = time+input$start_date)
  # set initial conditions for prediction
  P = tail(model_res, 1)
  y0 = c(S=P$S, E=P$E, I0=P$I0, I1=P$I1, I2=P$I2, I3=P$I3, R=P$R, D=P$D)

  # BASIC: generate simple confidence intervals by running model on upper and lower bounds
  # of a0 and the last b1 value.
  if(method=='basic'){
    # use last b1 value
    input$b1 = P$b1
    # vary asymptomatics from 20-40%
    asym_range = list('lb'=20, 'ub'=40, 'default'=30)
    ci_dfs = list()
    for(asym_type in names(asym_range)){
      tmp_input = input
      tmp_input$FracAsympto = asym_range[[asym_type]]
      # get lower bound results
      res =  run_model_lite(tmp_input, Tmax, hdata=hosp_cap_data, offset=P$time, y0=y0) %>%
        mutate(type=asym_type, FracAsympto=asym_range[[asym_type]])
      ci_dfs[[asym_type]] = res
    }
    final_df = ci_dfs$default %>%
      mutate(date = time+input$start_date, value=D) %>%
      left_join(ci_dfs$lb %>% select(time, D) %>% rename(lb=D), by='time') %>%
      left_join(ci_dfs$ub %>% select(time, D) %>% rename(ub=D), by='time')

  }else if(method == 'mean'){
    tmp_input = input
    b1s = tail(model_res$b1, n)
    tmp_input$b1 = mean(b1s, na.rm=T)
    # mean results
    mean_res =  run_model_lite(tmp_input, Tmax, hdata=hosp_cap_data, offset=P$time, y0=y0)
    # upper bound using 90th percentile of b1 values
    tmp_input$b1 = quantile(b1s, p=.90, na.rm=T)[[1]]
    ub_res =  run_model_lite(tmp_input, Tmax, hdata=hosp_cap_data, offset=P$time, y0=y0)
    # lower bound using 10th percentile of b1 values
    tmp_input$b1 = quantile(b1s, p=.10, na.rm=T)[[1]]
    lb_res =  run_model_lite(tmp_input, Tmax, hdata=hosp_cap_data, offset=P$time, y0=y0)
    final_df = mean_res %>%
      mutate(date = time+input$start_date, value=D) %>%
      left_join(lb_res %>% select(time, D) %>% rename(lb=D), by='time') %>%
      left_join(ub_res %>% select(time, D) %>% rename(ub=D), by='time')
    
  }else if(method=='montecarlo'){ 
    # use b1 from past n days to generate sample to
    dfunc = density(tail(model_res$b1, n))
    dfunc_df = data.frame(x=dfunc$x, y=dfunc$y) %>% filter(x >= 0)
    # generate random sample of 1000 b1s
    b1s = sample(dfunc_df$x, replace=T, prob=dfunc_df$y, size=iters)
    uniq_b1s = unique(model_res$b1)
    #b1s = sample(tail(model_res$b1, n), replace=T, size=1000)
    b1_cts = table(b1s)

    # generate random asymptomac frac for each unique b1 above (based on 20 study estimates)
    asym_estimates = get_asymptomatic_estimates()
    asyms = sample(asym_estimates, replace=T, size=nrow(b1_cts))

    # generate param lists
    non_tuned_input = input[setdiff(names(input), c('b1', 'FracAsympto'))]
    input_lists = lapply(1:nrow(b1_cts), function(i){c(non_tuned_input, list('b1'=uniq_b1s[i], 'FracAsympto'=asyms[i]))})
    b1_res = pblapply(1:length(input_lists), function(i){run_model_lite(input_lists[[i]], Tmax=Tmax,
                                                                        hdata=hosp_cap_data, y0=y0,
                                                                        offset=P$time) %>%
        mutate(iter = paste0('iter_',i))})

    comb_res = bind_rows(b1_res) %>% select(time, D, iter)
    comb_res_wide = pivot_wider(comb_res, id='time', names_from='iter', values_from='D')
    #mean_deaths = as.matrix(comb_res_wide[,-c(1)]) %*% as.matrix(b1_cts) / sum(b1_cts)

    final_df = comb_res_wide %>%
      melt(id=c('time')) %>%
      group_by(time) %>%
      summarise(q10 = quantile(value, p=.10, na.rm=T),
                q90 = quantile(value, p=.90, na.rm=T),
                stdev = sqrt(var(value, na.rm=T)),
                avg = mean(value, na.rm=T),
                q25 = quantile(value, p=.25, na.rm=T),
                q75 = quantile(value, p=.75, na.rm=T),
                med = median(value, na.rm=T)
                ) %>%
      mutate(date=time + input$start_date, value=med, ub=q75, lb=q25) %>%
      filter(time <= P$time + Tmax)
  }else if(method == 'grid'){
    # generate a grid based on all asymptomatic estimates and last n b1 values
    # enumerate last n b1 values (2*n because each step is 2 days)
    b1s = tail(model_res$b1, 2*n)
    b1_cts = table(b1s)
    uniq_b1s = as.numeric(names(b1_cts))
    
    # enumerate all estimates of FracAsymptomatic
    asym_estimates = get_asymptomatic_estimates()
    asym_cts = table(asym_estimates)
    uniq_asyms = as.numeric(names(asym_cts))
    
    # generate param lists
    param_grid = expand.grid(list('b1'=uniq_b1s, 'FracAsympto'=uniq_asyms))
    non_tuned_input = input[setdiff(names(input), c('b1', 'FracAsympto'))]
    input_lists = lapply(1:nrow(param_grid), function(i){c(param_grid[i,], non_tuned_input)})
    
    b1_res = pblapply(1:length(input_lists), function(i){run_model_lite(input_lists[[i]], Tmax=Tmax,
                                                                        hdata=hosp_cap_data, y0=y0,
                                                                        offset=P$time) %>%
        mutate(iter = paste0('iter_',i))})
    
    comb_res = bind_rows(b1_res) %>% select(time, D, iter)
    comb_res_wide = pivot_wider(comb_res, id='time', names_from='iter', values_from='D')
    
    final_df = comb_res_wide %>%
      melt(id=c('time')) %>%
      group_by(time) %>%
      summarise( lb = quantile(value, p=.10, na.rm=T),
                ub = quantile(value, p=.90, na.rm=T),
                stdev = sqrt(var(value, na.rm=T)),
                avg = mean(value, na.rm=T),
                q25 = quantile(value, p=.25, na.rm=T),
                q75 = quantile(value, p=.75, na.rm=T),
                med = median(value, na.rm=T)
      ) %>%
      mutate(date=time + input$start_date, value=med) %>%
      filter(time <= P$time + Tmax)
  }
  if(plt){
    # get region time series
    region_df = get_region_plot_ts(region_name)
    max_date = format(max(final_df$date), format="%b %d, %Y")
    print(ggplotly(ggplot(final_df, aes(x=date, y=value)) +
               ggtitle(paste0(region_name, ' Projected Deaths Through ', max_date)) +
               labs(x='Date', y='Cumulative Deaths') +
               geom_line(color='blue', linetype='dotdash') +
               geom_ribbon(aes(ymin=lb, ymax=ub), fill='grey70', alpha=.5) +
               geom_line(data=model_res, aes(x=date,y=D), color='blue') +
               geom_point(data=region_df, aes(x=date,y=value), color='red') + theme_bw()) %>%
      layout(yaxis=list(yref='paper')))
  }
  return(final_df)
}

get_asymptomatic_estimates <- function(){
  # table of estimates pulled from https://www.cebm.net/covid-19/covid-19-what-proportion-are-asymptomatic/
  asym_df = read.csv('data/frac_asymptomatic_estimates.csv', stringsAsFactors = F)
  asym_ests = asym_df %>% filter(!is.na(P)) %>% .$P
  return(asym_ests)
}

tune_region_list_moving_window = function(region_list, Tmax=180, parallel=F, fname=NULL, window=20, just_new_days=F, dbg=F,
                                          free_params=c('b1', 'start_date'), fixed_params=NULL, baseline_tuning=F, trim_beta_days=0, plt=F){
  ti = Sys.time()
  if(is.null(fname)) fname = 'data/test_new_region_fits.csv'
  print(paste0('Tuning ', length(region_list), ' regions, starting with: '))
  print(head(region_list))
  if(parallel){
    comb_df = foreach(i=1:length(region_list), .combine=rbind) %dopar% {
      source('code/tuning_moving_window.R', local=TRUE)
      res = NULL
      res = tryCatch(expr = {
        region_name = region_list[[i]]
        state_df = get_region_plot_ts(region_name, 'Deaths')
        hosp_cap_data = get_region_hosp_data(region_name, HDATA)
        print('getting state param input')
        input = suppressWarnings(get_all_region_params(region_name, fixed_params=list()))
        tune_region_model_moving_window(region_name, baseline_tuning=baseline_tuning, win=window, smooth=T,
                                            parallel=F, free_params=free_params, fixed_params=fixed_params,
                                            trim_beta_days=trim_beta_days, just_new_days=just_new_days, plt=plt) %>%
        mutate(region=region_name, date_tuned=Sys.Date())  %>% select(region, time, date, period, b1)
        
        
        },
        error = function(e){warning(paste0('ERROR tuning for ',region_name, ' \n', e))}
      )
      if(!is.null(res)){
        fname = paste0('tuning_results/region_betas/', region_name,'_betas.csv')
        if(dbg) print(paste0('Exporting results to: ', fname))
        write.csv(res, file=fname, row.names=F)
      }
    }
  }else{
    comb_df = data.frame()
    for(i in 1:length(region_list)){
      #source('code/tuning_moving_window.R', local=TRUE)
      region_name = region_list[[i]]
      #print(region_name)
      res = NULL
      res = tryCatch(expr = {
        state_df = get_region_plot_ts(region_name, 'Deaths')
        hosp_cap_data = get_region_hosp_data(region_name, HDATA)
        #print('getting state param input')
        input = suppressWarnings(get_all_region_params(region_name, fixed_params=list()))
        tune_region_model_moving_window(region_name, baseline_tuning=baseline_tuning,win=window,
                                            smooth=T, parallel=parallel, free_params=free_params, 
                                            trim_beta_days=trim_beta_days, just_new_days=just_new_days) %>%
        mutate(region=region_name, date_tuned=Sys.Date())  %>% select(region, time, date, period, b1)
        }, 
        error = function(e){warning(paste0('ERROR tuning for ',region_name, ' \n', e))}
      )
      if(!is.null(res)){
        fname = paste0('tuning_results/region_betas/', region_name,'_betas.csv')
        if(dbg) print(paste0('Exporting results to: ', fname))
        write.csv(res, file=fname, row.names=F)
        comb_df = rbind.data.frame(comb_df, res)
      }
    }
  }
  write.csv(comb_df, file=fname, row.names=F)
  if(parallel){if(exists('CLUSTER')) stopCluster(CLUSTER)}
  tot_time = Sys.time()-ti
  print(paste0('DONE. Total time: ', round(tot_time,2), ' ', attr(tot_time, 'units')))
  print(paste0('Avg time per region: ', tot_time/length(region_list),' ', attr(tot_time, 'units')))
  return(comb_df)
}
