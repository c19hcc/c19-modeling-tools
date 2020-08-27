##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: Forecasts mobility based on historical data for model.
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

# build a smoothed, polynomial lm of mobility based up to last n days of data
build_mob_spline_lm <- function(region_name, n=NULL, deg=2, plt=F, val_days=5, smoothing_val=.55){
  
  region_mob = MOBILITY_DATA %>% filter(mState.Providence==region_name) %>%
    mutate(date = as.Date(date), 
           mob = percent_off_norm_maxMobility)
  
  # smooth mobility data to get rid of noise
  mob_spline = smooth.spline(x=region_mob$date, y=region_mob$mob, spar=min(1, smoothing_val))
  sp_df = data.frame(date=as.Date(mob_spline$x, origin='1970-01-01'),
                     mob=mob_spline$y) %>%
    mutate(mob = ifelse(mob < 0, 0, mob))
  region_mob$mob_smooth = sp_df$mob
  # trim dataframe to only include last n days of data
  start_date = max(sp_df$date - n)
  first_mob_date = min(sp_df$date, na.rm=T)
  start_time = as.integer(start_date - first_mob_date)
  
  # if now window size provided, find optimal number of days to tran model
  # that minimizes RMSE on test set (last val_days of data)
  if(is.null(n)){
    winds = seq(from=15, to=60, by=5)
    loss_df = data.frame('window'=winds, 'loss'=rep(NA, length(winds)))
    
    for(i in c(1:length(winds))){
      start_date = max(sp_df$date,na.rm=T) - val_days - winds[i]
      start_time = as.integer(start_date - first_mob_date)
      
      w_mob_mod_df = sp_df %>% 
        filter(date >= start_date) %>%
        mutate(time = as.integer(date - first_mob_date)) %>% select(-date)
      
      train_inds = 1:(nrow(w_mob_mod_df)-val_days)
      train = w_mob_mod_df[train_inds,]
      test = w_mob_mod_df[-train_inds,]
      
      w_mob_lm = lm(mob ~ poly(time, degree=deg), data=train)
      w_preds = predict(w_mob_lm, newdata=test)
      loss = RMSE(w_preds, test$mob)
      loss_df[i, 'loss'] = loss
    }
    n = loss_df[which.min(loss_df$loss), 'window']
    print(paste0('optimal window size: ', n))
    start_date = max(sp_df$date,na.rm=T) - n
  }
 
  # prep data for modeling
  mob_mod_df = sp_df %>% 
    filter(date >= start_date) %>%
    mutate(time = as.integer(date - first_mob_date)) %>% select(-date)
  
  # build 'deg' degree polynomial to fit mobility data
  mob_lm = lm(mob ~ poly(time, degree=deg), data=mob_mod_df)
  summary(mob_lm)
  pred_df <- data.frame(pred = mob_lm$fitted.values,
                        time=mob_mod_df$time,
                        date=mob_mod_df$time + first_mob_date)
  
  model_obj = list('typ'='poly', 'poly_lm'=mob_lm, 'start_date' = start_date, 'y'=mob_mod_df$mob,
                   'all_mob_data' = region_mob, 'fitted.values'= mob_lm$fitted.values,
                   'mod_df'=mob_mod_df, 'region'=region_name, 'window'=n)
  return(model_obj)
}

# build a trend + seasonality time-series model of mobility
build_mob_ts_lm <- function(region_name, seasonal_lags = NULL, smooth=F, smooth_amt=.20, w=20){
  # get region's mobility data
  region_mob = MOBILITY_DATA %>% filter(mState.Providence==region_name) %>%
    mutate(date = as.Date(date), dow=lubridate::wday(date, label=TRUE), ts=percent_off_norm_maxMobility)
  ts = region_mob$percent_off_norm_maxMobility
  # if smooth=T, smooth data first before fitting
  if(smooth){
    spline_res = smooth.spline(x=1:length(ts), y=ts, spar=smooth_amt)
    ts = spline_res$y
    region_mob$mob_smooth = ts
  }
  names(ts) = c(1:length(ts))
  # account for trend in time time after NPIs have taken full effect (valley of mobility curve)
  # find valley of curve (defining as time when mobility reaches 10th percentile of curve values)
  valley_line = quantile(ts, p=.1, na.rm=T)[[1]]
  start_day = which.min(ts-valley_line)[[1]]
  model_ts = ts[start_day:length(ts)]
  model_df = data.frame(y=model_ts, time=c(as.integer(names(model_ts)))) %>%
    mutate(d1 = y - lag(y, n=1, default=NA),
           dow = region_mob$dow[start_day:length(ts)])
  
  trend_lm = lm(y~time, data=model_df)
  # account for seasonality using auto-correlation
  model_df[,'t_resids'] = residuals(trend_lm)
  if(is.null(seasonal_lags)){
    # build acf plot of previous 12 days
    acfs = acf(model_df$t_resids, lag=12, plot=FALSE) # lags of 1 and 7 seem to be most significant
    # choose lags with correlation coefficients above +-.25 as starting point
    seasonal_lags = which(abs(tail(acfs$acf, -1)) > .25)
  }
  for(l in seasonal_lags){
    # lag_col = lag(ts, n=l, default=NA)
    # model_df[,paste0('l',l)] = lag_col[start_day:length(lag_col)]
    model_df[,paste0('l',l)] = lag(model_df$y, n=l, default=NA)
  }
  # build auto-correlation lm for trend residuals based on lags from above
  seasonal_df = drop_na(model_df[,c('t_resids', paste0('l',seasonal_lags), 'dow')])
  seasonal_lm = lm(t_resids ~ ., data=seasonal_df)
  # to step-wise regression to reduce model to only most significant indicators
  seasonal_lm.step = step(seasonal_lm, direction=c('backward'))
  # only keep significant terms/lags
  sig_terms = attr(seasonal_lm.step$terms, 'term.labels')
  model_df = model_df[,c('y','time','t_resids',sig_terms)]
  sig_lags = intersect(sig_terms, paste0('l',seasonal_lags))
  seasonal_lags = as.integer(str_extract(sig_lags, '[0-9]+'))
  # tack on predictions to model dataframe
  trend_preds = predict(trend_lm, newdata=model_df)
  seasonal_preds = predict(seasonal_lm.step, newdata=model_df)
  # combine predictions, but only use trend predictions leading up to first day of seasonal model
  comb_preds = tail(trend_preds+seasonal_preds, -max(seasonal_lags))
  comb_preds = c(trend_preds[1:max(seasonal_lags)], comb_preds)
  model_obj = list('typ'='trend_seas', 'trend_lm'=trend_lm, 'seasonal_lm'=seasonal_lm.step, 
                   'start_date' = region_mob$date[start_day], 'all_mob_data'=region_mob,
                   'y'=model_ts, 'fitted.values'=comb_preds, 'lags'=seasonal_lags,
                   'mod_df'=model_df, 'region'=region_name)
  return(model_obj)
}



plot_mod_fit <- function(model_obj, pltly=T){
  region_name = model_obj$region
  # isCounty = detect_county(region_name)
  # if(!isCounty) region_name = state_lookup(region_name)

  if(model_obj$typ == 'b1_lm'){
    lm_pred_df = model_obj$mod_df %>% mutate(pred=model_obj$fitted.values)
    lm_pred_df$lagged_mobility = lm_pred_df[,model_obj$best_lag]
    error = RMSE(actual=lm_pred_df$mean_b1, pred=lm_pred_df$pred)
    plot_df = lm_pred_df[,c('mean_b1', 'time', 'date', 'lagged_mobility')] %>%
      rename('b1'=mean_b1, 'Lagged Mobility'=lagged_mobility) %>% melt(id=c('time', 'date'))
   plt = ggplot(plot_df,aes(x=date, y=value, color=variable)) + geom_line() +
               geom_line(data=lm2_pred_df, aes(x=date, y=pred, color='Predicted b1'), size=1.5, linetype='dotdash') +
               ggtitle(paste0('Lagged mobility, B1, and Modeled B1 Rates for ', region_name)) +
               annotate("text", x = max(plot_df$date, na.rm=T)-7, y = quantile(plot_df$value, p=.40, na.rm=T),
                        label=paste0("RMSE: ", round(error,2))) +
               labs(x='Date', y='Value')

  }else{
    pred_df = data.frame(y=model_obj$y,
                         pred=model_obj$fitted.values,
                         time=model_obj$mod_df$time)
    error = RMSE(preds=model_obj$fitted.values, actual=model_obj$y)
    plt = ggplot(pred_df, aes(x=time, y=y)) +
      geom_point(color='red') +
      geom_line(aes(x=time, y=pred), color='blue') +
      labs(x='Time', y='Mobility (% of pre-covid levels)') +
      annotate("text", x = max(pred_df$time, na.rm=T)-7,
               y = quantile(pred_df$y, .1, na.rm=T),
               label=paste0("RMSE: ", round(error,2)))
  }

  if(pltly){ plt = ggplotly(plt)}
  return(plt)
}


# predict using pre-trained seasonal and trend models built with build_ts_lm()
forecast_mobility <- function(model_obj, n=30, plt=F, add_init_offset=T){
  pred_start = tail(model_obj$mod_df$time, 1) + 1
  pred_end = pred_start + n
  
  pred_data = data.frame(time=c(1:pred_end))
  
  # forecast incrementally if model is auto-regressive
  if('typ' %in% names(model_obj)){
    if(grepl('seas', model_obj$typ)){
      # seed forecasting period with last L days of data
      L = max(model_obj$lags)
      lag_names = paste0('l',model_obj$lags)
      
      all_data = model_obj$mod_df %>% mutate(period='fit') %>% select(-t_resids)
      for(i in c(pred_start:pred_end)){
        # STEP 1: get trend prediction based solely on time
        trend_pred = predict.lm(model_obj$trend_lm, newdata=data.frame(time=i))
        
        # STEP 2: get seasonal forecast based on auto-regressive lags and potentially day of week
        # get_lags
        lag_vals = lapply(model_obj$lags, function(lag_val){all_data[which(all_data$time ==(i-lag_val)), 'y']})
        names(lag_vals) = lag_names
        seas_data = lag_vals
        # if day of week is a significant predictor, calculate it
        if('dow' %in% attr(model_obj$seasonal_lm$terms, 'term.labels')){
          dow = lubridate::wday(label=T, i + model_obj$start_date)
          seas_data = c(seas_data, list('dow'=dow))
        }
        
        # seasonal forecast
        seasonal_pred = predict.lm(model_obj$seasonal_lm, newdata=seas_data)
        
        # STEP 3: combine trend and seasonal preds
        comb_pred = list('y'=trend_pred + seasonal_pred)
        
        # STEP 4: add results to dataframe
        all_data <- rbind.data.frame(all_data, c(comb_pred, list('time'=i), seas_data, list('period'='forecast')))
      }
      all_data = all_data %>% select(time, y, period)
    }else if(grepl('poly', model_obj$typ)){
      all_data = model_obj$mod_df %>% rename(y=mob) %>%
        mutate(period='fit', y_upper=y, y_lower=y) %>%
        select(y, y_lower, y_upper, time, period)
      
      forecasts = data.frame(predict(model_obj$poly_lm, newdata = data.frame(time = c(1:n)+max(model_obj$mod_df$time)), interval='confidence'))
      forecast_df = forecasts %>%
        rename(y=fit, y_lower=lwr, y_upper=upr) %>%
        mutate(time=c(1:nrow(forecasts)) + max(all_data$time),
               period='forecast')
      if(add_init_offset){
        offset = tail(model_obj$all_mob_data$mob_smooth, 1) - head(forecast_df$y, 1) 
        forecast_df$y = forecast_df$y + offset
        }
      all_data = rbind.data.frame(all_data, forecast_df)
    }
    else{
      stop('Could not detect model type (typ). Please add "typ" property to your model obj.
           (trend_seas, and poly are currently supported).')
    }
  }
  all_data = all_data %>% mutate(date = time + min(model_obj$all_mob_data$date))
  if(plt){
    fit_df = data.frame(time = as.integer(names(model_obj$fitted.values)), 
                         pred = model_obj$fitted.values) %>%
      mutate(date = time + model_obj$start_date)

    forecast_df = all_data %>% filter(period=='forecast')
    plt = ggplot(forecast_df,aes(x=date))
    # if possible, plot confidence intervals
    if((sum(grepl('y_upper', colnames(all_data))) > 0) && (sum(grepl('y_upper', colnames(all_data))))){
        plt = plt + geom_ribbon(aes(x=date, ymin=y_lower, ymax=y_upper), color='#4915a7', fill='#4915a7', alpha=.3)
    }
    plt = plt + geom_line(data=all_data %>% filter(period=='forecast'), aes(x=date, y=y), color='#4915a7',
                            linetype='dashed', size=.8) +
      geom_line(data=fit_df, aes(x=date, y=pred), color='blue') +
      geom_point(data=model_obj$all_mob_data, aes(x=date, y=mob_smooth), color='red') +
      ggtitle(paste0('Forecasted Mobility for ', model_obj$region, ' through ', format(max(forecast_df$date,na.rm=T), format='%A %d, %Y')))

    print(plt)
  }

  return(all_data)
}





#===============EXPERIMENTS=========================#
# see if model needs differencing using Dickey-fuller test (adf.test)
# ts = region_mob$percent_off_norm_maxMobility
# tsp = tail(ts, -21)
# plot(tsp, type='l', col='blue') # seems to be a trend
# adf.test(tsp, alternative = "stationary") # p=.30 -> series is non-stationary
# count_d1 = diff(tsp, differences = 1) # take first order differences
# lines(count_d1, col='red') # differences seem stationary
# adf.test(count_d1, alternative = "stationary") # p is roughly 0 -> differences are stationary
# # step 2: figure out p and q for arima model using ACF and PACF plots on differences
# acf(count_d1, lag=20)
# 
# arima.model = auto.arima(tsp, seasonal=TRUE)
# res = forecast(arima.model, h=30)
# 
# smooth_df = data.frame(y=ts) %>% mutate(l1 = lag(y, 1), l2 = lag(y,2), l3=lag(y,3),
#                                         num=l1+l2+l3,
#                                         denom=(as.integer((!is.na(l1)) + as.integer(!is.na(l2)) + as.integer(!is.na(l3)))),
#                                         smoothed=lead(num/denom, n=3, default=0))
# 
# 
# comb_df = data.frame(y=y_res) %>%
#   mutate(l1 = lag(y, 1),l2 = lag(y,2),
#          l3=lag(y,3), l4=lag(y,4),
#          l5=lag(y,5), l6=lag(y,6), l7=lag(y,7))
# 
# comb_lm = lm(y ~ l1+l7, data=comb_df)
# 
# comb_df <- comb_df %>%
#   mutate(
#     residual_pred =c(rep(NA, n()-length(comb_lm$fitted.values)), comb_lm$fitted.values), 
#     idx=c(1:n())+20, 
#     trend_pred = c(rep(NA, n()-length(reg$fitted.values)), reg$fitted.values), 
#     total_pred = trend_pred + residual_pred
#   )
# pred_df = data.frame(times=c(1:20)+tail(comb_df$idx, 1))
# og_df = data.frame(idx=1:nrow(region_mob), mob=region_mob$percent_off_norm_maxMobility)
# ggplot(comb_df, aes(x=idx, y=trend_pred)) +
#   geom_line(aes(x=idx,y=total_pred), color='green') + 
#   geom_line(data=og_df, aes(x=idx, y=mob), color='red')
# 
#

#=======B1 modeling============#
region_name = 'Texas'

build_b1_model = function(region_name, mob_model=NULL, mob_mod_typ='poly', plt=F, withTime=T){

  if(is.null(mob_model)){
    if(mob_mod_typ == 'poly'){
      mob_model = build_mob_spline_lm(region_name, deg=2, val_days = 5)
    }else if(mob_mod_typ == 'trend_seas'){
      mob_model = build_mob_ts_lm(region_name)
    }else{stop('Please provide a recognized mobility model typ. (currently support poly (polynomial) and trend_seas (trend/seasonality)')}
  }
  # build mobility model
  mob_model = suppressWarnings(build_mob_spline_lm(region_name, deg=2, val_days = 5, plt=plt))

  # build dataframe combining forecasts with pre-model mobility
  mob_forecasts = forecast_mobility(mob_model, plt=plt) %>% rename(mob=y)

  pre_mob_forecasts = mob_model$all_mob_data %>%
    select(mob=mob_smooth, date) %>%
    mutate(time = c(1:nrow(mob_model$all_mob_data)),
           period='pre',
           y_lower=mob, y_upper=mob) %>%
    filter(date < min(mob_forecasts$date)) %>%
    select(mob, y_lower, y_upper, time, period, date)

  all_mob_df = rbind.data.frame(pre_mob_forecasts, mob_forecasts) %>% mutate(mob = mob/100)

  # b1 ts
  reg_params = get_all_region_params(region_name)
  b1_ts = MOVING_BETA_DATA %>% filter(region==region_name) %>%
    select(date, time, b1) %>% group_by(time) %>%
    summarise(mean_b1 = mean(b1)) %>%
    mutate(date = time + reg_params$start_date) %>%
    ungroup()

  # use smoothed mobility to predict b1
  sp_lag_df = all_mob_df %>% left_join(b1_ts[,c('date','mean_b1')], by='date') %>%select(-period)
  # use lag with highest correlation to b1
  lag_max = 14
  for(i in c(1:lag_max)){
    sp_lag_df[,paste0('mob_lag_',i)] = lag(sp_lag_df$mob, n=i, default=NA)
  }
  corr_df = as.data.frame(cor(drop_na(select(sp_lag_df, -time, -date))))
  corr_df = corr_df[which(corr_df$mean_b1 != 1),]
  best_match = rownames(corr_df)[which.max(head(corr_df$mean_b1, -1))]
  print(paste0('best match: ', best_match))
  model_df = drop_na(sp_lag_df[,c(best_match, 'mean_b1', 'time', 'date')])
  if(!withTime){model_df = select(model_df, -time)}

  beta_lm2 = lm(mean_b1~., data=model_df %>% select(-date))
  model_obj = list('b1_model' = beta_lm2, 'mob_model'=mob_model, 'region'=region_name, 'start_date'=min(model_df$date),
                   'mod_df'=model_df, 'best_lag'=best_match, 'typ'='b1_lm', 'fitted.values'=beta_lm2$fitted.values,
                   'sp_lag_df'=sp_lag_df)
  return(model_obj)
}

#=============old beta lm code=============#

# first_date = mob_ts_lm$start_date - (min(mob_ts_lm$mod_df$time)-1)
# all_mob_df = data.frame(time = as.integer(names(all_mob_ts)), mob=as.vector(all_mob_ts), period=period) %>%
#   mutate(mob = mob/100,  date = time + first_date)
# # npi data
# region_npis = get_state_npis(region_name) %>%
#   filter(!is.na(`Start Date`)) %>%
#   rename(start_date = `Start Date`,
#          end_date = `End Date`,
#          policy_long=Policy,
#          code=Code)
# npi_wts = NPI_Mild_Effectiveness_Map
# timeline = c(1:(max(b1_ts$time)+pred_days))
# npi_vectors = data.frame(time=timeline) %>% mutate(date = time + input$start_date)
#
# for(i in 1:nrow(region_npis)){
#   if(!is.na(region_npis[i, 'start_date'])){
#     npi_vec = c()
#     npi_vec = c(rep(0, max(0, region_npis[i,'start_date']-input$start_date)))
#     rem_days = max(timeline) - length(npi_vec)
#     if(is.na(region_npis[i, 'end_date'])){
#       # assume NPI is active for remaining duration
#       npi_vec = c(npi_vec, rep(1, rem_days))
#     }else{
#       # see if end date occurs before end of timeline. If so, only pad up to end date. Otherwise, pad everything.
#       exp_duration = region_npis[i,'end_date'] - region_npis[i,'start_date']
#       one_cutoff = min(rem_days, exp_duration)
#       npi_vec = c(npi_vec, rep(1, one_cutoff))
#       # if remaining days in timeline after NPI ends, turn off for remainder of time vector
#       if(exp_duration < rem_days){
#         npi_vec = c(npi_vec, rep(0, rem_days - exp_duration))
#       }
#     }
#     npi_vectors[, region_npis[i, 'policy_var']] = npi_vec * npi_wts[region_npis[i,'policy_long']]
#   }
#
# }
#
# model_df = b1_ts %>% select(date, time, b1) %>%
#   group_by(time) %>% summarise(mean_b1 = mean(b1)) %>%
#   mutate(date = time + input$start_date) %>%
#   ungroup() %>%
#   #left_join(select(npi_vectors, -date), by='time')
#
# # vary the lagtime between mobility and b1, and have model decide which is most significant
# diff_max = 14 # leading time of at most 2 weeks
# lag_df = all_mob_df %>% select(-time)
# for(i in c(1:diff_max)){
#   lag_df[,paste0('mob_lag_',i)] = lag(lag_df$mob, n=i, default=NA)
# }
# lag_df = lag_df %>% left_join(model_df[,c('date','mean_b1')], by='date')
# corr_df = as.data.frame(cor(drop_na(select(lag_df, -date, -period))))
# # use lag with highest correlation to b1
# best_match = rownames(corr_df)[which.max(head(corr_df$mean_b1, -1))]
# model_df = drop_na(model_df %>% left_join(lag_df[,c('date',best_match)], by='date'))
# lm_start_time = min(model_df$time)
#
#
# #beta_lm <- lm(mean_b1 ~ ., data=model_df %>% select(-date))
# lm1_pred_df = data.frame(time = c(1:length(beta_lm$fitted.values)) + lm_start_time,
#                          pred = beta_lm$fitted.values) %>%
#   mutate(date = time + (input$start_date-1))
#
# plot_df = model_df[,c('mean_b1', 'time', 'date', best_match)] %>% melt(id=c('time', 'date'))
# ggplotly(ggplot(plot_df,aes(x=date, y=value, color=variable)) + geom_line() +
#   geom_line(data=lm1_pred_df, aes(x=date, y=pred), color='darkgreen', size=2) +
#   ggtitle(paste0('Lagged mobility, Trans. Rates, and Modeled Trans. Rates for ', region_name)))

#===========B1 FORECASTING============#
# forecast mobility and gather predictions
# NOTE - 'mobility' forecasts will likely include real mobility data at the beginning since it is lagged to fit b1

# region_name = 'New York'
# b1_model_obj = build_b1_model(region_name, withTime=withTime)
# input = get_all_region_params(b1_model_obj$region)
forecast_b1 <- function(b1_model_obj, n=30, plt=F, add_init_offset=F, input=NULL){
  model_df = b1_model_obj$mod_df
  b1_model = b1_model_obj$b1_model
  sp_lag_df = b1_model_obj$sp_lag_df

  if(is.null(input)) input = get_all_region_params(b1_model_obj$region)

  # set time relative to SEIR start date, not mobility start date
  forecast_input_df = sp_lag_df[,c(b1_model_obj$best_lag, 'time', 'date')] %>% filter(date >= max(model_df$date)) #%>%
    #mutate(time = time + as.integer(min(sp_lag_df$date) - input$start_date))
  b1_forecasts = data.frame(predict.lm(b1_model, newdata=forecast_input_df, interval='confidence'))
  b1_forecast_df = cbind.data.frame(b1_forecasts,
                                    data.frame(date=forecast_input_df$date)#,
                                               # time = c(1:nrow(forecast_input_df)) + min(forecast_input_df$time) +
                                               #   as.integer(min(sp_lag_df$date) - input$start_date))
                                    ) %>%
    rename(b1 = fit, b1_lower = lwr, b1_upper = upr) %>%
    mutate(region=b1_model_obj$region,
           period=paste0('forecast', c(1:nrow(b1_forecasts)))) %>%
    select(region, date, period, b1, b1_lower, b1_upper)

  # combine forecasted b1s with actual b1s and generate new model df
  comb_b1_ts = MOVING_BETA_DATA %>% filter(region==b1_model_obj$region) %>% select(-time) %>% union_all(b1_forecast_df)
  comb_b1_ts = comb_b1_ts %>% mutate(time=as.integer(date - input$start_date))

  beta_df = gen_beta_df(beta_ts = comb_b1_ts)

  if(plt){
    fit_df = data.frame(
                        pred = b1_model_obj$fitted.values,
                        date = model_df$date)
    print(ggplot(data = b1_forecast_df, aes(x=date)) +
            geom_ribbon(aes(ymin=b1_lower, ymax=b1_upper), fill='grey70', alpha=.5) +
            geom_line(aes(x=date, y=b1), color='purple', linetype='dashed') +
            geom_line(data=fit_df,  aes(x=date, y=pred), color='blue') +
            geom_point(data=b1_model_obj$mod_df, aes(x=date, y=mean_b1), color='red')
          )
  }
  return(comb_b1_ts)
}

run_model_with_forecasts <- function(region_name, ndays=30, plt=F, input=NULL, withTime=T){
  # build b1 model (based on region's mobility)
  b1_model = build_b1_model(region_name, withTime=withTime)

  # generate projection curve
  forecasted_b1s  = forecast_b1(b1_model, n=ndays, input=input)
  middle_betas = gen_beta_df(region_name, beta_ts=forecasted_b1s)
  middle_res = gen_region_seir_df(region_name, b1_timelines=middle_betas)
  #na_end = forecasted_b1s[max(which(is.na(forecasted_b1s$b1_lower))),'time']
  na_end = which.max(which(is.na(forecasted_b1s$b1_lower)))

  # generate lower bounds of confidence interval
  forecasted_b1s$b1 = c(forecasted_b1s$b1[c(1:na_end)], forecasted_b1s$b1_lower[c((na_end+1):nrow(forecasted_b1s))])
  lower_betas = gen_beta_df(region_name, beta_ts=forecasted_b1s)
  lower_res = gen_region_seir_df(region_name, b1_timelines=lower_betas) %>% select(-period, -date, -time)
  colnames(lower_res) <- paste0(colnames(lower_res), '_lower')

  # generate upper bounds of confidence interval
  forecasted_b1s$b1 = c(forecasted_b1s$b1[c(1:na_end)], forecasted_b1s$b1_upper[c((na_end+1):nrow(forecasted_b1s))])
  upper_betas = gen_beta_df(region_name, beta_ts=forecasted_b1s)
  upper_res = gen_region_seir_df(region_name, b1_timelines=upper_betas) %>% select(-period, -date, -time)
  colnames(upper_res) <- paste0(colnames(upper_res), '_upper')

  comb_df = cbind.data.frame(middle_res, lower_res, upper_res) %>% mutate(region=region_name)
  return(comb_df)
}

# DIAGNOSTICS
#===========NEW APPROACH================#
# region_list = setdiff(STATE_NAMES$full, c("District of Columbia"))
# region_plts = list()
#
# for(region_name in region_list){
#   region_mob = MOBILITY_DATA %>% filter(mState.Providence==region_name) %>%
#     mutate(date = as.Date(date))
#   region_plts[[region_name]] = ggplot(region_mob, aes(x=date, y=percent_off_norm_maxMobility)) +
#     geom_line() +
#     labs(x='', y='Mobility') +
#     ggtitle(paste0(state_lookup(region_name))) +
#     theme(plot.title = element_text(hjust = 0.5))
# }
# plot_grid(plotlist=region_plts, nrow = 7)#floor(sqrt(length(region_plts))))
#
# region_name = 'Connecticut'

# pull in all necessary data
# region_betas = gen_beta_df(region_name, beta_ts=MOVING_BETA_DATA)
# region_ts = get_region_plot_ts(region_name, metric='Deaths')
# region_seir = gen_region_seir_df(region_name, b1_timelines=region_betas, trim_beta_days=7)
# region_list = setdiff(STATE_NAMES$full, c("District of Columbia"))[sample(c(1:50), size=10, replace=F)]
# region_plts = list()
# for(region_name in region_list){
#   region_mob = MOBILITY_DATA %>% filter(mState.Providence==region_name) %>%
#     mutate(date = as.Date(date))
#   ts = region_mob$percent_off_norm_maxMobility
#   spline_res = smooth.spline(x=1:length(ts), y=ts, spar=smooth_amt)
#   ts = spline_res$y
#   model_df = data.frame(y=ts, time=c(1:length(ts))) %>%
#     mutate(d1 = y - lag(y, n=1, default=NA),
#            d2 = d1 - lag(d1, n=1, default=NA))
#
#   # account for trend
#   w=20
#   vars = sapply(1:(nrow(model_df)-w), function(i){sqrt(var(model_df$y[i:(i+w)], na.rm=T))})
#   var_df = data.frame(time=1:(nrow(model_df)-w), var=vars)
#   region_plts[[region_name]] = ggplot(var_df, aes(x=time, y=var)) + geom_line() +
#     geom_hline(yintercept = quantile(vars, p=.10, na.rm=T), linetype='dotdash') +
#     geom_hline(yintercept = quantile(model_df$y, p=.10, na.rm=T), linetype='dashed', color='red') +
#     geom_line(data=model_df, aes(x=time, y=y), color='red') +
#     ggtitle(state_lookup(region_name)) +
#     labs(y='Value', x='Time') +
#     theme(plot.title = element_text(hjust = 0.5))
#
# }
#
# plot_grid(plotlist=region_plts, nrow = 2)#floor(sqrt(length(region_plts))))

# region_plts = list()
# region_list = setdiff(STATE_NAMES$full, c("District of Columbia"))[sample(c(1:50), size=10, replace=F)]
#
# for(region_name in region_list){
#   model_obj = build_mob_spline_lm(region_name, n=30, deg=1)
#   forecast_res = forecast_mobility(model_obj, n=30)
#   fit_df = forecast_res %>% filter(period=='fit')
#   forecast_df = forecast_res %>% filter(period=='forecast')
#   region_plts[[region_name]] = ggplot(fit_df, aes(x=date, y=y)) +
#     geom_line(color='blue') +
#     geom_point(data=model_obj$all_mob_data, aes(x=date, y=mob_smooth), color='red') +
#     geom_line(data=forecast_df, aes(x=date, y=y), color='purple',
#               linetype='dashed')
# }
#
#
# for(region_name in region_list){
#   region_mob = MOBILITY_DATA %>% filter(mState.Providence==region_name) %>%
#     mutate(date = as.Date(date), ts=percent_off_norm_maxMobility)
#   region_mob$dow = format(region_mob$date, format='%A')
#   region_plts[[region_name]] = ggplot(region_mob, aes(x=date, y=ts, color=as.factor(dow))) + geom_line() +
#     ggtitle(paste0('Mobility by DOW for ', state_lookup(region_name))) +
#     labs(x='Date', y='Mobility (% of pre-covid norm. mobility)', color='Day of Week') +
#     theme(plot.title = element_text(hjust = 0.5))
# }
# plot_grid(plotlist=region_plts, nrow=2)
#================#
