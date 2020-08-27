##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: Functions for scatterplot comparison tab.
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

SAH_mobility_scatterplot_data <- function(){

  mobility_update_date = as.Date(max(MOBILITY_DATA$date, na.rm = TRUE))
  data_update_date = as.Date(max(STATE_INF_STATS$date, na.rm = TRUE))
  
  ret.df <- STATE_INF_STATS %>%
    filter(Country.Region == "United States") %>% 
    left_join(MOBILITY_DATA %>%
                mutate(date = as.Date(date)) %>%
                select(date, mobility = percent_off_norm_maxMobility, mState.Providence, Country.Region),
              by = c("mState.Providence", "Country.Region", "date")) %>% 
    left_join(WORLD_POP, by=c("mState.Providence", "Country.Region")) %>%
    left_join(
      NPI_DATA %>%
        filter(policy_long == "Stay At Home Order") %>%
        select(mState.Providence = state, start_date, end_date) %>%
        group_by(mState.Providence) %>%
        summarise(firstSAHDate = as.Date(min(start_date, na.rm=FALSE)), lastSAHDate = as.Date(max(end_date, na.rm = FALSE))) %>%
        mutate(
          SAHdays = ifelse(
            is.na(firstSAHDate) | firstSAHDate == Inf | firstSAHDate == -Inf, 
            0, 
            ifelse(
              is.na(lastSAHDate) | lastSAHDate == Inf | lastSAHDate == -Inf, 
              as.numeric(data_update_date - as.Date(firstSAHDate)), 
              as.numeric(as.Date(lastSAHDate) - as.Date(firstSAHDate))
              )
            )
          ) %>%
        ungroup() %>%
        distinct() %>%
        select(mState.Providence, SAHdays, firstSAHDate, lastSAHDate),
      by = c("mState.Providence")
    ) %>% 
    left_join(
      DAILY_STATE_TESTING %>%
        mutate(date = as.Date(date),
               all_daily_tests = daily_positive + daily_negative,
               all_tot_tests = tot_positive + tot_negative) %>%
        mutate(daily_positive = ifelse(daily_positive < 0, 0, daily_positive),
               all_daily_tests = ifelse(all_daily_tests < 0, 0, all_daily_tests)) %>%
        select(State = state, date, tot_positive, daily_positive, all_daily_tests, all_tot_tests),
      by = c("State", "date")
    ) %>%
    select(state = mState.Providence, state_abbr = State, date, SAHdays, firstSAHDate, lastSAHDate, case_count, death_count, mobility, population, 
           tot_positive, daily_positive, all_daily_tests, all_tot_tests)
    
  ret.df <- ret.df %>%
    group_by(state) %>%
    mutate(
      avg_daily_cases_week = (case_count - lag(case_count, n = 7, default = 0))/7, #7 day avg
      avg_daily_cases_prev_week = (lag(case_count, n = 8, default = 0) - lag(case_count, n = 14, default = 0))/7, #7 day avg
      
      num_pos_tests_week = daily_positive + lag(daily_positive, n=1, default = 0) + lag(daily_positive, n=2, default = 0) + 
        lag(daily_positive, n=3, default = 0) + lag(daily_positive, n=4, default = 0) + lag(daily_positive, n=5, default = 0) + lag(daily_positive, n=6, default = 0), 
      num_all_tests_week = all_daily_tests + lag(all_daily_tests, n=1, default = 0) + lag(all_daily_tests, n=2, default = 0) + 
        lag(all_daily_tests, n=3, default = 0) + lag(all_daily_tests, n=4, default = 0) + lag(all_daily_tests, n=5, default = 0) + lag(all_daily_tests, n=6, default = 0),
      
      mobility_pre = ifelse(date<firstSAHDate, mobility, NA),
      mobility_during = ifelse(date>=firstSAHDate & date <= min(c(lastSAHDate,data_update_date), na.rm = T), mobility, NA),
      mobility_post = ifelse(date>lastSAHDate, mobility, NA),
      mobility_lastweekupdated = ifelse(date>(mobility_update_date - 7), mobility, NA)
    ) %>% 
    mutate(
      avg_pos_tests_week = num_pos_tests_week/7, #7 day avg
      avg_all_tests_week = num_all_tests_week/7, #7 day avg
    )
  
  ret.df <- ret.df %>%
    summarise(
      tot_mobility_change = sum(mobility, na.rm = TRUE),
      avg_mobility_change = mean(mobility, na.rm = TRUE),
      avg_mobility_pre = mean(mobility_pre, na.rm = TRUE),
      avg_mobility_during = mean(mobility_during, na.rm = TRUE),
      avg_mobility_post = mean(mobility_post, na.rm = TRUE),
      avg_mobility_lastweekupdated = mean(mobility_lastweekupdated, na.rm = TRUE)
    ) %>%
    inner_join(ret.df, by='state') %>% 
    ungroup()
  
  ret.df <- ret.df %>% 
    mutate(
      cum_pos_pct = tot_positive / all_tot_tests,
      cum_cases_per100k = case_count / population * 100000,
      cum_deaths_per100k = death_count / population * 100000,
      avg_daily_cases_week_per100k = avg_daily_cases_week / population * 100000,
      avg_daily_weekly_change = avg_daily_cases_week/avg_daily_cases_prev_week - 1,
      week_pct_pos = num_pos_tests_week / num_all_tests_week, 
      avg_all_tests_week_per100k = avg_all_tests_week / population * 100000,
      cum_tests_per100k = all_tot_tests / population * 100000
    ) %>%
    select(state, state_abbr, date, firstSAHDate, lastSAHDate, SAHdays, cum_pos_pct,
           tot_mobility_change, avg_mobility_change, avg_mobility_pre, avg_mobility_during, avg_mobility_post, avg_mobility_lastweekupdated,
           cum_cases_per100k, cum_deaths_per100k, avg_daily_cases_week_per100k, avg_daily_weekly_change,
           week_pct_pos, avg_all_tests_week_per100k, cum_tests_per100k
           )
  
  ret.df <- ret.df %>% filter(date == max(date)) %>%
    left_join(STATE_DEMOGRAPHIC) %>% mutate(pct65over = pct65over/100)
  
  return(ret.df)
  
}

SAH_mobility_scatterplot <- function(selected_x, selected_y, dat){
  
  label.key <- c('SAHdays' = 'Total Days under Stay at Home Order',
                 "cum_pos_pct" = "Cumulative Test Positivity (%)",
                 'tot_mobility_change' = "Cumulative Change in Mobility", 
                 'avg_mobility_change' = "Avg. Change in Mobility", 
                 'avg_mobility_pre' = "Avg. Change in Mobility Before Stay at Home", 
                 'avg_mobility_during' = "Avg. Change in Mobility During Stay at Home", 
                 'avg_mobility_post' = "Avg. Change in Mobility After Stay at Home", 
                 'avg_mobility_lastweekupdated' = "Avg. Change in Mobility Last Week",
                 'cum_cases_per100k' = "Cumulative Cases per 100,000", 
                 'cum_deaths_per100k' = "Cumulative Deaths per 100,000", 
                 'avg_daily_cases_week_per100k' = "Avg. Daily Cases per 100,000  (Prev. 7 days)", 
                 'avg_daily_weekly_change' = "Change in Avg. Daily Cases per 100,000 from Last Week (%)",
                 'week_pct_pos' = "Avg. Test Positivity (%) (Prev. 7 days)", 
                 'avg_all_tests_week_per100k' = "Avg. Daily Tests per 100,000 (Prev. 7 days)", 
                 'cum_tests_per100k' = "Cumulative Tests per 100,000", 
                 'density' = "Population Density (people/sq. mile)",
                 'pct65over' = "Percent of Population over Age 65",
                 'medIncome' = "Median Household Income", 
                 'bedsPer1k' = "Hospital Beds per 1000 Inhabitants")
  
  if ((selected_y %in% c('avg_daily_weekly_change', 'week_pct_pos')) & (selected_x %in% c('cum_pos_pct'))) {
    dat$trace.hovertext = paste0("<b>", dat$state,
                                 "</b><br>","<i>", "Stay at Home Start Date: ", dat$firstSAHDate, 
                                 "</b><br>","<i>", "Stay at Home End Date: ", dat$lastSAHDate, 
                                 "</b><br>","<i>", label.key[selected_x], ": ", round(dat[[selected_x]],2)*100, "%", 
                                 "</b><br>","<i>", label.key[selected_y], ": ", round(dat[[selected_y]],2)*100, "%")
  } else if (selected_y %in% c('avg_daily_weekly_change', 'week_pct_pos')) {
    dat$trace.hovertext = paste0("<b>", dat$state,
                                 "</b><br>","<i>", "Stay at Home Start Date: ", dat$firstSAHDate, 
                                 "</b><br>","<i>", "Stay at Home End Date: ", dat$lastSAHDate, 
                                 "</b><br>","<i>", label.key[selected_x], ": ", dat[[selected_x]], 
                                 "</b><br>","<i>", label.key[selected_y], ": ", round(dat[[selected_y]],2)*100, "%")
  } else if (selected_x %in% c('cum_pos_pct', 'pct65over')) {
    dat$trace.hovertext = paste0("<b>", dat$state,
                                 "</b><br>","<i>", "Stay at Home Start Date: ", dat$firstSAHDate, 
                                 "</b><br>","<i>", "Stay at Home End Date: ", dat$lastSAHDate, 
                                 "</b><br>","<i>", label.key[selected_x], ": ", round(dat[[selected_x]],2)*100, "%",
                                 "</b><br>","<i>", label.key[selected_y], ": ", round(dat[[selected_y]],2) )
  } else {
    dat$trace.hovertext = paste0("<b>", dat$state,
                                 "</b><br>","<i>", "Stay at Home Start Date: ", dat$firstSAHDate, 
                                 "</b><br>","<i>", "Stay at Home End Date: ", dat$lastSAHDate, 
                                 "</b><br>","<i>", label.key[selected_x], ": ", dat[[selected_x]], 
                                 "</b><br>","<i>", label.key[selected_y], ": ", round(dat[[selected_y]],2) )
  }
  
  ret.plt <- plot_ly(type = 'scatter', mode = 'markers+text') %>%
    add_trace(x = dat[[selected_x]], y = dat[[selected_y]], text = dat$state_abbr,
              hovertext = dat$trace.hovertext, hoverinfo="text",
              textposition = "middle right") %>% 
    layout(title = paste0("State Comparisons: ",label.key[selected_y], " vs.<br>", label.key[selected_x]),
           xaxis = list(title = label.key[selected_x]), yaxis = list(title = label.key[selected_y]), 
           showlegend = FALSE, margin=list(t=42,b=57, pad=3))
  
  if (selected_y %in% c('avg_daily_weekly_change', 'week_pct_pos')) {ret.plt <- ret.plt %>% layout(yaxis = list(tickformat = "%"))}
  if (selected_x %in% c('cum_pos_pct', 'pct65over')) {ret.plt <- ret.plt %>% layout(xaxis = list(tickformat = "%"))}
  
  return(ret.plt)
}
