## Mortality Analysis

draw_mortality <- function(forecast_plot_choices) {
  
  national_dat <- COUNTRY_INF_STATS %>% filter(Country.Region == 'United States') %>% select(date, cases = case_count, deaths = death_count) %>%
    mutate(week = floor_date(date, unit = "week", week_start = 7)) %>% 
    group_by(week) %>%
    summarise(cases = max(cases), deaths = max(deaths)) %>% 
    ungroup()
  
  cases_update_date = max(as.Date(CDC_AGE$cases_update_date))
  
  cdc_age_large_groups <- CDC_AGE %>% 
    mutate(
      age_group_large = ifelse(age_group_small %in% c("0-4 Years", "5-9 Years"), "0-9 Years", ifelse(
        age_group_small %in% c("10-14 Years","15-19 Years"), "10-19 Years", ifelse(
          age_group_small %in% c("20-24 Years", "25-29 Years"), "20-29 Years", ifelse(
            age_group_small %in% c("30-34 Years", "35-39 Years"), "30-39 Years", ifelse(
              age_group_small %in% c("40-44 Years", "45-49 Years"), "40-49 Years", ifelse(
                age_group_small %in% c("50-54 Years", "55-59 Years"), "50-59 Years", ifelse(
                  age_group_small %in% c("60-64 Years", "65-69 Years"), "60-69 Years", ifelse(
                    age_group_small %in% c("70-74 Years", "75-79 Years"), "70-79 Years", "80+ Years"
          ))))))))
    ) %>% group_by(age_group_large, week) %>%
    summarise(pct_cases = sum(pct_cases), pct_deaths = sum(pct_deaths), pct_cases_raw = sum(pct_cases_raw), pct_deaths_raw = sum(pct_deaths_raw))
  
  if (TRUE) {
    plot1 <- plot_ly(type = "scatter", mode = "markers")
    if ("cases.age" %in% forecast_plot_choices) {
      plot1 <- plot1 %>% 
        add_trace(x=cdc_age_large_groups$week[cdc_age_large_groups$week <= cases_update_date],
                  y=cdc_age_large_groups$pct_cases_raw[cdc_age_large_groups$week <= cases_update_date],
                  name = cdc_age_large_groups$age_group_large[cdc_age_large_groups$week <= cases_update_date],
                  type = 'scatter',
                  mode='lines')
      plt_title = 'Percentage of all COVID-19 Cases by Age Group'
      y_axis = 'Percent of All Cases (%)'
      
      plot1 <- plot1 %>%
        layout(title=plt_title,
               xaxis=list(title = ""),
               yaxis=list(title = y_axis, range=c(0,0.3), tickformat = "%"),
               margin = list(t=42,b=57, pad=3),
               shapes = list(
                 list(type = "rect",
                      fillcolor = "orange", line = list(color = "orange"), opacity = 0.2,
                      x0 = as.character(cases_update_date - 3*7), x1 = as.character(cases_update_date + 7), xref = "x",
                      y0 = 0, y1 = .3, yref = "y")),
               annotations = list(
                 list(x = as.character(cases_update_date - 7), y = 0.28, showarrow=FALSE,
                      text = "Data may be less\nreliable due to CDC\nprocessing time",
                      xref = "x", yref = "y")
               )
         )
    }
    if ("deaths.age" %in% forecast_plot_choices) {
      plot1 <- plot1 %>%
        add_trace(x=cdc_age_large_groups$week,
                  y=cdc_age_large_groups$pct_deaths_raw,
                  name = cdc_age_large_groups$age_group_large,
                  type = 'scatter',
                  mode='lines') 
      plt_title = 'Percentage of all COVID-19 Deaths by Age Group'
      y_axis = 'Percent of All Deaths (%)'
      
      plot1 <- plot1 %>%
        layout(title=plt_title,
               xaxis=list(title = ""),
               yaxis=list(title = y_axis, tickformat = "%", range=c(0,0.55)),
               margin = list(t=42,b=57, pad=1),
               shapes = list(
                 list(type = "rect",
                      fillcolor = "orange", line = list(color = "orange"), opacity = 0.2,
                      x0 = as.character(max(as.Date(cdc_age_large_groups$week)) - 3*7), x1 = as.character(max(as.Date(cdc_age_large_groups$week)) + 7), xref = "x",
                      y0 = 0, y1 = .55, yref = "y")),
               annotations = list(
                 list(x = as.character(max(as.Date(cdc_age_large_groups$week)) - 7), y = 0.50, showarrow=FALSE,
                      text = "Data may be less\nreliable due to CDC\nprocessing time",
                      xref = "x", yref = "y")
               )
         )
    }
    if ("mort.age" %in% forecast_plot_choices) {
      apply_ratios_cdc_large = cdc_age_large_groups %>% 
        mutate(week = as.Date(week)) %>%
        left_join(national_dat) %>% 
        group_by(age_group_large) %>%
        mutate(
          n_cases_new = (cases-lag(cases))*pct_cases,
          n_deaths_new = (deaths-lag(deaths))*pct_deaths
        ) %>%
        mutate(
          mortality21 = round(n_deaths_new/lag(n_cases_new,3),3)
        )
      
      plot1 <- plot1 %>%
        add_trace(x=apply_ratios_cdc_large$week,
                  y=apply_ratios_cdc_large$mortality21,
                  name = apply_ratios_cdc_large$age_group_large,
                  type = 'scatter',
                  mode='lines') 
      plt_title = 'Mortality Rate by Age Group with 3-Week Lag'
      y_axis = 'Mortality Rate, Assuming 3-Week Lag (%)'
      
      plot1 <- plot1 %>%
        layout(title=plt_title,
               xaxis=list(title = ""),
               yaxis=list(title = y_axis, range=c(0,0.35), tickformat = "%"),
               margin = list(t=42,b=57, pad=1),
               shapes = list(
                 list(type = "rect",
                      fillcolor = "orange", line = list(color = "orange"), opacity = 0.2,
                      x0 = as.character(cases_update_date - 2*7), x1 = as.character(max(as.Date(apply_ratios_cdc_large$week)) + 7), xref = "x",
                      y0 = 0, y1 = .35, yref = "y"),
                 list(type = "rect",
                      fillcolor = "red", line = list(color = "red"), opacity = 0.2,
                      x0 = '2020-04-12', x1 = '2020-05-17', xref = "x",
                      y0 = 0, y1 = .35, yref = "y")
                 ),
               annotations = list(
                 list(x = as.character(cases_update_date + floor(max(as.Date(apply_ratios_cdc_large$week)) + 7 - cases_update_date - 2*7) / 2), y = 0.3, showarrow=FALSE,
                      text = paste0("Estimates may be less\nreliable due to CDC\nprocessing time and\nextrapolated case distributions\nafter ", 
                                    format(cases_update_date, format = '%B %d')),
                      xref = "x", yref = "y"),
                 list(x = '2020-04-30', y = 0.3, showarrow=FALSE,
                      text = "Calculated mortality rate\nis not representative of\nactual rate due to\nlimited testing",
                      xref = "x", yref = "y")
               )
        )
    }
    if ("testing" %in% forecast_plot_choices) {
      testing_data = DAILY_STATE_TESTING %>% 
        group_by(date) %>%
        summarise(tot_tests = sum(tot_positive, na.rm = T) + sum (tot_negative, na.rm = T), 
                  tot_positive = sum(tot_positive, na.rm = T)) %>%
        mutate(pct_pos_tot = tot_positive / tot_tests, 
               new_tests = (tot_tests - lag(tot_tests,7))/7, 
               new_pos = (tot_positive - lag(tot_positive,7))/7, 
               pct_pos_new = new_pos/new_tests
        ) %>%
        filter(date >= as.Date("2020/03/10"))
      
      p1 <-  plot_ly(testing_data, x=~date,y=~(new_tests - new_pos), name = 'Negative Tests', type = "scatter",                   
                     mode = 'none', stackgroup = 'one', fillcolor = '#67a9cf') %>%
        add_trace(x=~date,
                  y=~new_pos,
                  type = 'scatter',
                  name = 'Positive Tests', 
                  fillcolor = '#ef8a62') %>%
        layout(title='COVID-19 Testing',
               xaxis=list(title = ""),
               yaxis=list(title = 'Number of COVID-19 Tests (Daily)'),
               margin = list(t=42,b=57, pad=1))
      
      p2 <- plot1 %>%
        add_trace(x=testing_data$date,
                  y=testing_data$pct_pos_new,
                  type = 'scatter',
                  mode='lines', 
                  name = 'Daily Test Postivity (%)', 
                  line = list(color = '#ef8a62', width = 2)) %>%
        layout(title='',
               xaxis=list(title = ""),
               yaxis=list(title = 'Test Postivity (%)', range=c(0,0.25), tickformat = "%"),
               margin = list(t=42,b=57, pad=1))
      
      plot1 <- plotly::subplot(p1,p2, nrows = 2, shareX = TRUE, titleY = TRUE)
    }
  } 
  
  return(plot1)
}
