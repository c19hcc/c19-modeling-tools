##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: Support functions for Hotspot Identification tab.
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

plot_hotspots_data <- function(plot_data = 'current', forecast_date = NULL){
  if (plot_data == 'forecast') {
    req(forecast_date)
    dat = FORECAST_DATA
    
    dat <- dat %>%
      arrange(countyFIPS, date) %>%
      left_join(countypop %>% select(fips, population = pop_2015), by = c("countyFIPS" = "fips")) %>%
      mutate(
        fips = countyFIPS,
        y_pred_per100k = y_pred/population*100000
      ) %>%
      group_by(countyFIPS) %>%
      mutate(
        new_cases = (y_pred_per100k - lag(y_pred_per100k, n = 1, default = 0))/1,
        new_cases_2weeks_raw = (y_pred - lag(y_pred, n = 14, default = 0)),
        new_cases_2weeks = (y_pred_per100k - lag(y_pred_per100k, n = 14, default = 0)),
        avg_daily_cases_week = (y_pred - lag(y_pred, n = 7, default = 0))/7, #7 day avg
        avg_daily_cases_prev_week = (lag(y_pred, n = 8, default = 0) - lag(y_pred, n = 14, default = 0))/7 #7 day avg
      ) %>% 
      mutate(
        change_new_cases = new_cases - lag(new_cases, n = 1, default = 0),
        change_new_cases_pct = (new_cases - lag(new_cases, n = 1, default = 0))/lag(new_cases, n = 1, default = 0),
        week_change_pct = (avg_daily_cases_week - avg_daily_cases_prev_week)/avg_daily_cases_prev_week
      )  %>%
      mutate(
        week_change_pct = ifelse(avg_daily_cases_prev_week == 0, NA, week_change_pct)
      ) 
    
    dat <- dat %>%
      inner_join(
        dat %>% filter(date > (forecast_date - 7), !is.na(new_cases)) %>%
          do(mod = lm(new_cases~date, data=.)) %>%
          mutate(slope = suppressWarnings(summary(mod))$coeff[2]) %>%
          select(-mod)
      ) %>% 
      mutate(change_new_cases = slope)
    
    dat <- dat %>% ungroup %>%
      filter(date == forecast_date) %>% 
      mutate(text_tip = paste0(
        "<b>",paste0(County.Name, ", ", State),'</b>',
        "<br><b>Population</b>: ", population,
        "<br><b>Predicted Cases</b>: ", round(y_pred,2),
        "<br><b>Predicted Cases per 100k Inhabitants</b>: ", round(y_pred_per100k,2),
        "<br><b>Daily Predicted Cases per 100k Inhabitants (7-day avg.)</b>: ", round(new_cases,2),
        "<br><b>Change in Daily Predicted Cases per 100k Inhabitants</b>: ", round(change_new_cases,2),
        "<br><b>Number of Total New Predicted Cases in the previous 2 Weeks (raw)</b>: ", new_cases_2weeks_raw,
        "<br><b>Number of Total New Predicted Cases in the previous 2 Weeks per 100k Inhabitants</b>: ", round(new_cases_2weeks),
        "<br><b>Percent Change in Average Daily Cases from Prior Week (%)</b>: ", paste0(round(week_change_pct*100,2), '%')
        )
      )
  } else {
    dat <- COUNTY_INF_STATS %>% 
      filter(date > (max(date, na.rm = T) - 21)) %>%
      inner_join(COUNTY_CLEANUP %>% mutate(mState.Providence = paste0(County.Pretty, ", ", abbr)) %>% select(mState.Providence, fips)) %>%
      mutate(fips = str_pad(fips, 5, '0', side = c('left'))) %>%
      left_join(countypop %>% select(fips, population = pop_2015), by = c("fips")) %>%
      mutate(
        y_pred_per100k = case_count/population*100000
      ) %>%
      group_by(fips) %>%
      mutate(
        date = as.Date(date),
        new_cases = (y_pred_per100k - lag(y_pred_per100k, n = 7, default = 0))/7,
        new_cases_2weeks_raw = (case_count - lag(case_count, n = 14, default = 0)),
        new_cases_2weeks = (y_pred_per100k - lag(y_pred_per100k, n = 14, default = 0)),
        avg_daily_cases_week = (case_count - lag(case_count, n = 7, default = 0))/7, #7 day avg
        avg_daily_cases_prev_week = (lag(case_count, n = 8, default = 0) - lag(case_count, n = 14, default = 0))/7 #7 day avg
      ) %>% 
      mutate(
        change_new_cases = new_cases - lag(new_cases, n = 1, default = 0),
        change_new_cases_pct = (new_cases - lag(new_cases, n = 1, default = 0))/lag(new_cases, n = 1, default = 0),
        week_change_pct = (avg_daily_cases_week - avg_daily_cases_prev_week)/avg_daily_cases_prev_week
      ) %>%
      mutate(
        week_change_pct = ifelse(avg_daily_cases_prev_week == 0, NA, week_change_pct)
      ) 
    
    dat <- dat %>%
      inner_join(
        dat %>% filter(date > (max(date, na.rm = T) - 7), !is.na(new_cases)) %>%
          do(mod = lm(new_cases~date, data=.)) %>%
          mutate(slope = suppressWarnings(summary(mod))$coeff[2]) %>%
          select(-mod)
      ) %>% 
      mutate(change_new_cases = slope)
    
    dat <- dat %>%
      filter(date == max(date, na.rm = T)) %>%
      mutate(text_tip = paste0(
        "<b>",mState.Providence,'</b>',
        "<br><b>Population</b>: ", population,
        "<br><b>Cases</b>: ", round(case_count,2) ,
        "<br><b>Deaths</b>: ", round(death_count,2) ,
        "<br><b>Cases per 100k Inhabitants</b>: ", round(y_pred_per100k,2),
        "<br><b>Daily Cases per 100k Inhabitants (7-day avg.)</b>: ", round(new_cases,2),
        "<br><b>Trend in Change in Daily Cases per 100k Inhabitants</b>: ", round(change_new_cases,2),
        "<br><b>Number of Total New Cases in the past 2 Weeks (raw)</b>: ", new_cases_2weeks_raw,
        "<br><b>Number of Total New Cases in the past 2 Weeks per 100k Inhabitants</b>: ", round(new_cases_2weeks),
        "<br><b>Percent Change in Average Daily Cases from Prior Week (%)</b>: ", paste0(round(week_change_pct*100,2), '%')
      ))
  }
  return(dat)
}


plot_hotspots <- function(dat, hotspot_plot_options = 'cases.100', forecast_date = NULL){

  data_map <- c("cases.raw" = "new_cases_2weeks_raw",
                "cases.100" = "new_cases_2weeks",
                "cases.change" = "change_new_cases",
                "change.week" = "week_change_pct")
  
  dat$plot_data <- dat[,data_map[[hotspot_plot_options]]]
  
  if (hotspot_plot_options == "cases.change") {
    dat <-  dat %>%
      mutate(plot_data_cat = factor(sapply(plot_data, function(x){ifelse(x <= -.1, '< -0.1', 
                                                                         ifelse(x <= 0.1, '-0.1 to 0.1',
                                                                                ifelse(x <= 1, '0.1 to 1', 
                                                                                       ifelse(x <= 10, '1 to 10', 
                                                                                              '> 10'))))}),
                                    levels = c('< -0.1', '-0.1 to 0.1', '0.1 to 1', '1 to 10', '> 10'))
      ) %>% select(fips, plot_data_cat, text_tip)
    colorlist = c('#aec1d6', '#EBF5FB', '#ffb6a8', '#f8626d', '#cb0030')
  }else if (hotspot_plot_options == "cases.100") {
    dat <-  dat %>%
      mutate(plot_data_cat = factor(sapply(plot_data, function(x){ifelse(x <= 0, '0', 
                                                                         ifelse(x <= 10, '>0 to 10',
                                                                                ifelse(x <= 50, '>10 to 50', 
                                                                                       ifelse(x <= 50, '>50 to 100', 
                                                                                              '> 100'))))}),
                                    levels = c('0', '>0 to 10', '>10 to 50', '50 to 100', '> 100'))
      ) %>% select(fips, plot_data_cat, text_tip)
    colorlist = c('#aec1d6', '#EBF5FB', '#ffb6a8', '#f8626d', '#cb0030')
  } else if (hotspot_plot_options == "cases.raw") {
    dat <-  dat %>%
      mutate(plot_data_cat = factor(sapply(plot_data, function(x){ifelse(x <= 0, '0', 
                                                                         ifelse(x <= 10, '>0 to 10',
                                                                                ifelse(x <= 50, '>10 to 50', 
                                                                                       ifelse(x <= 50, '>50 to 100', 
                                                                                              '> 100'))))}),
                                    levels = c('0', '>0 to 10', '>10 to 50', '50 to 100', '> 100'))
      ) %>% select(fips, plot_data_cat, text_tip)
    colorlist = c('#aec1d6', '#EBF5FB', '#ffb6a8', '#f8626d', '#cb0030')
  } else if (hotspot_plot_options == "change.week") {
    dat <-  dat %>%
      mutate(plot_data_cat = factor(sapply(plot_data, function(x){ifelse(x == -1, 'No new cases this week',
                                                                         ifelse(is.na(x), 'No new cases last week',
                                                                              ifelse(x <= -1, 'More than 100% decrease', 
                                                                                   ifelse(x <= -0.1, '10 to 100% decrease',
                                                                                          ifelse(x <= 0, '0 to 10% decrease', 
                                                                                                 ifelse(x <= 0.1, '0 to 10% increase', 
                                                                                                        ifelse(x <= 0.5, '10 to 50% increase', 
                                                                                                               ifelse(x <= 1, '50 to 100% increase', 
                                                                                              'More than 100% increase'))))))))}),
                                    levels = c('No new cases this week','No new cases last week','More than 100% decrease', '10 to 100% decrease',
                                               '0 to 10% decrease', '0 to 10% increase', '10 to 50% increase', '50 to 100% increase', 'More than 100% increase')),
             plot_data_cat = factor(if_else(is.na(plot_data_cat), 'No new cases last week', as.character(plot_data_cat)),
                                    levels = c('No new cases this week','No new cases last week','More than 100% decrease', '10 to 100% decrease',
                                               '0 to 10% decrease', '0 to 10% increase', '10 to 50% increase', '50 to 100% increase', 'More than 100% increase'))
      ) %>% select(fips, plot_data_cat, text_tip)
    colorlist = c('#aec1d6', '#bdcedf', '#ccdbe8', '#dce8f2', '#ebf5fb', '#fb9089', '#f1696b', '#e0414d', '#cb0030')
  }
 
  dat <- suppressWarnings(left_join(spdf_counties, dat))
  dat$text_tip[is.na(dat$text_tip)] = paste0("<b>",dat[is.na(dat$text_tip),]$name, " ", 
                                             dat[is.na(dat$text_tip),]$lsad, ", ", 
                                             dat[is.na(dat$text_tip),]$iso_3166_2,
                                             "</b><br>", "No Data Available")
  
  pal <- colorFactor(colorlist, domain = dat$plot_data_cat, na.color = '#ffffff')
  labels <- dat$text_tip %>% lapply(htmltools::HTML)
  
  #bounds <- c(-118, 17 ,-63, 50)
  coord = as.data.frame(st_coordinates((spdf_us)$geometry))
  bounds <- c(min(coord$X), min(coord$Y), max(coord$X), max(coord$Y)) 
  bounds[3] <- bounds[3] + abs(bounds[1] - bounds[3])*.2
  bounds[2] = 13
  
  if (hotspot_plot_options == "cases.change") {
    legend.txt <- "Change in<br>New Daily Cases<br>per 100k Inhabitants"
  } else if (hotspot_plot_options == "cases.100") {
    legend.txt <- "Total New Cases<br>in Previous 2 Weeks<br>per 100k Inhabitants"
  } else if (hotspot_plot_options == "cases.raw") {
    legend.txt <- "Total New Cases<br>in Previous 2 Weeks"
  } else if (hotspot_plot_options == "change.week") {
    legend.txt <- "Percent Change in<br>Average Daily Cases<br>from Prior Week"
  }
  
  fig <- leaflet(dat, options = leafletOptions(crs = EPSG2163, minZoom = 15, zoomSnap = 0, zoomControl = FALSE), height = '700px') %>%
    addPolygons(weight = 0.25, color = "#999999", opacity = 1,
                fillColor = ~pal(plot_data_cat),
                fillOpacity = 1, smoothFactor = 0.5, 
                label = labels,
                labelOptions = labelOptions(direction = "auto")) %>%
    addLegend("bottomright", pal = pal, values = ~plot_data_cat,
              title = legend.txt,
              opacity = 1, na.label = "No Data") %>%
    setView(lng = -95, lat = 30, zoom = 1.5) %>%
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4], options = list(padding = c(50,50))) %>% 
    setMaxBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    addResetMapButton()
  
  return(fig)
}


