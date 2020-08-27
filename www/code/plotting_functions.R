##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: Map plotting functions for region data tab.
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

library(maps)
library(ggstance)
library(ggplot2)
library(scales)
library(car)
library(dplyr)
library(RColorBrewer)
library(mapproj)

# converts a count column to categories suitable for geospatial plotting
numeric_to_factor <- function(df, ct_col, cat_groups=NULL){
  df <- data.frame(df)
  if(is.null(cat_groups)) cat_groups <- min(as.integer(length(table(as.character(df[,ct_col])))/2),6)
  cat_col <- paste0(ct_col, '_cat')
  if(cat_groups > 4){
    if(is.integer(df[,ct_col])) digs = 0  else digs = 2
    df[,cat_col] <- as.character(cut2(round(df[,ct_col],digs) , digits = 3 + digs, g=cat_groups))
    df[,cat_col] <- sapply(df[,cat_col], trans_level)
  }else{
    df[,cat_col] <- as.character(df[,ct_col])
  }
  uniq_lvls <- as.character(unique(df[,cat_col]))
  level_df <- data.frame(cbind(uniq_lvls,
                               as.vector(sapply(uniq_lvls, function(x){str_replace(regmatches(x, regexpr('[0-9\\,]+', x)),',','')})))) %>%
    mutate('key' = as.numeric(as.character(V2))) %>% arrange(key)
  lvl_order <- as.vector(level_df$uniq_lvls)
  df[,cat_col] <- factor(df[,cat_col], levels=lvl_order, labels=lvl_order)#, ordered=T)
  
  return(df)
}


# refined from https://stackoverflow.com/questions/22416612/how-can-i-get-cut2-to-use-commas
trans_level <- function(x,nsep=" to ") {
  x <- as.character(x)
  # is category just a number? if so, just don't change
  if(grepl('[0-9]+', x)){
    if(nchar(regmatches(x, regexpr('[0-9]+', x))) == nchar(x)){
      return(x)
    }else{
      n <- str_extract_all(x,"\\d+", simplify = TRUE)## extract numbers
      digs = 0
      if (length(n)>2){ # decimals
        n <- str_extract_all(x,"\\d+\\.\\d*", simplify = TRUE)
        digs = 2
        lower_bound <- substring(x, 1, 1)
        # if lower bound exclusive, make inclusive by subtracting 1
        if(lower_bound == '('){n[1] <- round(as.numeric(n[1]), digs)-.01}
        upper_bound <- substring(x, nchar(x), nchar(x))
        # if upper bound exclusive, make inclusive by subtracting 1
        if(upper_bound == ')'){n[2] <- round(as.numeric(n[2]), digs)-.01}
      } else {
        lower_bound <- substring(x, 1, 1)
        # if lower bound exclusive, make inclusive by subtracting 1
        if(lower_bound == '('){n[1] <- round(as.numeric(n[1]), digs)-1}
        upper_bound <- substring(x, nchar(x), nchar(x))
        # if upper bound exclusive, make inclusive by subtracting 1
        if(upper_bound == ')'){n[2] <- round(as.numeric(n[2]), digs)-1}
      }
      v <- format(round(as.numeric(n),digs) ,big.mark=",",trim=TRUE) ## change format
      return(paste(v,collapse=nsep))
    }
  }else{
    return(NA)
  }
 
}

update_national_choropleth2 <- function(metric='tot_deaths', test_data=NULL, n=6, ret_data=F){
  
  fill_metric <- metric 
  metric_key = list('tot_deaths' = 'Total Deaths', 'tot_cases'='Total Cases', 'doubling_time'='Doubling Time', 'r_t'='R_t')
  
  state_df <- STATE_INF_METRICS %>% 
    mutate(state_abbr = sapply(state, state_lookup)) %>%
    rename(state_name = state, state=state_abbr) %>%
    left_join(CUMULATIVE_STATE_TESTING, by='state') %>% 
    mutate(code = as.factor(state),
           hover = paste0('<b>',state_name, '</b><br><b>Pop: </b>', comma(pop),
                          "<br><b>Deaths: </b>", tot_deaths,
                          "<br><b>Cases: </b>", comma(tot_cases),
                          "<br><b>R<sub>t</sub>: </b>", round(rt, 2),
                          "<br><b>Doubling Time: </b>", round(doubling_time,2),
                          "<br><b>Pos. Tests: </b>", comma(positive),
                          "<br><b>Neg. Tests: </b>", comma(negative),
                          "<br><b>Hospitalizations: </b>", comma(hospitalized),
                          "<br><b>First Infection Date: </b>", as.Date(fi_date)),
           state_name_lower = tolower(state_name))
  
  if(tolower(fill_metric)=='doubling_time'){
    state_df <- state_df %>%
      mutate(
        doubling_time_cat = str_replace_all(cut2(doubling_time, g = 6), pattern = ", ", replacement = " to "),
        doubling_time_cat = str_replace_all(doubling_time_cat, pattern = ",", replacement = " to "),
        doubling_time_cat = trimws(str_remove_all(string = doubling_time_cat, pattern = "[\\[\\]\\(\\)]+")))
    
    doub.time.cats <- levels(factor(state_df$doubling_time_cat))
    if(sum(str_detect(doub.time.cats, 'to')) > 0){
      doub.time.first.val <- -rank(as.numeric(str_extract_all(string = doub.time.cats, pattern = "(\\d+\\.\\d*)(?= to)", simplify = TRUE)[,1]))
    }else{
      doub.time.first.val <- -rank(as.numeric(str_extract_all(string = doub.time.cats, pattern = "(\\d+\\.\\d*)", simplify = TRUE)[,1]))
    }
    names(doub.time.first.val) <- doub.time.cats
    doub.time.first.val <- sort(doub.time.first.val)
    doub.time.first.val.levels.vec <- names(doub.time.first.val)
    state_df <- state_df %>% 
      mutate(doubling_time_cat = factor(doubling_time_cat, levels = doub.time.first.val.levels.vec))
  }else if(tolower(fill_metric)=='r_t'){
    state_df <- state_df %>%
      mutate(r_t_cat = factor(sapply(r_t, function(x){ifelse(x <= .5, '0 to 0.5', 
                                                             ifelse(x <= 1, '0.51 to 1',
                                                                    ifelse(x <= 1.5, '1.01 to 1.5', '> 1.5')))}),
                              levels = c('0 to 0.5', '0.51 to 1', '1.01 to 1.5', '> 1.5')))
  }else{
    state_df <- state_df %>% 
      numeric_to_factor(ct_col = fill_metric, cat_groups=6) 
  }
  
  state_map_df <- state_df %>%
    right_join(map_data("state"), # state data for maps
      by = c("state_name_lower"="region"))  %>% 
    arrange(order) %>%
    mutate(fill_metric = get(paste0(fill_metric, "_cat")))
  
  fig <- state_map_df %>%
    ggplot(mapping = aes(long, lat, group = group, fill=fill_metric, text=hover, key=state_name)) +
    scale_fill_brewer(palette = "Blues") +
    geom_polygon(color = "black", size = .25) +
    #coord_map(projection = "bonne", lat0 = 39) + 
    #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    blank_theme +
    theme(legend.title = element_blank(),
          legend.key.width = unit(.25, "in"))
  
  metric_name <- switch(metric, 'tot_deaths'='Total Deaths', 'tot_cases'='Total Cases', 'doubling_time'='Doubling Time', 'rt'='Viral Reproduction Rate')
  map.title <- paste0(metric_name, ' Across the United States')
  
  # convert to ggplotly
  fig = ggplotly(fig, tooltip='text', width = 800, height = 450) %>%
    layout(legend=list(yanchor='bottom', y=.25),
           font = list(family = "Arial"),
           #geo = list(scope = 'usa', projection = list(type = 'albers usa')),
           title=list(text=map.title, y=.97, font=list(size=18)), 
           margin=list(t=50))
  
  
  if(ret_data){
    return(list('fig'=fig, df=state_df))
  }else{return(fig)}
  
}
  
  
  
  


update_national_choropleth <- function(state_df, metric='tot_deaths', color_list=NULL, test_data=NULL, n=6, ret_data=F){
  
  state_df <- state_df %>% mutate(state_abbr = sapply(state, state_lookup)) %>%
    rename(state_name = state, state=state_abbr) %>%
    left_join(test_data, by='state') %>% mutate(code = as.factor(state))
  
  state_df$hover <- with(state_df, paste0('<b>',state_name, '</b><br><b>Pop: </b>', comma(pop),
                                          "<br><b>Deaths: </b>", tot_deaths,
                                          "<br><b>Cases: </b>", comma(tot_cases),
                                          "<br><b>R<sub>t</sub>: </b>", round(rt, 2),
                                          "<br><b>Doubling Time: </b>", round(doubling_time,2),
                                          "<br><b>Pos. Tests: </b>", comma(positive),
                                          "<br><b>Neg. Tests: </b>", comma(negative),
                                          "<br><b>Hospitalizations: </b>", comma(hospitalized),
                                          "<br><b>First Infection Date: </b>", as.Date(fi_date)))
  
  metric_name <- switch(metric, 'tot_deaths'='Total Deaths', 'tot_cases'='Total Cases', 'doubling_time'='Doubling Time', 'rt'='Viral Reproduction Rate')
  reverse = (metric=='doubling_time')
  
  
  # create categorical variable and colorscale mapping for plot
  cat_col = paste0(metric, '_cat')
  if(metric=='rt'){
    state_df$cat_int = sapply(state_df[,metric], function(x){ifelse(x <= .7, 1,
                                                                          ifelse(x <= .9, 2,
                                                                              ifelse(x <= 1, 3,
                                                                                     ifelse(x <= 1.1, 4,
                                                                                              ifelse(x <= 1.3, 5, 6)))))})
    #cat_key = c('0 to .5','.51 to 1', '1.01 to 1.5','> 1.5')
    cat_key = c('0 to 0.7','0.71 to 0.9', '0.91 to 1','1.01 to 1.1','1.11 to 1.3','>1.3')
    state_df[,cat_col] = factor(sapply(state_df$cat_int, function(x){cat_key[[x]]}), levels = cat_key)
    #colorlist = c('#51b364', '#f0bd27', '#e03531', '#b60a1c')
    colorlist = c('#00429d', '#4271b7', '#6da3ce', '#a0d5e1', '#f37c64', '#e24b50', '#c11e42', '#93003a')
    names(colorlist) = cat_key
    
    #n=4
    n= 8
  }else{
    state_df = numeric_to_factor(state_df, ct_col=metric, cat_groups=n)
    state_df$cat_int = as.numeric(state_df[,cat_col])
    colorlist <- gen_colscale(n, color_list=color_list, reverse=reverse)
    names(colorlist) = levels(state_df[,cat_col])
  }
  
  #colorScale <- data.frame(z=Z_Breaks(n), col=rep(colorlist,each=2),stringsAsFactors=FALSE)
  
  # projection options
  # g <- list(
  #   scope = 'usa',
  #   projection = list(type = 'albers usa'),
  #   showland = TRUE,
  #   landcolor = toRGB("gray95"),
  #   subunitcolor = toRGB("black"),
  #   countrycolor = toRGB("black"),
  #   countrywidth = 0.5,
  #   subunitwidth = 0.5
  # )
  # 
  # fig <- plot_ly(data = state_df,type = "choropleth",locations = ~ state,
  #                locationmode = "USA-states", z = ~cat_int, text = ~hover,  hoverinfo='text',
  #                colorscale=colorScale, stroke=I('black'),
  #                colorbar=list(tickmode = "array", tickvals=1:length(colorlist), ticktext=c(names(colorlist)), y=.75, ypad=10, title=list())
  # ) %>% layout(geo = g, 
  #              font = list(family = "Arial"),
  #              paper_bgcolor='rgba(255,255,255,1)',
  #              plot_bgcolor='rgba(255,255,255,1)',
  #              margin=list('l'=0,'r'=0,'b'=0,'t'=50), 
  #              title=list(text=paste0(metric_name, ' Across the United States'), size=18, y=.97)) 
  
  pal <- colorFactor(colorlist, domain = state_df[,cat_col], na.color = '#ffffff')
  
  #bounds <- c(-118, 17 ,-63, 50)
  
  coord = as.data.frame(st_coordinates((spdf_us)$geometry))
  bounds <- c(min(coord$X), min(coord$Y), max(coord$X), max(coord$Y)+1) 
  bounds[3] <- bounds[3] + abs(bounds[1] - bounds[3])*.2
  bounds[2] = 13
  
  state_df <- suppressWarnings(left_join(spdf_us, state_df, by = c('name' ='state_name')))
  
  labels <- state_df$hover %>% lapply(htmltools::HTML)

  fig <- leaflet(state_df, options = leafletOptions(crs = EPSG2163, minZoom = 2, zoomSnap = 0, zoomControl = FALSE), height = '400px') %>%
    addPolygons(weight = 0.25, color = "#999999", opacity = 1, layerId=~name,
                fillColor = ~pal(state_df[[cat_col]]), #as.name(cat_col)
                fillOpacity = 1, smoothFactor = 0.5,
                label = labels,
                labelOptions = labelOptions(direction = "auto"))  %>%
    addLegend("bottomright", pal = pal, values = ~state_df[[cat_col]],
              title = metric_name,
              opacity = 1, na.label = "No Data") %>%
    setView(lng = (bounds[1]+bounds[3])/2, lat = (bounds[2]+bounds[4])/2, zoom = 1.5) %>%
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4], options = list(padding = c(50,50))) %>%
    setMaxBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    addResetMapButton()
  
  if(ret_data){
    return(list('fig'=fig, df=state_df))
  }else{return(fig)}
}



Z_Breaks = function(n){
  CUTS = seq(0,1,length.out=n+1)
  rep(CUTS,ifelse(CUTS %in% 0:1,1,2))
}


gen_state_covid_plot <- function(state_name, max_width=800, fill_metric='tot_deaths', measure_title=NULL, show_hosps=F, ret_data=F){
  
  state_abbr = state_lookup(state_name)

  metric_key = list('tot_deaths' = 'Total Deaths', 'tot_cases'='Total Cases', 'doubling_time'='Doubling Time', 'r_t'='R_t')
  if(is.null(measure_title)) measure_title = metric_key[[fill_metric]]
  #doubling rate new way; KRF 4/15
  county_info <- COUNTY_INF_STATS %>%
    separate(col=mState.Providence, into = c("County.Name", "state.abbrev"), sep = ", ", remove = FALSE) %>%
    #mutate(state.abbrev = str_extract(mState.Providence, pattern = "(?<=, )[A-Z]{2}")) %>%
    
    filter(state.abbrev == state_abbr, case_count>0) %>%
    group_by(mState.Providence) %>%
    arrange(date) %>%
    top_n(n=8, wt=as.Date(date)) %>%
    mutate(lag1 = lag(case_count, n=1, default = 0),
           lag1r = ifelse(lag1==0, 0, (case_count-lag1)/lag1),
           lag2 = lag(case_count, n=2, default = 0),
           lag2r = ifelse(lag2==0, 0, (lag1-lag2)/lag2),
           lag3 = lag(case_count, n=3, default = 0),
           lag3r = ifelse(lag3==0, 0, (lag2-lag3)/lag3),
           lag4 = lag(case_count, n=4, default = 0),
           lag4r = ifelse(lag4==0, 0, (lag3-lag4)/lag4),
           lag5 = lag(case_count, n=5, default = 0),
           lag5r = ifelse(lag5==0, 0, (lag4-lag5)/lag5),
           doub_lag_denom = (lag1r > 0) + (lag2r > 0) + (lag3r > 0) + (lag4r > 0) + (lag5r > 0),
           growth.rate.all = (lag1r + lag2r + lag3r + lag4r + lag5r)/doub_lag_denom,
           doubling_time = 70/(100*growth.rate.all),
           doubling_time = ifelse(doubling_time==Inf, NA, doubling_time),
           tot_cases = max(case_count, na.rm = TRUE),
           tot_cases = ifelse(is.na(tot_cases), 0, tot_cases),
           tot_deaths = max(death_count,na.rm = TRUE),
           tot_deaths = ifelse(is.na(tot_deaths), 0, tot_deaths)
           ) %>%
    filter(date==max(date,na.rm = TRUE)) %>%
    ungroup() %>%
    select(mState.Providence, County.Name, State = state.abbrev, date, tot_cases, tot_deaths, doubling_time) %>%
    left_join(COUNTY_CLEANUP %>% filter(full==state_name), by = c("County.Name"="County.Pretty")) %>%
    mutate(
      county.mid = sapply(county.ugly, simpleCap),
      pop.mState.key = ifelse(paste(county.ugly, State, sep=", ")==mState.Providence, 
                                   paste(county.ugly, State, sep=", "), 
                                   mState.Providence)) %>%
    left_join(select(WORLD_POP, mState.Providence, population), 
              by = c("pop.mState.key"="mState.Providence")) %>%
    left_join(RT %>% filter(state==state_name, region!=state_name) %>%
                arrange(region, date) %>%
                group_by(region) %>%
                dplyr::summarize(r_t = last(mean), RT_date= max(date)), 
              by=c("mState.Providence"="region")) %>%
    distinct() %>% 
    mutate(county.ugly2 = tolower(county.ugly),
           low.cases.flag = tot_cases < 50) %>%
    filter(State==state_abbr, !grepl('Cruise Ship', mState.Providence), !grepl('Unallocated',  mState.Providence)) 

  if (str_detect(string = measure_title, pattern = "100,000")){
    # replace NA population counts with mean population across all counties (to prevent plotting errors)
    county_info <- county_info %>% mutate(
      population = ifelse(is.na(population), mean(population, na.rm=T), population), 
      tot_cases = 100000 * tot_cases/population, 
      tot_deaths = 100000 * tot_deaths/population
    )
  }
  
  if(tolower(fill_metric)=='doubling_time'){
    county_info <- county_info %>%
      mutate(
        doubling_time_cat = str_replace_all(cut2(doubling_time, g = 6), pattern = ", ", replacement = " to "),
        doubling_time_cat = str_replace_all(doubling_time_cat, pattern = ",", replacement = " to "),
        doubling_time_cat = trimws(str_remove_all(string = doubling_time_cat, pattern = "[\\[\\]\\(\\)]+")))
    
    doub.time.cats <- levels(factor(county_info$doubling_time_cat))
    if(sum(str_detect(doub.time.cats, 'to')) > 0){
      doub.time.first.val <- -rank(as.numeric(str_extract_all(string = doub.time.cats, pattern = "(\\d+(\\.|)\\d*)(?= to)", simplify = TRUE)[,1]))
    }else{
      doub.time.first.val <- -rank(as.numeric(str_extract_all(string = doub.time.cats, pattern = "(\\d+(\\.|)\\d*)", simplify = TRUE)[,1]))
    }
    names(doub.time.first.val) <- doub.time.cats
    doub.time.first.val <- sort(doub.time.first.val)
    doub.time.first.val.levels.vec <- names(doub.time.first.val)
    county_info <- county_info %>% mutate(doubling_time_cat = factor(doubling_time_cat, levels = doub.time.first.val.levels.vec))
  }else if(tolower(fill_metric)=='r_t'){
    county_info$r_t_cat = sapply(county_info$r_t, function(x){ifelse(x <= .7, 1,
                                                             ifelse(x <= .9, 2,
                                                                    ifelse(x <= 1, 3,
                                                                           ifelse(x <= 1.1, 4,
                                                                                  ifelse(x <= 1.3, 5, 6)))))})
    #cat_key = c('0 to .5','.51 to 1', '1.01 to 1.5','> 1.5')
    cat_key = c('0 to 0.7','0.71 to 0.9', '0.91 to 1','1.01 to 1.1','1.11 to 1.3','>1.3')
    county_info$r_t_cat = factor(sapply(county_info$r_t_cat, function(x){cat_key[x]}), levels = cat_key)
  }else{
    county_info <- county_info %>% numeric_to_factor(ct_col = fill_metric, cat_groups=6) 
  }

  county_info <- county_info %>%
    mutate(
      doubling_time.ft = ifelse(is.na(doubling_time), "Insufficient Data", round(doubling_time, 2)),
      r_t.ft = ifelse(is.na(r_t) | low.cases.flag==T, "Insufficient Data", round(r_t, 2)),
      text_tip = paste0("<b>",mState.Providence,'</b>',
                             "<br><b>Population</b>: " , population,
                             "<br><b>Cases</b>: ", round(tot_cases,2) ,
                             "<br><b>Deaths</b>: ", round(tot_deaths,2) ,
                             "<br><b>Doubling Time</b>: ", doubling_time.ft,
                             "<br><b>R<sub>t</sub></b>: ", r_t.ft),
           fill_metric = get(paste0(fill_metric, "_cat"))
    ) 

  fill_met_cat = paste0(fill_metric, '_cat')
  
  if(tolower(fill_metric)=='r_t') {
    cat_key = c('0 to 0.7','0.71 to 0.9', '0.91 to 1','1.01 to 1.1','1.11 to 1.3','>1.3')
    colorlist = c('#00429d', '#4271b7', '#6da3ce', '#a0d5e1', '#f37c64', '#e24b50', '#c11e42', '#93003a')
    names(colorlist) = cat_key
    pal <- colorFactor(colorlist, domain = county_info$fill_metric, na.color = '#ffffff')
  } else {
    pal <- colorFactor("Blues", domain = county_info$fill_metric, na.color = '#ffffff')
  }

  bounds <- c(-118, 17 ,-63, 50)
  
  county_info <- county_info %>%
    mutate(fips = str_pad(fips, 5, 'left', '0' ),
           text_tip = ifelse(is.na(text_tip),
                  paste0("<b>",simpleCap(county.ugly2)," County, ", state_abbr,'</b>',
                         "<br><b>Cases</b>: 0",
                         "<br><b>Deaths</b>: 0",
                         "<br><b>Doubling Time</b>: Insufficient Data", 
                         "<br><b>R<sub>t</sub></b>: Insufficient Data"),
                  text_tip))
  
  county_info <- suppressWarnings(left_join(spdf_counties %>% filter(iso_3166_2 == state_abbr), 
                                            county_info, by = c('fips')))
  
  labels <- county_info$text_tip %>% lapply(htmltools::HTML)
  
  coord = as.data.frame(st_coordinates((spdf_us %>% filter(name == state_name))$geometry))
  bounds <- c(min(coord$X), min(coord$Y), max(coord$X), max(coord$Y)) 
  bounds[3] <- bounds[3] + abs(bounds[1] - bounds[3])*.25
  
  fig <- leaflet(county_info, options = leafletOptions(crs = EPSG2163_2, minZoom = 5, zoomSnap = 0, zoomControl = FALSE), height = '400px') %>%
    addPolygons(weight = 0.25, color = "#999999", opacity = 1, layerId=~County.Name,
                fillColor = ~pal(county_info[[fill_met_cat]]),
                fillOpacity = 1, smoothFactor = 0.5,
                label = labels,
                labelOptions = labelOptions(direction = "auto"))  %>%
    addLegend("bottomright", pal = pal, values = ~county_info[[fill_met_cat]],
              title = measure_title,
              opacity = 1, na.label = "No Data") %>%
    setView(lng = (bounds[1]+bounds[3])/2, lat = (bounds[2]+bounds[4])/2, zoom = 1.5) %>%
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4], options = list(padding = c(50,50))) %>%
    setMaxBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    addResetMapButton()
    #suspendScroll(sleep = TRUE, sleepTime = 1, wakeTime = 300000, sleepNote = F, sleepOpacity = 1, wakeMessage = F, hoverToWake = F)
  
  # plot hospitals if show_hosps=True
  if(show_hosps){
    fig = add_hosps_to_map_plot(fig, state_name=state_name)
  }
  
  if(ret_data){return(list('fig'=fig, 'df'=county_info))}
  else{return(fig)}
}

add_hosps_to_map_plot <- function(gg_map_plt, state_name, hosp_df=HOSPITALS, pt_color='blue', opacity=.5){
  
  state_abbr = state_lookup(state_name)
  
  # if(state_abbr %in% c('AK', 'HI')){
  #   map_df = us_map('counties') %>% select(group, abbr, county) %>% distinct() %>%
  #     filter(abbr==state_abbr) %>% rename(county_full=county)
  # }else{
  #   map_df = map_data('county') %>% select(group, region, subregion) %>%
  #     filter(region==tolower(state_name)) %>%
  #     mutate(abbr = state_abbr) %>% distinct() %>%
  #     mutate(county_full = sapply(subregion, function(x){paste0(simpleCap(x), ' County')}))
  # }
  
  hosp_df <- hosp_df %>% filter(State==state_abbr) %>%
    #left_join(map_df, by=c('County'='county_full')) %>%
    #select(Longitude, Latitude, NUM_STAFFED_BEDS, text_tip, group) %>% drop_na()
    select(Longitude, Latitude, NUM_STAFFED_BEDS, text_tip) %>% drop_na()
  
  
  # if(state_abbr %in% c('AK', 'HI')){
  #   hosp_df <- usmap_transform(hosp_df)
  # }else {
  #   hosp_df <- hosp_df %>% rename(longitude.1=Longitude, latitude.1=Latitude)
  # } 
  
  # gg_map_plt <- gg_map_plt +
  #   geom_point(data = hosp_df, aes(x = longitude.1, group=group, y = latitude.1, size = NUM_STAFFED_BEDS, text=text_tip),
  #              color = pt_color, alpha = opacity, inherit.aes = F )
  
  labels2 <- hosp_df$text_tip %>% lapply(htmltools::HTML)
  
  gg_map_plt <- gg_map_plt %>%
    addCircles(data = hosp_df, lng = ~Longitude, lat = ~Latitude, weight = 1,
              radius = ~sqrt(NUM_STAFFED_BEDS)*300, label = labels2)
  return(gg_map_plt)
}


# modified slightly from usmap's default plot_usmap function
plot_usmap_custom <- function (regions = c("states", "state", "counties", "county"),  include = c(), exclude = c(), tooltip_col = NULL,
                               data = data.frame(), values = "values",  theme = theme_map(), labels = FALSE, label_color = "black", ...) 
{
  geom_args <- list(...)
  if (is.null(geom_args[["colour"]]) & is.null(geom_args[["color"]])) {
    geom_args[["color"]] <- "black"
  }
  if (is.null(geom_args[["size"]])) {
    geom_args[["size"]] <- 0.4
  }
  if (is.null(geom_args[["fill"]]) & nrow(data) == 0) {
    geom_args[["fill"]] <- "white"
  }
  else if (!is.null(geom_args[["fill"]]) & nrow(data) != 0) {
    warning("`fill` setting is ignored when `data` is provided. Use `fill` to color regions with solid color when no data is being displayed.")
  }
  regions_ = regions
  #TODO: problem happening somewhere in here
  if (nrow(data) == 0) {
    map_df <- us_map(regions = regions_, include = include, 
                     exclude = exclude)
    geom_args[["mapping"]] <- ggplot2::aes(x = map_df$x, 
                                           y = map_df$y, group = map_df$group)
  }
  else {
    if(is.null(tooltip_col)){
      map_df <- map_with_data(data, values = values, include = include,  exclude = exclude)
      geom_args[["mapping"]] <- ggplot2::aes(x = map_df$x, 
                                             y = map_df$y, group = map_df$group, fill = map_df[,values])
    }
    
    else{
      data$tooltip <- data[,tooltip_col]
      data[,tooltip_col] = NULL
      map_df <- map_with_data(data, values = values, include = include,  exclude = exclude)
      geom_args[["mapping"]] <- ggplot2::aes(x = map_df$x, text=map_df$tooltip,
                                             y = map_df$y, group = map_df$group, fill = map_df[,values])
    }
    
  }

  polygon_layer <- do.call(ggplot2::geom_polygon, geom_args)
  if (labels) {
    centroidLabelsColClasses <- c("numeric", "numeric", "character", 
                                  "character", "character")
    if (regions_ == "county" | regions_ == "counties") {
      centroidLabelsColClasses <- c(centroidLabelsColClasses, 
                                    "character")
    }
    centroid_labels <- utils::read.csv(system.file("extdata", 
                                                   paste0("us_", regions_, "_centroids.csv"), package = "usmap"), 
                                       colClasses = centroidLabelsColClasses, stringsAsFactors = FALSE)
    if (length(include) > 0) {
      centroid_labels <- centroid_labels[centroid_labels$full %in% 
                                           include | centroid_labels$abbr %in% include | 
                                           centroid_labels$fips %in% include, ]
    }
    if (length(exclude) > 0) {
      centroid_labels <- centroid_labels[!(centroid_labels$full %in% 
                                             exclude | centroid_labels$abbr %in% exclude | 
                                             centroid_labels$fips %in% exclude | substr(centroid_labels$fips, 
                                                                                        1, 2) %in% exclude), ]
    }
    if (regions_ == "county" | regions_ == "counties") {
      label_layer <- ggplot2::geom_text(data = centroid_labels, 
                                        ggplot2::aes(x = centroid_labels$x, y = centroid_labels$y, 
                                                     label = sub(" County", "", centroid_labels$county)), 
                                        color = label_color)
    }
    else {
      label_layer <- ggplot2::geom_text(data = centroid_labels, 
                                        ggplot2::aes(x = centroid_labels$x, y = centroid_labels$y, 
                                                     label = centroid_labels$abbr), color = label_color)
    }
  }
  else {
    label_layer <- ggplot2::geom_blank()
  }
  ggplot2::ggplot(data = map_df) + polygon_layer + label_layer + 
    ggplot2::coord_equal() + theme
}
 

map_theme <-  theme_bw(base_size = 9, base_family = "") %+replace% 
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid = element_blank(), panel.spacing = unit(0,  "lines"), plot.background = element_blank(), 
        legend.position='right', 
        legend.title = element_text(size=14),
        legend.text = element_text(size=10),
        legend.key.width = unit(.25, "in")
  )

covid_ts_plot <- function(df, xcol='date', xname='Date', ycol='value', yname='Count', color='red', title=NULL){
  plt <- ggplot(df, aes(x=!!as.name(xcol), y=!!as.name(ycol), col=color)) +
    geom_line(color='red') + geom_point() +
    labs(x="Date", y="Case Count") +
    theme(legend.position='none')
  if(!is.null(title)){
    plt <- plt + labs(title=title)
  }
  return(ggplotly(plt))
}

gen_resource_plot <- function(hosp_df, sample=FALSE, n=100, resource = 'NUM_ICU_BEDS'){
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showland = TRUE,
    landcolor = toRGB("gray95"),
    subunitcolor = toRGB("black"),
    countrycolor = toRGB("black"),
    countrywidth = 0.5,
    subunitwidth = 0.5
  )
  title = paste0('Hospital Resources Across the United States')
  hosp_df <- hosp_df %>% filter(!is.na(BED_UTILIZATION))
  if(sample){
    hosp_df <- hosp_df[sample(1:nrow(hosp_df), 100),]
    title <- paste0(title, ' (Sample of ',n,' hospitals)')
  }
  fig <- plot_geo(hosp_df, lat = ~Latitude, lon = ~Longitude)
  fig <- fig %>% add_markers(
    text = ~text_tip,
    symbol = I("square"), size = I(8), hoverinfo = "text",
  )
  fig <- fig %>% layout(
    title = title , geo = g,
    font = list(family = "Arial"),
    paper_bgcolor='rgba(0,0,0,0)',
    plot_bgcolor='rgba(0,0,0,0)',
    margin=list('l'=0,'r'=0,'b'=0,'t'=30) 
  )
  return(fig)
}


gen_resource_plot_v2 <- function(hosp_df, state=NULL, fill_metric='BED_UTILIZATION', pt_color='orange', per_cap=F, scale=10000, n_state=NULL){
  county_data = map_data("county")
  hosp_df <- hosp_df %>% mutate(county_lower = sapply(County, function(x){gsub(' County', '', x) %>% tolower()}))  %>%
    merge(countypop, by.x='County', by.y='county', all.x=T) %>%
    filter(State %in% STATE_NAMES$abbr) %>%
    mutate(State = droplevels(State))
  
  hosp_df[,'fill_metric'] = hosp_df[,fill_metric]
  if(!is.null(n_state)){
    hosps_per_state <- min(n_state, min(table(hosp_df$State)))
    hosp_df <- hosp_df %>% group_by(State) %>%
      arrange(desc(fill_metric)) %>%
      top_n(hosps_per_state, fill_metric) %>%
      ungroup()
  }

  if(!is.null(state)){
    state_abbr = state_lookup(state)
    county_data <- county_data %>% filter(region==tolower(state))
    hosp_df <- hosp_df %>% filter(State == state_abbr)
  }
  
  if(per_cap){
    # hosp_df <- hosp_df %>% filter(!is.na(fill_metric)) %>%
    #                 mutate('fill_metric' = fill_metric / pop_2015 * scale)
    hosp_df$fill_metric = hosp_df$fill_metric / hosp_df$pop_2015 * scale
    hosp_df <- hosp_df %>% filter(!is.na(fill_metric))
    measure_title = paste0(fill_metric, ' per ', scale, 'ppl')
  }else{
    measure_title <- fill_metric
  }
  comb_df <- merge(county_data, hosp_df, by.y='county_lower',by.x='subregion', all.x=T)
  comb_df <- comb_df[order(comb_df$order),]
  
  uniq_groups =  county_data %>% select(group, subregion) %>% distinct()
  hosp_df <- hosp_df %>% merge(uniq_groups, by.x='county_lower', by.y='subregion')
  fig <- comb_df %>%
    ggplot(mapping = aes(long, lat, group = group, fill=fill_metric)) +
    geom_polygon(color = "black", size = .25) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    blank_theme +
    theme(legend.title = element_text(),
          legend.key.width = unit(.25, "in")) +
    labs(fill = measure_title) +
    #geom_point(data=comb_df, aes(Longitude,Latitude))
    geom_point(data=hosp_df, aes(Longitude, Latitude, text=text_tip), colour=pt_color)
  fig <- ggplotly(fig, tooltip="text")
  return(fig)
}

gen_map_plot <- function(hosp_df, state=NULL, fill_metric='BED_UTILIZATION', pt_color='orange', per_cap=F, scale=10000){
  county_data = map_data("county")
  hosp_df <- hosp_df %>% mutate(county_lower = sapply(County, function(x){gsub(' County', '', x) %>% tolower()}))  %>%
    merge(countypop, by.x='County', by.y='county', all.x=T) 
  if(!is.null(state)){
    state_abbr = state_lookup(state)
    county_data <- county_data %>% filter(region==tolower(state))
    hosp_df <- hosp_df %>% filter(State == state_abbr)
  }
  
  hosp_df[,'fill_metric'] = hosp_df[,fill_metric]
  if(per_cap){
    # hosp_df <- hosp_df %>% filter(!is.na(fill_metric)) %>%
    #                 mutate('fill_metric' = fill_metric / pop_2015 * scale)
    hosp_df$fill_metric = hosp_df$fill_metric / hosp_df$pop_2015 * scale
    hosp_df <- hosp_df %>% filter(!is.na(fill_metric))
    measure_title = paste0(fill_metric, ' per ', scale, 'ppl')
  }else{
    measure_title <- fill_metric
  }
  comb_df <- merge(county_data, hosp_df, by.y='county_lower',by.x='subregion', all.x=T)
  comb_df <- comb_df[order(comb_df$order),]
  
  uniq_groups =  county_data %>% select(group, subregion) %>% distinct()
  hosp_df <- hosp_df %>% merge(uniq_groups, by.x='county_lower', by.y='subregion')
  fig <- comb_df %>%
    ggplot(mapping = aes(long, lat, group = group, fill=fill_metric)) +
    geom_polygon(color = "#ffffff", size = .25) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    blank_theme +
    theme(legend.title = element_text(),
          legend.key.width = unit(.25, "in")) +
    labs(fill = measure_title) +
    geom_point(data=hosp_df, aes(Longitude, Latitude, text=text_tip), colour=pt_color)
  fig <- ggplotly(fig, tooltip="text")
  return(fig)
}


blank_theme <- theme(
   panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(),
   panel.background = element_blank(),
   axis.title.x = element_blank(),
   axis.title.y = element_blank())

# generate a custom colorscale for plotting
gen_colscale <- function(n, color_list=NULL, reverse=FALSE){
  if(is.null(color_list)) color_list = MITRE_BLUES
  if(reverse) color_list <- rev(color_list)
  return(colorRampPalette(color_list)(n))
}

WYOR <- c("white", "yellow","orange", "red", "darkred")
REDS <- c('#fff5f0', '#fcbba1','#fd7354','#cb181d','#67000d' )
BLUES <- c('#deebf7', '#9ecae1', '#3182bd')

MITRE_BLUES <- c('#e6f1f8', '#4fa0d3', '#005b94', '#0D2F4F')

gen_demog_heatmap <- function(demog.flag, epid.flag, health.flag, county.name){
  
  selected.fips.char <- USFACTS_CONFIRMED %>%
    select(fips = countyFIPS, county = County.Name, state=State) %>%
    mutate(county.state = paste(county, state, sep=", ")) %>%
    filter(county.state == county.name) %>%
    distinct() %>%
    .$fips
  
  ## Set up data frame; ex: bronx 36005; westchester 36119
  
  tgt_fips <- aggregate_distance(demog.flag, epid.flag, health.flag, HRSA_DEMOG_CATS, 
                                 whichfips = as.character(selected.fips.char)[1], data = HRSA_SIMILARITY_PCT)
  
  #tgt_fips <- tgt_fips[[2]]
  ordered_fips <- names(tgt_fips[[1]][order(tgt_fips[[1]])])
  # Plot Heatmap
  
  heatmap_data <- data.frame(as.matrix(tgt_fips[[2]][rev(ordered_fips), ])) %>%
    mutate(fips = as.numeric(rownames(.)),
           index = 1:n()) %>%
    left_join(COUNTY_CLEANUP, by = c("fips")) %>%
    mutate(label = paste(County.Pretty, abbr, sep=", ")) %>%
    distinct() %>%
    filter(!is.na(County.Pretty), County.Pretty != "Yellowstone County") %>%
    arrange(index)
  
  elig.metrics <- names(tgt_fips[[2]])
  elig.counties <- row.names(heatmap_data)
  
  rownames(heatmap_data) <- elig.counties
  
  #rownames(heatmap_data) <- heatmap_data_key$label #paste(heatmap_data_key$County.Pretty, heatmap_data_key$abbr, sep=", ")
  fig <- plot_ly() 
  fig <- fig %>%
    add_trace(
      type = 'heatmap',
      x = colnames(heatmap_data)[colnames(heatmap_data) %in% elig.metrics],
      y = rownames(heatmap_data),
      z = 100-as.numeric(as.matrix(heatmap_data)[,names(heatmap_data) %in% elig.metrics]),
      colors="Blues",
      hovertemplate = paste('<b>%{x}</b><br>',
                            '<i>County: %{y}</i><br>',
                            'Similarity: %{z}',
                            '<extra></extra>'),
      hoverinfo="text",
      showlegend = FALSE
    ) %>%
    layout(autosize = FALSE, 
           font = list(family = "Arial"),
           title=paste("Similarity to County", "36005", "(0=least similar, 100=most similar)"),
           xaxis=list(title = "Measure", showticklabels = FALSE, dtick=1000000000000),
           yaxis=list(title = "County", showticklabels = FALSE, dtick=1000000000000))
  
  return(fig)
}

draw_mobility_plot <- function(state_name, graph.start.date=NULL, sim.days=NULL, show.npis=T, plt.height=NULL){
   
  # mobility data
  if(state_name != 'United States'){
    # if no start date provided, use first infection date
    if(is.null(graph.start.date)) graph.start.date = as.Date(STATE_INF_METRICS %>% filter(state==state_name) %>% .$fi_date)
    state_abbr = state_lookup(state_name)
    mobil.dat = MOBILITY_DATA %>% filter(str_detect(mState.Providence, state_abbr)==T) %>% select(-Country.Region)
  }else{
    # if no start date provided, use first infection date
    if(is.null(graph.start.date)) graph.start.date = min(as.Date(STATE_INF_METRICS[,'fi_date']))
    mobil.dat = MOBILITY_DATA %>% filter(mState.Providence %in% STATE_NAMES$full) %>% select(-Country.Region)
  }
    
  mobil.dat = mobil.dat %>%
      left_join(WORLD_POP %>% filter(Country.Region == 'United States'), by='mState.Providence') %>%
    mutate(pop_norm = population-min(population, na.rm=T) / (max(population, na.rm=T)-min(population, na.rm=T))) %>%
    group_by(date) %>%
      drop_na() %>%
      summarise(
                #mean_mobility1 = sum(percent_off_norm_maxMobility * population) / sum(population), 
                mean_mobility1 = sum(percent_off_norm_maxMobility * pop_norm) / sum(pop_norm), 
                mean_mobility = mean(percent_off_norm_maxMobility, na.rm=T),
                med_mobility = median(percent_off_norm_maxMobility, na.rm=T),
                sd_mobility = sqrt(var(percent_off_norm_maxMobility, na.rm=T)),
                q90 = quantile(percent_off_norm_maxMobility, na.rm=T, p=.90),
                q10 = quantile(percent_off_norm_maxMobility, na.rm=T, p=.10),
                count = length(!which(is.na(percent_off_norm_maxMobility)))) %>%
      mutate(date = as.Date(date),
             value = ifelse(mean_mobility1 > 200,200, mean_mobility1), 
             mState.Providence = state_name, 
             metric='Relative Mobility'
             )
    
  # build npi lines
  region.list = c(paste0(state_name, '___United States'))
  npi.plt.dat <- NPI_DATA %>%
    mutate(state.country.key = paste(state, Country.Region, sep = "___")) %>%
    filter(state.country.key %in% region.list) %>%
    mutate(tmp.txt.helper = paste0(state_abb, ": ", policy_long)) %>%
    distinct() %>%
    group_by(state, state.country.key, start_date) %>%
    summarise(policy.ft = paste(unique(policy), collapse = "\n"),
              tooltip.txt = unique(if_else(!str_detect(state, pattern = "[A-Z]{2}-[A-Z]{2}"),
                                           paste0("<b>",unique(state),"<br>",unique(start_date), "</b><br><i>", paste(unique(policy_long), collapse = "<br>"),"</i>"), #default TT
                                           paste0("<b>",unique(state),"<br>",unique(start_date), "</b><br><i>", paste(unique(tmp.txt.helper), collapse = "<br>"),"</i>") #MSAs that span a state
              )),
              policy.long.ft = paste(unique(policy_long), collapse = "\n")
    ) %>%
    ungroup() %>%
    mutate(start_date=as.Date(start_date)) %>%
    distinct() %>%
    filter(start_date >= as.Date("2020-01-22"))
  
  npi.lines <- list()
 
  plt_cols <- c("#882255", "#44AA99")
  names(plt_cols) <- c("Est. &#946;<sub>1</sub>", "Relative Mobility")
  
  current.day <- Sys.Date()
  
  yaxis.ops = list(
    tickfont = list(color="#44AA99"),
    ticksuffix = "%",
    nticks=5,
    title = 'Relative Mobility'
  )
  
  ret.plt <- plot_ly(colors = plt_cols, height = plt.height) %>%
    add_trace(data = mobil.dat, x=~date, y=~value, color=~metric, type='scatter',
              mode='lines', line=list(color="#44AA99",dash='solid',width = 2),
              yaxis = "y1") %>%
    add_ribbons(data=mobil.dat, x=~date, ymin=~q10, ymax=~q90, line = list(color = 'rgba(201, 201, 201, 1)'),
                fillcolor = 'rgba(201, 201, 201, 0.3)',showlegend=F) %>% 
    layout(hovermode = "x unified",
           xaxis=list(title=""), #,range = c(as.Date(graph.start.date), as.Date(graph.start.date)+ddays(sim.days))), 
           yaxis=yaxis.ops,
           font = list(family = "Arial"),
           margin=list(l=60, t=60, r=60, autoexpand=FALSE),
           legend = list(x=1.08)
    )
  
  if(nrow(npi.plt.dat) > 0){
    plot.title <- paste(state_name, "Mobility Changes after NPIs")
    npi.max.ys = mobil.dat %>% 
      rename(state = mState.Providence) %>%
      group_by(state) %>%
      summarise(updated.y = max(value, na.rm=T))
    
    npi.plt.dat <- npi.plt.dat %>%
      left_join(npi.max.ys, by = "state") %>%
      mutate(max.y = ifelse(!is.na(updated.y) & updated.y != Inf, updated.y, 0))
    
    for (i in 1:nrow(npi.plt.dat)) {
      line <- list(type = "line", line = list(color = "blue"), xref = "x", yref = "y")
      line <- list()
      line[["x0"]] <- as.Date(npi.plt.dat$start_date[i])
      line[["x1"]] <- line[["x0"]]
      line[["y0"]] <- 0
      line[["y1"]] <- npi.plt.dat$max.y[i]
      line[["line"]] <- list(color="gray")
      npi.lines <- c(npi.lines, list(line)) 
    }
    ret.plt <- ret.plt %>%
      add_trace(type='scatter',data=npi.plt.dat, x=~start_date, y=~max.y, text=~policy.ft, 
                textposition="top middle", mode="text",showlegend=FALSE, hovertext = ~tooltip.txt, hoverinfo="text") %>%
      layout(shapes=npi.lines,
             font = list(family = "Arial"))
  }else{
    plot.title <- paste(state_name, "Mobility Data since First Infection Date (No NPIs)")
  }

  ret.plt <- ret.plt %>% layout(title = list(text=plot.title,x=0,xref="paper"),
                                font = list(family = "Arial"))
  return(ret.plt)
}

draw_beta_mobility_plot <- function(region_name, graph.start.date, sim.days, inter.df=data.frame(), plt.height=NULL, b1.val = NULL, new_betas=T,
                                    show_forecasts=F, forecast_df=NULL, smooth=T, smooth_amt=.55){
  
  plt_cols <- c("#882255", "#44AA99")
  plt_linetypes <- c("solid", "solid")
  plt_line_names <- c("Est &#946;<sub>1</sub> (Transmission)", "Relative Mobility (Smoothed)")
  
  if (!is.null(b1.val)){
    plt_cols <- c(plt_cols, "#882255")
    plt_linetypes <- c(plt_linetypes, "dash")
    plt_line_names <- c(plt_line_names, "Modeled &#946;<sub>1</sub> (Transmission) ")
  }
  
  names(plt_cols) <- plt_line_names
  names(plt_linetypes) <- plt_line_names
  
  if(length(plt.height==0)) plt.height=NULL
  
  ret.plt <- plot_ly(colors = plt_cols, height = plt.height, linetypes = plt_linetypes)
  
  mobil.dat <- MOBILITY_DATA %>%
    filter(mState.Providence == region_name) %>%
    #filter(mState.Providence == "Maryland") %>% #for testing
    mutate(value = percent_off_norm_maxMobility,
           value = ifelse(value>200, 200, value),
           metric = "Relative Mobility", 
           date = as.Date(date)) %>%
    filter(as.Date(date) >= as.Date(graph.start.date))
  
  # if smooth=True, smooth mobility data using splines
  if(smooth){
    mob_spline = smooth.spline(x=mobil.dat$date, y=mobil.dat$value, spar=min(1, smooth_amt))
    sp_df = data.frame(date=mob_spline$x + min(mobil.dat$date, na.rm=T),
                       mob=mob_spline$y) %>%
      mutate(mob = ifelse(mob < 0, 0, mob))
    mobil.dat$value = sp_df$mob
    mobil.dat$metric = paste0(mobil.dat$metric, ' (Smoothed)')
  }
 
  
  isCounty = detect_county(region_name)
  if(isCounty){
    sel.abb = strsplit(region_name, ', ')[[1]][[2]]
    state = state_lookup(sel.abb)
    new_betas=T # override if false because old b1-tracking doesn't have county-level data.
  }else{
    state = region_name
    sel.abb <- STATE_NAMES %>%
      filter(full == state) %>%
      .$abbr
  }
  
  if(new_betas){
    b1.dat <- MOVING_BETA_DATA %>%
      rename(start.date=date, beta.step=period) %>%
      mutate(state = sapply(state, state_lookup), 
             start.middate = start.date) %>%
      filter(region == region_name)
   
  }else{
    b1.dat <- BETA1_TRACKING %>%
      filter(state == sel.abb[1])
    }
  b1.dat <- b1.dat  %>%
      mutate(value = ifelse(b1>2, 2, b1),
             date = as.Date(start.middate),
             metric = "Est. &#946;<sub>1</sub> (Transmission)") %>%
      filter(as.Date(date) >= as.Date(graph.start.date))
  
  if (!is.null(b1.val) && (!show_forecasts)){
    if(nrow(inter.df)>0){
      
      modeled.b1.dat <- inter.df %>%
        select(start_time, end_time, beta.step, b1, Prop_Reduction_Mild_Trans) %>%
        mutate(start_date = as.Date(graph.start.date) + ddays(start_time),
               end_date = as.Date(graph.start.date) + ddays(end_time),
               modeled.b1 = ifelse(beta.step == "proj", b1*(1-Prop_Reduction_Mild_Trans), b1 ),
               metric = "Modeled &#946;<sub>1</sub> (Transmission)"
        ) %>%
        distinct() %>%
        gather(key = "date_type", value = "date", start_date, end_date) %>%
        mutate(date = as.Date(date),
               date = ifelse(date_type == "end_date", date - ddays(1), date),
               date = as.Date(date, origin="1970-01-01")) %>%
        select(date, modeled.b1, metric) %>%
        distinct() %>%
        arrange(date)
      
      date.range <- data.frame(date = seq.Date(from=as.Date(graph.start.date), 
                                               to = as.Date(max(modeled.b1.dat$date,na.rm = TRUE)),
                                               by = "1 day"))
      
      modeled.b1.dat <- date.range %>%
        left_join(modeled.b1.dat, by = "date") %>%
        fill(modeled.b1, metric, .direction = "down") %>%
        mutate(modeled.b1 = ifelse(is.na(modeled.b1), b1.val, modeled.b1),
               metric = "Modeled &#946;<sub>1</sub> (Transmission)"
               ) %>%
        filter(!is.na(modeled.b1)) %>%
        distinct()
    }else{
      modeled.b1.dat <- data.frame(date = seq.Date(from=as.Date(graph.start.date), 
                                               to = as.Date(graph.start.date)+ddays(sim.days),
                                               by = "1 day"),
                               modeled.b1 = b1.val,
                               metric = "Modeled &#946;<sub>1</sub> (Transmission)")
    }
  }
  
  yaxis1.ops <- list(
    tickfont = list(color="#882255"),
    title = "Est &#946;<sub>1</sub> (Transmission)",
    nticks = 5,
    range = c(0, max(b1.dat$value, na.rm=TRUE)*1.20)
  )
  
  yaxis2.opts <- list(
    overlaying = "y",
    tickfont = list(color="#44AA99"),
    side = "right",
    title = "Relative Mobility (Smoothed)",
    showgrid = FALSE,
    ticksuffix = "%",
    range = c(0, max(mobil.dat$value,na.rm=TRUE)*1.30)
  )
  plot.title <- paste(region_name, "Mobility & Transmission Metrics")
  current.day <- Sys.Date()
  
  all.inter.regions <- list()
  if (nrow(inter.df)>0){
    for (i in 1:nrow(inter.df)){
      this.region <- list(type="rect",
                          x0 = as.Date(graph.start.date) + ddays(inter.df$start_time[i]),
                          x1 = as.Date(graph.start.date) + ddays(inter.df$end_time[i]),
                          y0 = 0,
                          y1 = 1,
                          line = list(color = "transparent"),
                          fillcolor = "blue",
                          xref = "x",
                          yref = "paper",
                          opacity = 0.2*inter.df$Prop_Reduction_Mild_Trans[i])
      all.inter.regions <- c(all.inter.regions, list(this.region))
    }
  }
  
  all.inter.regions <- c(all.inter.regions,
                         list(list(type="line",
                              x0=current.day,
                              x1=current.day,
                              xref="x",
                              yref= 'paper', y0= 0, y1= 1,
                              line = list(color="red")
                         )))
  
  ret.plt <- ret.plt %>%
    add_trace(data = b1.dat, x=~date, y=~value, color=~metric, type='scatter', 
              mode='lines', line=list(color="#882255",dash='solid',width = 2)) %>%
    add_trace(data = mobil.dat, x=~date, y=~value, color=~metric, type='scatter', 
              mode='lines', line=list(color="#44AA99",dash='solid',width = 2), 
              yaxis = "y2") %>%
    layout(title = list(text=plot.title,x=0,xref="paper"),
           hovermode = "x unified",
           xaxis=list(title="", range = c(as.Date(graph.start.date), as.Date(graph.start.date)+ddays(sim.days))), 
           yaxis=yaxis1.ops,
           font = list(family = "Arial"),
           yaxis2=yaxis2.opts,
           margin=list(l=45, r=200, autoexpand=FALSE),
           legend = list(x=1.08),
           shapes = all.inter.regions,
           annotations = list(x = current.day,
                              y = 1,
                              text = "<i>Today</i>",
                              font = list(color="red"),
                              xref = "x",
                              yref = "paper",
                              showarrow = FALSE,
                              xanchor="left"))
  
  if (!is.null(b1.val) && (!show_forecasts)){
    ret.plt <- ret.plt %>%
      add_trace(data = modeled.b1.dat, x=~date, y=~modeled.b1, 
                color= ~metric , type='scatter', mode='lines', line=list(color="#882255",dash='dash',width = 1.2), 
                yaxis = "y")
  }
  
  # if show_forecasts=T, eithr display the ones provided in forecast_df, or generate forecasts for chosen region
  if(show_forecasts && ('b1_lower' %in% colnames(forecast_df) )){
    if(is.null(forecast_df)) forecast_df = run_model_with_forecasts(region_name, n=30)
    
    # add mobility forecasts
    mob_model = build_mob_spline_lm(region_name, deg=2, val_days=5)
    mob_forecasts = forecast_mobility(mob_model, n=30, plt=F, add_init_offset = F)
    ret.plt <- ret.plt %>%
      add_trace(data = mob_forecasts, name='Forecasted Mobility',  x=~date, y=~y,  type='scatter', 
                mode='lines', line=list(color="#44AA99",dash='dash',width = 2), 
                yaxis = "y2") %>%
      add_ribbons(data = mob_forecasts %>% filter(date >= max(mobil.dat$date)), x=~date,ymin=~y_lower, ymax=~y_upper,
                  line = list(color = 'rgba(68,170,153, .25)'),
                  fillcolor = 'rgba(68,170,153, 0.25)',showlegend=F, 
                  yaxis="y2")
    
    # add b1 forecasts
    ret.plt <- ret.plt %>% 
      add_trace(data=forecast_df, x=~date, y=~b1, name="Forecasted &#946;<sub>1</sub> (Transmission)", type='scatter', mode='lines', 
                line=list(color="#882255", dash='dash')) %>%
      add_ribbons(data = forecast_df %>% filter(date >= max(b1.dat$date)), name="Forecasted Mobility", x=~date,ymin = ~b1_lower ,ymax = ~b1_upper,
               line = list(color = 'rgba(136,34,85, .25)'),
               fillcolor = 'rgba(136,34,85, 0.25)',showlegend=F)
    
 
  }
  
  return(ret.plt)
  
}

detect_county <- function(region_name){
  return((grepl('[cC]ounty', region_name)) || (grepl(',', region_name)))
}

draw_beta_fit_plot <- function(model_res, metric='Deaths', region_df=NULL, plt.height=NULL){
  assert_that(tolower(metric) %in% c('deaths', 'cases', 'hospits'))
  if(is.null(region_df)){
    if('region' %in% colnames(model_res)){
      region_name = model_res$region[[1]]
      region_df = get_region_plot_ts(region_name, metric=metric)
    }else{
      stop('ERROR: Need to either provide region_df or model_results with region as a column.')
    }

  }
  plt_cols <- c("#882255", "#44AA99")
  plt_line_names <- c("Est. &#946;<sub>1</sub>", "Cumulative Deaths")
  names(plt_cols) <- plt_line_names
  state_name = unique(region_df$mState.Providence)[1]
  ret.plt <- plot_ly(colors = plt_cols, height = plt.height, legendgroup='group1')
  
  # b1.dat <- BETA1_TRACKING %>%
  #   filter(state == sel.abb[1]) %>%
  #   mutate(value = ifelse(b1>2, 2, b1),
  #          date = as.Date(start.middate),
  #          metric = "Est. &#946;<sub>1</sub>") %>%
  #   filter(as.Date(date) >= as.Date(graph.start.date))
  
  yaxis1.ops <- list(
    tickfont = list(color="#882255"),
    title = "Est &#946;<sub>1</sub> (Transmission)",
    nticks = 5,
    range = c(0, max(model_res$b1, na.rm=TRUE)*1.05)
  )
  
  yaxis2.opts <- list(
    overlaying = "y",
    #tickfont = list(color="#44AA99"),
    side = "right",
    title = "Death Counts",
    showgrid = FALSE,
    range = c(0, max(region_df$value,na.rm=TRUE)*1.20)
  )
  plot.title <- paste0("Model Fit and b1 Changes for ", state_name)
  current.day <- Sys.Date()

  model_res <- model_res %>% mutate(tot_cases = I1+I2+I3+D+R)
  if(tolower(metric)=='deaths'){
    model_res$value = model_res$D
  }else if(tolower(metric)=='cases'){
    model_res$value = model_res$tot_cases
  }else{
    model_res$value = model_res$I2
  }
  if(sum(grepl('forecast', model_res$period)) > 0){
    forecast_res = model_res %>% filter(grepl('forecast', period))
    model_res = model_res %>% filter(!grepl('forecast', period))
  }else{
    forecast_res = data.frame()
  }
  ret.plt <- ret.plt %>%
    add_trace(data = model_res, x=~date, y=~b1, color=' &#946;<sub>1</sub>', type='scatter', 
              mode='lines', line=list(color="#882255",dash='solid',width = 2), legendgroup='group1') %>%
    add_trace(data = region_df, x=~date, y=~value, color=paste0('Actual ', metric), type='scatter',
              mode='markers', marker=list(color="red", size=6), 
              #mode='lines', line=list(color="red",dash='solid',width = 2), 
              yaxis = "y2", legendgroup='group2') %>%
    add_trace(data = model_res, x=~date, y=~D, color=paste0('Estimated ', metric), type='scatter',
              mode='lines', line=list(color="#44AA99",dash='solid',width = 3), 
              yaxis = "y2", legendgroup='group2')
  if(nrow(forecast_res) > 0){
    ret.plt %>%
      add_trace(data = forecast_res, x=~date, y=~b1, color='Forecasted &#946;<sub>1</sub>', type='scatter',
                mode='lines', line=list(color="#882255",dash='dash',width = 2), legendgroup='group1') %>%
      add_trace(data = forecast_res, x=~date, y=~value, color=paste0('Actual ', metric), type='scatter',
                mode='markers', line=list(color="red",dash='dotdash',width = 2),
                yaxis = "y2", legendgroup='group2') %>%
      add_trace(data = forecast_res, x=~date, y=~D, color=paste0('Estimated ', metric), type='scatter',
                mode='lines', line=list(color="#44AA99",dash='dash',width = 2),
                yaxis = "y2", legendgroup='group2')
  }
  ret.plt <- ret.plt %>% layout(title = list(text=plot.title,x=0,xref="paper"),
         hovermode = "x unified",
         xaxis=list(title="", range = c(min(region_df$date), max(model_res$date))),
         yaxis=yaxis1.ops,
         yaxis2=yaxis2.opts,
         margin=list(l=45, r=200, autoexpand=FALSE),
         legend = list(x=1.08))
  
  return(ret.plt)
  
}

# Plots a state's average positive and negative tests by week. Generates testing target based on 
# heuristic from of on avg, 45-50 per 10k people per month (https://www.statnews.com/2020/04/27/coronavirus-many-states-short-of-testing-levels-needed-for-safe-reopening/)

gen_testing_plot <- function(state_name=NULL, test_per_week_target=12, plt.height=NULL){
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
    mutate(tooltip = paste0('<i>Week of ', start_date, '</i><br> <b>Total Positive:</b> ', comma(total_positive),
                            '<br> <b>Total Negative: </b>', comma(total_negative),
                            '<br> <b>Positive Rate: </b>', percent(round(total_positive)/(total_positive + total_negative), 2),
                            '<br> <b>Avg. Tests per Day: </b>', comma(round(avg_tests_per_day))))
  
  # testing threshold table
  target_tbl = STATE_NAMES %>% left_join(
    STATE_INF_METRICS, by=c('full'='state') 
  ) %>% mutate(
    cases_per_capita = tot_cases / pop * 100000,
    daily_target = round(test_per_week_target * cases_per_capita/mean(cases_per_capita,na.rm=T) * pop/10000))
  if(!is.null(state_name)){
    regional_target = target_tbl[target_tbl$abbr==state_abbr, 'daily_target'][[1]]
    plt_title = paste0('Daily Testing Data for ', state_name)
  }else{
    # sum targerts across all states
    plt_title = paste0('United States Daily Testing Data')
    us_pop = WORLD_POP[WORLD_POP$mState.Providence=='United States', 'population']
    regional_target = round(sum(target_tbl$daily_target, na.rm=T))
  }
  ax_cols = c("#3780c4","#44AA99")
  target_line_hor = list(type = "line", x0 = min(weekly_df$start_date, na.rm=T), x1 = max(weekly_df$start_date, na.rm=T)-1,
                     y0 = regional_target, y1 = regional_target, line = list(color = 'grey', 
                                                                       width=3, dash='dash', y_ref='paper'))
  target_line_vert = list(type = "line", x0 = max(weekly_df$start_date, na.rm=T), x1 = max(weekly_df$start_date, na.rm=T),
                         y0 = 0, y1 = regional_target, line = list(color = 'grey', 
                                                                                 width=3, dash='dash', y_ref='paper'))
  
  max_date = max(weekly_df$start_date, na.rm=T)
  latest_testing = round(weekly_df %>% filter(start_date==max_date) %>% .$avg_tests_per_day)
  target_point_df = data.frame(target_date= max(weekly_df$start_date, na.rm=T),
                               value = regional_target,
                               tooltip = paste0('<i><b>Current Target (',max_date, ')</b></i>',
                                                '<br><b>Needed Tests/Day</b>: ',comma(regional_target),
                                                '<br><b>Latest Tests/Day</b>: ', comma(latest_testing)))
  crosshair = list(target_line_hor, target_line_vert)
  xaxis.ops <- list(
     showspikes=TRUE,
     title=list(text='Date'),
     range=c(min(weekly_df$start_date, na.rm=T), max(weekly_df$start_date, na.rm=T)+3)
  )
  # plot daily testing on first axis and % positive rate on second axis
  yaxis2.ops <- list(
    overlaying = "y",
    #tickfont = list(color=ax_cols[[2]]),
    color=ax_cols[[1]],
    side = "right",
    title = "Positive Rate (%)",
    showgrid = FALSE,
    ticksuffix = "%",
    range = c(0, 100)#max(weekly_df$positive_rate,na.rm=TRUE)*1.05)
  )
  
  
  fig <- plot_ly(weekly_df, x = ~start_date, y = ~avg_pos_per_day, name = 'Positive', type = 'scatter',
                 stackgroup = 'one', mode = 'lines', fillcolor='rgba(158,20,21,.3)', line=list(color='rgb(158,20,21'), 
                 hovertext=~tooltip, hoverinfo='text', height=plt.height) %>%
    add_trace(y = ~avg_neg_per_day, name = 'Negative', fillcolor = 'rgba(12,99,15,.3)',line=list(color='rgb(12,99,15')) %>%
    add_trace(stackgroup='', type='scatter', mode='lines', y = ~positive_rate, name = 'Pos. Rate (%)', yaxis='y2',
              line=list(color=ax_cols[[1]], dash='dashdot')) %>%
    layout(xaxis=xaxis.ops, 
           yaxis=list(title=list(text='Average Daily Tests by Week'),
                      range=c(0,max(1.2*regional_target, 1.05*max(weekly_df$avg_tests_per_day, na.rm=T)))),
           yaxis2=yaxis2.ops,
           font = list(family = "Arial"),
           hovermode = "closest",
           title=list(text=plt_title), 
           shapes=crosshair, 
           legend=list(orientation='h', y=1.02, x=.2), 
           margin=list(r=50, t=40)
           ) %>%
    add_trace(type='scatter', stackgroup='', data=target_point_df, x=~target_date, y=~value, mode='markers',
              marker=list(size=12, color='black'), name='Testing Target') 
  
  return(fig)
}

gen_national_tesing_plot <- function(test_per_week_target = 12){
  
  fig <- plot_ly(weekly_df, x = ~start_date, y = ~avg_pos_per_day, name = 'Positive', type = 'scatter',
                 stackgroup = 'one', mode = 'lines', fillcolor='rgba(158,20,21,.6)', line=list(color='rgb(158,20,21'), 
                 hovertext=~tooltip, hoverinfo='text') %>%
    add_trace(y = ~avg_neg_per_day, name = 'Negative', fillcolor = 'rgba(12,99,15,.6)',line=list(color='rgb(12,99,15')) %>%
    #add_trace(y = ~tot_pending, name = 'Total Pending Tests', fillcolor = 'gray') %>%
    layout(xaxis=list(showspikes=TRUE, title=list(text='Week')), 
           font = list(family = "Arial"),
           yaxis=list(title=list(text='Average Daily Cases')), 
           title=list(text=paste0('Weekly Testing Data for ', state_name)), 
           shapes=target_line)
}

gen_regional_rank_table <- function(region, fill_metric, per.capita = FALSE){
  if (region=="National"){
    ret.dat <- STATE_INF_METRICS %>%
      rename(region_key=state) %>%
      group_by(region_key) %>%
      mutate(tot_deaths = ifelse(per.capita, 100000*tot_deaths/pop, tot_deaths),
             tot_cases = ifelse(per.capita, 100000*tot_cases/pop, tot_cases),
             r_t = rt
      ) %>%
      ungroup()
  }else{
    #feed in input$chosen_state
    state_abbr <- state_lookup(region)
    ret.dat <- COUNTY_INF_STATS %>%
      separate(col=mState.Providence, into = c("County.Name", "state.abbrev"), sep = ", ", remove = FALSE) %>%
      filter(state.abbrev == state_abbr, case_count>0) %>%
      group_by(mState.Providence) %>%
      arrange(date) %>%
      top_n(n=8, wt=as.Date(date)) %>%
      mutate(lag1 = lag(case_count, n=1, default = 0),
             lag1r = ifelse(lag1==0, 0, (case_count-lag1)/lag1),
             lag2 = lag(case_count, n=2, default = 0),
             lag2r = ifelse(lag2==0, 0, (lag1-lag2)/lag2),
             lag3 = lag(case_count, n=3, default = 0),
             lag3r = ifelse(lag3==0, 0, (lag2-lag3)/lag3),
             lag4 = lag(case_count, n=4, default = 0),
             lag4r = ifelse(lag4==0, 0, (lag3-lag4)/lag4),
             lag5 = lag(case_count, n=5, default = 0),
             lag5r = ifelse(lag5==0, 0, (lag4-lag5)/lag5),
             doub_lag_denom = (lag1r > 0) + (lag2r > 0) + (lag3r > 0) + (lag4r > 0) + (lag5r > 0),
             growth.rate.all = (lag1r + lag2r + lag3r + lag4r + lag5r)/doub_lag_denom,
             doubling_time = 70/(100*growth.rate.all),
             doubling_time = ifelse(doubling_time==Inf, NA, doubling_time),
             tot_cases = max(case_count, na.rm = TRUE),
             tot_cases = ifelse(is.na(tot_cases), 0, tot_cases),
             tot_deaths = max(death_count,na.rm = TRUE),
             tot_deaths = ifelse(is.na(tot_deaths), 0, tot_deaths)
      ) %>%
      filter(date==max(date,na.rm = TRUE)) %>%
      ungroup() %>%
      select(mState.Providence, County.Name, State = state.abbrev, date, tot_cases, tot_deaths, doubling_time) %>%
      left_join(COUNTY_CLEANUP %>%
                  filter(abbr == state_abbr), 
                by = c("County.Name"="County.Pretty")) %>%
      mutate(
        county.mid = sapply(county.ugly, simpleCap),
        pop.mState.key = ifelse(paste(county.ugly, State, sep=", ")==mState.Providence, 
                                paste(county.ugly, State, sep=", "), 
                                mState.Providence)) %>%
      left_join(select(WORLD_POP, mState.Providence, population), 
                by = c("pop.mState.key"="mState.Providence")) %>%
      left_join(RT %>% 
                  rename(region_key = region) %>%
                  filter(state==region) %>%
                  arrange(region_key, date) %>%
                  group_by(region_key) %>%
                  dplyr::summarize(r_t = last(mean), RT_date= max(date)) %>%
                  ungroup() %>%
                  mutate(rt = r_t), 
                by=c("mState.Providence"="region_key")) %>%
      distinct() %>% 
      group_by(County.Name) %>%
      mutate(county.ugly2 = tolower(county.ugly),
             region_key = str_replace_all(County.Name, "County", "Cty."),
             tot_deaths = ifelse(per.capita, 100000*tot_deaths/population, tot_deaths),
             tot_cases = ifelse(per.capita, 100000*tot_cases/population, tot_cases)) %>%
      ungroup() %>%
      filter(State==state_abbr, !grepl('Cruise Ship', mState.Providence), !grepl('Unallocated',  mState.Providence))
    
  }
  
  col_digs = list('region_key'=-1, 'tot_deaths'=0,'tot_cases'=0,'doubling_time'=1,'rt'=2, 'r_t'=2)
  name_map = c('region_key'= 'Region','tot_deaths'='Total Deaths', 'tot_cases'='Total Cases',
                  'rt'='Reproduction Rate', 'r_t'='Reproduction Rate', 'doubling_time'='Doubling Time')
  name_map2 = list('Region'='region_key','Total Deaths'='tot_deaths', 'Total Cases'='tot_cases',
                   'Reproduction Rate'='rt', 'Reproduction Rate'='r_t', 'Doubling Time'='doubling_time')
  
  second_metric = switch(fill_metric, 'tot_cases'='tot_deaths', 'tot_cases')
  decreasing_order = (fill_metric != 'doubling_time')
  
  selected.cols <- c('region_key', fill_metric, second_metric)
  selected.cols <- as.vector(sapply(selected.cols, function(x){return(as.character(name_map[[x]]))}))
  
  ret.dat <- ret.dat %>%
    mutate(tot_deaths = round(tot_deaths, col_digs[["tot_deaths"]]),
           tot_cases = round(tot_cases, col_digs[["tot_cases"]]),
           doubling_time = round(doubling_time, col_digs[["doubling_time"]]),
           rt = round(rt, col_digs[["rt"]]),
           r_t = round(r_t, col_digs[["r_t"]])) %>%
    mutate(main_metric = get(fill_metric),
           secondary_metric = get(second_metric)) 
  
  if (decreasing_order){
    ret.dat <- ret.dat %>%
      select(Region = region_key, main_metric, secondary_metric) %>%
      arrange(desc(main_metric)) %>%
      head(5)
  }else{
    ret.dat <- ret.dat %>%
      select(Region = region_key, main_metric, secondary_metric) %>%
      arrange(main_metric) %>%
      head(5)
  }
  
  # add commas if necessary
  if(col_digs[[fill_metric]] == 0){
    ret.dat = ret.dat %>% mutate(main_metric = comma(main_metric))
  }
  if(col_digs[[second_metric]] == 0){
    ret.dat = ret.dat %>% mutate(secondary_metric = comma(secondary_metric))
  }

  ret.dat = ret.dat %>%
    mutate(main_metric = as.character(main_metric),
         secondary_metric = as.character(secondary_metric))
  
  names(ret.dat) <- selected.cols
  return(ret.dat)
}

build_regional_rank_table <- function(df, sort_metric='tot_deaths', col_digs=NULL, name_map=NULL){
  
  if(is.null(col_digs)){
    col_digs = list('region_key'=-1, 'tot_deaths'=0,'tot_cases'=0,'doubling_time'=1,'rt'=2, 'r_t'=2)
  }
  if(is.null(name_map)){
    name_map = list('region_key'= 'Region','tot_deaths'='Total Deaths', 'tot_cases'='Total Cases',
                    'rt'='Reproduction Rate', 'r_t'='Reproduction Rate', 'doubling_time'='Doubling Time')
  }
  
  second_metric = switch(sort_metric, 'tot_cases'='tot_deaths', 'tot_cases')
  decreasing_order = (sort_metric != 'doubling_time')
  
  # abbreviate names when appropriate to save space.
  if('region_key' %in% colnames(df)){
    all_cols = c('region_key', sort_metric, second_metric)
    df$region_key = sapply(df$region_key, function(x){gsub('County', 'Cty.', x)})
  }else{
    df$region_key <- df$state
    all_cols = c('region_key', sort_metric, second_metric)
  }
  
  rank_df = df[order(df[,sort_metric], decreasing=decreasing_order),all_cols] %>% distinct()
  
  for(col in colnames(rank_df)){
    if(col_digs[[col]] >= 0){
      digs = col_digs[[col]]
      rank_df[,col] = round(rank_df[,col], digs)
    }
  }
  colnames(rank_df) = sapply(colnames(rank_df), function(col){name_map[[col]]})
  return(rank_df)
}