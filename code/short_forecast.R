##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: Plotting functions for the short-term forecast tab.
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

#COUNTY_FORECAST_LIST <- read.csv('data/county_info.csv', colClasses=c("countyFIPS"="character"), as.is = TRUE)
source_python("code/short_model_v2.py")
## Support Functions
lastMonday <- function(x) x - as.numeric(x-1+4)%%7
isSingleString <- function(input) {
  is.character(input) & length(input) == 1
}


## Global Variables
COUNTY_FORECAST_LIST <- read.csv('data/county_info.csv', colClasses=c("countyFIPS"="character"), as.is = TRUE)
cleantable <- read.csv('data/table_template.csv')
cleantable <- cleantable %>%
  select(
    State=state,
    County=county,
    Date=date,
    Item=item,
    'Low Estimate'=low,
    'Mean Estimate'=mean,
    'High Estimate'=high
  )
cleantable <- cleantable[0:0,]



item_selector <- c("Surg/Proc. Mask"="Surg/Proc. Mask", 
                   "N95 Respirator"="N95 Respirator", 
                   "Face Shield"="Face Shield", 
                   "Goggles"="Goggles", 
                   "Sterile Exam Gloves"="Sterile Exam Gloves",
                   "Non-sterile Exam Gloves"="Non-sterile Exam Gloves",
                   "Bouffant"="Bouffant",
                   "Shoe Covers"="Shoe Covers",
                   "Isolation Gown"="Isolation Gown")

item_selector <- sort(item_selector)

item_dict <- list(
  'Surg/Proc. Mask' = 'isolation_mask',
  'N95 Respirator' = 'n95_respirator',
  'Isolation Gown' = 'isolation_gown',
  'Face Shield' = 'face_shield',
  'Goggles' = 'goggles',
  'Sterile Exam Gloves' = 'sterile_exam_gloves',
  'Non-sterile Exam Gloves' = 'non-sterile_exam_gloves',
  'Bouffant' = 'bouffant',
  'Shoe Covers' = 'shoe_covers'
)



draw_forecast <- function(data, forecast_plot_choices) {
  ## Plotly Visualizations
  
  # Styling Parameters
  color_palette <- list()
  
  # Data Cleaning
  
  daily_data <- data %>%
    filter(
      date >= Sys.Date() - 60
    )
  
  daily_data_short <- daily_data %>%
    filter(
      y >= 0
    )
  
  # TO DO: Add pop_non_crit_care, pop_crit_care, and pop_crit_care_vent to plot
  
  # Predicated Cases Plot
  
  if (length(forecast_plot_choices) > 0) {
    plot1 <- plot_ly(type = "scatter", mode = "markers")
    if ("f.cases" %in% forecast_plot_choices) {
        plot1 <- plot1 %>% 
          add_trace(x=daily_data[["date"]],
                y=daily_data[["y_pred"]],
                type = 'scatter',
                mode='lines',
                name='Forecasted Cumulative Cases') 
    }
    if ("a.cases" %in% forecast_plot_choices) {
      plot1 <- plot1 %>% 
      add_trace(x=daily_data_short[["date"]],
                 y=daily_data_short[["y"]],
                 mode='markers',
                 name='Actual Cumulative Cases') 
    }
    if ("f.hosp" %in% forecast_plot_choices) {
      plot1 <- plot1 %>% 
      add_trace(x=daily_data[["date"]],
                y=daily_data[["pop_non_crit_care"]],
                type = 'scatter',
                mode='lines',
                name='Forecasted Hospitalized (Non-Critical) Population') 
    }
    if ("f.critical" %in% forecast_plot_choices) {
      plot1 <- plot1 %>% 
      add_trace(x=daily_data[["date"]],
                y=daily_data[["pop_crit_care"]],
                type = 'scatter',
                mode='lines',
                name='Forecasted Critical Care Population') 
    }
    if ("f.vent" %in% forecast_plot_choices) {
      plot1 <- plot1 %>% 
      add_trace(x=daily_data[["date"]],
                y=daily_data[["pop_crit_care_vent"]],
                type = 'scatter',
                mode='lines',
                name='Forecasted Ventilator Population') 
    }
    plt_title = 'COVID-19 Short Term Forecast'
    if('region' %in% colnames(data)) plt_title = paste0(plt_title, ' for ', data[1,'region'])
    plot1 <- plot1 %>% 
      layout(title=plt_title,
             xaxis=list(title = ""),
             yaxis=list(title="Count"), 
             margin = list(t=42,b=57, pad=3))
  } 
  
  return(plot1)
}
