##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: App file for R shiny dashboard
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(ggthemes)
library(shinyjs)
library(waiter)
library(shinycssloaders)
library(dplyr)
library(reshape2)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(maps)
library(timevis)
library(googlesheets4)
sheets_deauth()
library(knitr)
library(sodium)
library(Hmisc)
library(V8)
library(DT)
library(shinyBS)
library(htmltools)
library(reticulate)
library(leaflet)
library(sf)
library(leaflet.extras)
library(plotly)

options(shiny.sanitize.errors = FALSE)
source('code/plotting_functions.R')
source('code/npi_definitions.R')
source('code/comp_plots_functions.R')
source('code/ode_functions.R')
#source('code/login_setup.R')
source('code/optimizer.R')
source('code/similarity_functions.R')
source('code/ui_dev.R')
source('code/short_forecast.R')
source('code/tuning_moving_window.R')
source('code/SAH_scatterplots.R')
source('code/hotspot_detection.R')
source('code/mortality_rates.R')


# #=========DELETE WHEN DONE DEBUGGING=========#
# # tell shiny to log all reactivity
# library(reactlog)


## Info button tooltip functions
# source: https://stackoverflow.com/questions/36132204/reactive-radiobuttons-with-tooltipbs-in-shiny
radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choice, "') {
            opts = $.extend(", options, ", {html: true});
            $(this.parentElement).tooltip('destroy');
            $(this.parentElement).tooltip(opts);
          }
        })
      }, 500)
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

buildSelectizeTooltipOrPopover <- function(options, type, id, choice){
  
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      var opts = $.extend(", options, ", {html: true});
      var selectizeParent = document.getElementById('", id, "').parentElement;
      var observer = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation){
          $(mutation.addedNodes).filter('div').filter(function(){return(this.getAttribute('data-value') == '", choice, "');}).each(function() {
            $(this).", type, "('destroy');
            $(this).", type, "(opts);
          });
        });
      });
      observer.observe(selectizeParent, { subtree: true, childList: true });
      
      selectizeObserverDict['", type, "' + '", id, "' + '", choice, "'] = observer;
    });
  ")))
  
  htmltools::attachDependencies(bsTag, shinyBSDep)
}

## User Interface
header <- dashboardHeader(
  title="COVID-19 Dashboard"
)

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 

body <- dashboardBody(
  #shinyjs::useShinyjs(),
  use_waiter(),
  waiter_show_on_load(spin_fading_circles()),
  #extendShinyjs(text = login_js),
  uiOutput("body"),
  waiter_hide_on_render('body'))

app_sidebar <- sidebarMenu(
  id='sidebarTabs',
  menuItem(text = "Introduction", tabName = NULL, startExpanded = TRUE,
    menuSubItem(text = "Home", tabName = "caveat", icon=icon("user-cog"), selected = TRUE),
    menuSubItem(text = "Sources",  tabName = "sources", icon=icon("bookmark")),
    menuSubItem(text = "Definitions", tabName = "definitions", icon=icon("info"))
  ),
  menuItem(text = "Data", tabName = NULL, startExpanded = TRUE,
    menuSubItem(text = "C19HCC Decision Dashboard", icon=icon("external-link-alt"), href = "https://c19hcc.org/resources/decision-support/"),
    menuSubItem(text = "Regional Data", tabName = "region_spread", icon=icon("map-marked-alt")),
    menuSubItem(text = "Region Comparison Tool", tabName = "region_comparison", icon=icon("chart-area")),
    menuSubItem(text = "Similar Regions Tool", tabName = "region_similar", icon=icon("globe")),
    menuSubItem(text = "State Comparison Analysis", tabName = "region_sah_comp", icon=icon("home")),
    menuSubItem(text = "NPI Analysis", icon=icon("external-link-alt"), href = "https://c19hcc.org/resources/npi-dashboard/"),
    menuSubItem(text = "Mortality Analysis", tabName = "mortality", icon=icon("users"))
    ),
  menuItem(text = "Models and Forecasts", tabName = NULL, startExpanded = TRUE,
    menuSubItem(text = "About the Models", tabName = "model", icon=icon("gears")),
    menuSubItem(text = "Short-Term Forecast", tabName = "forecast", icon = icon("chart-bar")),
    menuSubItem(text = "Intervention Model", tabName = "npi_impact", icon=icon("tasks")),
    menuSubItem(text = "Advanced Model Settings", tabName = "adv_settings", icon=icon("code")),
    menuItem(text = "Model Comparison Tool", tabName = "model_comparison", icon=icon("exchange-alt", lib = "font-awesome")),
    menuSubItem(text = "Hotspot Identification", tabName = "hotspot", icon = icon("fire")),
    menuSubItem(text = "PPE & Pharma Model", icon=icon("external-link-alt"), href = "http://dashboards.c19hcc.org/ppe/")#,
    #menuSubItem(text="System Dynamics Model",icon=icon("external-link-alt"), href = "https://exchange.iseesystems.com/public/stephanieglasser/covid-npi/index.html#page1" )
    ),
    
  menuItem(text = tags$span("COVID-19 Healthcare Coalition", style = "z-index: 5000; font-family: trade-gothic-next-condensed, sans-serif; font-size: 16px;"), icon=icon("external-link-alt"), href = "https://c19hcc.org/")
)

app_body <- div(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom_style.css"),
      tags$link(rel = "stylesheet", type = "text/css", href="https://use.typekit.net/rrd2zlt.css"),
      useShinyjs(),
      useShinydashboard(),
      tags$script('
      var dimension = [0, 0];
      $(document).on("shiny:connected", function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
      });
      $(window).resize(function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
      });'),
      tags$style("
      #HospBedper.form-control.shiny-bound-input {display: none;}
      #HospBedOcc.form-control.shiny-bound-input {display: none;}
      #ICUBedper.form-control.shiny-bound-input {display: none;}
      #ICUBedOcc.form-control.shiny-bound-input {display: none;}
      #ConvMVCap.form-control.shiny-bound-input {display: none;}
      #ContMVCap.form-control.shiny-bound-input {display: none;}
      #CrisisMVCap.form-control.shiny-bound-input {display: none;}
      #npi\\.sc\\.prop\\.mild.form-control.shiny-bound-input {display: none;}
      #npi\\.nbc\\.prop\\.mild.form-control.shiny-bound-input {display: none;}
      #npi\\.bar\\.prop\\.mild.form-control.shiny-bound-input {display: none;}
      #npi\\.lgb\\.prop\\.mild.form-control.shiny-bound-input {display: none;}
      #npi\\.mqt\\.prop\\.mild.form-control.shiny-bound-input {display: none;}
      #npi\\.sah\\.prop\\.mild.form-control.shiny-bound-input {display: none;}
      #npi\\.mm\\.prop\\.mild.form-control.shiny-bound-input {display: none;}
      #npi\\.rev\\.prop\\.mild.form-control.shiny-bound-input {display: none;}

      #adv_settings_eff_box {display: none;}
      #npi_param_table tr, td {margin-top: 0px;
                              margin-bottom: 0px;
                              padding-top: 0px;
                              padding-bottom: 0px;}
      #npi_param_table.shiny-input-container {margin-top: 0px;
                              margin-bottom: 0px;
                              padding-top: 0px;
                              padding-bottom: 0px;}
      #npi_param_table.form-group {margin-top: 0px;
                              margin-bottom: 0px;
                              padding-top: 0px;
                              padding-bottom: 0px;}
      #npi_param_table.form-group.shiny-input-container {margin-top: 0px;
                              margin-bottom: 0px;
                              padding-top: 0px;
                              padding-bottom: 0px;}
      #npi_param_table.form-control.shiny-bound-input {margin-top: 0px;
                              margin-bottom: 0px;
                              padding-top: 0px;
                              padding-bottom: 0px;}
      .custom-footer{
                            font-size: 1.1em;
                            margin-right: 5px;
                            margin-bottom: 5px;
                            position: fixed;
                            background-color: #FFF;
                            bottom: 0;
                            right: 0;
                            z-index: 501;
                            text-anchor: end;
      }
      
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
          color: white !important;
      }
      
                 ")
    ),
    tags$head(tags$script('
                        var width = 0;
                        $(document).on("shiny:connected", function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        $(window).resize(function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        ')),
    HTML("<span class = 'custom-footer'><b>&copy; 2020, The MITRE Corporation</b></span>"),
    tabItems(
      tabItem(tabName = "region_spread",
              fluidRow(
                div(
                  tags$h4(tags$b('Summary Type: ')),
                  div( radioGroupButtons('summary_type',label=NULL, c("Spread" = "Spread","Resources" = "Resources"),
                                         selected='Spread', individual=T, direction='horizontal')),
                  tags$h4(tags$b('View: ')),
                  radioGroupButtons('granularity',label=NULL, c("National" = "National","State" = "State"),
                                    selected='National', individual=T, direction='horizontal'),
                  hidden(div(id='spread_state_and_county',
                      tags$h4(tags$b('State: ')),
                      div(selectInput(inputId = "chosen_state", label=NULL, choices = STATE_NAMES$full, selected = c("California"), multiple = FALSE),
                        style='width: 120px; margin-left:10px; margin-right: 20px;'),

                      tags$h4(tags$b('County: ')),
                      div(uiOutput("spread.selector.county.ui"), style='width: 160px; margin-left:10px; margin-right: 20px'),

                      style='display: flex;'
                  )),
                  style='display: flex; margin-left: 40px;'
                )
              ),
              fluidPage(
                div(
                  div(uiOutput('region_title'), style='display: flex', class='spread-title', style='font-size:25px;'),
                  div(id='per_capita_div',
                      h4(strong('Per 100,000 ')),
                      actionButton("q_per_cap", label = "", icon = icon("info-circle"), style='padding:1px; font-size:120%; border:0; color: #7e8284; background-color: transparent'),
                      bsTooltip("q_per_cap", "Applies per 100,000  calculation to relevant statistics boxes below", "right", options = list(container = "body")),
                      div(
                        materialSwitch(inputId="per_capita_stats", value = FALSE, inline = TRUE, status = "info")
                      ),
                      style='display: flex; margin-left: 20px; margin-right: 20px;'
                  ),
                  style='display: flex;'
                ),
                fluidRow(
                  uiOutput('calloutbox1'),
                  uiOutput('calloutbox2'),
                  uiOutput('calloutbox3'),
                  uiOutput('calloutbox4'),
                  uiOutput('calloutbox5'),
                  uiOutput('calloutbox6')
                ),
                fluidRow(
                    column(width=2,
                       box(width=NULL,
                           title = 'Map Data',
                           radioButtons('geo_metric', 'Select a map metric to view',
                                        choiceNames = c('Cumulative Cases \u24d8',
                                                        'Cumulative Deaths \u24d8',
                                                        'Cumulative Cases per 100,000 \u24d8',
                                                        'Cumulative Deaths per 100,000 \u24d8',
                                                        'Doubling Time \u24d8',
                                                        'Reproduction Rate \u24d8'
                                                        ),
                                        choiceValues = c("Cumulative Cases",
                                                         'Cumulative Deaths',
                                                         'Cumulative Cases per 100,000',
                                                         'Cumulative Deaths per 100,000',
                                                         "Doubling Time",
                                                         'Reproduction Rate'),
                                        selected=c('Cumulative Cases')),
                           radioTooltip(id = "geo_metric", choice = "Cumulative Cases", title = "Total reported cases.  Source: USAFacts.com", placement = "right", trigger = "hover"),
                           radioTooltip(id = "geo_metric", choice = "Cumulative Deaths", title = "Total reported deaths.  Source: USAFacts.com", placement = "right", trigger = "hover"),
                           radioTooltip(id = "geo_metric", choice = "Cumulative Cases per 100,000", title = "Total reported cases per 100,000 inhabitants.  Source: USAFacts.com", placement = "right", trigger = "hover"),
                           radioTooltip(id = "geo_metric", choice = "Cumulative Deaths per 100,000", title = "Total reported deaths per 100,000 inhabitants.  Source: USAFacts.com", placement = "right", trigger = "hover"),
                           radioTooltip(id = "geo_metric", choice = "Doubling Time", title = "Time it would take for the number of cases to double.  This is a useful metric during rapid growth phases.", placement = "right", trigger = "hover"),
                           radioTooltip(id = "geo_metric", choice = "Reproduction Rate", title = "Time varying reproduction rate, computed using the approach developed at rt.live. Updated weekly.", placement = "right", trigger = "hover"),
                           hr(),
                           div(id='show_hosps_div',
                               h5(strong('Show Hospitals')),
                               tags$i('Show hospital locations and resources for chosen state.'),
                               materialSwitch(inputId="show_hosps", value = FALSE, inline = TRUE, status = "info"),
                               style='margin-left: 10px; margin-right: 20px;'
                           )

                       )
                  ),
                  column(width=7,
                         div(uiOutput("regionalplottitle"), align = "center", style = "font-size:20px; background-color:#ffffff"),
                         withSpinner(leafletOutput("regionalmap", height = "400px"))
                         ),
                  column(width=3,
                        uiOutput('regional_rank_table_ui'),
                        uiOutput("spread_county_disclaimer")
                        )
              ),
              fluidRow(
                column(width=2,
                  box(width=NULL,
                      title = 'Chart Data',
                      radioButtons('plot_metric', 'Select a chart metric to view',
                                   choiceNames = c('Cases (New and Total) \u24d8',
                                                   'Deaths (New and Total) \u24d8',
                                                   'Doubling Time \u24d8',
                                                   'Reproduction Rate \u24d8',
                                                   'Testing \u24d8',
                                                   'Mobility \u24d8',
                                                   'Hospitalizations \U24D8'
                                                   ),
                                   choiceValues=c('Cases (New and Total)',
                                              'Deaths (New and Total)',
                                              'Doubling Time',
                                              'Reproduction Rate',
                                              'Testing',
                                              'Mobility',
                                              'Hospitalizations'),
                                   selected=c('Cases (New and Total)')),
                      radioTooltip(id = "plot_metric", choice = "Cases (New and Total)", title = "New and total cases, by day of report.  Source: USAFacts.com", placement = "right", trigger = "hover"),
                      radioTooltip(id = "plot_metric", choice = "Deaths (New and Total)", title = "New and total deaths, by day of report.  Source: USAFacts.com", placement = "right", trigger = "hover"),
                      radioTooltip(id = "plot_metric", choice = "Doubling Time", title = "Time it would take for the number of cases to double.  This is a useful metric during rapid growth phases.", placement = "right", trigger = "hover"),
                      radioTooltip(id = "plot_metric", choice = "Reproduction Rate", title = "Time varying reproduction rate, computed using the approach developed at rt.live. Updated weekly.", placement = "right", trigger = "hover"),
                      radioTooltip(id = "plot_metric", choice = "Testing", title = "Testing data from covidtracking.com, taken as states report the data.  There is no guarantee this only includes rt-pcr testing.", placement = "right", trigger = "hover"),
                      radioTooltip(id = "plot_metric", choice = "Mobility", title = "Mobility changes in response to COVID-19, measured as the % of the median weekday mobility the week of Feb 16th, as captured by mobile device location services. Source: Descartes Labs", placement = "right", trigger = "hover"),
                      radioTooltip(id = "plot_metric", choice = "Hospitalizations", title = "Proportion of patients in inpatient care locations who have suspected or confirmed COVID-19. Only available at the state and national levels. Source: CDC National Healthcare Safety Network (NHSN)", placement = "right", trigger = "hover"),

                      hr()
                  )
                ),
                column(width=7,
                       withSpinner(plotlyOutput('regionalts', height="600px"))
                       ),

                column(width=3,
                       uiOutput("regional_npi_table_ui"))
            )
        )
      ),
      tabItem(tabName = "region_comparison",
              fluidPage(
                style="height: 1100px;",

                fluidRow(
                  column(width=3,
                         div(style="display: inline-block;", tags$h3("Regional Comparison")),
                          actionButton("q1",
                          label = "",
                          icon = icon("info-circle"),
                          style='padding:1px; font-size:80%; border:0; color: #7e8284; background-color: transparent'),
                          bsTooltip("q1", "Select any country, state, county, or metro area for comparison, and then select which data to compare and how to which to align  If the data are not available, no data will be displayed for that region.", "left",
                          options = list(container = "body")),
                         h4("Select Region(s) for Comparison"),
                         fluidRow(
                           column(width = 6,
                                  uiOutput(outputId = "comp_countries_ui"),
                                  uiOutput(outputId = "comp_states_ui")),
                           column(width = 6,
                                  uiOutput("comp_metros_ui"),
                                  uiOutput("comp_counties_ui"))
                         ),
                         materialSwitch(inputId="comp_overlay_npis", label = tags$b("Overlay Region-Specific NPIs"), value = TRUE, inline = TRUE, status = "info"),
                         br(),
                         box(title = "Data to Graph",
                             radioButtons(inputId = "comp_yaxis", label = "Select one of the following options to display.",
                                          choiceNames = c("Cumulative Cases/Deaths \u24d8",
                                                      "Cumulative Cases/Deaths (Log) \u24d8",
                                                      "New Daily Cases/Deaths (smoothed) \u24d8",
                                                      "Case/Death Incidence per 100,000 \u24d8",
                                                      "New Daily Cases/Deaths per 100,000 (smoothed) \u24d8",
                                                      "Daily Growth Rate \u24d8",
                                                      "Cumulative Growth Rate % (smoothed) \u24d8",
                                                      "Doubling Rate \u24d8",
                                                      "Mobility \u24d8",
                                                      "Reproduction Rate \u24d8",
                                                      'Test Positivity \u24d8',
                                                      'Hospitalizations \u24d8'
                                                      ),
                                          choiceValues = c("raw.vals", "log10", "smoothed.delta","per.million", "per.million.daily",
                                                           "daily.rate",  "smoothed.rate", "doubling.days", "mobility", "R_t", "testing",
                                                           "hospitalizations"),
                                          selected = c("per.million.daily")),
                             radioTooltip(id = "comp_yaxis", choice = "raw.vals", title = "New and total cases, by day of report on a linear axis.  Source: USAFacts.com", placement = "right", trigger = "hover"),
                             radioTooltip(id = "comp_yaxis", choice = "log10", title = "New and total deaths, by day of report on a logarithmic axis.  Source: USAFacts.com", placement = "right", trigger = "hover"),
                             radioTooltip(id = "comp_yaxis", choice = "smoothed.delta", title = "The daily change in reported cases and deaths.", placement = "right", trigger = "hover"),
                             radioTooltip(id = "comp_yaxis", choice = "per.million", title = "Cases and Deaths per 100,000 inhabitants of the selected region.", placement = "right", trigger = "hover"),
                             radioTooltip(id = "comp_yaxis", choice = "per.million.daily", title = "The daily change in reported cases and deaths per 100,000 inhabitants of the selected region.", placement = "right", trigger = "hover"),
                             radioTooltip(id = "comp_yaxis", choice = "daily.rate", title = "The percentage change in the new daily cases/deaths.", placement = "right", trigger = "hover"),
                             radioTooltip(id = "comp_yaxis", choice = "smoothed.rate", title = "The percentage change in cumulative cases/deaths.", placement = "right", trigger = "hover"),
                             radioTooltip(id = "comp_yaxis", choice = "doubling.days", title = "Number of days it would take for the number of cases to double based on a recent trajectory.  This is a useful metric during rapid growth phases.", placement = "right", trigger = "hover"),
                             radioTooltip(id = "comp_yaxis", choice = "mobility", title = "Mobility changes in response to COVID-19, measured as the % of the median weekday mobility the week of Feb 16th, as captured by mobile device location services. Source: Descartes Labs", placement = "right", trigger = "hover"),
                             radioTooltip(id = "comp_yaxis", choice = "R_t", title = "Time varying reproduction rate, computed using the approach developed at rt.live. Updated weekly.", placement = "right", trigger = "hover"),
                             radioTooltip(id = "comp_yaxis", choice = "testing", title = "The test positivity rate from covidtracking.com, taken as states report the data. ", placement = "right", trigger = "hover"),
                             radioTooltip(id = "comp_yaxis", choice = "hospitalizations", title = "Proportion of patients in inpatient care locations who have suspected or confirmed COVID-19. Only available at the state and national levels. Source: CDC National Healthcare Safety Network (NHSN)", placement = "right", trigger = "hover"),
                             width = "100%"
                         ),
                         box(title = "Align Regions",
                             radioButtons(inputId = "comp_xaxis", label = "Select one of the following to align the data.",
                                          choices = c("By Date"="all.dates", "From First Case"="first.case",
                                                      "From First 100 Cases"="first.100", "From First 20 Cases"="first.20",
                                                      "From First Death"="first.death",
                                                      "From Hitting 0.2 Deaths per 100,000" = "2.deaths.per.mil",
                                                      "From Stay at Home Order" = "sah.order"),
                                          selected=c("From Hitting 0.2 Deaths per 100,000" = "2.deaths.per.mil")),
                             width = "100%"
                         ),
                         downloadButton(outputId='downloadRegionCompData', label = 'Download Data', class='download_button')
                  ),
                column(width=9,
                       uiOutput("region.comp.disclaimer"),
                         tags$div(
                           plotlyOutput("comparison_plots2"),
                           tags$span(class="fa fa-user-alt-slash", style="position: absolute; top: 10px; right: 5px; font-size: 2em; opacity: 0.2; z-index:500;"),
                           tags$span(class="fa fa-hospital", style="position: absolute; top: 610px; right: 5px; font-size: 2em; opacity: 0.2; z-index:500;"),
                           style="position: relative;")
                       )
               )
              )
      ),
      tabItem(tabName = "region_similar",
              tags$head(tags$style(".modal-dialog{ width:800px}")),
              fluidRow(column(width=3, h3("Similar Regions")),
                       column(width=6, uiOutput("sim.warning.ui"))
                       ),
              fluidRow(
                column(width=3,
                       tags$div("This tool automatically generates a list of regions (counties, states, metro areas, and nations) that are similar to the selected region. Regions with fewer than two weeks of at least 20 cases are excluded. Definitions of the different types of similarity can be found below."),
                       hr(),
                       h4("Select Region and Similarity Type"),
                       fluidRow(
                         column(width = 6, uiOutput(outputId = "sim_region_ui_state")),
                         column(width = 6, uiOutput(outputId = "sim_region_ui_county"))
                       ),
                       fluidRow(
                         column(width = 6, uiOutput(outputId = "similarity.type.ui")),
                         column(width = 6, uiOutput(outputId = "demog.similarity.type.ui"))
                       ),
                       materialSwitch(inputId="sim_ahead_only", label = "Only Include Regions 1+ Week Ahead", value = FALSE, inline = TRUE, status = "info"),
                       materialSwitch(inputId="sim_county_only", label = "Only Include US Counties", value = TRUE, inline = TRUE, status = "info"),
                       materialSwitch(inputId="sim_overlay_npis", label = "Overlay Region-Specific NPIs", value = TRUE, inline = TRUE, status = "info"),
                       hr(),
                       box(title = "Data to Graph",
                           radioButtons(inputId = "sim_yaxis", label = "Select one of the following options to display.",
                                        choiceNames = c("Cumulative Cases/Deaths \u24d8",
                                                        "Cumulative Cases/Deaths (Log) \u24d8",
                                                        "New Daily Cases/Deaths (smoothed) \u24d8",
                                                        "Case/Death Incidence per 100,000 \u24d8",
                                                        "New Daily Cases/Deaths per 100,000 (smoothed) \u24d8",
                                                        "Daily Growth Rate \u24d8",
                                                        "Cumulative Growth Rate % (smoothed) \u24d8",
                                                        "Doubling Rate \u24d8",
                                                        "Mobility \u24d8",
                                                        "Reproduction Rate \u24d8"#,
                                                       # "Test Positivity \u24d8"
                                        ),
                                        choiceValues = c("raw.vals", "log10", "smoothed.delta","per.million", "per.million.daily", "daily.rate",  "smoothed.rate", "doubling.days",
                                                         "mobility", "R_t"
                                                         #"testing"
                                                         ),
                                        selected = c("Cumulative Cases (Log)"="log10")),
                           radioTooltip(id = "sim_yaxis", choice = "raw.vals", title = "New and total cases, by day of report on a linear axis.  Source: USAFacts.com", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sim_yaxis", choice = "log10", title = "New and total deaths, by day of report on a logarithmic axis.  Source: USAFacts.com", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sim_yaxis", choice = "smoothed.delta", title = "The daily change in reported cases and deaths.", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sim_yaxis", choice = "per.million", title = "Cases and Deaths per 100,000 inhabitants of the selected region.", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sim_yaxis", choice = "per.million.daily", title = "The daily change in reported cases and deaths per 100,000 inhabitants of the selected region.", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sim_yaxis", choice = "daily.rate", title = "The percentage change in the new daily cases/deaths.", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sim_yaxis", choice = "smoothed.rate", title = "The percentage change in cumulative cases/deaths.", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sim_yaxis", choice = "doubling.days", title = "Number of days it would take for the number of cases to double based on a recent trajectory.  This is a useful metric during rapid growth phases.", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sim_yaxis", choice = "mobility", title = "Mobility changes in response to COVID-19, measured as the % of the median weekday mobility the week of Feb 16th, as captured by mobile device location services. Source: Descartes Labs", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sim_yaxis", choice = "R_t", title = "Time varying reproduction rate, computed using the approach developed at rt.live. Updated weekly.", placement = "right", trigger = "hover"),
                        #   radioTooltip(id = "sim_yaxis", choice = "testing", title = "The test positivity rate from covidtracking.com, taken as states report the data. ", placement = "right", trigger = "hover"),
                           width = "100%"
                       ),
                       box(title = "Align Regions",
                         radioButtons(inputId = "sim_xaxis", label = "Select one of the following to align the data.",
                                    choices = c("By Date"="all.dates", "From First Case"="first.case",
                                                "From First 100 Cases"="first.100", "From First 20 Cases" = "first.20",
                                                "From First 20 Cases, with Other Region Alignment" = "first.20.shift",
                                                "From Hitting 300 Cases/Million, with Other Region Alignment" = "300.cases.per.mil.shift",
                                                "From First Death"="first.death",
                                                "From Hitting 0.2 Deaths per 100,000" = "2.deaths.per.mil",
                                                "From Stay at Home Order" = "sah.order"),
                                    #multiple = FALSE,
                                    selected=c("From First 20 Cases, with Other Region Alignment" = "first.20.shift")),
                         tags$i("\"Other Region Alignment\" shifts the x axis for each region by up to 14 days to best align the curves to the selected region, maximizing their similarity."),
                         width = "100%"
                         )
                       ),
                column(width=6,
                       tags$div(
                         plotlyOutput("sim_plot_cases2", height = "30%"),
                         tags$span(class="fa fa-hospital", style="position: absolute; top: 10px; right: 5px; font-size: 2em; opacity: 0.2; z-index:500;"),
                         style="position: relative;"
                       ),
                       hr(),
                       tags$b("Click a row in the data table below to compare NPI actions and timeframes between the selected region and the clicked region."),
                       withSpinner(DT::dataTableOutput('sim_table')),
                       tags$i("Demographic data currently only available for US Counties."), tags$br(),
                       downloadButton(outputId='downloadSimilarityData', label = 'Download Data', class='download_button')
                ),
                column(width=3,
                       h4("Definitions"),
                       uiOutput(outputId = "sim_definitions")
                )
              )
      ),
      tabItem(tabName = "region_sah_comp",
              tags$head(tags$style(".modal-dialog{ width:800px}")),
              fluidRow(column(width=3, h3("State Comparison Analysis Tool"))
              ),
              fluidRow(
                column(width=3,
                       box(title = "Data to Graph (Y Axis)",
                           radioButtons(inputId = "sah_yaxis", label = "Select one of the following options to display.",
                                        choiceNames = c("Cumulative Change in Mobility \u24d8",
                                                        "Avg. Change in Mobility \u24d8",
                                                        "Avg. Change in Mobility Before Stay at Home \u24d8",
                                                        "Avg. Change in Mobility During Stay at Home \u24d8",
                                                        "Avg. Change in Mobility After Stay at Home \u24d8",
                                                        "Avg. Change in Mobility Last Week \u24d8",
                                                        "Cumulative Cases per 100,000  \u24d8",
                                                        "Cumulative Deaths per 100,000 \u24d8",
                                                        "Avg. Daily Cases per 100,000  (Prev. 7 days) \u24d8",
                                                        "Change in Avg. Daily Cases per 100,000 from Last Week (%) \u24d8",
                                                        "Avg. Test Positivity (Prev. 7 days) (%) \u24d8",
                                                        "Avg. Daily Tests per 100,000 (Prev. 7 days) \u24d8",
                                                        "Cumulative Tests per 100,000 \u24d8"
                                        ),
                                        choiceValues = c('tot_mobility_change', 'avg_mobility_change', 'avg_mobility_pre', 'avg_mobility_during', 'avg_mobility_post', 'avg_mobility_lastweekupdated',
                                                         'cum_cases_per100k', 'cum_deaths_per100k', 'avg_daily_cases_week_per100k', 'avg_daily_weekly_change',
                                                         'week_pct_pos', 'avg_all_tests_week_per100k', 'cum_tests_per100k'
                                        ),
                                        selected = c('tot_mobility_change')),
                           radioTooltip(id = "sah_yaxis", choice = "tot_mobility_change", title = "Sum of mobility from 3/1 to present. Mobility measured as the % of the median weekday mobility the week of Feb 16th, as captured by mobile device location services. Source: Descartes Labs", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sah_yaxis", choice = "avg_mobility_change", title = "Average mobility from 3/1 to present. Mobility measured as the % of the median weekday mobility the week of Feb 16th, as captured by mobile device location services. Source: Descartes Labs", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sah_yaxis", choice = "avg_mobility_pre", title = "Average mobility prior to stay at home order. Mobility measured as the % of the median weekday mobility the week of Feb 16th, as captured by mobile device location services. Source: Descartes Labs", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sah_yaxis", choice = "avg_mobility_during", title = "Average mobility during stay at home order. Mobility measured as the % of the median weekday mobility the week of Feb 16th, as captured by mobile device location services. Source: Descartes Labs", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sah_yaxis", choice = "avg_mobility_post", title = "Average mobility after stay at home order. Mobility measured as the % of the median weekday mobility the week of Feb 16th, as captured by mobile device location services. Source: Descartes Labs", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sah_yaxis", choice = "avg_mobility_lastweekupdated", title = "Average mobility in the last 7 days of available data. Mobility measured as the % of the median weekday mobility the week of Feb 16th, as captured by mobile device location services. Source: Descartes Labs", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sah_yaxis", choice = "cum_cases_per100k", title = "Total cumulative cases per 100,000 inhabitants of the state.  Source: USAFacts.com", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sah_yaxis", choice = "cum_deaths_per100k", title = "Total cumulative deaths per 100,000 inhabitants of the state.  Source: USAFacts.com", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sah_yaxis", choice = "avg_daily_cases_week_per100k", title = "Average new daily cases over the past 7 days per 100,000 inhabitants of the state.  Source: USAFacts.com", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sah_yaxis", choice = "avg_daily_weekly_change", title = "Percent change in weekly average of new daily cases per 100,000 inhabitants of the state between the past week and the prior week.  Source: USAFacts.com", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sah_yaxis", choice = "week_pct_pos", title = "Average test positivity rate over the past 7 days.  Source: covidtracking.com", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sah_yaxis", choice = "avg_all_tests_week_per100k", title = "Average number of tests completed each day over the past 7 days per 100,000 inhabitants of the state.  Source: covidtracking.com", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sah_yaxis", choice = "cum_tests_per100k", title = "Cumulative number of tests completed per 100,000 inhabitants of the state.  Source: covidtracking.com", placement = "right", trigger = "hover"),
                           width = "100%"
                       ),
                       box(title = "Data to Graph (X Axis)",
                           radioButtons(inputId = "sah_xaxis", label = "Select one of the following to align the data.",
                                        choiceNames = c("Total Days under Stay at Home Order \u24d8",
                                                    "Cumulative Test Positivity (%) \u24d8",
                                                    "Population Density (people/sq. mile)",
                                                    "Percent of Population over Age 65" ,
                                                    "Median Household Income",
                                                    "Hospital Beds per 1000 Inhabitants"),
                                        choiceValues = c("SAHdays","cum_pos_pct", "density", "pct65over","medIncome", "bedsPer1k"),
                                        selected=c("SAHdays")),
                           radioTooltip(id = "sah_xaxis", choice = "SAHdays", title = "Number of days stay at home order was in effect. Source: MITRE", placement = "right", trigger = "hover"),
                           radioTooltip(id = "sah_xaxis", choice = "cum_pos_pct", title = "Cumulative test positivity rate.  Source: covidtracking.com", placement = "right", trigger = "hover"),
                           width = "100%"
                       )
                ),
                column(width=9,
                       tags$div(
                         withSpinner(plotlyOutput("sah_scatterplot", height = "60%"))
                       ),
                       hr(),
                       tags$div("As states relax their shelter-in-place or stay at home orders and move to reopen economies, it will be important to understand how the changes in policies are affecting trends in cases of COVID-19 and population behavior. We use these scatterplots to compare states according to population mobility, growth of COVID-19 within the state, and percent positive testing rate. From these scatterplots, we can identify clusters, trends, and potential outlier states to better understand any potential relationship between a state’s policy and their epidemic outcomes."),
                       hr(),
                       span(textOutput("sah_missing_data_message"), style="color:red"),
                       span(textOutput("sah_missing_data_message2"), style="color:red")
                )
              )
      ),
      tabItem(tabName="mortality",
              fluidRow(
                column(width = 3,
                       h3("Mortality Analysis"),
                       tags$div("This analysis was completed to evalute if the COVID-19 mortality rate is changing over time.
                                This analysis utilized the ", tags$a(href="https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf", "CDC COVID-19 Case Surveillance Public Use Dataset"),
                                " to evalute the distribution of cases across different age groups over time, and the",
                                tags$a(href="https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-W/vsak-wrfu", "CDC Provisional COVID-19 Death Counts by Sex, Age, and Week Dataset"),
                                " to evalute the distribution of deaths across different age groups over time. COVID-19 testing data comes from ",
                                tags$a(href="https://covidtracking.com/", "The COVID Tracking Project"),
                                "while case and death count data comes from ",
                                tags$a(href="https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/", "USAFacts"),
                                ". COVID-19 case and death counts were stratified using smoothed age group estimates from the CDC datasets.
                                The case data was then lagged by three weeks to compute the weekly mortality rate for each age group (new cases / new deaths).
                                A lag time of three weeks was selected based on published studies of the time between case identification and death in China (",
                                tags$a(href="http://dx.doi.org/10.1016/S2213-2600(20)30079-5", "source 1"), ", ",
                                tags$a(href="https://doi.org/10.1016/S0140-6736(20)30566-3", "source 2"), ")."
                                ),
                       hr(),
                       box(title = "Data to Graph",
                           radioButtons(inputId = "mortality_plot_choices", label = "Select one of the following.",
                                              choices = c('Mortality Rate by Age Group' = 'mort.age',
                                                          'Distribution of Cases by Age Group' = "cases.age",
                                                          'Distribution of Deaths by Age Group' = "deaths.age",
                                                          'Testing Availability' = 'testing'
                                              ),
                                              selected = c("mort.age")
                           ),
                           width = "100%"
                       ),
                       hr(),
                       tags$div("Note: The CDC COVID-19 Case Surveillance Public Use Dataset is updated monthly and currently contains data though the week of ",
                                format(max(as.Date(CDC_AGE$cases_update_date)), format = "%m/%d/%Y"), ". ",
                                "The CDC Provisional COVID-19 Death Counts by Sex, Age, and Week Dataset is updated weekly and currently contains data though the week of ",
                                format(max(as.Date(CDC_AGE$week)), format = "%m/%d/%Y"), "."
                       )
                ),
                column(width = 8,
                       withSpinner(plotlyOutput(outputId = "mortality.plot", height = "700px"))
                )
              )
      ),
      tabItem(tabName = "npi_impact",
              intervention_ui
      ),
      tabItem(tabName="forecast",
              fluidRow(
                column(width = 3,
                       h3("Short-Term Forecast"),
                       tags$div("This tool provides a simple yet effective two-week forecast of COVID-19 reported cases using a statistical curve fitting approach.  "),
                       tags$div("Select a state and county by typing its name into each box."),
                       hr(),
                       h4("Select County"),
                       selectizeInput(inputId = "forecast.state", label = "Filter by US State", c(structure(state.abb, names=state.name)), selected = "NY", multiple=FALSE),
                       uiOutput(outputId = "forecast_county"),
                       box(title = "Data to Graph",
                           checkboxGroupInput(inputId = "forecast_plot_choices", label = "Select one or more of the following.",
                                              choices = c('Forecasted Cumulative Cases' = "f.cases",
                                                          'Actual Cumulative Cases' = "a.cases",
                                                          'Forecasted Hospitalized (Non-Critical) Population' = "f.hosp",
                                                          'Forecasted Critical Care Population' = "f.critical",
                                                          'Forecasted Ventilator Population' = "f.vent"
                                              ),
                                              selected = c("f.cases", "a.cases")
                           ),
                           width = "100%"
                       ),
                       span(textOutput("input_check_message"), style="color:red")
                       ),
                column(width = 8,
                       withSpinner(plotlyOutput(outputId = "curve.forecast.plot", height = "700px"))
                      )
                )
              ),
      tabItem(tabName="hotspot",
              fluidRow(
                column(width = 3,
                       h3("Hotspot Identification"),
                       tags$div("COVID-19 \"hotspots\" are defined as counties where the the number of new daily cases cases is growing, or the change in new daily cases is positive. The change in new daily cases is defined as the slope of the linear model fit to the 7-day moving average of new daily cases per hundred thousand inhabitants of the county.  The forecasted hotspots employ the projections of the C19HCC Short-term forecast model."),
                       hr(),
                       box(title = "Data to Plot: Timeframe",
                           radioButtons(inputId = "hotspot_plot_data", label = "Select one of the following.",
                                        choiceNames = c('Current Hotspots',
                                                    'Forecasted Hotspots \u24d8'),
                                        choiceValues = c('current', 'forecast'),
                                        selected=c('Current Hotspots' = 'current')),
                           radioTooltip(id = "hotspot_plot_data", choice = "forecast", title = "Model: C19HCC Short-term Forecast", placement = "right", trigger = "hover"),
                           uiOutput(outputId = "hotspot_forecast_date"),
                           width = "100%"
                       ),
                       box(title = "Data to Plot: Hotspot Criteria",
                         radioButtons(inputId = "hotspot_plot_options", label = "Select one of the following criteria to define hotspots.",
                                      choices = c('Total New Cases in Previous 2 Weeks \u24d8' = "cases.raw",
                                                  'Total New Cases in Previous 2 Weeks per 100k Inhabitants \u24d8' = "cases.100",
                                                  'Change in the Number of New Daily Cases per 100k Inhabitants \u24d8' = "cases.change",
                                                  'Change in Avg. Daily Cases from Prior Week (%)' = 'change.week'
                                      ),
                                      selected = c("cases.change")
                         ),
                         radioTooltip(id = "hotspot_plot_options", choice = "cases.raw", title = "Sum of all new cases over the past 14 days.", placement = "right", trigger = "hover"),
                         radioTooltip(id = "hotspot_plot_options", choice = "cases.100", title = "Sum of all new cases over the past 14 days per 100k inhabitants of the county.", placement = "right", trigger = "hover"),
                         radioTooltip(id = "hotspot_plot_options", choice = "cases.change", title = "Defined as the slope of the linear model fit to the 7-day moving average of new daily cases per 100k inhabitants of the county.", placement = "right", trigger = "hover"),
                         radioTooltip(id = "hotspot_plot_options", choice = "change.week", title = "Defined as the percent change in the average number of new cases this past week as compared to the prior week.", placement = "right", trigger = "hover"),
                         width = "100%"
                       )
                ),
                column(width = 9,
                       tags$style(HTML(".leaflet-container { background: #ffffff; }")),
                       div(align = "center", style = "font-size:20px; background-color:#ffffff", textOutput("hotspot.map.plot.title")),
                       withSpinner(leafletOutput(outputId = "hotspot.map.plot", height = '700px'))
                )
              )
      ),
      tabItem(tabName = "model_comparison",
              model_comp_ui
              ),
      tabItem(tabName = "definitions",
              h3("Definitions"),
              fluidRow(
                tableOutput("npi_defs")
              )
      ),
      tabItem(tabName="model",
              fluidPage(
                br(),
                uiOutput("modelDesc")
              )

      ),
      tabItem(tabName="caveat",
              fluidPage(style = "background-color: white;",
                br(),
                column(width = 1),
                column(width = 10,
                       caveat_ui
                       ),
                column(width = 1)
              )

      ),

      tabItem(tabName = "sources",
              fluidPage(
                br(),
                uiOutput("parameterDesc")
              )
      ),
      tabItem(tabName = "adv_settings",
              h3("Advanced Settings: Clinical & Simulation Parameters"),
              tags$blockquote(tags$b("Disclaimer: "), "The models and projections shown in this application are computed with the most-current and/or
                              widely accepted clinical and population-health related parameters found in the literature and empirical data. Most of these
                              parameters are documented in the ", tags$u("SOURCES"), " page, and others were derived via a collaborative process with
                              internal and external Subject Matter Experts, health-policy experts, and clinicians. This being said, there is a high degree
                              of uncertainty surrounding many of these parameters given the scale and speed at which COVID-19 has developed. Therefore, the models
                              and projections found across this application may be recalibrated using the interactive sliders and inputs found on this page. Adjustments
                              made to these parameters should be done with extreme care and policy interventions evaluated using custom parameter values may not be
                              as accurate as the default parameters proposed."),
              tags$blockquote("Additionally, parameters that have been analytically and automatically tuned to better align with empirical data are denoted with (", tags$sup(style="color: red; font-color: red;", icon("asterisk")), ")"),
              tags$blockquote(uiOutput('tuning_date')),
              fluidRow(
                column(width=6,
                       box(title = "Simulation Parameters",status = "info", width = "100%",collapsible = TRUE,
                           h5("Time Parameters"),
                           dateInput("start_date", label = span("Start Date",  tags$sup(style="color: red; font-color: red;", icon("asterisk"))), value = get_def_param('start_date'), format = "mm/dd/yyyy"),
                           sliderInput("Tmax", div(HTML("Model Duration: ")),0, 365*3, get_def_param('Tmax'), step=1, post=" days"),
                           hr(),
                           h5("Population Parameters"),
                           tags$i("The population size parameter is initialized to whichever state is selected in the Intervention tab"),
                           sliderInput("population.size", label = "Population Size:", 100, 400000000, get_def_param('population.size'), step=1),
                           numericInput("initial.infected", label = span("Initial # infected", tags$sup(style="color: red; font-color: red;", icon("asterisk"))),
                                        value = get_def_param('initial.infected'), min = 1, step = 1)
                           ),
                       box(title = "Transmission Settings", status = "info", width = "100%", collapsible = TRUE,
                           "Use the following sliders to adjust the infection transmission rates in the Intervention Model:",
                           id = "transmission_params",
                           sliderInput("FracAsympto",label = "Proportion of Asymtomatic Infections", 0, 100, get_def_param('FracAsympto'), step=5), # updated to % to be consistent with usage in ode_functions
                           sliderInput("b1", label = span("Mild infection trans. rate", tags$sup(style="color: red; font-color: red;", icon("asterisk"))),
                                       min=0, max=3, value=get_def_param('b1'), step=0.01),
                           sliderInput("b21", label = "Severe infection trans. rate (relative to mild)",0, 2, get_def_param('b21'), step=0.01),
                           sliderInput("b31", label = "Critical infection trans. rate (relative to mild)", 0, 2, get_def_param('b31'), step=0.01)
                        ),
                       br(),
                       actionButton(inputId = "adv_settings_reset", label = "Reset All Settings", class = "btn-reset")
                       ),
                column(width=6,
                       box(title = "Clinical Parameters", status = "info", width = "100%", collapsible = TRUE,
                           id = "clinical_params_reset_id",
                           sliderInput("IncubPeriod", "Duration of incubation period", min=0, max=20, value=get_def_param('IncubPeriod'), step=0.1, post = " days"),
                           sliderInput("AsymptoCrossSect", "Asymptomatic Scaling Factor", 0, 10, get_def_param('AsymptoCrossSect'), step=.5), #KF I'm not sure what this is, Eric?
                           sliderInput("DurMildInf", "Duration of mild infections", min=0, max=20, value=get_def_param('DurMildInf'), step=1, post = " days"),
                           sliderInput("FracSevere", "% of infections that are severe", min=0, max=100, value=get_def_param('FracSevere'), step=1, post="%"),
                           sliderInput("FracCritical", "% of infections that are critical", min=0, max=20, value=get_def_param('FracCritical'), step=1, post="%"),
                           sliderInput("ProbDeath", "Death rate for critical infections", 0, 100, get_def_param('ProbDeath'), step=1, post="%"),
                           #htmlOutput("CFR"), clin.params["CFR"] not  currently show
                           br(),
                           sliderInput("DurHosp", "Duration of hospitalization", min=0, max=20, value=get_def_param('DurHosp'), step=1, post = " days"),
                           sliderInput("TimeICUDeath", "Duration critical infection/ICU stay", min=0, max=30, value=get_def_param('TimeICUDeath'), step=0.5, post = " days")
                           ),
                       tags$div(id = "adv_settings_eff_box",
                                box(title="Intervention Effectiveness", status = "info", width = "100%", collapsible = TRUE,
                                    style= "border-collapse: collapse;",
                                    tags$table(id="npi_param_table",
                                               tags$thead(
                                                 tags$tr(
                                                   tags$th("", style="width: 40%;"),
                                                   #tags$th("% Reduction Mild Transmission", style="width: 20%;"),
                                                   tags$th("% Reduction Severe Transmission", style="width: 20%;"),
                                                   tags$th("% Reduction Critical Transmission", style="width: 20%;")
                                                 )
                                               ),
                                               tags$tbody(
                                                 tags$tr(
                                                   tags$td(tags$b("Bar/Restaurant Limits"), style="width: 40%;"),
                                                   #tags$td(numericInput("npi.bar.prop.mild", label = "", min = 0, max=1, value=0.1, step = 0.01)),
                                                   tags$td(numericInput("npi.bar.prop.severe", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   tags$td(numericInput("npi.bar.prop.critical", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"
                                                 ),
                                                 tags$tr(
                                                   tags$td(tags$b("Large Gatherings Ban"), style="width: 40%;"),
                                                   #tags$td(numericInput("npi.lgb.prop.mild", label = "", min = 0, max=1, value=0.25, step = 0.01)),
                                                   tags$td(numericInput("npi.lgb.prop.severe", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   tags$td(numericInput("npi.lgb.prop.critical", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"
                                                 ),
                                                 tags$tr(
                                                   tags$td(tags$b("Mandatory Mask Usage"), style="width: 40%;"),
                                                   #tags$td(numericInput("npi.mm.prop.mild", label = "", min = 0, max=1, value=0.05, step = 0.01)),
                                                   tags$td(numericInput("npi.mm.prop.severe", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   tags$td(numericInput("npi.mm.prop.critical", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"
                                                 ),
                                                 tags$tr(
                                                   tags$td(tags$b("Mandatory Quarantine for Travelers"), style="width: 40%;"),
                                                   #tags$td(numericInput("npi.mqt.prop.mild", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   tags$td(numericInput("npi.mqt.prop.severe", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   tags$td(numericInput("npi.mqt.prop.critical", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black; display: none;"
                                                 ),
                                                 tags$tr(
                                                   tags$td(tags$span(tags$b("Non-Essential Business Closures"), tags$sup(style="color: red; font-color: red;", icon("asterisk"))), style="width: 40%;"),
                                                   #tags$td(numericInput("npi.nbc.prop.mild", label = "", min = 0, max=1, value=0.25, step = 0.01)),
                                                   tags$td(numericInput("npi.nbc.prop.severe", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   tags$td(numericInput("npi.nbc.prop.critical", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"
                                                 ),
                                                 tags$tr(
                                                   tags$td(tags$b("State-Mandated School Closures"), style="width: 40%;"),
                                                   #tags$td(numericInput("npi.sc.prop.mild", label = "", min = 0, max=1, value=0.15, step = 0.01)),
                                                   tags$td(numericInput("npi.sc.prop.severe", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   tags$td(numericInput("npi.sc.prop.critical", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"
                                                 ),
                                                 tags$tr(
                                                   tags$td(tags$span(tags$b("Stay At Home Order"), tags$sup(style="color: red; font-color: red;", icon("asterisk"))), style="width: 40%;"),
                                                   #tags$td(numericInput("npi.sah.prop.mild", label = "", min = 0, max=1, value=0.85, step = 0.01)),
                                                   tags$td(numericInput("npi.sah.prop.severe", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   tags$td(numericInput("npi.sah.prop.critical", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"
                                                 ),
                                                 tags$tr(
                                                   tags$td(tags$b("Reopening Plan Reversal"), style="width: 40%;"),
                                                   #tags$td(numericInput("npi.sah.prop.mild", label = "", min = 0, max=1, value=0.85, step = 0.01)),
                                                   tags$td(numericInput("npi.rev.prop.severe", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   tags$td(numericInput("npi.rev.prop.critical", label = "", min = 0, max=1, value=0, step = 0.01)),
                                                   style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"
                                                 ),
                                                 tags$tr(id="custom_npi_1_params_span", style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"),
                                                 tags$tr(id="custom_npi_2_params_span", style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"),
                                                 tags$tr(id="custom_npi_3_params_span", style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"),
                                                 tags$tr(id="custom_npi_4_params_span", style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"),
                                                 tags$tr(id="custom_npi_5_params_span", style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"),
                                                 tags$tr(id="custom_npi_6_params_span", style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"),
                                                 tags$tr(id="custom_npi_7_params_span", style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"),
                                                 tags$tr(id="custom_npi_8_params_span", style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"),
                                                 tags$tr(id="custom_npi_9_params_span", style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;"),
                                                 tags$tr(id="custom_npi_10_params_span",style="margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom:0px; border-top: 1px solid black;")
                                               )
                                    ),
                                    tags$span(actionButton(inputId = "sub1", label = "Remove Custom NPI", icon = icon("minus")), style = "display: inline-block;vertical-align:top;")
                                    )
                                )
                       )

              )
      )
    )
)

ui <- function(request){
  dashboardPage(header = header, sidebar = sidebar, body=body, skin = "black")
}


## Shiny Server
server <- function(input, output, session){
  # refresh global variables to pull in most recent data
  source('database.R')

  output$sidebarpanel <- renderUI({
      app_sidebar
  })
  
  output$body <- renderUI({
      app_body
  })

  updateTabItems(session, 'sidebarTabs', "caveat")
  
  #logic to set tab on loadout
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['sidebarTabs']])) {
      updateTabItems(session, 'sidebarTabs', query[['sidebarTabs']])
    }
  })

  # list to store global, reactive variables
  values <- reactiveValues()

  #CHANGED 10 TO 5; made it more responsive and sync w/ time series
  values$doubling_days <- reactive({5})
  
  values$npi_data <- reactive({
    NPI_DATA
  })

  values$hdata <- reactive({
    HDATA
  })
  
  values$active_covid_tab <- reactive('Covid-19')

  values$regional_covid_data <- reactive({
      get_national_covid_data()
  })
  
  values$nat_stats <- reactive({get_national_stats(rt_df=RT, doubling_days=values$doubling_days())})
  # start off with national covid stats. Then calculate based on chosen region going forward

  values$metric <- reactive({
    switch(input$geo_metric,
           'Cumulative Cases'='tot_cases',
           'Cumulative Deaths'='tot_deaths',
           'Cumulative Cases per 100,000'='tot_cases_per100k',
           'Cumulative Deaths per 100,000'='tot_deaths_per100k',
           'Doubling Time'='doubling_time',
           'Reproduction Rate' = 'rt',
           'None'
    )
  })

  output$spread.selector.county.ui <- renderUI({
    req(input$granularity)
    req(input$chosen_state)
    if (input$granularity == "State"){
      county.opts <- COUNTY_INF_STATS %>%
        separate(col=mState.Providence, into=c("County", "abbr"), sep = ", ", remove = FALSE) %>%
        left_join(STATE_NAMES, by = "abbr") %>%
        filter(!is.na(full), #gets rid of DC which TBH is fine...
               full == input$chosen_state
        ) %>%
        select(County) %>%
        distinct() %>%
        .$County

      if (length(county.opts)>1){
        ret.ui <- selectizeInput(inputId = "chosen_county", label=NULL, choices = c("Statewide", sort(county.opts)), selected = "Statewide", multiple = FALSE, width = "100%")
        return(ret.ui)
      }
    }
  })
  
  observeEvent(input$about_mitre_ens_link, {
    updateTabItems(session = session, inputId = "sidebarTabs", selected = "model")
    delay(ms = 500, expr = runjs('document.getElementById("model_iframe").contentWindow.document.getElementById("ensemble-model").scrollIntoView();'))
  })

  missing.interv.counties <- reactive({
    req(input$inter_state)
    if (!str_detect(input$inter_state, pattern = "District of")){
      all.counties <- COUNTY_INF_STATS %>%
        separate(col=mState.Providence, into=c("County", "abbr"), sep = ", ", remove = FALSE) %>%
        left_join(STATE_NAMES, by = "abbr") %>%
        filter(!is.na(full), #gets rid of DC which TBH is fine...
               full == input$inter_state
        ) %>%
        select(County) %>%
        distinct() %>%
        .$County
      
      county.opts <- COUNTY_INF_STATS %>%
        separate(col=mState.Providence, into=c("County", "abbr"), sep = ", ", remove = FALSE) %>%
        left_join(STATE_NAMES, by = "abbr") %>%
        filter(!is.na(full), #gets rid of DC which TBH is fine...
               full == input$inter_state, 
               mState.Providence %in% (COUNTY_TUNING_PARAMS$region)
        ) %>%
        select(County) %>%
        distinct() %>%
        .$County
      
      missing.counties <- all.counties[(!all.counties %in% county.opts) & str_detect(all.counties, "Statewide", negate = TRUE)]
      return(missing.counties)
    }
  })
  
  observeEvent(missing.interv.counties(), ignoreInit = TRUE, {
    
    if (length(missing.interv.counties()>1)){
      tt.text <- paste0("Note: Counties with fewer than two weeks of data or fewer than 14 confirmed deaths are excluded.")
      addTooltip(session=session, id="intervention_county_div", title = tt.text, placement = "right")
    }
    
  })
  
  output$intervention.selector.county.ui <- renderUI({
    req(input$inter_state)
    if (!str_detect(input$inter_state, pattern = "District of")){
      county.opts <- COUNTY_INF_STATS %>%
        separate(col=mState.Providence, into=c("County", "abbr"), sep = ", ", remove = FALSE) %>%
        left_join(STATE_NAMES, by = "abbr") %>%
        filter(!is.na(full), #gets rid of DC which TBH is fine...
               full == input$inter_state,
               mState.Providence %in% (COUNTY_TUNING_PARAMS$region)
        ) %>%
        select(County) %>%
        distinct() %>%
        .$County
      
      if (length(county.opts)>1){
        ret.ui <- selectizeInput(inputId = "inter_county", label=NULL, choices = c("Statewide", sort(county.opts)), selected = "Statewide", multiple = FALSE)
        return(ret.ui)
      }else{
        ret.ui <- selectizeInput(inputId = "inter_county", label=NULL, choices = c("Statewide"), selected = "Statewide", multiple = FALSE)
      }
    }else{
      ret.ui <- selectizeInput(inputId = "inter_county", label=NULL, choices = c("District-Wide"="Statewide"),
                                        selected = c("District-Wide"="Statewide"), multiple = FALSE)
      return(ret.ui)
    }
  })
  
  # time series data
  spread.data <- reactiveValues(state_ts = filter(STATE_INF_STATS, mState.Providence == "California"))

  values$nat_ts <- reactive(
    get_national_time_series()
  )
  
  values$cap_scale <- reactive({100000})

  values$regional_covid_stats<- reactive({
    get_infection_stats(values$regional_covid_data(), doubling_days=values$doubling_days(), rt_df=RT)
  })
  
  values$state_covid_stats<- reactive({
    STATE_INF_METRICS
  })
  
  values$region_resource_stats <- reactive({
    get_states_resource_stats(HOSPITALS) %>% distinct()
  })
  
  # initialize state resource params to national ones - then update as user chooses a state
    values$region_resource_params <- reactive({values$hdata()})
  # keep track of active tab on covid spread page (heatmap or resources)
  
  observeEvent(input$regional_plots, {
    if(grepl('[rR]esources', input$regional_plots)) tab='Resources'
    else tab='Spread'
    values$active_covid_tab <- reactive(tab)
    tab
  })

  values$regional_hosp_data <- reactive({
    if(input$granularity=='State'){
      HOSPITALS %>% filter(State == state_lookup(input$chosen_state))
    }else{
      HOSPITALS
    }
  })
  
  values$regional_test_data <- reactive({
    if(input$granularity=='State'){
      CUMULATIVE_STATE_TESTING %>% filter(state == state_lookup(input$chosen_state))
    }else{
      CUMULATIVE_STATE_TESTING
    }
  })
  
  values$weekly_regional_test_data <- reactive({
    if(input$granularity=='State'){
      most_recent_weekly_testing_data(input$chosen_state)
    }else{
      most_recent_weekly_testing_data()
    }
  })
  
  output$regionalmap <- renderLeaflet({
    plt.height = 450 
    if(input$granularity=='National'){
      fill_metric = values$metric()
      state_df = STATE_INF_METRICS
      if(values$metric() %in% c('tot_cases_per100k', 'tot_deaths_per100k')){
        per_cap_measures <- c('tot_deaths', 'tot_cases')
        state_df[,c(per_cap_measures)] <- round(state_df[,c(per_cap_measures)] /
                                                  state_df$pop * values$cap_scale(), 2)
        if (values$metric() =='tot_cases_per100k'){
          fill_metric = 'tot_cases'
        }else{
          fill_metric = 'tot_deaths'
        }
      }
      res = update_national_choropleth(state_df, metric=fill_metric, test_data=values$regional_test_data(), ret_data=T)

      res$df <- res$df %>% mutate(region_key=state)
    }else{
      fill_metric = values$metric()
      if(input$geo_metric=='Reproduction Rate') {
        fill_metric='r_t'
        met_title <- "Reproduction Rate"
      } else if (str_detect(fill_metric, pattern = "oubling")){
        met_title <- "Doubling Time"
      } else if (str_detect(fill_metric, pattern = "100k")){
        met_title <- paste0(input$geo_metric)
        if (values$metric() =='tot_cases_per100k'){
          fill_metric = 'tot_cases'
        }else{
          fill_metric = 'tot_deaths'
        }
      }else{
        met_title <- paste0('Total ',input$geo_metric)
      }
      res = gen_state_covid_plot(input$chosen_state, fill_metric=fill_metric, measure_title=met_title, max_width=800, show_hosps=input$show_hosps,
                                 ret_data=T)
      res$df <- data.frame(res$df %>% mutate(region_key=County.Name, rt=r_t))
    }
    fig = res$fig
    return(fig)
  })

  width_status <- reactiveVal(0)

  observeEvent((input$width), {
    if (abs(input$width - width_status()) > 100 & width_status()>0) {
      output$hotspot.map.plot <- renderLeaflet({
        req(input$hotspot_plot_data)
        fig = plot_hotspots(hotspot_plot_data(), input$hotspot_plot_options, input$hotspot.forecast.date)
        return(fig)
      })

      output$regionalmap <- renderLeaflet({
        plt.height = 450
        if(input$granularity=='National'){
          fill_metric = values$metric()
          state_df = STATE_INF_METRICS
          if(values$metric() %in% c('tot_cases_per100k', 'tot_deaths_per100k')){
            per_cap_measures <- c('tot_deaths', 'tot_cases')
            state_df[,c(per_cap_measures)] <- round(state_df[,c(per_cap_measures)] /
                                                      state_df$pop * values$cap_scale(), 2)
            if (values$metric() =='tot_cases_per100k'){
              fill_metric = 'tot_cases'
            }else{
              fill_metric = 'tot_deaths'
            }
          }
          res = update_national_choropleth(state_df, metric=fill_metric, test_data=values$regional_test_data(), ret_data=T)

          res$df <- res$df %>% mutate(region_key=state)
        }else{
          fill_metric = values$metric()
          if(input$geo_metric=='Reproduction Rate') {
            fill_metric='r_t'
            met_title <- "Reproduction Rate"
          } else if (str_detect(fill_metric, pattern = "oubling")){
            met_title <- "Doubling Time"
          } else if (str_detect(fill_metric, pattern = "100k")){
            met_title <- paste0(input$geo_metric)
            if (values$metric() =='tot_cases_per100k'){
              fill_metric = 'tot_cases'
            }else{
              fill_metric = 'tot_deaths'
            }
          }else{
            met_title <- paste0('Total ',input$geo_metric)
          }
          res = gen_state_covid_plot(input$chosen_state, fill_metric=fill_metric, measure_title=met_title, max_width=800, show_hosps=input$show_hosps,
                                     ret_data=T)
          res$df <- data.frame(res$df %>% mutate(region_key=County.Name, rt=r_t))
        }
        fig = res$fig
        return(fig)

      })
    }
    width_status(input$width)
  })

  hotspot_plot_data <- eventReactive(
    {input$hotspot.forecast.date}, {
    req(input$hotspot_plot_data)
    return(plot_hotspots_data(input$hotspot_plot_data, input$hotspot.forecast.date))
  }, ignoreNULL = T)

  output$hotspot.map.plot <- renderLeaflet({
    req(!is.null(hotspot_plot_data()))
    fig = plot_hotspots(hotspot_plot_data(), input$hotspot_plot_options, input$hotspot.forecast.date)
    return(fig)
  })
  
  output$hotspot.map.plot.title <- renderText({
    req(input$hotspot_plot_data)
    req(input$hotspot_plot_options)
    ret.txt = ""
    if (input$hotspot_plot_options == "cases.change") {
      ret.txt <- "Change in the Number of New Daily Cases per 100k Inhabitants"
    } else if (input$hotspot_plot_options == "cases.100") {
      ret.txt <- "Total New Cases in Previous 2 Weeks per 100k Inhabitants"
    } else if (input$hotspot_plot_options == "cases.raw") {
      ret.txt <- "Total New Cases in Previous 2 Weeks"
    } else if (input$hotspot_plot_options == "change.week") {
      ret.txt <- "Percent Change in Average Daily Cases from Prior Week"
    }
    if (!is.null(input$hotspot.forecast.date) & input$hotspot_plot_data == 'forecast') {
      ret.txt = paste0("Forecasted ", ret.txt, " as of ", as.character(format(input$hotspot.forecast.date, "%m/%d/%Y")))
    }
    return(ret.txt)
  })

  output$hotspot_forecast_date <- renderUI({
    req(input$hotspot_plot_data)

    #min and max date to select
    if (input$hotspot_plot_data == 'current'){
      hidden(dateInput(inputId = "hotspot.forecast.date", label = "Select Forecast Date",
                value = NULL, min = Sys.Date(), max = max(FORECAST_DATA$date)))
    } else if (input$hotspot_plot_data == 'forecast') {
      dateInput(inputId = "hotspot.forecast.date", label = "Select Forecast Date",
                value = Sys.Date()+14, min = Sys.Date(), max = max(FORECAST_DATA$date))
    }

  })



  output$regional_rank_table <- renderTable(expr = {
    req(input$granularity)
    
    rank.df <- data.frame()

    percap <- F
    if (str_detect(values$metric(), pattern = "100k")){
      percap <- T
      if (values$metric() =='tot_cases_per100k'){
        fill_metric = 'tot_cases'
      }else{
        fill_metric = 'tot_deaths'
      }
    } else {
      fill_metric = values$metric()
    }
    if (input$granularity == "National"){
      rank.df <- gen_regional_rank_table(input$granularity, fill_metric = fill_metric, per.capita = percap)
    }else{
      req(input$chosen_state)
      rank.df <- gen_regional_rank_table(input$chosen_state, fill_metric = fill_metric, per.capita = percap)
    }
    
    if (nrow(rank.df)>0){
      ret.tab <- rank.df
      
      return(ret.tab)
    }
  }, striped = TRUE, hover = TRUE)

  
  output$regional_npi_table <- renderTable({
    values$npi_data() %>% filter(state==input$chosen_state, !is.na(start_date)) %>%
      arrange(start_date) %>% 
      mutate(Start_Date = as.character(start_date),
             Start_Date = format.Date(Start_Date, "%m/%d/%y"),
             End_Date = as.character(end_date),
             End_Date = format.Date(End_Date, "%m/%d/%y"),
             End_Date = ifelse(is.na(End_Date), "None", End_Date)
      ) %>%
      select(`Start Date` = Start_Date, `End Date` = End_Date, `Policy` = policy_long)

  },  striped=TRUE)
  
  output$comp_countries_ui <- renderUI({

    # COUNTRY_INF_STATS def in database.R as global var
    elig.countries <- sort(unique(COUNTRY_INF_STATS$Country.Region))

    ret.ui <-selectizeInput(inputId = "comp_regions_national",
                            label = "Country",
                            choices = elig.countries,
                            selected = "United States",
                            multiple = TRUE)
  })

  output$comp_metros_ui <- renderUI({
    elig.mbsas <- sort(as.character(unique(MBSA_INF_STATS$mState.Providence)))
    selectizeInput(inputId = "comp_regions_local",
                   label = "Metro Area (USA)",
                   choices = elig.mbsas,
                   multiple = TRUE)

  })
  
  output$comp_states_ui <- renderUI({
    elig.states = sort(STATE_NAMES$full)
    elig.mex.states <- STATE_INF_STATS %>%
      filter(Country.Region == "Mexico") %>%
      select(mState.Providence) %>%
      distinct() %>%
      arrange(mState.Providence) %>%
      .$mState.Providence 
    
    selectizeInput(inputId = "comp_regions",
                   label = "State (USA, MEX)",
                   choices = list(`USA` = elig.states,
                                  `Mexico` = elig.mex.states),
                   selected = "New York", multiple = TRUE)
  })
  
  output$comp_counties_ui <- renderUI({
    elig.counties <- sort(as.character(unique(COUNTY_INF_STATS$mState.Providence)))
    selectizeInput(inputId = "comp_counties",
                   label = "County (USA)",
                   choices = elig.counties,
                   multiple = TRUE)
    
  })
  
  output$sim_region_ui_state <- renderUI({
    elig.reg <- sort(as.character(unique(STATE_NAMES$full)))
    selectizeInput(inputId = "sim_region_state",
                   label = "Filter by US State",
                   choices = elig.reg,
                   selected = "District of Columbia",
                   multiple = FALSE)
    
  })
  
  output$sim_region_ui_county <- renderUI({
    req(input$sim_region_state)
    state_counties <- COUNTY_CLEANUP[COUNTY_CLEANUP$full == input$sim_region_state,] %>% 
      mutate(county.state = paste(County.Pretty, abbr, sep=", ")) %>%
      filter(county.state %in% SIM_REGION_LIST$mState.Providence)
    elig.reg <- sort(as.character(unique(state_counties$county.state)))
    selectizeInput(inputId = "sim_region",
                   label = "Select US County",
                   choices = elig.reg,
                   selected = "Washington, DC",
                   multiple = FALSE)
    
  })
  
  refactored.spread.ts.df <- reactive({
    req(input$plot_metric)

    #eventually...revise the entire regionalts to get kicked off in this f(x).
    #for now, just using for hosp
    if (input$plot_metric != "Hospitalizations"){
      ret.obj <- list()
    }else{
      req(input$granularity)
      if (input$granularity == "National"){
        region.list <- c(paste("United States", "United States", sep="___"))
      }else{
        req(input$chosen_state)
        region.list <- c(paste(input$chosen_state, "United States", sep="___"))
      }
      y.scale <- "hospitalizations"
      selected.y <- "Hospitalizations"
      link.cases <- FALSE
      ret.obj <- create_comp_df(region.list = region.list,
                                selected.x = "all.dates", selected.y = selected.y, y.scale = y.scale,
                                overlay.npis = TRUE, link.cases.plot = link.cases, gantt.chart=TRUE)

    }

    return(ret.obj)
  })

  output$regionalts <- renderPlotly({
    req(input$granularity)
    plt.height <- min(425)
    # have separate function for testing plot for now - will likely merge with draw_comparisons later
    if(input$plot_metric == 'Testing'){
      if(input$granularity=='State') region = input$chosen_state else region = NULL
      return(gen_testing_plot(region, plt.height=plt.height))
    }else if(input$plot_metric=='Mobility'){
      if(input$granularity=='National'){
        region = 'United States___United States' 
        ret.plt <- plot_ly()
      }else if (input$chosen_county == "Statewide"){
        region <- paste(input$chosen_state, "United States", sep="___")
        ret.plt <- draw_comparisons(region, selected.x = "all.dates", selected.y = "Mobility", overlay.npis = TRUE,
                                    y.scale = "mobility", plt.height=plt.height, gantt.chart = TRUE, plot.title = TRUE) %>%
          layout(showlegend=F)
      }else{
        state.abbr <- STATE_NAMES$abbr[STATE_NAMES$full==input$chosen_state]
        region <- paste(paste0(input$chosen_county, ", ", state.abbr), "United States", sep = "___")
        ret.plt <- draw_comparisons(region, selected.x = "all.dates", selected.y = "Mobility", overlay.npis = TRUE,
                                    y.scale = "mobility", plt.height=plt.height, gantt.chart = TRUE, plot.title = TRUE)  %>%
          layout(showlegend=F)
      } 
      
      return(ret.plt)
      
    }else if (input$plot_metric == "Hospitalizations"){
      plt.obj <- refactored.spread.ts.df()
      selected.y <- "Hospitalizations"
      ret.plt <- draw_refactored_comparison_plot(plt.obj=plt.obj, link.cases.plot = FALSE, gantt.chart = TRUE) %>%
        layout(showlegend=FALSE, height = plt.height)

      return(ret.plt)
    }else{
      if (input$granularity=="National"){
        ts.region <- "United States___United States"
      }else if (is.null(input$chosen_county)){
        ts.region <- paste(input$chosen_state, "United States", sep="___")
      }else if (input$chosen_county == "Statewide"){
        ts.region <- paste(input$chosen_state, "United States", sep="___")
      }else{
        state.abbr <- STATE_NAMES$abbr[STATE_NAMES$full==input$chosen_state]
        ts.region <- paste(paste0(input$chosen_county, ", ", state.abbr), "United States", sep = "___")
      }
      if(input$plot_metric != 'Doubling Time') y=input$plot_metric else y='Cases'
      yscale='raw.vals'

      if(input$plot_metric == 'Reproduction Rate'){
        y='R_t'
        x <- "all.dates"
        yscale <- 'raw.vals'
      }else{
        x <- 'all.dates'
      }
      
      if (str_detect(input$plot_metric, pattern = "Cases") ){
        delta.cumul.plot <- "Cases"
      }else if (str_detect(input$plot_metric, pattern = "Deaths")){
        delta.cumul.plot <- "Deaths"
      }else{
        delta.cumul.plot <- "none"
      }
      
      if(input$plot_metric == "Doubling Time") {
        yscale="doubling.days"
      } else if(input$plot_metric == 'Cases (New and Total)') {
        yscale = "smoothed.delta"
        y = 'Cases'
        x <- "first.case" # required x scale for the delta/cumulative plot
      } else if(input$plot_metric == 'Deaths (New and Total)') {
        yscale = "smoothed.delta"
        y = 'Deaths'
        x <- "first.case" # required x scale for the delta/cumulative plot
      }

      fig_title = paste0( strsplit(ts.region, '___')[[1]][1], ' ', input$plot_metric, ' Over Time')
      fig <- draw_comparisons(ts.region, selected.x = x, selected.y = y, overlay.npis = TRUE, y.scale = yscale,
                              plt.height=plt.height, gantt.chart = TRUE, bar.line.duo = delta.cumul.plot) %>%
        layout(showlegend=F, title = list(text=fig_title ,font=list(size=18), x=.5))
    }
   
  })
  

  output$hosp_disclaimer_tag <- renderUI({
    ret.tag <- tags$i(paste("All figures assumed for the state of", input$inter_state))
    return(ret.tag)
  })
  
  output$similarity.type.ui <- renderUI({
    ret.ui <- radioButtons(inputId = "sim_type", label = "Type of Similarity:",
                           choices = c("Case Growth (Epidemiological)"="epi",
                                       "Demographic (US Counties Only)" = "demographics"),
                           selected=c("Demographic (US Counties Only)" = "demographics"))
    return(ret.ui)
  })

  output$demog.similarity.type.ui <- renderUI({
    req(input$sim_type)
    if (input$sim_type=="demographics"){
      ret.ui <- checkboxGroupInput(inputId = "demog.similarity.type", label = "Demographic Categories",
                               choices = c("Population" = "d",
                                           "Vulnerable Population Health Measures" = "e",
                                           "Healthcare System Characteristics" = "h"),
                               selected = c("Population" = "d",
                                            "Vulnerable Population Health Measures" = "e",
                                            "Healthcare System Characteristics" = "h")#,
                              )
      return(ret.ui)
    } else {
      ret.ui <- radioButtons(inputId = "epi.similarity.type", label = "Growth Type",
                               choices = c("Case Growth Rates"="case.cosine", 
                                           "Confirmed Case Counts"="case.logMSE",
                                           #"Raw Case Counts"="case.MSE",
                                           "Case Incidence per 100,000"="case.permil"),
                               selected = c("Confirmed Case Counts"="case.logMSE")
                             )
    }
  })
  
  output$sim_definitions <- renderUI({
    req(input$sim_type)
    if (input$sim_type=="demographics"){
      ret.ui <- 
        tabBox(
          tabPanel("Similarity Types", tableOutput("sim_type_list")),
          tabPanel("Demographic Measures", tableOutput("sim_cluster_list_table")),
          width = "100%"
        )
      return(ret.ui)
    } else {
      ret.ui <- tabBox(
        tabPanel("Similarity Types", tableOutput("sim_type_list")),
        width = "100%"
      )
      return(ret.ui)
    }
  })

  output$comp_definitions <- renderUI({
    ret.ui <-
      tabBox(
        tabPanel("NPI Key", tableOutput("npi_defs2")),
        width = "100%"
      )
    return(ret.ui)
  })
  
  output$sim_cluster_list_table <- renderTable({
    req(input$demog.similarity.type)
    map <- c("d" = "Population",
             "e" = "Vulnerable Population Health Measures",
             "h" = "Healthcare System Characteristics")
    HRSA_DEMOG_CATS %>% filter(Category %in% as.vector(map[input$demog.similarity.type])) %>%
      select("Measure" = Description, "Demographic Category" = Category)
  },striped = TRUE)
  
  observeEvent((input$epi.similarity.type) , {
    if (input$sim_type == "epi"){
      if (input$epi.similarity.type == "case.cosine"){
        updateRadioButtons(session=session, inputId = "sim_xaxis", 
                             choices = c("By Date"="all.dates", "From First Case"="first.case",
                                         "From First 100 Cases"="first.100", "From First 20 Cases" = "first.20",
                                         "From First 20 Cases, with Other Region Alignment" = "first.20.shift",
                                         "From First Death"="first.death",
                                         "From Hitting 0.2 Deaths per 100,000" = "2.deaths.per.mil",
                                         "From Stay at Home Order" = "sah.order"),
                             selected = c("From First 20 Cases, with Other Region Alignment" = "first.20.shift"))
        updateRadioButtons(session=session, inputId = "sim_yaxis", selected = c("Cumulative Growth Rate % (smoothed)" = "smoothed.rate"))
      }else if (input$epi.similarity.type == "case.logMSE"){
        updateRadioButtons(session=session, inputId = "sim_xaxis", 
                             choices = c("By Date"="all.dates", "From First Case"="first.case",
                                         "From First 100 Cases"="first.100", "From First 20 Cases" = "first.20",
                                         "From First 20 Cases, with Other Region Alignment" = "first.20.shift",
                                         "From First Death"="first.death",
                                         "From Hitting 0.2 Deaths per 100,000" = "2.deaths.per.mil",
                                         "From Stay at Home Order" = "sah.order"),
                             selected = c("From First 20 Cases, with Other Region Alignment" = "first.20.shift"))
        updateRadioButtons(session=session, inputId = "sim_yaxis", selected = c("Cumulative Cases/Deaths (Log)"="log10"))
      }else if (input$epi.similarity.type == "case.MSE"){
        updateRadioButtons(session=session, inputId = "sim_xaxis", 
                             choices = c("By Date"="all.dates", "From First Case"="first.case",
                                         "From First 100 Cases"="first.100", "From First 20 Cases" = "first.20",
                                         "From First 20 Cases, with Other Region Alignment" = "first.20.shift",
                                         "From First Death"="first.death",
                                         "From Hitting 0.2 Deaths per 100,000" = "2.deaths.per.mil",
                                         "From Stay at Home Order" = "sah.order"),
                             selected = c("From First 20 Cases, with Other Region Alignment" = "first.20.shift"))
        updateRadioButtons(session=session, inputId = "sim_yaxis", selected = c("Cumulative Cases"="raw.vals"))
      }else if (input$epi.similarity.type == "case.permil"){
        updateRadioButtons(session=session, inputId = "sim_xaxis",
                             choices = c("By Date"="all.dates", "From First Case"="first.case",
                                          "From First 100 Cases"="first.100", "From First 20 Cases" = "first.20",
                                          "From Hitting 300 Cases/Million, with Other Region Alignment" = "300.cases.per.mil.shift",
                                          "From First Death"="first.death",
                                          "From Hitting 0.2 Deaths per 100,000" = "2.deaths.per.mil",
                                          "From Stay at Home Order" = "sah.order"),
                             selected = c("From Hitting 300 Cases/Million, with Other Region Alignment" = "300.cases.per.mil.shift"))
        updateRadioButtons(session=session, inputId = "sim_yaxis", selected = c("Case/Death Incidence per 100,000"="per.million"))
      }
    }

  })
  
  observeEvent(input$comp_yaxis,{
    if (input$comp_yaxis %in% c("mobility", "R_t", "testing")){
      disable("comp_regions_national")
      disable("comp_regions_local")
    }else if (input$comp_yaxis == "hospitalizations"){
      enable("comp_regions_national")
      disable("comp_counties")
      disable("comp_regions_local")
    }
    else{
      enable("comp_counties")
      enable("comp_regions_national")
      enable("comp_regions_local")
    }
  })
  
  observeEvent((input$sim_type), {
    if (input$sim_type == "demographics"){
      updateRadioButtons(session=session, inputId = "sim_xaxis", 
                           choices = c("By Date"="all.dates", "From First Case"="first.case",
                                       "From First 100 Cases"="first.100", "From First 20 Cases" = "first.20",
                                       "From First Death"="first.death",
                                       "From Hitting 0.2 Deaths per 100,000" = "2.deaths.per.mil",
                                       "From Stay at Home Order" = "sah.order"),
                           selected = c("From First 20 Cases" = "first.20"))
      updateRadioButtons(session=session, inputId = "sim_yaxis", selected = c("Cumulative Cases"="raw.vals"))
    } else if (input$sim_type == "epi"){
        if (TRUE) {
          updateRadioButtons(session=session, inputId = "sim_xaxis", 
                               choices = c("By Date"="all.dates", "From First Case"="first.case",
                                           "From First 100 Cases"="first.100", "From First 20 Cases" = "first.20",
                                           "From First 20 Cases, with Other Region Alignment" = "first.20.shift",
                                           "From First Death"="first.death",
                                           "From Hitting 0.2 Deaths per 100,000" = "2.deaths.per.mil",
                                           "From Stay at Home Order" = "sah.order"),
                               selected = c("From First 20 Cases, with Other Region Alignment" = "first.20.shift"))
          updateRadioButtons(session=session, inputId = "sim_yaxis", selected = c("Cumulative Cases/Deaths (Log)"="log10"))
        }
    }
  })

  output$sim.mod.x <- renderUI({
    selectizeInput(inputId = "sim_mod_xaxis", label = "Align Regions:",
                   choices = c("By Date"="all.dates", "From First Case"="first.case",
                               "From First 100 Cases"="first.100", "From First 20 Cases" = "first.20",
                               "From First 20 Cases, with Other Region Alignment" = "first.20.shift",
                               "From Hitting 300 Cases/Million, with Other Region Alignment" = "300.cases.per.mil.shift",
                               "From First Death"="first.death",
                               "From Hitting 0.2 Deaths per 100,000" = "2.deaths.per.mil",
                               "From Stay at Home Order" = "sah.order"),
                   multiple = FALSE,
                   selected=input$sim_xaxis,
                   width = '100%')
  })

  output$sim.mod.y <- renderUI({
    selectizeInput(inputId = "sim_mod_yaxis", label = "Data to Graph:",
                   choices = c("Cumulative Cases/Deaths"="raw.vals","Cumulative Cases/Deaths (Log)"="log10", "New Daily Cases (smoothed)" = "smoothed.delta",
                               "Case/Death Incidence per 100,000" = "per.million", "Daily Growth Rate" = "daily.rate",
                               "Cumulative Growth Rate % (smoothed)" = "smoothed.rate", "Doubling Rate" = "doubling.days"),
                   multiple = FALSE,
                   selected = input$sim_yaxis,
                   width = '100%')
  })

  observeEvent(input$sim_table_rows_selected, {
    if (input$sim_table_rows_selected > 1) {
      showModal(modalDialog(
        h3(paste("NPI Implementation Comparison:", input$sim_region,"vs.", as.character(sim.table()[input$sim_table_rows_selected,2]))),
        hr(),
        fluidRow(
          column(width = 6, uiOutput(outputId = "sim.mod.x")),
          column(width = 6, uiOutput(outputId = "sim.mod.y"))
        ),
        tags$div(
          withSpinner(plotlyOutput("sim_mod_plot_cases", height = "30%", width = "100%")),
          tags$span(class="fa fa-hospital", style="position: absolute; top: 10px; right: 5px; font-size: 2em; opacity: 0.2; z-index:500;"),
          style="position: relative;"
        ),
        hr(),
        DT::dataTableOutput('sim_mod_table'),
        size = "l", easyClose = TRUE
      ))
    }
  })

  output$parameterDesc <- renderUI({
    tags$iframe(src="Parameters.html",width="100%",frameBorder="0",height="5000px")
  })

  output$modelDesc <- renderUI({
    tags$iframe(src="all_model_descriptions.html",width="100%",frameBorder="0", height="5000px",id="model_iframe")
  })

  model.comp.checks <- reactive({

    ret.checks <- c( input$model.comp.mitre.curve, input$model.comp.mitre.ens, input$model.comp.cu_80,
                      input$model.comp.ihme, input$model.comp.austin, input$model.comp.geneva, input$model.comp.mobs, input$model.comp.yyg)
    names(ret.checks) <- c("C19HCC Short-term", "C19HCC Ensemble", "Columbia University (20% Contact Reduction)",
                           "IHME", "UT Austin", "University of Geneva", "Northeastern (MOBS)", "Youyang Gu (YYG)") #, "MITRE Gaussian Process")

    ret.checks <- names(ret.checks)[ret.checks]

    if (length(ret.checks)==0){
      ret.checks <- c("")
    }

    return(ret.checks)
  })

  npi.counter <- reactiveValues(npi.counter.value = 0)
  npi.dates <- reactiveValues(data = data.frame(npi.counter.value=1:10,
                                                npi.name = paste("NPI", 1:10),
                                                npi.effectiveness = rep(0, 10),
                                                start_date = rep(Sys.Date(),10),
                                                end_date = rep(Sys.Date()+ddays(120),10),
                                                stringsAsFactors = FALSE))

  observeEvent(input$add1, {

    if (npi.counter$npi.counter.value <= 9){
      npi.counter$npi.counter.value <- npi.counter$npi.counter.value + 1     # if the add button is clicked, increment the value by 1 and update it

      insertUI(
        session = session,
        selector = paste0("#custom_npi_", npi.counter$npi.counter.value, "_params_span"),

        ui = tagList(
          tags$td(textInput(inputId = paste0("custom.npi.", npi.counter$npi.counter.value, ".name"), label = "", value = paste("Custom NPI", npi.counter$npi.counter.value)), style="width: 40%; font-weight: bold;"),
          #tags$td(numericInput(paste0("custom.npi.", npi.counter$npi.counter.value, ".prop.mild"), label = "", min = 0, max=1, value=0.0, step = 0.01)),
          tags$td(numericInput(paste0("custom.npi.", npi.counter$npi.counter.value, ".prop.severe"), label = "", min = 0, max=1, value=0, step = 0.01)),
          tags$td(numericInput(paste0("custom.npi.", npi.counter$npi.counter.value, ".prop.critical"), label = "", min = 0, max=1, value=0, step = 0.01))
          )
      )
    }else{
      sendSweetAlert(session = session,
                     title = "Custom NPI Error",
                     text = "Error: Only 10 Custom NPIs can be added to the table at this time",
                     type = "error")
    }
  })


  observeEvent(input$add.inter.form.confirm, {
    if (isTRUE(input$add.inter.form.confirm)) {

      if (npi.counter$npi.counter.value <= 9){
        npi.counter$npi.counter.value <- npi.counter$npi.counter.value + 1

        npi.dates$data[npi.counter$npi.counter.value, "npi.name"] <- input$new.inter.name
        npi.dates$data[npi.counter$npi.counter.value, "npi.effectiveness"] <- input$new.inter.effectiveness
        npi.dates$data[npi.counter$npi.counter.value, "start_date"] <- input$new.inter.dates[1]
        npi.dates$data[npi.counter$npi.counter.value, "end_date"] <- input$new.inter.dates[2]

        insertUI(
          session = session,
          selector = paste0("#custom_npi_", npi.counter$npi.counter.value, "_params_span"),

          ui = tagList(
            tags$td(textInput(inputId = paste0("custom.npi.", npi.counter$npi.counter.value, ".name"), label = "", value = input$new.inter.name), style="width: 40%; font-weight: bold;"),
            #tags$td(numericInput(paste0("custom.npi.", npi.counter$npi.counter.value, ".prop.mild"), label = "", min = 0, max=1, value=(input$new.inter.effectiveness/100), step = 0.01)),
            tags$td(numericInput(paste0("custom.npi.", npi.counter$npi.counter.value, ".prop.severe"), label = "", min = 0, max=1, value=0, step = 0.01)),
            tags$td(numericInput(paste0("custom.npi.", npi.counter$npi.counter.value, ".prop.critical"), label = "", min = 0, max=1, value=0, step = 0.01))
          )
        )


      }else{
        sendSweetAlert(session = session,
                       title = "Custom NPI Error",
                       text = "Error: Only 10 Custom NPIs can be added to the table at this time",
                       type = "error")
      }
    } else {
      reset("new.inter.name")
      reset("new.inter.dates")
      reset("new.inter.effectiveness")
    }
  }, ignoreNULL = TRUE)

  observeEvent(input$add.inter.form, {

    # reset inputs; prep for new ones
    reset("new.inter.name")
    reset("new.inter.dates")
    reset("new.inter.effectiveness")

    confirmSweetAlert(
      session=session,
      inputId = "add.inter.form.confirm",
      type = "info",
      title = NULL,
      text = tags$div(
        column(width = 8, offset = 2,
               textInput(inputId = "new.inter.name", label = "NPI Name", value = paste("Policy",npi.counter$npi.counter.value+1), placeholder = "Policy", width = "100%"),
               dateRangeInput(inputId = "new.inter.dates", label = "Implementation Dates", start = Sys.Date(), end =  Sys.Date() + ddays(120),
                              min = as.Date("2020-01-01"), max = as.Date("2021-01-01"), format = "mm/dd/yyyy",
                              width = "100%"),
               sliderInput(inputId = "new.inter.effectiveness", label = "NPI Effectiveness", min = 0, max=100, step = 1, post="%",value = 20,
                           dragRange = FALSE, width = "100%")
        )
      ),
      html=TRUE
    )
  })

  observeEvent(input$reg_spread_link,{
    updateTabItems(session = session, inputId = "sidebarTabs", "region_spread")
  })
  
  observeEvent(input$mortality_analysis_link,{
    updateTabItems(session = session, inputId = "sidebarTabs", "mortality")
  })

  observeEvent(input$reg_comp_link,{
    updateTabItems(session = session, inputId = "sidebarTabs", "region_comparison")
  })
  
  observeEvent(input$similarity_link,{
    updateTabItems(session = session, inputId = "sidebarTabs", "region_similar")
  })
  
  observeEvent(input$model_link,{
    updateTabItems(session = session, inputId = "sidebarTabs", "npi_impact")
  })
  
  observeEvent(input$model_comp_link,{
    updateTabItems(session = session, inputId = "sidebarTabs", "model_comparison")
  })

  observeEvent(input$forecast_link,{
    updateTabItems(session = session, inputId = "sidebarTabs", "forecast")
  })

  observeEvent(input$sources_link,{
    updateTabItems(session = session, inputId = "sidebarTabs", "sources")
  })
  
  observeEvent(input$region_sah_comp_link,{
    updateTabItems(session = session, inputId = "sidebarTabs", "region_sah_comp")
  })

  observeEvent(input$hotspots_link,{
    updateTabItems(session = session, inputId = "sidebarTabs", "hotspot")
  })



  observeEvent(input$sub1proxy, {
    click(id = "sub1")
  })

  observeEvent(input$sub1, {
    if (npi.counter$npi.counter.value >=1 ) {
      removeUI(
        session = session,
        multiple = TRUE,
        ## pass in appropriate div id
        selector = paste0("table#npi_param_table tr#custom_npi_", npi.counter$npi.counter.value, "_params_span td")
      )

      npi.counter$npi.counter.value <- npi.counter$npi.counter.value - 1     # if the add button is clicked, increment the value by 1 and update it
    }else{
      sendSweetAlert(session = session,
                     title = "Custom NPI Error",
                     text = "Error: No Custom NPIs to remove.",
                     type = "error")
    }
  })

  output$custom.npi.1.calendar.ui <- renderUI({
    if (npi.counter$npi.counter.value>=1){
      req(input$custom.npi.1.name)
      ret.ui <- fluidRow(style="border-top: 1px solid grey; margin-left: 1%; margin-right: 1%;",
                         column(width = 4, checkboxInput(inputId = "interv.custom.npi.1", label = npi.dates$data[1, "npi.name"], value = TRUE)),
                         column(width = 4, dateRangeInput(inputId = "interv.custom.npi.1.date", label = NULL, start=npi.dates$data[1, "start_date"], end=npi.dates$data[1, "end_date"],
                                                          format = "mm/dd/yyyy", min = "2020-02-01",max = "2021-12-31")),
                         column(width = 4, numericInput("custom.npi.1.prop.mild", label = NULL, min = 0, max=1, value=npi.dates$data[1, "npi.effectiveness"]/100, step = 0.01))
      )
      return(ret.ui)
    }
  })

  output$custom.npi.2.calendar.ui <- renderUI({
    if (npi.counter$npi.counter.value>=2){
      req(input$custom.npi.2.name)
      ret.ui <- fluidRow(style="border-top: 1px solid grey; margin-left: 1%; margin-right: 1%;",
                         column(width = 4, checkboxInput(inputId = "interv.custom.npi.2", label = npi.dates$data[2, "npi.name"], value = TRUE)),
                         column(width = 4, dateRangeInput(inputId = "interv.custom.npi.2.date", label = NULL, start=npi.dates$data[2, "start_date"], end=npi.dates$data[2, "end_date"],
                                                          format = "mm/dd/yyyy", min = "2020-02-01",max = "2021-12-31")),
                         column(width = 4, numericInput("custom.npi.2.prop.mild", label = NULL, min = 0, max=1, value=npi.dates$data[2, "npi.effectiveness"]/100, step = 0.01))
      )
      return(ret.ui)
    }
  })

  output$custom.npi.3.calendar.ui <- renderUI({
    if (npi.counter$npi.counter.value>=3){
      req(input$custom.npi.3.name)
      ret.ui <- fluidRow(style="border-top: 1px solid grey; margin-left: 1%; margin-right: 1%;",
                         column(width = 4, checkboxInput(inputId = "interv.custom.npi.3", label = npi.dates$data[3, "npi.name"], value = TRUE)),
                         column(width = 4, dateRangeInput(inputId = "interv.custom.npi.3.date", label = NULL, start=npi.dates$data[3, "start_date"], end=npi.dates$data[3, "end_date"],
                                                          format = "mm/dd/yyyy", min = "2020-02-01",max = "2021-12-31")),
                         column(width = 4, numericInput("custom.npi.3.prop.mild", label = NULL, min = 0, max=1, value=npi.dates$data[3, "npi.effectiveness"]/100, step = 0.01))
      )
      return(ret.ui)
    }
  })

  output$custom.npi.4.calendar.ui <- renderUI({
    if (npi.counter$npi.counter.value>=4){
      req(input$custom.npi.4.name)
      ret.ui <- fluidRow(style="border-top: 1px solid grey; margin-left: 1%; margin-right: 1%;",
                         column(width = 4, checkboxInput(inputId = "interv.custom.npi.4", label = npi.dates$data[4, "npi.name"], value = TRUE)),
                         column(width = 4, dateRangeInput(inputId = "interv.custom.npi.4.date", label = NULL, start=npi.dates$data[4, "start_date"], end=npi.dates$data[4, "end_date"],
                                                          format = "mm/dd/yyyy", min = "2020-02-01",max = "2021-12-31")),
                         column(width = 4, numericInput("custom.npi.4.prop.mild", label = NULL, min = 0, max=1, value=npi.dates$data[4, "npi.effectiveness"]/100, step = 0.01))
      )
      return(ret.ui)
    }
  })

  output$custom.npi.5.calendar.ui <- renderUI({
    if (npi.counter$npi.counter.value>=5){
      req(input$custom.npi.5.name)
      ret.ui <- fluidRow(style="border-top: 1px solid grey; margin-left: 1%; margin-right: 1%;",
                         column(width = 4, checkboxInput(inputId = "interv.custom.npi.5", label = npi.dates$data[5, "npi.name"], value = TRUE)),
                         column(width = 4, dateRangeInput(inputId = "interv.custom.npi.5.date", label = NULL, start=npi.dates$data[5, "start_date"], end=npi.dates$data[5, "end_date"],
                                                          format = "mm/dd/yyyy", min = "2020-02-01",max = "2021-12-31")),
                         column(width = 4, numericInput("custom.npi.5.prop.mild", label = NULL, min = 0, max=1, value=npi.dates$data[5, "npi.effectiveness"]/100, step = 0.01))
      )
      return(ret.ui)
    }
  })

  output$custom.npi.6.calendar.ui <- renderUI({
    if (npi.counter$npi.counter.value>=6){
      req(input$custom.npi.6.name)
      ret.ui <- fluidRow(style="border-top: 1px solid grey; margin-left: 1%; margin-right: 1%;",
                         column(width = 4, checkboxInput(inputId = "interv.custom.npi.6", label = npi.dates$data[6, "npi.name"], value = TRUE)),
                         column(width = 4, dateRangeInput(inputId = "interv.custom.npi.6.date", label = NULL, start=npi.dates$data[6, "start_date"], end=npi.dates$data[6, "end_date"],
                                                          format = "mm/dd/yyyy", min = "2020-02-01",max = "2021-12-31")),
                         column(width = 4, numericInput("custom.npi.6.prop.mild", label = NULL, min = 0, max=1, value=npi.dates$data[6, "npi.effectiveness"]/100, step = 0.01))
      )
      return(ret.ui)
    }
  })

  output$custom.npi.7.calendar.ui <- renderUI({
    if (npi.counter$npi.counter.value>=7){
      req(input$custom.npi.7.name)
      ret.ui <- fluidRow(style="border-top: 1px solid grey; margin-left: 1%; margin-right: 1%;",
                         column(width = 4, checkboxInput(inputId = "interv.custom.npi.7", label = npi.dates$data[7, "npi.name"], value = TRUE)),
                         column(width = 4, dateRangeInput(inputId = "interv.custom.npi.7.date", label = NULL, start=npi.dates$data[7, "start_date"], end=npi.dates$data[7, "end_date"],
                                                          format = "mm/dd/yyyy", min = "2020-02-01",max = "2021-12-31")),
                         column(width = 4, numericInput("custom.npi.7.prop.mild", label = NULL, min = 0, max=1, value=npi.dates$data[7, "npi.effectiveness"]/100, step = 0.01))
      )
      return(ret.ui)
    }
  })

  output$custom.npi.8.calendar.ui <- renderUI({
    if (npi.counter$npi.counter.value>=8){
      req(input$custom.npi.8.name)
      ret.ui <- fluidRow(style="border-top: 1px solid grey; margin-left: 1%; margin-right: 1%;",
                         column(width = 4, checkboxInput(inputId = "interv.custom.npi.8", label = npi.dates$data[8, "npi.name"], value = TRUE)),
                         column(width = 4, dateRangeInput(inputId = "interv.custom.npi.8.date", label = NULL, start=npi.dates$data[8, "start_date"], end=npi.dates$data[8, "end_date"],
                                                          format = "mm/dd/yyyy", min = "2020-02-01",max = "2021-12-31")),
                         column(width = 4, numericInput("custom.npi.8.prop.mild", label = NULL, min = 0, max=1, value=npi.dates$data[8, "npi.effectiveness"]/100, step = 0.01))
      )
      return(ret.ui)
    }
  })

  output$custom.npi.9.calendar.ui <- renderUI({
    if (npi.counter$npi.counter.value>=9){
      req(input$custom.npi.9.name)
      ret.ui <- fluidRow(style="border-top: 1px solid grey; margin-left: 1%; margin-right: 1%;",
                         column(width = 4, checkboxInput(inputId = "interv.custom.npi.9", label = npi.dates$data[9, "npi.name"], value = TRUE)),
                         column(width = 4, dateRangeInput(inputId = "interv.custom.npi.9.date", label = NULL, start=npi.dates$data[9, "start_date"], end=npi.dates$data[9, "end_date"],
                                                          format = "mm/dd/yyyy", min = "2020-02-01",max = "2021-12-31")),
                         column(width = 4, numericInput("custom.npi.9.prop.mild", label = NULL, min = 0, max=1, value=npi.dates$data[9, "npi.effectiveness"]/100, step = 0.01))
      )
      return(ret.ui)
    }
  })

  output$custom.npi.10.calendar.ui <- renderUI({
    if (npi.counter$npi.counter.value>=10){
      req(input$custom.npi.10.name)
      ret.ui <- fluidRow(style="border-top: 1px solid grey; margin-left: 1%; margin-right: 1%;",
                         column(width = 4, checkboxInput(inputId = "interv.custom.npi.10", label = npi.dates$data[10, "npi.name"], value = TRUE)),
                         column(width = 4, dateRangeInput(inputId = "interv.custom.npi.10.date", label = NULL, start=npi.dates$data[10, "start_date"], end=npi.dates$data[10, "end_date"],
                                                          format = "mm/dd/yyyy", min = "2020-02-01",max = "2021-12-31")),
                         column(width = 4, numericInput("custom.npi.10.prop.mild", label = NULL, min = 0, max=1, value=npi.dates$data[10, "npi.effectiveness"]/100, step = 0.01))
      )
      return(ret.ui)
    }
  })

  output$model.comp.info.table <- DT::renderDataTable({
    req(model.comp.checks())

    model.dates <- CDC_COMP_DAT %>%
      select(fc.name, forecast_date) %>% distinct()

    model.list <- model.comp.checks()
    model.info.df <- MODEL_DESCR %>%
      filter(Model %in% model.list | Model %in% str_remove_all(model.list, pattern = "( \\(.*\\))")) %>%
      left_join(model.dates, by = c("model_var_name"="fc.name")) %>%
      group_by(Model) %>%
      mutate(
        forecast_date = as.Date(forecast_date),
        forecast_date = ifelse(str_detect(toupper(model_var_name), "MITRE"), Sys.Date(),  min(forecast_date, na.rm=TRUE)),
        forecast_date = as.Date(forecast_date, origin="1970-01-01")) %>%
      ungroup() %>%
      select(-model_var_name) %>%
      distinct() %>%
      mutate(
        Assumptions = ifelse(str_detect(Assumptions, pattern = "; "), #if semicolon - then bullet point
                             paste0("<li>", str_replace_all(Assumptions, pattern = "; ", replacement = "</li><li>"), "</li>"),
                             Assumptions),
        #Strengths = ifelse(str_detect(Strengths, pattern = "; "), #if semicolon - then bullet point
        #                   paste0("<li>", str_replace_all(Strengths, pattern = "; ", replacement = "</li><li>"), "</li>"),
        #                   Strengths),
        Limitations = ifelse(str_detect(Limitations, pattern = "; "), #if semicolon - then bullet point
                             paste0("<li>", str_replace_all(Limitations, pattern = "; ", replacement = "</li><li>"), "</li>"),
                             Limitations),
        "Forecast Date" = format(forecast_date, "%b %d, %Y")
      ) %>%
      select(-forecast_date,-Strengths)
    
    DT::datatable(model.info.df, 
                  rownames = FALSE, filter = "none", editable = FALSE, escape = FALSE,
                  options = list(searching=FALSE, paging=FALSE, ordering=FALSE, info = FALSE)) %>%
      formatStyle(columns = 'Model', fontWeight = 'bold')
    
  })

  output$tuning_date <- renderUI({
    # if states tuned on different dates, choose earliest date to set expectations
    tuning_date <- format(min(STATE_TUNING_PARAMS$tuning_date), format="%b %d, %Y")
    div(
      tags$b("Last Date of Model Tuning:    "),
      tuning_date
    )
  })

  #===============SPREAD CALLOUTS=================#
  values$callout_color = reactive({'navy'})

  output$calloutbox1 <- renderUI({
    if(input$summary_type == 'Resources'){
      val = values$weekly_regional_test_data()[['total_positive']]
      txt = 'Positive Tests (Since Last Mon.)'
      icn = 'vial'
    }else{
      val = values$regional_covid_stats()[['tot_cases']]
      txt = 'Cumulative Cases'
      icn = 'hospital'}
    if(input$per_capita_stats){
      val = round(val/values$regional_covid_stats()$pop * values$cap_scale(), 2)
      txt = paste0(txt, ' per 100,000')
    }
    
    vboxCustom(comma(val), txt, width=2, icon(icn, class='callout-icon'))
  })
  
  output$calloutbox2 <- renderUI({
    if(input$summary_type == 'Resources'){
      val = values$weekly_regional_test_data()[['positive_rate']]
      val = paste0(val, " %")
      txt = 'Test Positivity (Since Last Mon.)'
      icn = 'vial'
    }else{
      val = format(as.Date(values$regional_covid_stats()[['fi_date']][[1]]), "%m-%d-%y")
      txt =  "First Infection Date"
      icn = 'calendar'
    }
    vboxCustom(val, txt, width=2, icon(icn, class='callout-icon'))
  })
  
  output$calloutbox3 <- renderUI({
    if(input$summary_type == 'Resources'){
      val = sum(values$regional_test_data()$hospitalized, na.rm=T)
      txt = ' Total Hospitalizations'
      digs=0
      icn='hospital'
    }else{
      val = values$regional_covid_stats()[['tot_deaths']]
      txt = 'Cumulative Deaths'
      icn = 'user-alt-slash'
    }
    if(input$per_capita_stats){
      val = round(val/values$regional_covid_stats()$pop * values$cap_scale(), 2)
      txt = paste0(txt, ' per 100,000')
    }
    vboxCustom(comma(val), txt, width=2, icon(icn, class='callout-icon'))
  })
  
  output$calloutbox4 <- renderUI({
    if(input$summary_type == 'Resources'){
      val = sum(values$regional_hosp_data()[,'AVAIL_ICU_BEDS'], na.rm=T)
      txt = 'Total ICU Bed Cap.'
      icn = 'first-aid'
      if(input$per_capita_stats){
        val = round(val/values$regional_covid_stats()$pop * values$cap_scale(), 2)
        txt = paste0(txt, ' per 100,000')
      }
      val = comma(val)
    }else{
      val = format(as.Date(values$regional_covid_stats()[['fd_date']][[1]]), "%m-%d-%y")
      txt =  "First Death Date "
      icn = 'calendar'
    }
    vboxCustom(val, txt, width=2, icon(icn, class='callout-icon'))
  })
  
  output$calloutbox5 <- renderUI({
    if(input$summary_type == 'Resources'){
      val = sum(values$regional_hosp_data()[,'AVAIL_BEDS'], na.rm=T)
      txt = 'Total Hospital Bed Cap.'
      icn = 'bed'
      if(input$per_capita_stats){
        val = round(val/values$regional_covid_stats()$pop * values$cap_scale(), 2)
        txt = paste0(txt, ' per 100,000')
      }
      val = comma(val)
    }else{
      val = round(values$regional_covid_stats()[['doubling_time']][[1]], 1)
      val = paste0(val, " Days")
      txt =  paste0('Doubling Time (Last ', values$doubling_days(), ' days)')
      icn = 'signal'
    }
    vboxCustom(val, txt, width=2, icon(icn, class='callout-icon'))
  })

  output$calloutbox6 <- renderUI({
    if(input$summary_type != 'Resources'){
      val = round(values$regional_covid_stats()[['rt']][[1]], 2)
      txt = HTML(paste0("Reproduction Rate (R",tags$sub("t"),")"))
      icn = 'signal'
      if(val <= 1) col='olive'
      else if(val <= 1.5) col='yellow'
      else col='red'
      vboxCustom(val, txt, width=2, icon(icn, class='callout-icon'))
    }else{
      div()
    }

  })
  
  comparison.region.list <- reactive({
    ret.list <- c()
    # counties
    if (length(input$comp_counties)>0 & (!input$comp_yaxis %in% c("testing", "hospitalizations"))){
      #adding 3 underscores
      ret.list <- c(ret.list, paste(input$comp_counties, "United States", sep="___"))
    }
    # metropolitan areas
    if (length(input$comp_regions_local)>0 & (!input$comp_yaxis %in% c("R_t", "mobility", "testing", "hospitalizations"))){
      #adding 3 underscores
      ret.list <- c(ret.list, paste(input$comp_regions_local, "United States", sep="___"))
    }
    # states
    if (length(input$comp_regions)>0){
      #adding 3 underscores
      
      new.regions <- STATE_INF_STATS %>%
        filter(mState.Providence %in% input$comp_regions) %>%
        select(mState.Providence, Country.Region) %>%
        distinct() %>%
        mutate(state.country.key = paste(mState.Providence, Country.Region, sep = "___")) %>%
        .$state.country.key
      
      ret.list <- c(ret.list, unique(new.regions))
    }
    # countries
    if (length(input$comp_regions_national)>0 & (!input$comp_yaxis %in% c("R_t", "mobility", "testing"))){
      ret.list <- c(ret.list, paste(input$comp_regions_national, input$comp_regions_national, sep="___"))
    }

    return(ret.list)

  })
  
  refactored.region.comp.df <- reactive({
    req(comparison.region.list())
    req(input$comp_yaxis)

    if (input$comp_yaxis %in% c("R_t", "mobility", "testing", "hospitalizations")){
      selected.y <- ifelse(input$comp_yaxis == "R_t", "Reproduction_Rate", simpleCap(input$comp_yaxis))
      link.cases <- FALSE
    }else{
      selected.y <- "Deaths"
      link.cases <- TRUE
    }

    if (length(comparison.region.list())>0){
      ret.obj <- create_comp_df(region.list = comparison.region.list(),
                                selected.x = input$comp_xaxis, selected.y = selected.y, y.scale = input$comp_yaxis,
                                overlay.npis = input$comp_overlay_npis, link.cases.plot = link.cases, gantt.chart=input$comp_overlay_npis)
    }else{
      ret.obj <- list()
    }

    return(ret.obj)
  })

  region.comp.hover <- reactiveVal(value = NULL)

  observeEvent(suppressWarnings(event_data(event = "plotly_hover", source = "H")), {

    if (length(comparison.region.list())>3){
      hover.val <- event_data(event = "plotly_hover", source = "H")$customdata
      region.comp.hover(hover.val) #sets value of region.comp.hover
    }else{
      region.comp.hover(NULL)
    }
  })


  output$comparison_plots2 <- renderPlotly({
    req(refactored.region.comp.df())

    if (input$comp_yaxis %in% c("R_t", "mobility", "testing", "hospitalizations")){
      selected.y <- ifelse(input$comp_yaxis == "R_t", "Reproduction_Rate", simpleCap(input$comp_yaxis))
      link.cases <- FALSE
    }else{
      selected.y <- "Deaths"
      link.cases <- TRUE
    }

    plt.obj <- refactored.region.comp.df()

    ret.plt <- draw_refactored_comparison_plot(plt.obj=plt.obj, link.cases.plot = link.cases, gantt.chart = input$comp_overlay_npis, hovered.region = region.comp.hover()) %>%
      event_register("plotly_hover")

    return(ret.plt)
  })


  similarity.df <- reactive({
    req(input$sim_region)
    
    if (input$sim_type == "epi") {
      similarity_df = region_similarity_calc(input$sim_region, input$epi.similarity.type)
      dat_len_reg1 <- as.numeric((similarity_df %>% filter(mState.Providence == input$sim_region))$data_len)
    } else {
      req(input$demog.similarity.type)
      
      arg.d <- "d" %in% input$demog.similarity.type
      arg.e <- "e" %in% input$demog.similarity.type
      arg.h <- "h" %in% input$demog.similarity.type
      
      similarity_df = region_similarity_calc_demographics(demog.flag = arg.d, epid.flag = arg.e, health.flag = arg.h, input$sim_region)
      similarity_df <- similarity_df %>% left_join(SIM_REGION_LIST[,c("mState.Providence", "data_len")], by = c("region" = "mState.Providence"))
      dat_len_reg1 <- as.numeric((similarity_df %>% filter(region == input$sim_region))$data_len)
    }

    similarity_df$data_len_diff = similarity_df$data_len - as.numeric(dat_len_reg1)

    if (input$sim_ahead_only) {
      if (input$sim_type == "epi") {
        sim_rank <- similarity_df %>%
          filter((data_len_diff >= 7) | (mState.Providence == input$sim_region)) %>%
          arrange(-similarity)
      } else {
        sim_rank <- similarity_df %>%
          filter((data_len_diff >= 7) | (region == input$sim_region)) %>%
          arrange(-similarity)
      }
    } else {
      sim_rank <- similarity_df %>% 
        arrange(-similarity)
    }
    
    if (input$sim_county_only & input$sim_type == "epi") {
      sim_rank <- sim_rank %>% filter(region_type == "County")
    }
    
    return(sim_rank)
  })

  sim.region.list <- reactive({
    req(input$sim_region)

    sim_rank <- similarity.df()

    ret.list <- c()
    topn = min(5, nrow(sim_rank)-1)
    
    for (i in 1:(topn+1)) {
      if (input$sim_type == "epi") {
        if (sim_rank[i,2] == "Country") {
          ret.list <- c(ret.list, paste(sim_rank[i,1], sim_rank[i,1], sep="___"))
        }else {
          ret.list <-  c(ret.list, paste(sim_rank[i,1], "United States", sep="___"))
        }
      }else {
        ret.list <-  c(ret.list, paste(sim_rank[i,1], "United States", sep="___"))
      }
    }
    
    return(ret.list)
  })

  sim.region.list.mod <- reactive({
    req(input$sim_region)
    req(input$sim_table_rows_selected)

    sim_rank <- sim.table()
    rnum <- which(sim_rank[,2]==input$sim_region)


    ret.list <- c()
    topn = min(5, nrow(sim_rank)-1)
    
    for (i in c(rnum,input$sim_table_rows_selected)) {
      if (input$sim_type == "epi") {
        if (sim_rank[i,3] == "Country") {
          ret.list <- c(ret.list, paste(sim_rank[i,2], sim_rank[i,2], sep="___"))
        }
        else {
          ret.list <-  c(ret.list, paste(sim_rank[i,2], "United States", sep="___"))
        }
      } else {
        ret.list <-  c(ret.list, paste(sim_rank[i,2], "United States", sep="___"))
      }
    }

    return(ret.list)
  })
  
  output$sim.warning.ui <- renderUI({
    
    sim_rank <- similarity.df()
    topn = min(5, nrow(sim_rank)-1)
    
    if (topn < 5){
      ret.ui <- box(
        tags$b(icon("exclamation-triangle"), ' Warning: The current options have artificially restricted the number of regions available for comparison. Select another county, change the similarity type, or disable the "Only Include Regions 1+ Week Ahead" filter to broaden the comparison.'),
        title=NULL, footer=NULL, width = "100%", style = "background-color: #0D2F4F; color: #FFF601;")
      return(ret.ui)
    }
  })
  
  output$spread_county_disclaimer <- renderUI({

    req(input$chosen_state)
    req(input$chosen_county)
    if (!is.null(input$chosen_county) & input$chosen_county != "Statewide"){
      
      if (input$plot_metric == "Testing"){
        disclaimer.text <- ' Note: County selection only updates time series plot underneath the map and testing data is not available at the county-level. Callout metrics reflect state-level values.'
      }else{
        disclaimer.text <- ' Note: County selection only updates time series plot underneath the map. Callout metrics reflect state-level values.'
      }
      
      ret.ui <- box(
        tags$table(
          tags$tr(
            tags$td(tags$b(icon("exclamation-triangle")), style = "padding-right: 2.5px; font-size: 2.4em;"),
            tags$td(tags$b(disclaimer.text), style = "padding-left: 2.5px;")
          )
        ),
        title=NULL, footer=NULL, width = "100%", style = "background-color: #0D2F4F; color: #FFF601;")
      return(ret.ui)
    }
  })

  output$region.comp.disclaimer <- renderUI({
    req(refactored.region.comp.df())
    req(comparison.region.list())

    full.region.list <- comparison.region.list()

    regions.present <- refactored.region.comp.df()[['plt.dat']] %>%
      filter(!is.na(y_val)) %>%
      .$state.region.key %>%
      unique()

    missing.regions <- str_split(string = full.region.list[!(full.region.list %in% regions.present)],
                                 pattern = "___", simplify = TRUE)

    if (length(missing.regions)>0){
      missing.regions = missing.regions[,1]
      disclaimer.text <- paste0(" Error: The following regions cannot be displayed with the selected options: ", politeList(missing.regions))

      ret.ui <- box(
        tags$table(
          tags$tr(
            tags$td(tags$b(icon("exclamation-triangle")), style = "padding-right: 2.5px; font-size: 2.4em;"),
            tags$td(tags$b(disclaimer.text), style = "padding-left: 2.5px;")
          )
        ),
        title=NULL, footer=NULL, width = "100%", style = "background-color: #0D2F4F; color: #FFF601;")
      return(ret.ui)
    }
  })

  sim.region.list.shift <- reactive({
    req(input$sim_region)
    
    sim_rank <- similarity.df()

    if (input$sim_type == "epi") {
      ret.list <- c()
      ret.shift <- c()
      topn = min(5, nrow(sim_rank)-1)
      for (i in 1:(topn+1)) {
        if (sim_rank[i,2] == "Country") {
          ret.list <- c(ret.list, paste(sim_rank[i,1], sim_rank[i,1], sep="___"))
          ret.shift <- c(ret.shift, as.numeric(sim_rank[i,"shift"]))
        }
        else {
          ret.list <-  c(ret.list, paste(sim_rank[i,1], "United States", sep="___"))
          ret.shift <- c(ret.shift, as.numeric(sim_rank[i,"shift"]))
        }
      }
  
      return(data.frame(ret.list, ret.shift)) 
    } else {
      ret.list <- c()
      topn = min(5, nrow(sim_rank)-1)
      ret.shift <- rep(0,topn+1)
      for (i in 1:(topn+1)) {
        if (sim_rank[i,2] == "Country") {
          ret.list <- c(ret.list, paste(sim_rank[i,1], sim_rank[i,1], sep="___"))
        }
        else {
          ret.list <-  c(ret.list, paste(sim_rank[i,1], "United States", sep="___"))
        }
      }
      return(data.frame(ret.list, ret.shift)) 
    }
  })

  output$sim_plot_cases <- renderPlotly({
    req(input$sim_yaxis)
    req(sim.region.list())
    
    if (input$sim_yaxis %in% c("R_t", "mobility", "testing")){
      selected.y <- ifelse(input$sim_yaxis == "R_t", "Reproduction_Rate", simpleCap(input$sim_yaxis))
    }else{
      selected.y <- "Cases"
    }
    
    draw_comparisons(sim.region.list(), input$sim_xaxis, selected.y=selected.y,
                     overlay.npis=FALSE, y.scale=input$sim_yaxis, plot.title=TRUE,
                     time.shift = sim.region.list.shift(), gantt.chart = input$sim_overlay_npis)
  })

  region.sim.hover <- reactiveVal(value = NULL)

  observeEvent(suppressWarnings(event_data(event = "plotly_hover", source = "S")), ignoreInit = TRUE, {

    if (length(sim.region.list())>3){
      hover.val <- event_data(event = "plotly_hover", source = "S")$customdata
      region.sim.hover(hover.val) #sets value of region.comp.hover
    }else{
      region.sim.hover(input$sim_region)
    }
  })

  similarity_plot_cases_df <- reactive({
    req(sim.region.list())
    req(input$sim_yaxis)

    if (input$sim_yaxis %in% c("R_t", "mobility", "testing")){
      selected.y <- ifelse(input$sim_yaxis == "R_t", "Reproduction_Rate", simpleCap(input$sim_yaxis))
      link.cases <- FALSE
    }else{
      selected.y <- "Cases"
      link.cases <- FALSE
    }

    if (length(sim.region.list())>0){
      ret.obj <- create_comp_df(region.list = sim.region.list(),
                                selected.x = input$sim_xaxis, selected.y = selected.y, y.scale = input$sim_yaxis,
                                overlay.npis = TRUE, link.cases.plot = link.cases, gantt.chart=input$sim_overlay_npis,
                                time.shift = sim.region.list.shift())
    }else{
      ret.obj <- list()
    }

    return(ret.obj)
  })

  output$sim_plot_cases2 <- renderPlotly({
    req(similarity_plot_cases_df())

    plt.obj <- similarity_plot_cases_df()
    plt.obj[['plt.options']][['source.id']] <- "S"

    hover2 <- ifelse(is.null(region.sim.hover()), input$sim_region, region.sim.hover())

    ret.plt <- draw_refactored_comparison_plot(plt.obj=plt.obj, link.cases.plot = FALSE, gantt.chart = input$sim_overlay_npis, hovered.region = hover2) %>%
      event_register("plotly_hover")

    return(ret.plt)

  })

  output$sim_mod_plot_cases <- renderPlotly({
    req(input$sim_yaxis)
    req(input$sim_xaxis)
    req(input$sim_mod_xaxis)
    req(input$sim_mod_yaxis)
    req(sim.region.list())

    draw_comparisons(sim.region.list.mod(), input$sim_mod_xaxis, selected.y="Cases",
                     overlay.npis=TRUE, y.scale=input$sim_mod_yaxis, plot.title=TRUE,
                     time.shift = sim.region.list.shift(), gantt.chart = input$sim_overlay_npis)
  })
  
  sah.dataframe <- reactive({
    return(SAH_mobility_scatterplot_data())
  })
  
  output$sah_scatterplot <- renderPlotly({
    req(input$sah_yaxis)
    req(input$sah_xaxis)
    
    dat <- sah.dataframe()
    
    if (sum(dat[[input$sah_xaxis]]==0) > 0) {
      output$sah_missing_data_message <- renderText({paste0("The following states did not have stay at home orders for any duration of time: ", 
                                                politeList(dat$state[dat[[input$sah_xaxis]]==0]))})
    }
    else {
      output$sah_missing_data_message <- NULL
    }
    
    if (sum(is.na(dat[[input$sah_yaxis]])) > 0 | sum(is.nan(dat[[input$sah_yaxis]])) > 0 ) {
      output$sah_missing_data_message2 <- renderText({paste0("Sufficient y-axis data is not available for the following states: ", 
                                                             politeList(dat$state[is.na(dat[[input$sah_xaxis]]) | is.nan(dat[[input$sah_yaxis]])])
      )})
    }
    else {
      output$sah_missing_data_message2 <- NULL
    }
    
    suppressWarnings(SAH_mobility_scatterplot(input$sah_xaxis, input$sah_yaxis, dat))
  })

  observeEvent( c(input$model.comp.population.type, input$model.comp.state) , {

    if (input$model.comp.state == "United States"){
      if (input$model.comp.population.type == "D" | input$model.comp.population.type == "DeltaD"){
        enable(id = "model.comp.cu_80")
        enable(id = "model.comp.ihme")
        updateCheckboxInput(session=session, inputId = "model.comp.mitre.ens", value = FALSE)
        disable(id = "model.comp.mitre.ens")
        updateCheckboxInput(session=session, inputId = "model.comp.mitre.curve", value = FALSE)
        disable(id="model.comp.mitre.curve")
        enable(id = "model.comp.austin")
        enable(id = "model.comp.geneva")
        enable(id = "model.comp.mobs")
        enable(id = "model.comp.yyg")
      }else if (input$model.comp.population.type == "C"){
        updateCheckboxInput(session=session, inputId = "model.comp.cu_80", value = FALSE)
        disable(id = "model.comp.cu_80")
        enable(id = "model.comp.ihme")
        updateCheckboxInput(session=session, inputId = "model.comp.mitre.ens", value = FALSE)
        disable(id = "model.comp.mitre.ens")
        updateCheckboxInput(session=session, inputId = "model.comp.mitre.curve", value = FALSE)
        disable(id="model.comp.mitre.curve")
        updateCheckboxInput(session=session, inputId = "model.comp.austin", value = FALSE)
        disable(id = "model.comp.austin")
        updateCheckboxInput(session=session, inputId = "model.comp.geneva", value = FALSE)
        disable(id = "model.comp.geneva")
        updateCheckboxInput(session=session, inputId = "model.comp.mobs", value = FALSE)
        disable(id = "model.comp.mobs")
        updateCheckboxInput(session=session, inputId = "model.comp.yyg", value = FALSE)
        disable(id = "model.comp.yyg")
      }else if (input$model.comp.population.type == "DeltaI2_I3"){
        updateCheckboxInput(session=session, inputId = "model.comp.cu_80", value = FALSE)
        disable(id = "model.comp.cu_80")
        enable(id = "model.comp.ihme")
        updateCheckboxInput(session=session, inputId = "model.comp.mitre.ens", value = FALSE)
        disable(id = "model.comp.mitre.ens")
        updateCheckboxInput(session=session, inputId = "model.comp.mitre.curve", value = FALSE)
        disable(id="model.comp.mitre.curve")
        updateCheckboxInput(session=session, inputId = "model.comp.austin", value = FALSE)
        disable(id = "model.comp.austin")
        updateCheckboxInput(session=session, inputId = "model.comp.geneva", value = FALSE)
        disable(id = "model.comp.geneva")
        updateCheckboxInput(session=session, inputId = "model.comp.mobs", value = FALSE)
        disable(id = "model.comp.mobs")
        updateCheckboxInput(session=session, inputId = "model.comp.yyg", value = FALSE)
        disable(id = "model.comp.yyg")
      }
    }else{
      updateSelectizeInput(session = session, inputId = "inter_state", selected = input$model.comp.state)
      if (input$model.comp.population.type == "D" | input$model.comp.population.type == "DeltaD"){
        enable(id = "model.comp.cu_80")
        enable(id = "model.comp.ihme")
        enable(id = "model.comp.mitre.ens")
        updateCheckboxInput(session=session, inputId = "model.comp.mitre.curve", value = FALSE)
        disable(id="model.comp.mitre.curve")
        enable(id = "model.comp.austin")
        updateCheckboxInput(session=session, inputId = "model.comp.geneva", value = FALSE)
        disable(id = "model.comp.geneva")
        enable(id = "model.comp.mobs")
        enable(id = "model.comp.yyg")
      }else if (input$model.comp.population.type == "C"){
        updateCheckboxInput(session=session, inputId = "model.comp.cu_80", value = FALSE)
        disable(id = "model.comp.cu_80")
        enable(id = "model.comp.ihme")
        enable(id = "model.comp.mitre.ens")
        enable(id="model.comp.mitre.curve")
        updateCheckboxInput(session=session, inputId = "model.comp.austin", value = FALSE)
        disable(id = "model.comp.austin")
        updateCheckboxInput(session=session, inputId = "model.comp.geneva", value = FALSE)
        disable(id = "model.comp.geneva")
        updateCheckboxInput(session=session, inputId = "model.comp.mobs", value = FALSE)
        disable(id = "model.comp.mobs")
        updateCheckboxInput(session=session, inputId = "model.comp.yyg", value = FALSE)
        disable(id = "model.comp.yyg")
      }else if (input$model.comp.population.type == "DeltaI2_I3"){
        updateCheckboxInput(session=session, inputId = "model.comp.cu_80", value = FALSE)
        disable(id = "model.comp.cu_80")
        enable(id = "model.comp.ihme")
        enable(id = "model.comp.mitre.ens")
        enable(id="model.comp.mitre.curve")
        updateCheckboxInput(session=session, inputId = "model.comp.austin", value = FALSE)
        disable(id = "model.comp.austin")
        updateCheckboxInput(session=session, inputId = "model.comp.geneva", value = FALSE)
        disable(id = "model.comp.geneva")
        updateCheckboxInput(session=session, inputId = "model.comp.mobs", value = FALSE)
        disable(id = "model.comp.mobs")
        updateCheckboxInput(session=session, inputId = "model.comp.yyg", value = FALSE)
        disable(id = "model.comp.yyg")
      }
    }


  })

  output$demog.similarity.heatplot <- renderPlotly({
    req(input$demog.similarity.type)
    req(input$sim_region)

    arg.d <- "d" %in% input$demog.similarity.type
    arg.e <- "e" %in% input$demog.similarity.type
    arg.h <- "h" %in% input$demog.similarity.type

    gen_demog_heatmap(demog.flag = arg.d, epid.flag = arg.e, health.flag = arg.h, county.name = input$sim_region)

  })

  output$npi_defs <- renderTable({
    NPI_DEFS
  },striped = TRUE)
  
  output$npi_defs2 <- renderTable({
    NPI_DATA %>%
      select(Label = policy, Policy = policy_long) %>%
      distinct()
  },striped = TRUE)
  
  output$npi_defs3 <- renderTable({
    NPI_DATA %>%
      select(Label = policy, Policy = policy_long) %>%
      distinct()
  },striped = TRUE)

  
  sim.table <- reactive({
    req(input$sim_region)
    sim_rank <- similarity.df()
    if (input$sim_type == "epi") {
      sim_rank <- sim_rank %>% left_join(SIMILARITY_TABLE_VIEW, by = c("mState.Providence" = "mState.Providence"))
      sel_reg <- sim_rank[sim_rank$mState.Providence == input$sim_region,]
      sel_reg$rank = 0
      sim_rank <- sim_rank[sim_rank$mState.Providence != input$sim_region,]
      sim_rank$rank = 1:(nrow(sim_rank))
      sim_rank<- rbind(sel_reg, sim_rank)

      sim_rank<- sim_rank %>%
        mutate(pop_density = round(pop_density,1),
               staffed_beds_perM = round(staffed_beds_perM,1),
               icu_beds_perM = round(icu_beds_perM,1)
        ) %>%
        rename(region = mState.Providence) %>%
        select("Rank" = rank, "Region" = region, "Region Type" = region_type,
               "Population" = population, "Density per Sq. Mile" = pop_density,
               "Staffed Beds per 100k" = staffed_beds_perM, "ICU Beds per 100k" = icu_beds_perM,
               "Pop. in Long Term Care (%)" = nursing_pop_percent, "Pop. Age 65+ (%)" = pop_65_percent
        ) %>%
        distinct()
    } else {
      sim_rank <- sim_rank %>%
        left_join(SIMILARITY_TABLE_VIEW, by = c("region" = "mState.Providence"))
      sel_reg <- sim_rank[sim_rank$region == input$sim_region,]
      sel_reg$rank = 0
      sim_rank <- sim_rank %>%
        filter(region != input$sim_region) %>%
        mutate(rank=1:(n()))
      sim_rank<- rbind(sel_reg, sim_rank)
      sim_rank <- sim_rank %>%
        mutate(pop_density = round(pop_density,1),
               staffed_beds_perM = round(staffed_beds_perM,1),
               icu_beds_perM = round(icu_beds_perM,1),
               region_type = "County") %>%
        select("Rank" = rank, "Region" = region, "Region Type" = region_type,
               "Population" = population, "Density per Sq. Mile" = pop_density,
               "Staffed Beds per 100k" = staffed_beds_perM, "ICU Beds per 100k" = icu_beds_perM,
               "Pop. in Long Term Care (%)" = nursing_pop_percent, "Pop. Age 65+ (%)" = pop_65_percent
        ) %>%
        distinct()
    }
    return(sim_rank)
  })
  
  sim.mod.table <- reactive({
    req(input$sim_region)
    req(input$sim_table_rows_selected)

    sim_rank <- sim.table()

    region2 = as.character(sim_rank[input$sim_table_rows_selected,2])
    col_order = c("Policy", input$sim_region, region2)

    Policy <- c("State-Mandated School Closures", "Non-Essential Business Closures",
                     "Bar/Restaurant Limits", "Large Gatherings Ban",
                     "Stay At Home Order", "Mandatory Masks")

    if(sim_rank[input$sim_table_rows_selected,3] == "Country") {
      ret.dat <- NPI_DATA %>%
        filter(state == input$sim_region | Country.Region == region2, !is.na(start_date)) %>%
        mutate(date_range = paste(format(start_date, "%b %d"), "-", ifelse(is.na(end_date), "Present", format(end_date, "%b %d"))),
               days_enacted = ifelse(is.na(end_date), Sys.Date() - start_date, end_date - start_date),
               valtext = paste0(date_range, " (", days_enacted, " Days)")) %>%
        select("Region" = state, "Policy" = policy_long, "Enaction Dates" = valtext) %>%
        spread(Region, "Enaction Dates")

      if(ncol(ret.dat < 3)) {
        ret.dat[,region2] = NA
      }

      ret.dat <- ret.dat[, col_order]
    } else if (sim_rank[input$sim_table_rows_selected,3] == "Metro Area") {
      ret.dat <- NPI_DATA %>%
        filter(state == input$sim_region | state == region2, !is.na(start_date)) %>%
        group_by(state, policy_long) %>%
        summarise(earliest_start = min(start_date, na.rm = T), latest_end = suppressWarnings(max(end_date, na.rm = T))) %>%
        mutate(date_range = paste(format(earliest_start, "%b %d"), "-", ifelse(is.infinite(latest_end), "Present", format(latest_end, "%b %d"))),
               days_enacted = ifelse(is.infinite(latest_end), Sys.Date() - earliest_start, latest_end - earliest_start),
               valtext = ifelse(is.infinite(earliest_start) & is.infinite(latest_end),NA,paste0(date_range, " (", days_enacted, " Days)"))) %>%
        select("County" = state, "Policy" = policy_long, "Enaction Dates" = valtext) %>%
        spread(County, "Enaction Dates") %>%
        full_join(data.frame(Policy))

      ret.dat <- ret.dat[, col_order]
    } else {
      ret.dat <- NPI_DATA %>%
        filter(state == input$sim_region | state == region2, !is.na(start_date)) %>%
        mutate(date_range = paste(format(start_date, "%b %d"), "-", ifelse(is.na(end_date), "Present", format(end_date, "%b %d"))),
               days_enacted = ifelse(is.na(end_date), Sys.Date() - start_date, end_date - start_date),
               valtext = ifelse(is.na(start_date) & is.na(end_date),NA,paste0(date_range, " (", days_enacted, " Days)"))) %>%
        select("County" = state, "Policy" = policy_long, "Enaction Dates" = valtext) %>%
        spread(County, "Enaction Dates")

      ret.dat <- ret.dat[, col_order]
    }

    return(ret.dat)
  })

  output$sim_table <- renderDataTable(
    {
      sim.table()
    },
    options = list(
      pageLength = 6
      ),
    rownames = FALSE,
    selection = 'single'
    )

  output$sim_mod_table <- renderDataTable(
    {
      sim.mod.table()
    },
    options = list(
      pageLength = 25
    ),
    rownames = FALSE,
    selection = 'single'
  )

  output$sim_type_list <- renderTable({
    Label <- c("Case Growth (Epidemiological)",
               "Demographic")
    Description <- c("Regions share statistically similar epidemiological case growth trends (growth rates, case counts, or case counts per 100,000)",
                     "Regions share similar population, vulnerable population health, and/or health care system characteristics")
    sim_type_curve <- data.frame(Label, Description)
  },striped = TRUE)


  values$npi.weights <- reactive({

    default.policies <- c("State-Mandated School Closures", "Non-Essential Business Closures",
                          "Bar/Restaurant Limits", "Large Gatherings Ban", "Mandatory Quarantine for Travelers",
                          "Stay At Home Order", "Mandatory Masks", "Reopening Plan Reversal")
    def.prop.mild <- c(input$npi.sc.prop.mild, input$npi.nbc.prop.mild,input$npi.bar.prop.mild,
                       input$npi.lgb.prop.mild,input$npi.mqt.prop.mild,input$npi.sah.prop.mild,
                       input$npi.mm.prop.mild, input$npi.rev.prop.mild)
    def.prop.severe <- c(input$npi.sc.prop.severe, input$npi.nbc.prop.severe,input$npi.bar.prop.severe,
                          input$npi.lgb.prop.severe,input$npi.mqt.prop.severe,input$npi.sah.prop.severe,
                         input$npi.mm.prop.severe, input$npi.rev.prop.severe)
    def.prop.death <- c(input$npi.sc.prop.critical, input$npi.nbc.prop.critical,input$npi.bar.prop.critical,
                        input$npi.lgb.prop.critical,input$npi.mqt.prop.critical,input$npi.sah.prop.critical,
                        input$npi.mm.prop.critical, input$npi.rev.prop.critical)

    if (npi.counter$npi.counter.value >= 1){
      req(input[[paste0("custom.npi.", npi.counter$npi.counter.value, ".prop.mild")]])
      req(input[[paste0("custom.npi.", npi.counter$npi.counter.value, ".prop.severe")]])
      req(input[[paste0("custom.npi.", npi.counter$npi.counter.value, ".prop.critical")]])
      req(input[[paste0("custom.npi.", npi.counter$npi.counter.value, ".name")]])

      for (tmp.val in 1:npi.counter$npi.counter.value){
        default.policies <- c(default.policies, input[[paste0("custom.npi.", tmp.val, ".name")]])
        def.prop.mild <- c(def.prop.mild, input[[paste0("custom.npi.", tmp.val, ".prop.mild")]])
        def.prop.severe <- c(def.prop.severe, input[[paste0("custom.npi.", tmp.val, ".prop.severe")]])
        def.prop.death <- c(def.prop.death, input[[paste0("custom.npi.", tmp.val, ".prop.critical")]])
      }
    }

    ret.df <- data.frame(
      Policy = default.policies,
      Prop_Reduction_Mild_Trans = def.prop.mild,
      Prop_Reduction_Severe_Trans = def.prop.severe,
      Prop_Reduction_Death = def.prop.death,
      stringsAsFactors = FALSE
    )

    return(ret.df)

  })

  npi.intervention.dat <- reactive({
    if (is.null(values$npi.weights())){
      npi.table <- NPI_DATA %>%
        filter(policy_long %in% CURRENT_NPIS) %>%
        mutate(exp_start_date = if_else(is.na(start_date), Sys.Date(), as.Date(start_date)),
               exp_start_date = as.Date(exp_start_date),
               exp_end_date = if_else(is.na(end_date),  exp_start_date + days(input$Tmax-1), as.Date(end_date)),
               exp_end_date = as.Date(exp_end_date)) %>%
        group_by(state, Country.Region, state_abb) %>%
        mutate(SAH_value = ifelse("Stay At Home Order" %in% policy_long, Prop_Reduction_Mild_Trans[policy_long=="Stay At Home Order"],0.4)) %>%
               #Scaled_Prop_Reduction_Mild_Trans = Prop_Reduction_Mild_Trans*SAH_value/(sum(Prop_Reduction_Mild_Trans,na.rm = TRUE)-SAH_value),
               #Prop_Reduction_Mild_Trans = ifelse(policy_long=="Stay At Home Order", SAH_value, Scaled_Prop_Reduction_Mild_Trans)) %>%
        ungroup()

    }else{

      new.npi.table <- values$npi.weights()
      names(new.npi.table) <- c("Policy", "Prop_Reduction_Mild_Trans2", "Prop_Reduction_Severe_Trans2","Prop_Reduction_Death2")

      npi.table <- NPI_DATA %>%
        #first join gets any updated parameters; new policies entered as NAs for all state/start_dates
        full_join(new.npi.table, by = c("policy_long"= "Policy")) %>%
        distinct() %>%
        group_by(state, policy_long) %>%
        filter(n() == 1 | (Prop_Reduction_Mild_Trans != Prop_Reduction_Mild_Trans2) |
                 (Prop_Reduction_Severe_Trans != Prop_Reduction_Severe_Trans2) | (Prop_Reduction_Death != Prop_Reduction_Death2) ) %>%
        ungroup() %>%
        mutate(Prop_Reduction_Mild_Trans = Prop_Reduction_Mild_Trans2,
               Prop_Reduction_Severe_Trans = Prop_Reduction_Severe_Trans2,
               Prop_Reduction_Death = Prop_Reduction_Death2) %>%
        select(state, policy, policy_long, start_date, end_date, exp_start_date,
               exp_end_date, Prop_Reduction_Mild_Trans, Prop_Reduction_Severe_Trans,
               Prop_Reduction_Death) %>%
        distinct()

      npi.table$policy[is.na(npi.table$policy)] <- npi.table$policy_long[is.na(npi.table$policy)]

      npi.table <- npi.table %>%
        mutate(exp_start_date = if_else(is.na(start_date), Sys.Date(), as.Date(start_date)),
               exp_start_date = as.Date(exp_start_date),
               exp_end_date = if_else(is.na(end_date),  exp_start_date + days(input$Tmax-1), as.Date(end_date)),
               exp_end_date = as.Date(exp_end_date)) %>%
        group_by(state) %>%
        mutate(SAH_value = ifelse("Stay At Home Order" %in% policy_long,
                                  Prop_Reduction_Mild_Trans[policy_long=="Stay At Home Order"],
                                  0.4)) %>%
               #Scaled_Prop_Reduction_Mild_Trans = Prop_Reduction_Mild_Trans*SAH_value/(sum(Prop_Reduction_Mild_Trans,na.rm = TRUE)-SAH_value),
               #Prop_Reduction_Mild_Trans = ifelse(policy_long=="Stay At Home Order", SAH_value, Scaled_Prop_Reduction_Mild_Trans)) %>%
        ungroup()

    }
    npi.table
  })

  intervention.checks <- reactive({
    checks <- c(input$interv_bar, input$interv_lgb, input$interv_mm, input$interv_nbc, input$interv_sc, input$interv_sah, input$interv_rev)
    check.names <- c( "Bar/Restaurant Limits", "Large Gatherings Ban", "Mandatory Masks", "Non-Essential Business Closures", "State-Mandated School Closures", "Stay At Home Order", "Reopening Plan Reversal")

    if (npi.counter$npi.counter.value >= 1){
      req(input[[paste0("custom.npi.", npi.counter$npi.counter.value, ".name")]])
      if (!is.null(input[[paste0("interv.custom.npi.", npi.counter$npi.counter.value)]])){
        for (tmp.val in 1:npi.counter$npi.counter.value){
          check.input.name <- paste0("interv.custom.npi.", tmp.val)
          npi.input.name <- paste0("custom.npi.", tmp.val, ".name")
          req(input[[npi.input.name]])

          checks <- c(checks, input[[check.input.name]])
          check.names <- c(check.names, input[[npi.input.name]])

        }
      }
    }

    names(checks) <- check.names

    #only return true vals
    checks <- checks[checks==TRUE]
    ret.names <- names(checks)
    
    if (length(ret.names)==0){
      ret.names <- c("")
    }

    return(ret.names)
  })


  npi.check.dates.dat <- reactive({
    req(input$inter_state)
    
    if (!is.null(intervention.checks())){
      if ( all(intervention.checks() != "") ){
        if (is.null(input$inter_county)){
          state <- input$inter_state
        }else if (input$inter_county=="Statewide"){
          state <- input$inter_state
        }else{
          state.abbr <- STATE_NAMES$abbr[STATE_NAMES$full == input$inter_state]
          state <- paste(input$inter_county, state.abbr[1], sep=", ") #formatted as: CountyName County, MD
        }
        
        active.interventions <- intervention.checks()
        possible_intervs <- c("Bar/Restaurant Limits", "Large Gatherings Ban", "Mandatory Masks", "Non-Essential Business Closures", "State-Mandated School Closures", "Stay At Home Order", "Reopening Plan Reversal")
        
        if (npi.counter$npi.counter.value>=1){
          req(input[[paste0("custom.npi.",npi.counter$npi.counter.value,".name")]])
          req(input[[paste0("interv.custom.npi.",npi.counter$npi.counter.value,".date")]])
          possible_intervs_custom <- sapply(1:npi.counter$npi.counter.value, FUN = function(val){return(input[[paste0("custom.npi.",val,".name")]])})
          possible_interv_dates_start <- sapply(1:npi.counter$npi.counter.value, FUN = function(val){return(input[[paste0("interv.custom.npi.",val,".date")]][1])})
          possible_interv_dates_end <- sapply(1:npi.counter$npi.counter.value, FUN = function(val){return(input[[paste0("interv.custom.npi.",val,".date")]][2])})
        }else{
          possible_intervs_custom <- NULL
          possible_interv_dates_start <- NULL
          possible_interv_dates_end <- NULL
        }
        
        possible_intervs <- c(possible_intervs, possible_intervs_custom)
        
        exp_start_date.vec <- c(input$interv_bar_date[1], input$interv_lgb_date[1], input$interv_mm_date[1], input$interv_nbc_date[1], input$interv_sc_date[1], input$interv_sah_date[1], input$interv_rev_date[1], possible_interv_dates_start)
        exp_end_date.vec <- c(input$interv_bar_date[2], input$interv_lgb_date[2], input$interv_mm_date[2], input$interv_nbc_date[2], input$interv_sc_date[2], input$interv_sah_date[2],  input$interv_rev_date[2], possible_interv_dates_end)
        
        interv.dates <- data.frame(
          state = rep(state, length(possible_intervs)),
          policy_long = possible_intervs,
          exp_start_date = exp_start_date.vec,
          exp_end_date = exp_end_date.vec,
          edit.time = rep(Sys.time(), length(possible_intervs)),
          stringsAsFactors = FALSE
        ) %>%
          filter(policy_long %in% active.interventions)
        
      }else{
        interv.dates <- data.frame(matrix(ncol=5,nrow=0, 
                                            dimnames=list(NULL, c("state", "policy_long", 
                                                                  "exp_start_date", "exp_end_date", "edit.time"))), 
                                   stringsAsFactors = FALSE)
      }
      return(interv.dates)
    }
  
  })
  
  npi.interv.sim.dat <- reactive({
    tmp.dat <- npi.intervention.dat()
    if (is.null(npi.check.dates.dat())){
      ret.dat <- tmp.dat
    }else if (nrow(npi.check.dates.dat())==0){
      ret.dat <- tmp.dat
    }else{
      ret.dat <- npi.check.dates.dat() %>%
        select(state, policy_long, exp_start_date2=exp_start_date, exp_end_date2=exp_end_date) %>%
        full_join(tmp.dat, by=c("state", "policy_long")) %>%
        mutate(exp_start_date = if_else(is.na(exp_start_date2), as.Date(exp_start_date), as.Date(exp_start_date2) ),
               exp_end_date = if_else(is.na(exp_end_date2), as.Date(exp_end_date), as.Date(exp_end_date2))) %>%
        select(state, policy, policy_long, start_date, exp_start_date,
               exp_end_date, Prop_Reduction_Mild_Trans, Prop_Reduction_Severe_Trans,
               Prop_Reduction_Death) %>%
        distinct()
    }
    ret.dat
  })

  full.npi.react.df <- reactive({
    #custom interventions/NPIs from here with proportions

    int.eff.df <- npi.intervention.dat() %>%
      select(state, policy, policy_long, start_date, Prop_Reduction_Mild_Trans, Prop_Reduction_Severe_Trans, Prop_Reduction_Death) %>%
      distinct()

    interv.dates.df <- npi.interv.sim.dat() %>%
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
    full.npi.int.df
  })
  

  output$clinical_params <- renderUI({
    
    #defined in database/dataProcessing.R
    clin.params <- CLINICAL_PARAMS$value
    names(clin.params) <- CLINICAL_PARAMS$var.name
    
    ret.ui <- tags$div(
      sliderInput("IncubPeriod", "Duration of incubation period", min=0, max=20, value=clin.params["IncubPeriod"], step=0.1, post = " days"),
      sliderInput("AsymptoCrossSect", "Asymptomatic Scaling Factor", 0, 10, 1, step=.5), #KF I'm not sure what this is, Eric?
      sliderInput("DurMildInf", "Duration of mild infections", min=0, max=20, value=clin.params["DurMildInf"], step=1, post = " days"),
      sliderInput("FracSevere", "% of infections that are severe", min=0, max=100, value=clin.params["FracSevere"], step=1, post="%"),
      sliderInput("FracCritical", "% of infections that are critical", min=0, max=20, value=clin.params["FracCritical"], step=1, post="%"),
      sliderInput("ProbDeath", "Death rate for critical infections", 0, 100, 5.78, step=1, post="%"),
      #htmlOutput("CFR"), clin.params["CFR"] not  currently show
      br(),
      sliderInput("DurHosp", "Duration of hospitalization", min=0, max=20, value= clin.params["DurHosp"], step=1, post = " days"),
      sliderInput("TimeICUDeath", "Duration critical infection/ICU stay", min=0, max=30, value=clin.params["TimeICUDeath"], step=1, post = " days")
    )
    
  })
  
    output$regional_rank_table_ui <- renderUI({
    req(input$granularity)
    ret.ui <- div(box(width=NULL,
                  title = paste0('Top 5 Regions by ', input$geo_metric),
                  tableOutput("regional_rank_table")))
    return(ret.ui)
  })
  
  output$regional_npi_table_ui <- renderUI({
    req(input$granularity)
    if (input$granularity != "National"){
      ret.ui <- div(box(width=NULL,
                    h4('Recent NPIs Taken'), 
                    tableOutput('regional_npi_table')) )
      return(ret.ui)
    } 
    
  })

  p <- eventReactive(input$run_model, {
    req(input$inter_state)
    req(input$inter_county)
    req(full.npi.react.df())

    if (trimws(input$inter_county)=="" | input$inter_county == "Statewide"){
      selected.st <- input$inter_state
    }else{
      state.abbr <- STATE_NAMES$abbr[STATE_NAMES$full==input$inter_state]
      selected.st <- paste(input$inter_county, state.abbr[1], sep = ", ")
    }

    gr.start.date <- as.Date(input$start_date, tryFormats=c("%m/%d/%Y", "%Y-%m-%d"))
    n.days <- input$Tmax

    plt.height <- round(input$dimension[2]*0.2)

    intervention_df <- values$all.intervs.df()

    show_forecasts = TRUE

    if(show_forecasts){fcast_df = isolate({values$model_results()$mobility})}
    else{fcast_df = NULL}

    return(draw_beta_mobility_plot(region_name=selected.st,graph.start.date=gr.start.date, sim.days=n.days,
                            inter.df=intervention_df, plt.height=plt.height, b1.val = input$b1,
                            show_forecasts = show_forecasts, forecast_df = fcast_df))
  })

  output$mobility.beta.plt <- renderPlotly({
    p()
  })

  # updating plot data for intervention graphs

  values$hc_data <- reactive({

    if (is.null(input$inter_state)){
      region_name <- "California"
    }else{
      region_name = input$inter_state
    }
    if (is.null(input$inter_county)){
      ret.dat <- get_region_hosp_data(region_name, HDATA)
    }else{
      if(length(input$inter_county) > 0){
        if (!(trimws(input$inter_county)=="") & (input$inter_county == "Statewide")){
          region_name <- input$inter_county
        }
      }
      ret.dat <- get_region_hosp_data(region_name, HDATA)
    }
    return(ret.dat)
  })

  values$all.intervs.df <- reactive({
    req(input$inter_state)
    req(input$inter_county)
    req(full.npi.react.df())
    selected.interv <- intervention.checks()
    npi.simulation.times.dat <- full.npi.react.df()
    if (trimws(input$inter_county)=="" | input$inter_county == "Statewide"){
      selected.st <- input$inter_state
    }else{
      state.abbr <- STATE_NAMES$abbr[STATE_NAMES$full==input$inter_state]
      selected.st <- paste(input$inter_county, state.abbr[1], sep = ", ")
    }

    graph.start.date <- as.Date(input$start_date, tryFormats=c("%m/%d/%Y", "%Y-%m-%d"))

    if (any(selected.interv != "")){
      all.intervs.df <- generate_ode_intervention_df(selected.interventions = selected.interv,
                                                     npi.simulation.times.df = npi.simulation.times.dat,
                                                     selected.state = selected.st,
                                                     model.start.date = graph.start.date,
                                                     sim.duration = input$Tmax)
      beta1.df <- gen_beta_df(region_name = selected.st, beta_ts = MOVING_BETA_DATA)
      all.intervs.df <- merge_beta1_intervention_df(intervention.df = all.intervs.df, b1.df = beta1.df, state = input$inter_state, b1.proj.method = "min")
      
      return(all.intervs.df)
    }else{
      return(NULL)
    }
  })

  values$model_results <- eventReactive(input$run_model, {
    req(values$hc_data())
    req(values$all.intervs.df())

    region_name = input$inter_state
    if(!is.null(input$inter_county)){
      if(input$inter_county != 'Statewide'){
        region_name = paste(input$inter_county, state_lookup(input$inter_state), sep = ', ')
      }
    }
    
    forecast_period=30 #days
    
    # genereate baseline results (no interventions/changing b1s)
    baseline_results = run_model_lite(input, input$Tmax, offset=NULL, hdata=values$hc_data()) %>%
      mutate(period='baseline', 'b1'=input$b1, 'date'=time + input$start_date)
    
    # generate beta-fitted portion of curve (same for NPI and mobility models)
    region_beta_df = gen_beta_df(region_name, beta_ts=MOVING_BETA_DATA)
    int_results = gen_region_seir_df(region_name, hdata=HDATA, b1_timelines=region_beta_df) %>%
      mutate(date=time+as.Date(input$start_date))

    forecast_start = max(int_results$date, na.rm=T)
    forecast_end = forecast_start + forecast_period

    # generate NPI projections based on ending conditions of beta-fitted curve
    all.intervs.proj.df = values$all.intervs.df() %>% filter(beta.step=='proj')
    P = tail(int_results, 1)
    y0 = c(S=P$S, E=P$E, I0=P$I0, I1=P$I1, I2=P$I2, I3=P$I3, R=P$R, D=P$D)
    npi_projs <- run_model(input, hops.cap.dat = values$hc_data(), intervention.df=all.intervs.proj.df,
                          graph.start.date=as.Date(input$start_date, tryFormats=c("%m/%d/%Y", "%Y-%m-%d")),
                          beta1.override = TRUE, interv_y0=y0)
    
    # add necessary columns to projections for merging
    npi_projs$Intervention = npi_projs$Intervention %>% mutate(date=time+input$start_date, period='proj')
    npi_projs$Baseline = npi_projs$Baseline %>% mutate(date=time+input$start_date, period='proj')

    # average predictions
    weights = list('mobility'=1/3, 'npi'=1/3, 'ppe'=1/3)

    # ensemble death predictions from ppe, mobility, and NPI models
    pred_list = list()

    # mobility
    pred_list[['mobility']] = run_model_with_forecasts(region_name, input=input, withTime=T) %>%
      # select(-b1_lower, -b1_upper) %>%
      filter(date <= forecast_end) %>%
      distinct()
    # npi
    pred_list[['npi']] = rbind.data.frame(int_results, npi_projs$Intervention) %>%
      filter(date <= forecast_end) %>%
      distinct()
    # ppe
    ppe_res = gen_ppe_preds(region_name)
    if(!is.null(ppe_res)){
      ppe_res = ppe_res %>% rename(D = deaths_pred)
      pred_list[['ppe']] = pred_list[['npi']]  %>%
        select(date, time) %>%
        left_join(ppe_res, by=c('date')) %>%
        mutate(b1=-1, period=-1, R=0) %>%
        filter(date >= input$start_date, date <= forecast_end) %>% distinct()
    }

    # combine predictions in a weighted fashion, using weights defined above
    ensemble_cols = setdiff(colnames(baseline_results), c('period','date','time','b1', 'b1_lower', 'b1_upper'))
    ensemble_df = data.frame(zeros(length(unique(pred_list$npi$date)), length(ensemble_cols)))
    colnames(ensemble_df) = ensemble_cols
    weight_df = data.frame(ensemble_df)

    for(model in names(pred_list)){
      model_preds = pred_list[[model]]
      model_cols = setdiff(colnames(model_preds), c('b1_lower', 'b1_upper'))
      model_preds = model_preds[,model_cols] %>% filter(date >= input$start_date, date <= forecast_end) %>%
        select(-b1, -period) %>% distinct()
      update_cols = intersect(ensemble_cols, colnames(drop_na(model_preds)))
      tryCatch({
        ensemble_df[,update_cols] = suppressWarnings(ensemble_df[,update_cols] + weights[[model]] * model_preds[,update_cols])
      }, error=function(e){})
      weight_df[,update_cols] = weight_df[,update_cols] + weights[[model]]
    }

    ensemble_df = ensemble_df * 1/weight_df

    int_results_columns <- names(int_results)

    pred_list$ensemble = ensemble_df %>%
      mutate(time = c(1:nrow(ensemble_df)),
             date = time + input$start_date - 1,
             period = ifelse(date <= forecast_start, 'fit', 'forecast')) %>%
      left_join(distinct(pred_list$mobility[,c('date','b1')]), by='date') %>%
      select(!!!int_results_columns)

    pred_list$Baseline = baseline_results

    ret.dat = pred_list

    return(ret.dat)
  })
  

  values$model_res = reactive({
    req(values$model_results())
    tmp_dat = isolate({values$model_results()})
    tmp.baseline <- tmp_dat$Baseline
    tmp.intervention <- tmp_dat$Intervention
    ret.dat <- tmp.baseline %>%
      filter(time < min(tmp.intervention$time, na.rm = TRUE)) %>%
      mutate(Intervention='NPI Strategy') %>%
      union_all(
        tmp.intervention %>%
          mutate(Intervention='NPI Strategy')
      )
    
    return(ret.dat)
  })

  gen_ppe_preds = function(region_name, start_date=NULL, include_deaths=T, calc_seir_cols=T, pred_days=28){
    region_fips = get_fips(region_name)
    ret_df = NULL
    case_res = predictRegionValues(list(region_fips), metric='cases', n_days=list(pred_days)) %>%
      mutate(date = as.Date(dates_str, tryFormats=c('%m/%d/%Y')))
    if(class(case_res)=='data.frame'){
      case_cols = c('countyFIPS', 'date', 'pop_crit_care', 'pop_non_crit_care',
                    'admitted_crit_care', 'admitted_non_crit_care', 'admitted_hospital', 'admitted_crit_care_vent',
                    'pop_crit_care_vent')
      if(calc_seir_cols){
        case_res = case_res %>%
          rename('Itot'=y_pred, 'cases'=y) %>%
          mutate(I3= pop_crit_care,
                 I2  = pop_non_crit_care,
                 I1 = max(0, (Itot - I3 - I2)),
                 R = rep(0, nrow(case_res)))
        case_cols = c(case_cols, c('cases', 'Itot', 'I1','I2','I3', 'R'))
      }else{
        case_cols = c(case_cols, c('y', 'y_pred'))
      }
      ret_df = case_res[,case_cols]
      if(include_deaths){
        death_res = predictRegionValues(list(region_fips), metric='deaths',  n_days=list(pred_days)) %>%
          mutate(date = as.Date(dates_str, tryFormats=c('%m/%d/%Y'))) %>%
          rename('deaths_pred'=y_pred, 'deaths'=y)
        if(class(death_res)=='data.frame'){
          ret_df = ret_df %>% left_join(death_res[,c('date', 'deaths', 'deaths_pred')], by=c('date'))
        }
      }

      if(!is.null(start_date)) ret_df = ret_df %>% filter(date >= start_date)
    }else{
      print('could not find data for chosen region, or python script failed.')
    }
    return(ret_df)
  }
  gen_ppe_preds_old = function(region_name, start_date=NULL){
    isCounty = detect_county(region_name)
    if(isCounty){region_name = get_fips(region_name)
    }else{
      if(nchar(region_name) > 2){
        region_name = state_lookup(region_name)
      }
    }
    case_res = getCountyData(list(region_name), metric='cases')
    if(typeof(case_res)=='list'){
      case_res = py_to_r(case_res$daily_data) %>%
        mutate(date = as.Date(dates_str, tryFormats=c('%m/%d/%Y'))) %>%
        rename('Itot'=y_pred, 'cases'=y) %>%
        mutate(I3= pop_crit_care,
               I2  = pop_non_crit_care,
               I1 = Itot - I3 - I2)
    }

    death_res = getCountyData(list(region_name), metric='deaths')
    if(typeof(death_res)=='list'){
      death_res = py_to_r(death_res$daily_data) %>%
        mutate(date = as.Date(dates_str, tryFormats=c('%m/%d/%Y'))) %>%
        rename('deaths_pred'=y_pred, 'deaths'=y)
    }

    if(class(case_res)=='data.frame' && class(case_res)=='data.frame'){
      comb_df = case_res[,c('date', 'cases', 'Itot', 'I1','I2','I3', 'pop_crit_care', 'pop_non_crit_care',
                            'admitted_crit_care', 'admitted_non_crit_care')] %>%
        left_join(death_res[,c('date', 'deaths', 'deaths_pred')], by=c('date'))

      comb_df = comb_df %>% mutate(R = rep(0, nrow(comb_df)))

      if(!is.null(start_date)) comb_df = comb_df %>% filter(date >= start_date)
    }
    else{
      print('could not find data for chosen region, or python script failed.')
      comb_df = NULL
    }
    return(comb_df)
  }

  p_inter_model_plt_active_cases <- eventReactive(input$run_model, {
    req(values$model_results())
    req(values$all.intervs.df())

    graph.start.date <- as.Date(input$start_date, tryFormats=c("%m/%d/%Y", "%Y-%m-%d"))
    return(suppressWarnings(draw_intervention_plot(input, hops.cap.dat = values$hc_data(), intervention.df = values$all.intervs.df(), plotCounty=(input$inter_county != 'Statewide'),
                                            graph.start.date = graph.start.date, mod_results=isolate(values$model_results()), VarShowCap = c("CasesCap"), interv.real.overlay=TRUE))
    )
  })
  output$inter_model_plt_active_cases <-renderPlotly({
    p_inter_model_plt_active_cases()
    })

  p_inter_model_plt_hosp <- eventReactive(input$run_model, {
    req(values$model_results())
    req(values$all.intervs.df())

    graph.start.date <- as.Date(input$start_date, tryFormats=c("%m/%d/%Y", "%Y-%m-%d"))
    return(suppressWarnings(draw_intervention_plot(input, hops.cap.dat = values$hc_data(), intervention.df = values$all.intervs.df(),plotCounty=(input$inter_county != 'Statewide'),
                                            graph.start.date = graph.start.date, mod_results=values$model_results(), VarShowCap = c("I2_I3", "I3bed"), interv.real.overlay=FALSE))
    )
  })
  output$inter_model_plt_hosp <-renderPlotly({
    p_inter_model_plt_hosp()
  })

  p_inter_model_plt_deaths <-eventReactive(input$run_model, {
    req(values$model_results())
    req(values$all.intervs.df())
    
    graph.start.date <- as.Date(input$start_date, tryFormats=c("%m/%d/%Y", "%Y-%m-%d"))
    return(suppressWarnings(draw_intervention_plot(input, hops.cap.dat = values$hc_data(), intervention.df = isolate(values$all.intervs.df()), plotCounty=(input$inter_county != 'Statewide'),
                                            graph.start.date = graph.start.date, mod_results=values$model_results(), VarShowCap = c("Deaths"), interv.real.overlay=TRUE))
    )
    })
  output$inter_model_plt_deaths <-renderPlotly({
    p_inter_model_plt_deaths()
    })

  p_inter_model_plt_daily_deaths <-eventReactive(input$run_model, {
    req(values$model_results())
    req(values$all.intervs.df())

    graph.start.date <- as.Date(input$start_date, tryFormats=c("%m/%d/%Y", "%Y-%m-%d"))
    return(suppressWarnings(draw_intervention_plot(input, hops.cap.dat = values$hc_data(), intervention.df = values$all.intervs.df(),plotCounty=(input$inter_county != 'Statewide'),
                                            graph.start.date = graph.start.date, mod_results=values$model_results(), VarShowCap = c("Daily_Deaths"), interv.real.overlay=TRUE))
    )
    })
  output$inter_model_plt_daily_deaths <-renderPlotly({
    p_inter_model_plt_daily_deaths()
    })



  p_inter_model_plt_critical_cases <-eventReactive(input$run_model, {
    req(values$model_results())
    req(values$all.intervs.df())

    graph.start.date <- as.Date(input$start_date, tryFormats=c("%m/%d/%Y", "%Y-%m-%d"))
    return(suppressWarnings(draw_intervention_plot(input, hops.cap.dat = values$hc_data(), intervention.df = values$all.intervs.df(), plotCounty=(input$inter_county != 'Statewide'),
                                            graph.start.date = graph.start.date, mod_results=values$model_results(), VarShowCap = c("I3", "I3bed", "I3mv"), interv.real.overlay=FALSE))
    )
    })
  output$inter_model_plt_critical_cases <-renderPlotly({
    p_inter_model_plt_critical_cases()
    })

  model.comp.plot.df <- reactive({

    req(input$model.comp.state)

    selected.st <- input$model.comp.state

    #model.comp.start.date <- as.Date(input$start_date, tryFormats=c("%m/%d/%Y"))
    model.comp.start.date <- as.Date(STATE_TUNING_PARAMS %>% filter(state==selected.st) %>% .$start_date)
    
    sim.duration <- input$Tmax

    #MODEL CODE w/o reactivity (only model.comp reactivity)
   
    if (input$model.comp.population.type == "C"){
      selected.st.abb <- list(STATE_NAMES$abbr[STATE_NAMES$full==selected.st])
      mitre.curvefit.df0 <- FORECAST_DATA %>% filter(County.Name == selected.st.abb)

      if (!is.null(mitre.curvefit.df0)){
        # Data Cleaning

        mitre.curvefit.df <- mitre.curvefit.df0 %>%
          filter(date >= model.comp.start.date) %>%
          mutate(forecast_date = Sys.Date(),
                 target_date = date,
                 fc.name = "mitre_curve",
                 point = cumsum(admitted_hospital),
                 quant_0.25 = NA,
                 quant_0.75 = NA) %>%
          select(forecast_date, target_date, fc.name, point, quant_0.25, quant_0.75)
      }
    }else if(input$model.comp.population.type == "DeltaI2_I3"){
      selected.st.abb <- list(STATE_NAMES$abbr[STATE_NAMES$full==selected.st])
      mitre.curvefit.df0 <- FORECAST_DATA %>% filter(County.Name == selected.st.abb)

      if (!is.null(mitre.curvefit.df0)){
        # Data Cleaning
        mitre.curvefit.df <- mitre.curvefit.df0 %>%
          filter(date >= model.comp.start.date) %>%
          mutate(forecast_date = Sys.Date(),
                 target_date = date,
                 fc.name = "mitre_curve",
                 point = admitted_hospital,
                 quant_0.25 = NA,
                 quant_0.75 = NA) %>%
          select(forecast_date, target_date, fc.name, point, quant_0.25, quant_0.75)
      }
    }else{
      mitre.curvefit.df <- data.frame(matrix(ncol=6, nrow=0,
                                             dimnames = list(NULL, c("forecast_date", "target_date", "fc.name", "point", "quant_0.25", "quant_0.75"))),
                                      stringsAsFactors = FALSE)
    }

    run.mitre.ens <- TRUE

    if (run.mitre.ens){
        if(selected.st != 'United States'){
          
        print(paste0('getting state forecasts for ', selected.st))
        state_input = get_all_region_params(selected.st)
        full.npi.react.df <- NPI_DATA %>%
          filter(policy_long %in% CURRENT_NPIS, state == selected.st) %>%
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
  
        hc_data <- get_state_hosp_data(selected.st, HDATA)
  
        all.intervs.df <- generate_ode_intervention_df(selected.interventions = selected.interv,
                                                       npi.simulation.times.df = full.npi.react.df,
                                                       selected.state = selected.st,
                                                       model.start.date = model.comp.start.date,
                                                       sim.duration = sim.duration) #abs.max = 0.7)
        # abs.max = input$npi.sah.prop.mild)
  
        beta1.df <- gen_beta_df(region_name = selected.st, beta_ts = MOVING_BETA_DATA)
  
        updated.intervs.df <- merge_beta1_intervention_df(intervention.df = all.intervs.df, b1.df = beta1.df, state = selected.st, b1.proj.method = "min")#, sah.effectiveness = input$npi.sah.prop.mild)
  
        forecast_period=30 #days
  
        # genereate baseline results (no interventions/changing b1s)
        baseline_results = run_model_lite(state_input, sim.duration, offset=NULL, hdata=hc_data) %>%
          mutate(period='baseline', 'b1'=state_input$b1, 'date'=time + model.comp.start.date)
  
        # generate beta-fitted portion of curve (same for NPI and mobility models)
        int_results <- gen_region_seir_df(selected.st, hdata=HDATA, b1_timelines=beta1.df) %>%
          mutate(date=time+as.Date(model.comp.start.date))
  
        forecast_start = max(int_results$date, na.rm=T)
        forecast_end = forecast_start + forecast_period
  
        # generate NPI projections based on ending conditions of beta-fitted curve
        all.intervs.proj.df = updated.intervs.df %>% filter(beta.step=='proj')
        P = tail(int_results, 1)
        y0 = c(S=P$S, E=P$E, I0=P$I0, I1=P$I1, I2=P$I2, I3=P$I3, R=P$R, D=P$D)
        npi_projs <- run_model(state_input, hops.cap.dat = hc_data, intervention.df=all.intervs.proj.df,
                               graph.start.date=as.Date(model.comp.start.date, tryFormats=c("%m/%d/%Y", "%Y-%m-%d")),
                               beta1.override = TRUE, interv_y0=y0)
  
        # add necessary columns to projections for merging
        npi_projs$Intervention = npi_projs$Intervention %>% mutate(date=time+model.comp.start.date, period='proj')
        npi_projs$Baseline = npi_projs$Baseline %>% mutate(date=time+model.comp.start.date, period='proj')
  
        # average predictions
        weights = list('mobility'=1/3, 'npi'=1/3, 'ppe'=1/3)
  
        # ensemble death predictions from ppe, mobility, and NPI models
        pred_list = list()
  
        # mobility
        pred_list[['mobility']] = run_model_with_forecasts(selected.st, input=state_input, withTime=T) %>%
          # select(-b1_lower, -b1_upper) %>%
          filter(date <= forecast_end) %>%
          distinct()
        # npi
        pred_list[['npi']] = rbind.data.frame(int_results, npi_projs$Intervention) %>%
          filter(date <= forecast_end) %>%
          distinct()
        # ppe
        ppe_res = gen_ppe_preds(selected.st)
        if(!is.null(ppe_res)){
          ppe_res = ppe_res %>% rename(D = deaths_pred)
          pred_list[['ppe']] = pred_list[['npi']]  %>%
            select(date, time) %>%
            left_join(ppe_res, by=c('date')) %>%
            mutate(b1=-1, period=-1, R=0) %>%
            filter(date >= model.comp.start.date, date <= forecast_end) %>%
            distinct()
        }
  
  
        # combine predictions in a weighted fashion, using weights defined above
        ensemble_cols = setdiff(colnames(baseline_results), c('period','date','time','b1', 'b1_lower', 'b1_upper'))
        ensemble_df = data.frame(zeros(length(unique(pred_list$npi$date)), length(ensemble_cols)))
        colnames(ensemble_df) = ensemble_cols
        weight_df = data.frame(ensemble_df)
  
        for(model in names(pred_list)){
          model_preds = pred_list[[model]]
          model_cols = setdiff(colnames(model_preds), c('b1_lower', 'b1_upper'))
          model_preds = model_preds[,model_cols] %>% filter(date >= model.comp.start.date, date <= forecast_end) %>%
            select(-b1, -period) %>% distinct()
          update_cols = intersect(ensemble_cols, colnames(drop_na(model_preds)))
          tryCatch({
            ensemble_df[,update_cols] = suppressWarnings(ensemble_df[,update_cols] + weights[[model]] * model_preds[,update_cols])
          }, error=function(e){})
          weight_df[,update_cols] = weight_df[,update_cols] + weights[[model]]
        }
  
        ensemble_df = ensemble_df * 1/weight_df
  
        #print(names(int_results))
        int_results_columns <- names(int_results)
  
        pred_list$ensemble = ensemble_df %>%
          mutate(time = c(1:nrow(ensemble_df)),
                 date = time + model.comp.start.date - 1,
                 period = ifelse(date <= forecast_start, 'fit', 'forecast')) %>%
          left_join(distinct(pred_list$mobility[,c('date','b1')]), by='date') %>%
          select(!!!int_results_columns)
  
        pred_list$Baseline = baseline_results
  
        mitre.ens.df <- get_mitre_ens_df(model.data = pred_list$ensemble, selected.pop = input$model.comp.population.type,
                                         graph.start.date = model.comp.start.date, selected.st=selected.st)
        }else{
        mitre.ens.df = data.frame()
      }
    }

    target_type_map <- c("C" = "cumulative", "D" = "cumulative",
                         "DeltaD" = "delta", "DeltaI2_I3" = "delta")

    target_pop_map <- c("C" = "hosp", "D" = "death",
                        "DeltaD" = "death", "DeltaI2_I3" = "hosp")

    model_name_map <- c("MITRE SEIR", "C19HCC Short-term", "C19HCC Ensemble", "Columbia University (40% Contact Reduction)", "Columbia University (30% Contact Reduction)",
                        "Columbia University (20% Contact Reduction)", "IHME", "UT Austin", "University of Geneva",
                        "Northeastern (MOBS)", "Youyang Gu (YYG)")
    names(model_name_map) <- c("mitre_seir", "mitre_curve", "mitre_ens", "cu_60", "cu_70", "cu_80", "ihme", "austin", "geneva", "mobs", "yyg")

    model.comp.plt.dat <- CDC_COMP_DAT %>%
      mutate(forecast_date = as.Date(forecast_date),
             target_date=as.Date(target_date)) %>%
      filter(mState.Providence==selected.st,
             target_type == target_type_map[input$model.comp.population.type],
             target_pop == target_pop_map[input$model.comp.population.type]) %>%
      filter(type.quantile %in% c("point", "quant_0.25", "quant_0.75")) %>%
      distinct()
    
    ###====only works for cumulative deaths... need to clean cdc data/remove excess rows==========#
    if(selected.st=='United States'){
      init_date_vals = select(filter(model.comp.plt.dat, target_date==min(target_date)), value)[[1]]
      cutoff = quantile(init_date_vals, p=.66)
      model.comp.plt.dat = model.comp.plt.dat %>% filter(value >= cutoff)
      }
    #============================================================================================#
    model.comp.plt.dat = model.comp.plt.dat %>%
      spread(key = type.quantile, value = value, fill=NA) %>%
      select(-target_type, -target_pop, -mState.Providence) %>%
      union_all(mitre.ens.df) %>%
      union_all(mitre.curvefit.df) %>%
      mutate(full.model.name = sapply(fc.name, FUN = function(x){return(model_name_map[x])})) %>%
      filter(target_date <= (Sys.Date() + dweeks(8)))

    return(model.comp.plt.dat)
  })

  output$model.comp.plot <- renderPlotly({
    req(model.comp.checks())
    req(model.comp.plot.df())

    plt.dat <- model.comp.plot.df() %>%
      filter(full.model.name %in% model.comp.checks())

    draw_model_comparison_plot(dat = plt.dat, 
                               selected.y = input$model.comp.population.type,
                               selected.state = input$model.comp.state)
  })


  output$forecast_county <- renderUI({
    req(input$forecast.state)

    counties = COUNTY_FORECAST_LIST %>%
      filter(state == input$forecast.state) %>%
      select(region = name_display) %>%
      .$region %>%
      unique() %>%
      sort()
    
    selectizeInput(inputId = "forecast.county", label = "Select US County",
                   choices = c("Statewide", counties),
                   multiple=FALSE)

  })


  output$curve.forecast.plot <- renderPlotly({
    req(input$forecast.county)
    req(input$forecast_plot_choices)
    req(input$forecast.state)

    if (input$forecast.county != "Statewide") {
      region_name = input$forecast.county
    } else {
      region_name = input$forecast.state
    }
    data = gen_ppe_preds(region_name, include_deaths=F, calc_seir_cols=F) %>%
      mutate(region = region_name)
    if (!is.null(data)){
      p <- draw_forecast(data, input$forecast_plot_choices)
      output$input_check_message <- NULL
      return(p)
    }
    else{
      output$input_check_message <- renderText({"There was insufficient data for this county."})
      return(plot_ly(type = "scatter", mode = "markers"))
    }
    
  })

  output$mortality.plot <- renderPlotly({
    req(input$mortality_plot_choices)

    p <- draw_mortality(input$mortality_plot_choices)
    return(p)

  })


  output$intervention.timeline <- renderTimevis({
    req(npi.check.dates.dat())
    time.dat <- npi.check.dates.dat()
    if (nrow(time.dat)>0){

      timevis.height <- NA

      implemented.interv <- time.dat %>%
        select(policy_long, start = exp_start_date, end = exp_end_date) %>%
        filter(!is.null(start),
               !is.na(start)) %>%
        distinct() %>%
        mutate(timevis.id = 1:n(),
               content=policy_long,
               type="range")

      timevis.config <- list(
        editable = TRUE,
        multiselect = FALSE,
        align="center",
        maxHeight=timevis.height
      )

      timeline.start.date <- as.Date(input$start_date)
      timeline.end.date <- timeline.start.date + ddays(input$Tmax)

      timevis(implemented.interv, options = timevis.config) %>%
        setWindow(start=timeline.start.date, end=timeline.end.date)
    }
  })

  output$gantt.helper.text <- renderUI({
    req(npi.check.dates.dat())
    time.dat <- npi.check.dates.dat()
    if (nrow(time.dat)>0){
      ret.ui <- tags$i('Note: When using the Gantt chart to adjust an NPI\'s duration, please de-select/"unclick" the intervention bar to recalculate the model.')
      return(ret.ui)
    }
  })

  # If user clicks on a state in national view, zoom in on that state.
  observeEvent(input$regionalmap_shape_click, {
    p <- input$regionalmap_shape_click
    print(paste('click fired at:', Sys.time()))
    if(input$granularity=='National'){
      state_name = p$id
      updateRadioGroupButtons(session, 'granularity', selected='State')
      updateSelectInput(session, 'chosen_state', selected=state_name)

    }else if(input$granularity=='State'){
      county.name <- p$id
      updateSelectInput(session=session, inputId = "chosen_county", selected = as.character(county.name))
    }
  })

  observeEvent(input$model.comp.state, {
    req(input$sidebarTabs)
    if (input$sidebarTabs == "model_comparison"){
      updateSelectInput(session = session, inputId = "inter_state", selected = input$model.comp.state)

    }
  })

  observeEvent(input$inter_state, priority = 5000, {
    updateSelectizeInput(session = session, inputId = "inter_county", selected = "Statewide")
  })

  # logic to download data  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste0('mitre_seir_data-',Sys.Date(),'.csv')
    }, 
    content = function(con){
      data = values$model_res()
      print(str(data))
      write.csv(data, con, row.names=F)
    }
  )

  output$downloadSimilarityData <- downloadHandler(
    filename = function(){
      region_name = str_replace_all(input$sim_region, ' ', '_')
      region_name = str_replace_all(region_name, ',', '')
      paste0(region_name,'similarity_data_', Sys.Date(),'.csv')
    },
    content = function(con){
      if(input$sim_type=='demographics'){
        similarity_type = 'Demographics'
        similarity_metric = input$demog.similarity.type
      }else{
        similarity_type = 'Epidemeological'
        similarity_metric = input$epi.similarity.type
      }
      if(length(similarity_metric) > 1){
        similarity_metric = paste0(similarity_metric, collapse = ' & ')
      }
      data = sim.table() %>%
        mutate('similarity_type'=similarity_type,
               'similarity_metric'=similarity_metric)
      write.csv(data, con, row.names=F)
    }
  )
  
  # model comparison download
  output$downloadRegionCompData <- downloadHandler(
    filename = function(){
      paste0('region_comparison_', Sys.Date(),'.csv')
    }, 
    content = function(con){
      
      tmp.dat <- refactored.region.comp.df()[["plt.dat"]]
     
      write.csv(tmp.dat, con, row.names=F)
    }
  )
  
  observeEvent(c(input$inter_state, input$inter_county), priority = 4999, {
    state_stats <- STATE_INF_METRICS %>% filter(state==input$inter_state)

    
    if (is.null(input$inter_county)){
      new.param.dat <- STATE_INF_METRICS %>%
        filter(state==input$inter_state)
      sel.area <- input$inter_state
      isolate({
        values$region_resource_params <- reactive({values$region_resource_stats() %>%
            filter(state==input$inter_state)})
      })
    }else if (input$inter_county=="Statewide" | trimws(input$inter_county)==""){
      new.param.dat <- values$state_covid_stats() %>%
        filter(state==input$inter_state)
      sel.area <- input$inter_state
      isolate({
        values$region_resource_params <- reactive({values$region_resource_stats() %>%
            filter(state==input$inter_state)})
      })
    }else{
      req(input$inter_county)
      state.abbr <- STATE_NAMES$abbr[STATE_NAMES$full == input$inter_state]
      sel.area <- paste(input$inter_county, state.abbr[1], sep = ", ")
      isolate({
        values$region_resource_params <- reactive({get_county_resource_stats(HOSPITALS, sel.area)})
      })
      
          

      new.param.dat <- COUNTY_INF_STATS %>%
        filter(mState.Providence==sel.area) %>%
        left_join(WORLD_POP, by = c("mState.Providence", "Country.Region")) %>%
        select(mState.Providence, fi_date=firstCaseDate, pop=population) %>%
        distinct()
    }

    isolate({
      updateSliderInput(session=session, inputId = "population.size", value = new.param.dat$pop[1])
      updateSliderInput(session=session,"HospBedper2", value = as.numeric(values$region_resource_params()['TotHospBedsAvail']), min=1, step=1, max=as.numeric(values$region_resource_params()['TotHospBedsAvail'])*3)
      updateSliderInput(session=session,"HospBedOcc2","Occupied",value = values$region_resource_params()$TotHospBedsOcc, min = 0, step = 1, max = as.numeric(values$region_resource_params()['TotHospBedsAvail'])) #input$HospBedper2)
      updateSliderInput(session=session,"ICUBedper2","Quantity",value = values$region_resource_params()$TotICUHospBedsAvail, min = 1, step = 1, max=values$region_resource_params()$TotICUHospBedsAvail*3)
      updateSliderInput(session=session,"ICUBedOcc2","Occupied",value = values$region_resource_params()$TotICUHospBedsOcc, min = 1, step = 1, max=values$region_resource_params()$TotICUHospBedsAvail)
      
    })
    
    num.val <- 1000*as.numeric(values$region_resource_params()['TotHospBedsAvail'])/new.param.dat$pop[1]
    updateNumericInput(session = session, inputId = "HospBedper", value = num.val)
    num.val2 <- 1000*values$region_resource_params()$TotICUHospBedsAvail/new.param.dat$pop[1]
    updateNumericInput(session = session, inputId = "ICUBedper", value = num.val2)
    
    ## Intervention Tab updates
    this.npi.dat <- npi.intervention.dat() %>%
      filter(state == sel.area) %>%
      distinct() %>%
      filter(!is.na(start_date))
    
    isolate({
      updateCheckboxInput(session=session, inputId = "interv_bar", value = ("Bar/Restaurant Limits" %in% unique(this.npi.dat$policy_long)))
      updateCheckboxInput(session=session, inputId = "interv_lgb", value = ("Large Gatherings Ban" %in% unique(this.npi.dat$policy_long)))
      updateCheckboxInput(session=session, inputId = "interv_mm", value = ("Mandatory Masks" %in% unique(this.npi.dat$policy_long)))
      updateCheckboxInput(session=session, inputId = "interv_nbc", value = ("Non-Essential Business Closures" %in% unique(this.npi.dat$policy_long)))
      updateCheckboxInput(session=session, inputId = "interv_sc", value = ("State-Mandated School Closures" %in% unique(this.npi.dat$policy_long)))
      updateCheckboxInput(session=session, inputId = "interv_sah", value = ("Stay At Home Order" %in% unique(this.npi.dat$policy_long)))
      updateCheckboxInput(session=session, inputId = "interv_rev", value = ("Reopening Plan Reversal" %in% unique(this.npi.dat$policy_long)))
      
    })

   

    isolate({
      if ("Bar/Restaurant Limits" %in% unique(this.npi.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_bar_date",
                             start = this.npi.dat$exp_start_date[this.npi.dat$policy_long=="Bar/Restaurant Limits"],
                             end = this.npi.dat$exp_end_date[this.npi.dat$policy_long=="Bar/Restaurant Limits"])
      }
      if ("Large Gatherings Ban" %in% unique(this.npi.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_lgb_date",
                             start = this.npi.dat$exp_start_date[this.npi.dat$policy_long=="Large Gatherings Ban"],
                             end = this.npi.dat$exp_end_date[this.npi.dat$policy_long=="Large Gatherings Ban"])
      }
      if ("Mandatory Masks" %in% unique(this.npi.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_mm_date",
                             start = this.npi.dat$exp_start_date[this.npi.dat$policy_long=="Mandatory Masks"],
                             end = this.npi.dat$exp_end_date[this.npi.dat$policy_long=="Mandatory Masks"])
      }
      if ("Non-Essential Business Closures" %in% unique(this.npi.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_nbc_date",
                             start = this.npi.dat$exp_start_date[this.npi.dat$policy_long=="Non-Essential Business Closures"],
                             end = this.npi.dat$exp_end_date[this.npi.dat$policy_long=="Non-Essential Business Closures"])
      }
      if ("State-Mandated School Closures" %in% unique(this.npi.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_sc_date",
                             start = this.npi.dat$exp_start_date[this.npi.dat$policy_long=="State-Mandated School Closures"],
                             end =  this.npi.dat$exp_end_date[this.npi.dat$policy_long=="State-Mandated School Closures"])
      }
      if ("Stay At Home Order" %in% unique(this.npi.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_sah_date",
                             start = this.npi.dat$exp_start_date[this.npi.dat$policy_long=="Stay At Home Order"],
                             end = this.npi.dat$exp_end_date[this.npi.dat$policy_long=="Stay At Home Order"])
      }
      if ("Reopening Plan Reversal" %in% unique(this.npi.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_rev_date",
                             start = this.npi.dat$exp_start_date[this.npi.dat$policy_long=="Reopening Plan Reversal"],
                             end = this.npi.dat$exp_end_date[this.npi.dat$policy_long=="Reopening Plan Reversal"])
      }

    })

    req(input$sidebarTabs == "npi_impact")
    if (input$sidebarTabs == "npi_impact"){
      updateSelectInput(session=session, inputId = "chosen_state", selected = input$inter_state)
      if (!is.null(input$inter_county)){
        updateSelectInput(session=session, inputId = "chosen_county", selected = input$inter_county)
      }
    }
    click("adv_settings_reset")
  })
  
  observeEvent(input$intervention.timeline_selected, ignoreInit = TRUE, ignoreNULL = FALSE, {
    if (!is.null(input$intervention.timeline_data)){
      timeline.dat <- input$intervention.timeline_data %>%
        select(policy_long, exp_start_date=start, exp_end_date=end) %>%
        distinct()
      
      updateCheckboxInput(session=session, inputId = "interv_bar", value = ("Bar/Restaurant Limits" %in% unique(timeline.dat$policy_long)))
      updateCheckboxInput(session=session, inputId = "interv_lgb", value = ("Large Gatherings Ban" %in% unique(timeline.dat$policy_long)))
      updateCheckboxInput(session=session, inputId = "interv_nbc", value = ("Non-Essential Business Closures" %in% unique(timeline.dat$policy_long)))
      updateCheckboxInput(session=session, inputId = "interv_mm", value = ("Mandatory Masks" %in% unique(timeline.dat$policy_long)))
      updateCheckboxInput(session=session, inputId = "interv_sc", value = ("State-Mandated School Closures" %in% unique(timeline.dat$policy_long)))
      updateCheckboxInput(session=session, inputId = "interv_sah", value = ("Stay At Home Order" %in% unique(timeline.dat$policy_long)))
      updateCheckboxInput(session=session, inputId = "interv_rev", value = ("Reopening Plan Reversal" %in% unique(timeline.dat$policy_long)))

      if ("Bar/Restaurant Limits" %in% unique(timeline.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_bar_date",
                             start = timeline.dat$exp_start_date[timeline.dat$policy_long=="Bar/Restaurant Limits"],
                             end = timeline.dat$exp_end_date[timeline.dat$policy_long=="Bar/Restaurant Limits"])
      }
      if ("Large Gatherings Ban" %in% unique(timeline.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_lgb_date",
                             start = timeline.dat$exp_start_date[timeline.dat$policy_long=="Large Gatherings Ban"],
                             end = timeline.dat$exp_end_date[timeline.dat$policy_long=="Large Gatherings Ban"])
      }
      if ("Mandatory Masks" %in% unique(timeline.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_mm_date",
                             start = timeline.dat$exp_start_date[timeline.dat$policy_long=="Mandatory Masks"],
                             end = timeline.dat$exp_end_date[timeline.dat$policy_long=="Mandatory Masks"])
      } 
      if ("Non-Essential Business Closures" %in% unique(timeline.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_nbc_date",
                             start = timeline.dat$exp_start_date[timeline.dat$policy_long=="Non-Essential Business Closures"],
                             end = timeline.dat$exp_end_date[timeline.dat$policy_long=="Non-Essential Business Closures"])
      }
      if ("State-Mandated School Closures" %in% unique(timeline.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_sc_date",
                             start = timeline.dat$exp_start_date[timeline.dat$policy_long=="State-Mandated School Closures"],
                             end =  timeline.dat$exp_end_date[timeline.dat$policy_long=="State-Mandated School Closures"])
      }
      if ("Stay At Home Order" %in% unique(timeline.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_sah_date",
                             start = timeline.dat$exp_start_date[timeline.dat$policy_long=="Stay At Home Order"],
                             end = timeline.dat$exp_end_date[timeline.dat$policy_long=="Stay At Home Order"])
      }
      if ("Reopening Plan Reversal" %in% unique(timeline.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_rev_date",
                             start = timeline.dat$exp_start_date[timeline.dat$policy_long=="Reopening Plan Reversal"],
                             end = timeline.dat$exp_end_date[timeline.dat$policy_long=="Reopening Plan Reversal"])
      }

      if (npi.counter$npi.counter.value>=1){
        for (tmp.val in 1:npi.counter$npi.counter.value){
          checkbox.id <- paste0("interv.custom.npi.", tmp.val) #interv.custom.npi.9
          checkbox.name <- input[[paste0("custom.npi.", tmp.val, ".name")]] #eg the name of that ID
          daterange.id <- paste0("interv.custom.npi.",tmp.val,".date")
          
          updateCheckboxInput(session=session, inputId = checkbox.id, value = (checkbox.name %in% unique(timeline.dat$policy_long)))
          
          if (checkbox.name %in% unique(timeline.dat$policy_long)){
            updateDateRangeInput(session = session, inputId = daterange.id,
                                 start = timeline.dat$exp_start_date[timeline.dat$policy_long==checkbox.name],
                                 end = timeline.dat$exp_end_date[timeline.dat$policy_long==checkbox.name])
          }
        }
      }
    }
  })


  # Reset button in Inervention Tab
  observeEvent(input$reset.interv.info, {

    if (is.null(input$inter_county)){
      sel.area <- input$inter_state
    }else if (input$inter_county=="Statewide"){
      sel.area <- input$inter_state
    }else{
      state.abbr <- STATE_NAMES$abbr[STATE_NAMES$full == input$chosen_state]
      sel.area <- paste(input$chosen_county, state.abbr[1], sep = ", ")
    }

    this.npi.dat <- npi.intervention.dat() %>%
      filter(state == sel.area) %>%
      distinct() %>%
      filter(!is.na(start_date))

    updateCheckboxInput(session=session, inputId = "interv_bar", value = ("Bar/Restaurant Limits" %in% unique(this.npi.dat$policy_long)))
    updateCheckboxInput(session=session, inputId = "interv_lgb", value = ("Large Gatherings Ban" %in% unique(this.npi.dat$policy_long)))
    updateCheckboxInput(session=session, inputId = "interv_mm", value = ("Mandatory Masks" %in% unique(this.npi.dat$policy_long)))
    updateCheckboxInput(session=session, inputId = "interv_nbc", value = ("Non-Essential Business Closures" %in% unique(this.npi.dat$policy_long)))
    updateCheckboxInput(session=session, inputId = "interv_sc", value = ("State-Mandated School Closures" %in% unique(this.npi.dat$policy_long)))
    updateCheckboxInput(session=session, inputId = "interv_sah", value = ("Stay At Home Order" %in% unique(this.npi.dat$policy_long)))
    updateCheckboxInput(session=session, inputId = "interv_rev", value = ("Reopening Plan Reversal" %in% unique(this.npi.dat$policy_long)))

    isolate({
      if ("Bar/Restaurant Limits" %in% unique(this.npi.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_bar_date",
                             start = this.npi.dat$exp_start_date[this.npi.dat$policy_long=="Bar/Restaurant Limits"],
                             end = this.npi.dat$exp_end_date[this.npi.dat$policy_long=="Bar/Restaurant Limits"])
      }
      if ("Large Gatherings Ban" %in% unique(this.npi.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_lgb_date",
                             start = this.npi.dat$exp_start_date[this.npi.dat$policy_long=="Large Gatherings Ban"],
                             end = this.npi.dat$exp_end_date[this.npi.dat$policy_long=="Large Gatherings Ban"])
      }
      if ("Mandatory Masks" %in% unique(this.npi.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_mm_date",
                             start = this.npi.dat$exp_start_date[this.npi.dat$policy_long=="Mandatory Masks"],
                             end = this.npi.dat$exp_end_date[this.npi.dat$policy_long=="Mandatory Masks"])
      }
      if ("Non-Essential Business Closures" %in% unique(this.npi.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_nbc_date",
                             start = this.npi.dat$exp_start_date[this.npi.dat$policy_long=="Non-Essential Business Closures"],
                             end = this.npi.dat$exp_end_date[this.npi.dat$policy_long=="Non-Essential Business Closures"])
      }
      if ("State-Mandated School Closures" %in% unique(this.npi.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_sc_date",
                             start = this.npi.dat$exp_start_date[this.npi.dat$policy_long=="State-Mandated School Closures"],
                             end =  this.npi.dat$exp_end_date[this.npi.dat$policy_long=="State-Mandated School Closures"])
      }
      if ("Stay At Home Order" %in% unique(this.npi.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_sah_date",
                             start = this.npi.dat$exp_start_date[this.npi.dat$policy_long=="Stay At Home Order"],
                             end = this.npi.dat$exp_end_date[this.npi.dat$policy_long=="Stay At Home Order"])
      } 
      if ("Reopening Plan Reversal" %in% unique(this.npi.dat$policy_long)){
        updateDateRangeInput(session=session, inputId = "interv_rev_date",
                             start = this.npi.dat$exp_start_date[this.npi.dat$policy_long=="Reopening Plan Reversal"],
                             end = this.npi.dat$exp_end_date[this.npi.dat$policy_long=="Reopening Plan Reversal"])
      }
    })
  })

  observeEvent(c(input$granularity, input$chosen_state), ignoreInit = T, {
    if(input$granularity=='National'){
      shinyjs::hide(id = "spread_state_and_county", anim = FALSE)
      new_stats <- values$nat_stats()
      disable(selector = "#plot_metric input[value='Mobility']")
    }else{
      shinyjs::show(id = "spread_state_and_county", anim = FALSE)
      new_stats <- values$state_covid_stats() %>%
        filter(state==input$chosen_state)
      enable(selector = "#plot_metric input[value='Mobility']")
    }
    values$regional_covid_stats <- reactive(new_stats)
  })
  
  output$region_title <- renderText({
    if(input$granularity=='National'){
      region_name = 'National'
    }else{
      region_name = input$chosen_state
    }
    latest_date = as.Date(max(COUNTRY_INF_STATS$date))
    if(format(latest_date, format='%b') == 'May') latest_date_str = format(latest_date, format="%A, %b %d, %Y")
    else latest_date_str = format(latest_date, format="%A, %b. %d, %Y")
  
    
    if(input$summary_type == 'Resources') stat_typ = 'Resource' else stat_typ = 'COVID-19'
    paste(region_name, stat_typ, 'Statistics as of', latest_date_str, sep=' ')
  })
  
  output$regionalplottitle <- renderText({
    req(input$granularity)
    req(input$geo_metric)
    if(input$granularity=='National'){
      if (input$geo_metric == "Cumulative Cases") {
        ret.text = 'Cumulative Cases Across the United States'
      } else if (input$geo_metric == "Cumulative Deaths") {
        ret.text = 'Cumulative Deaths Across the United States'
      } else if (input$geo_metric == "Doubling Time") {
        ret.text = 'Doubling Time Across the United States'
      } else if (input$geo_metric == "Reproduction Rate") {
        ret.text = 'Viral Reproduction Rate Across the United States'
      } else ret.text = 'United States'
    }else{
      region_name = input$chosen_state
      ret.text = paste0(input$geo_metric, " in ", region_name)
      if (input$geo_metric == "Reproduction Rate") {
        ret.text = paste0("Viral ", input$geo_metric, " in ", region_name)
      }
    }

    return(ret.text)
  })

  output$interv_tab_title <- renderUI({

    region_title = ""
    region_name = NULL
    if(!is.null(input$inter_state)) region_name = input$inter_state
    if(!is.null(input$inter_county)){
      if(input$inter_county != 'Statewide'){
        region_name = paste0(input$inter_county, ', ', state_lookup(input$inter_state))
      }
    }
    if(!is.null(region_name)) region_title = paste0('for ', region_name)
    title = paste0('Ensemble Model Projections ', region_title)
    return(tags$h3(title, style='margin-top: 10px;'))
  })

  output$interv_disclaimer <- renderText({
    desc <- 'The Intervention Model Tab integrates three fundamentally different models to build a deeper understanding of what is possible. The goal is insight, and should be used together with expert judgement when making a forecast.'
    return(desc)
  })

  output$interv_tab_description <- renderUI({

    desc = div(span('Explore the impact of social distancing policies (NPIs), health system capacity, and mobility on the spread of COVID-19 for a given region.
    The plots below attempt to estimate future cases, deaths, and hospitalizations based on these factors. Projections driving these plots come from
    an ',
    actionLink(inputId = "ens_model_link", label = "Ensemble model"),
    'that averages predictions from a ',
    actionLink(inputId = "mob_seir_link", label = "Mobility-based SEIR model,"),
    'a ',
    actionLink(inputId = "pol_seir_link", label = "Policy-based SEIR model,",),
    'and a ',
    actionLink(inputId = "trend_stat_link", label = "Trendline-based statistical model"),
    '(See the "About the Models" page for more information).
    Adjust the NPI or Health System Capacity settings below to change the behavior of the mobility-based and policy-based models. Once a state or county is selected, this tab may take a moment to load.'))

    note = div(style="margin-top:10px;",
                   list(
                     span(style="font-weight:bold;font-size:18px;color:#14487b", "Note: "),
                     span(style="font-size:14px;", 'Adjust assumptions below about Health System Capacity and Non-Pharmaceutical Intervnetions
                          to influence the SEIR models.')
                     )
        )

    return(div(desc, note))
  })

  observeEvent(input$mob_seir_link, ignoreInit = TRUE, ignoreNULL = TRUE, {
    updateTabItems(session = session, inputId = "sidebarTabs", "model")
    delay(ms = 500, expr = runjs('document.getElementById("model_iframe").contentWindow.document.getElementById("mobility-based-seir-model").scrollIntoView();'))
  })

  observeEvent(input$ens_model_link, ignoreInit = TRUE, ignoreNULL = TRUE, {
    updateTabItems(session = session, inputId = "sidebarTabs", "model")
    delay(ms = 500, expr = runjs('document.getElementById("model_iframe").contentWindow.document.getElementById("ensemble-model").scrollIntoView();'))
  })

  observeEvent(input$trend_stat_link, ignoreInit = TRUE, ignoreNULL = TRUE, {
    updateTabItems(session = session, inputId = "sidebarTabs", "model")
    delay(ms = 500, expr = runjs('document.getElementById("model_iframe").contentWindow.document.getElementById("trend-line-based-statistical-model").scrollIntoView();'))
  })

  observeEvent(input$pol_seir_link, ignoreInit = TRUE, ignoreNULL = TRUE, {
    updateTabItems(session = session, inputId = "sidebarTabs", "model")
    delay(ms = 500, expr = runjs('document.getElementById("model_iframe").contentWindow.document.getElementById("policy-based-seir-model").scrollIntoView();'))
  })

  #Get default hospital capacity parameters and create sliders; KF: Copied from earlier app
  
  observeEvent(input$HospBedper2, ignoreInit = TRUE, {
    hosp.beds <- input$HospBedper2
    updateSliderInput(session=session, inputId = "HospBedOcc2", max = hosp.beds)
    num.val <- 1000*input$HospBedper2/input$population.size
    updateNumericInput(session = session, inputId = "HospBedper", value = num.val)
  })

  observeEvent(c(input$HospBedper2, input$HospBedOcc2),{
    num.val <- 100*input$HospBedOcc2/input$HospBedper2
    updateNumericInput(session = session, inputId = "HospBedOcc", value = num.val)
  })

  observeEvent(input$ICUBedper2, ignoreInit = TRUE, {
    icu.beds <- input$ICUBedper2
    updateSliderInput(session=session, inputId = "ICUBedOcc2", max = icu.beds)
    num.val <- 1000*input$ICUBedper2/input$population.size
    updateNumericInput(session = session, inputId = "ICUBedper", value = num.val)
  })

  observeEvent(c(input$ICUBedper2, input$ICUBedOcc2) ,{
    num.val <- 100*input$ICUBedOcc2/input$ICUBedper2
    updateNumericInput(session = session, inputId = "ICUBedOcc", value = num.val)
  })

  output$ConvVentCap2 <- renderUI({
    num.val <- round((values$hdata()$ConvMVCap*input$population.size)/1000, 0 )
    sliderInput("ConvMVCap2",label="Conventional", value = num.val, min = 0, step = 1, max=num.val*2, width = "100%")
  })

  observeEvent(input$ConvMVCap2,{
    num.val <- 1000*input$ConvMVCap2/input$population.size
    updateNumericInput(session=session, inputId = "ConvMVCap", value = num.val)
  })

  output$ContVentCap2 <- renderUI({
    num.val <- round((values$hdata()$ContMVCap*input$population.size)/1000, 0 )
    sliderInput("ContMVCap2","Contingency",value = num.val, min = 0, step = 1, max=num.val*3, width = "100%")
  })
  observeEvent(input$ContMVCap2,{
    num.val <- 1000*input$ContMVCap2/input$population.size
    updateNumericInput(session=session, inputId = "ContMVCap", value = num.val)
  })

  output$CrisisVentCap2 <- renderUI({
    num.val <- round((values$hdata()$CrisisMVCap*input$population.size)/1000, 0)
    sliderInput("CrisisMVCap2","Crisis",value = num.val, min = 0, step = 1, max=num.val*4, width = "100%")
  })
  observeEvent(input$CrisisMVCap2,{
    num.val <- 1000*input$CrisisMVCap2/input$population.size
    updateNumericInput(session=session, inputId = "CrisisMVCap", value = num.val)
  })
  
  observeEvent(input$npi.sc.prop.mild.button, ignoreInit = TRUE, ignoreNULL = TRUE,{
    updateNumericInput(session = session, inputId = "npi.sc.prop.mild", value = input$npi.sc.prop.mild.button)
  })
  
  observeEvent(input$npi.nbc.prop.mild.button, ignoreInit = TRUE, ignoreNULL = TRUE, {
    updateNumericInput(session = session, inputId = "npi.nbc.prop.mild", value = input$npi.nbc.prop.mild.button)
  })
  
  observeEvent(input$npi.bar.prop.mild.button, ignoreInit = TRUE, ignoreNULL = TRUE, {
    updateNumericInput(session = session, inputId = "npi.bar.prop.mild", value = input$npi.bar.prop.mild.button)
  })
  
  observeEvent(input$npi.lgb.prop.mild.button, ignoreInit = TRUE, ignoreNULL = TRUE, {
    updateNumericInput(session = session, inputId = "npi.lgb.prop.mild", value = input$npi.lgb.prop.mild.button)
  })
  
  observeEvent(input$npi.mqt.prop.mild.button, ignoreInit = TRUE, ignoreNULL = TRUE, {
    updateNumericInput(session = session, inputId = "npi.mqt.prop.mild", value = input$npi.mqt.prop.mild.button)
  })
  
  observeEvent(input$npi.sah.prop.mild.button, ignoreInit = TRUE, ignoreNULL = TRUE, {
    updateNumericInput(session = session, inputId = "npi.sah.prop.mild", value = input$npi.sah.prop.mild.button)
  })
  
  observeEvent(input$npi.mm.prop.mild.button, ignoreInit = TRUE, ignoreNULL = TRUE, {
    updateNumericInput(session = session, inputId = "npi.mm.prop.mild", value = input$npi.mm.prop.mild.button)
  })
  
  observeEvent(input$npi.rev.prop.mild.button, ignoreInit = TRUE, ignoreNULL = TRUE,{
    updateNumericInput(session = session, inputId = "npi.rev.prop.mild", value = input$npi.rev.prop.mild.button)
  })


  observeEvent(input$intervention.timeline_data, ignoreInit = TRUE, ignoreNULL = TRUE, {

    if (nrow(input$intervention.timeline_data)>0){
      wave1.npis <- c("Bar/Restaurant Limits", "Large Gatherings Ban", "Non-Essential Business Closures", "Stay At Home Order")

      first.possible.date <- input$intervention.timeline_data %>%
        select(policy_long, exp_start_date=start, exp_end_date=end) %>%
        distinct() %>%
        filter(policy_long %in% wave1.npis) %>%
        mutate(exp_end_date = as.Date(exp_end_date, origin="1970-01-01")) %>%
        summarise(last.date = max(exp_end_date, na.rm = TRUE)+ddays(1)) %>%
        .$last.date

      if (any(is.null(input$interv_rev_date))){
        init.date <- first.possible.date
        last.date <- first.possible.date+ddays(90)
      }else if (any(is.na(input$interv_rev_date))){
        init.date <- first.possible.date
        last.date <- first.possible.date+ddays(90)
      }else if (input$interv_rev_date[1] < first.possible.date){
        init.date <- first.possible.date
        last.date <- first.possible.date+ddays(90)
      }else{
        init.date <- input$interv_rev_date[1]
        last.date <- input$interv_rev_date[2]
      }


      if (!is.na(first.possible.date)){
        if (!is.null(first.possible.date)){
          isolate({
            updateDateRangeInput(session=session, inputId = "interv_rev_date", label = NULL,
                                 start = init.date,
                                 end = last.date,
                                 min = as.Date(first.possible.date, origin="1970-01-01"),
                                 max = as.Date(first.possible.date, origin="1970-01-01")+ddays(365*2))
          })
        }
      }
    }
  })

  observeEvent(input$reset.hosp.params, {
    updateSliderInput(session=session, inputId = "HospBedper2", value = values$region_resource_params()$TotHospBedsAvail)
    updateSliderInput(session=session, inputId = "HospBedOcc2", value = values$region_resource_params()$TotHospBedsOcc)
    updateSliderInput(session=session, inputId = "ICUBedper2", value = values$region_resource_params()$TotICUHospBedsAvail)
    updateSliderInput(session=session, inputId = "ICUBedOcc2", value = values$region_resource_params()$TotICUHospBedsOcc)
    reset(id="ConvMVCap2")
    reset(id="ContMVCap2")
    reset(id="CrisisMVCap2")
  })
  
  observeEvent(input$adv_settings_reset, {
    reset(id="clinical_params_reset_id")
    reset(id="FracAsympto")
    reset(id="b21")
    reset(id="b31")
    reset(id="Tmax")
    reset("npi.bar.prop.severe")
    reset("npi.bar.prop.critical") 
    reset("npi.lgb.prop.severe")
    reset("npi.lgb.prop.critical")
    reset("npi.mqt.prop.severe")
    reset("npi.mqt.prop.critical")
    reset("npi.mm.prop.severe")
    reset("npi.mm.prop.critical")
    reset("npi.nbc.prop.severe")
    reset("npi.nbc.prop.critical")
    reset("npi.sc.prop.severe")
    reset("npi.sc.prop.critical")
    reset("npi.sah.prop.severe")
    reset("npi.sah.prop.critical")
    
    # default to state params. If county is chose, use county params
    region_name = input$inter_state
    tuning_df = STATE_TUNING_PARAMS
    region_col = 'state'
    
    if(nchar(input$inter_state) > 0){
      if(length(input$inter_county) > 0){
        if(input$inter_county != 'Statewide'){
          print(paste0('updating county params for ', region_name))
          region_name = paste(input$inter_county, state_lookup(input$inter_state), sep=', ')
          tuning_df = COUNTY_TUNING_PARAMS
          region_col = 'region'
        }
      }
      
      #population update
      updated.pop <- WORLD_POP %>%
        filter(mState.Providence==region_name) %>%
        distinct() %>%
        .$population
      
      updateSliderInput(session=session, inputId = "population.size", value = updated.pop[1])
      
      # load tuning parameters for state model
      region_params <- tuning_df[which(tuning_df[,region_col]==region_name),]

      param_cols = setdiff(names(region_params), c('loss', 'region', 'state'))
      
      npi_param_name_mapper <- c("State-Mandated School Closures"="npi.sc.prop.mild", 
                                 "Non-Essential Business Closures"="npi.nbc.prop.mild",
                                 "Bar/Restaurant Limits"="npi.bar.prop.mild", 
                                 #"Mandatory Masks" = "npi.mm.prop.mild",
                                 "Large Gatherings Ban" = "npi.lgb.prop.mild",
                                 "Mandatory Quarantine for Travelers" = "npi.mqt.prop.mild",
                                 "Stay At Home Order"="npi.sah.prop.mild",
                                 "Reopening Plan Reversal" = "npi.rev.prop.mild")
      
      type_map = data.frame(param_cols, stringsAsFactors = F) %>%
        mutate(type = sapply(param_cols, function(x){PARAM_TYPES[[x]]}))
      
      print(paste0('region params for ', region_name))
      print(region_params)
      
      # deal with the rest of params
      isolate({
        for(param_name in param_cols){
          param_val = region_params[[param_name]]
          param_typ = PARAM_TYPES[[param_name]]
          print(paste0(param_name, ': ', param_val))
          if(param_typ == 'npi'){
            update_fn = updateNumericInput
          }else if(param_typ == 'date'){
            param_val= as.Date(region_params[[param_name]], tryFormats=c("%m/%d/%y","%Y-%m-%d", "%m/%d/%Y"))
            update_fn = updateDateInput
          }else if(param_typ == 'numeric'){
            update_fn = updateNumericInput
          }else if(param_typ == 'slider'){
            update_fn = updateSliderInput
          }else{
            update_fn = NULL
          }
          if(!is.null(update_fn)){
            update_fn(session, inputId=param_name, value=param_val)
          }
        }
      })

      
      while (npi.counter$npi.counter.value>0){
        #remove UIs 
        removeUI(
          session = session,
          multiple = TRUE,
          ## pass in appropriate div id
          selector = paste0("table#npi_param_table tr#custom_npi_", npi.counter$npi.counter.value, "_params_span td")
        )
        npi.counter$npi.counter.value <- npi.counter$npi.counter.value - 1 
      }
    }
   
  })
}

printvar <- function(var){
  print(var)
}

vboxCustom <- function (value, subtitle, icon = NULL, bg_color = "white", font_color="#1c56a0", width = 4, href = NULL) {
  boxContent <- div(class = paste0("small-box"),
                    div(class = "inner", h3(value), p(subtitle, style='font-family: inherit;')), if (!is.null(icon))
                      div(class = "icon-large", icon),
                    style=paste0("color: ",font_color, "; background-color: ", bg_color))
  if (!is.null(href))
    boxContent <- a(href = href, boxContent)
  return(div(class = if (!is.null(width))
    paste0("col-sm-", width), boxContent))
}

shinyApp(ui = ui, server = server)

# # once app has closed, display reactlog from shiny
# shiny::reactlogShow()

