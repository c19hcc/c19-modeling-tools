##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: Home page and various other UI elements for app
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

library(shiny)

caveat_ui <- tags$div(style="background-color: white;",
  tags$h1("COVID-19 Dashboard and Tools", class='h1-c19'),
  tags$p("These dashboards and model provide state and county-level insight into the dynamics of COVID-19 regarding hospital capacity, PPE, and non-pharmaceutical interventions (NPIs). This collection of tools developed by the ",
    tags$a(href= "https:c19hcc.org", "COVID-19 Healthcare Coalition"),  
    "in partnership with the Department of Homeland Security provides access to data and models to understand the secondary effects of key decisions, such as adding hospital capacity or lifting interventions early, and how the fatality rate will change if hospitals become overwhelmed. This tool is built with publicly availabile data and models." ),
  
  tags$p("Use the left-hand navigation bar or click an image below to explore data you care about most."),
  tags$p("Questions about this tool can be directed to the owners of each tool listed below."),
  
  fluidRow(
    column(3,
           tags$img(src="data_highres.png", style="width: 100%;")
          ),        
    column(3,
           # USE THIS TEMPLATE FOR EXTERNAL LINKS
           tags$a(
             tags$div(
                tags$span("Decision Support Tool",  style='font-size:17px;'),
                br(),
                tags$img(src="decision_dash.png", style="width: 80%; max-height: 160px;"),
                style = "text-align: center; box-shadow: 2px 2px 6px lightgray;"
              ),
             href = "https://c19hcc.org/resources/decision-support/"
             )

    ),
    column(3,
           # USE THIS TEMPLATE FOR INTERNAL LINKS
           actionLink(inputId = "reg_spread_link",
                      label = tags$div(
                                       tags$span("Regional Data", style='font-size:17px;'),
                                       br(),
                                       tags$img(src="spread_v2.png", style="width: 80%;"),
                                       style = "text-align: center; box-shadow: 2px 2px 6px lightgray;")
                      )
           ),
    column(3,
           actionLink(inputId = "reg_comp_link", 
                      label = tags$div(tags$span("Regional Comparison", style='font-size:17px;'),
                                       br(),
                                       tags$img(src="compare_v3.png", style="width: 80%;"),
                                       style = "text-align: center; box-shadow: 2px 2px 6px lightgray;")
           )
    )

    
    
  ),
  fluidRow(
    column(3,
           tags$img(src="blank.png", style="width: 80%; max-height: 160px")
    ),
    column(3,
           actionLink(inputId = "region_sah_comp_link", 
                      label = tags$div(
                        tags$span("State Comparison Analysis",  style='font-size:17px;'),
                        br(),
                        tags$img(src="sah.png", style="width: 80%;"),
                        style = "text-align: center; box-shadow: 2px 2px 6px lightgray;")
           )
    ),       
    column(3,
           tags$a(
             tags$div(
               tags$span("NPI Dashboard",  style='font-size:17px;'),
               br(),
               tags$img(src="npi.png", style="width: 80%; max-height: 160px;"),
               style = "text-align: center; box-shadow: 2px 2px 6px lightgray;"
             ),
             href =  "https://c19hcc.org/resources/npi-dashboard/"
             )
    ),
    
    column(3,
           actionLink(inputId = "similarity_link", 
                      label = tags$div(
                        tags$span("Regional Similarity",  style='font-size:17px;'),
                        br(),
                        tags$img(src="similar_v3.png", style="width: 80%;"),
                        style = "text-align: center; box-shadow: 2px 2px 6px lightgray;")
           )
    )
    
  ),
  fluidRow(
    column(3,
           tags$img(src="blank.png", style="width: 80%; max-height: 160px")
    ),
    column(3,
           # USE THIS TEMPLATE FOR INTERNAL LINKS
           actionLink(inputId = "mortality_analysis_link",
                      label = tags$div(
                        tags$span("Mortality Analysis", style='font-size:17px;'),
                        br(),
                        tags$img(src="mortality_v1.png", style="width: 80%;"),
                        style = "text-align: center; box-shadow: 2px 2px 6px lightgray;")
           )
    ),
    column(3,
           tags$img(src="blank.png", style="width: 80%; max-height: 160px")
    ),
    column(3,
           tags$img(src="blank.png", style="width: 80%; max-height: 160px")
    )
  ),
  
  hr(style='margin-top: 0px'),
  
  fluidRow(
    column(3,
           tags$img(src="models_highres.png", style="width: 100%;")
    ),        
    
    column(3,
           actionLink(inputId = "forecast_link", 
                      label = tags$div(
                        tags$span("Short-Term Forecast",  style='font-size:17px;'),
                        br(),
                        tags$img(src="forecast.png", style="width: 80%;"),
                        style = "text-align: center; box-shadow: 2px 2px 6px lightgray;")
           )
    ),
    column(3,
           actionLink(inputId = "model_link", 
                      label = tags$div(
                                       tags$span("Intervention Model",  style='font-size:17px;'),
                                       br(),
                                       tags$img(src="model_v3.png", style="width: 80%;"),
                                       style = "text-align: center; box-shadow: 2px 2px 6px lightgray;")
           )
    ),
    column(3,
           actionLink(inputId = "model_comp_link", 
                      label = tags$div(
                        tags$span("Model Comparison",  style='font-size:17px;'),
                        br(),
                        tags$img(src="model_comp.png", style="width: 80%;"),
                        style = "text-align: center; box-shadow: 2px 2px 6px lightgray;")
           )
    )
    
    
  ),
  
  fluidRow(
    column(3,
           tags$img(src="blank.png", style="width: 80%;")
    ),
    column(3,
           actionLink(inputId = "hotspots_link",
                      label = tags$div(
                        tags$span("Hotspot Identification", style='font-size:17px;'),
                        br(),
                        tags$img(src="hotspots.png", style="width: 80%;"),
                        style = "text-align: center; box-shadow: 2px 2px 6px lightgray;")
           )
    ),
    column(3,
           tags$a(
             tags$div(
               tags$span("PPE & Pharma Model",  style='font-size:17px;'),
               br(),
               tags$img(src="ppe_v2.png", style="width: 80%; max-height: 160px;"),
               style = "text-align: center; box-shadow: 2px 2px 6px lightgray;"
             ),
             href =   "http://dashboards.c19hcc.org/ppe/"
           )
     )#,
    # column(3,
    #
    #        tags$a(
    #          tags$div(
    #            tags$span("System Dynamics Model",  style='font-size:17px;'),
    #            br(),
    #            tags$img(src="sd.png", style="width: 80%; max-height: 160px;"),
    #            style = "text-align: center; box-shadow: 2px 2px 6px lightgray;"
    #          ),
    #          href = "https://exchange.iseesystems.com/public/stephanieglasser/covid-npi/index.html#page1"
    #        )
    #
    # )
    
  ),
  
  tags$hr(),
  tags$h2("Key Features", class='h2-c19'),
  
  tags$h3("C19HCC Decision Dashboard", class='h3-c19'),
  tags$span('An executive decision-aid for policy makers', class='italic-header'),
  tags$p(
    "The Decision Support Dashboard was inspired by the National Governor's Association's Roadmap to
    Recovery report release in April 2020. This report recommends key metrics and decision criteria necessary
    to reopen, manage, and react to the on-going pandemic.
    
    This Dashboard uses location specific data to help governor's, mayors, business owners, and other
    community-based decision makers answer their pandemic related questions.  It simplifies a tremendous amount
    of health specific data, turning dense information into stoplight harts"
  ),
  
  tags$h3("Regional Data", class='h3-c19'),
  tags$span("View the geographical spread of COVID", class='italic-header'),
  tags$p("This view presents a dashboard to discover trends in current cases, deaths, resources, testing, and mobility of a
         given region. Trends are shown temporarlly with a time-series chart, and across regions with geo-spatial maps. An
         interactive table to the right of the map allows you to quickly learn the most at-risk locations with respect to
         a chosen metric. Data can  be viewed at the national, state, and county level; simply mouse over areas to learn more and click on them to zoom in."),
  
  tags$h3("Regional Comparison Tool", class='h3-c19'),
  tags$span("Compare the progression of different areas", class='italic-header'),
  tags$p(
    "Select a country, state, county, or metro area by typing the name of the area into the appropriate text box.  This will allow you 
    to filter and then select the appropriate region.  You may select as many areas as desired in any combination.  As you do so, a
    graph of deaths will appear on top, and confirmed cases below.  Select from the ", tags$b("Begin Data"), " drop down to line up 
    the data chronologically, beginning with the first case, first death, first 100 cases, time when exceeded 0.2 cases per 100,000 people, 
    or by the date of a Stay at Home order.  It is useful to line up areas in different ways to make visual comparisons of trends."),
  tags$p("You can use the ", tags$b("Show Data As"), " dropdown to view the data as: raw data, on a logarithmic axis, the 3-day change, 
         the daily growth rate, the doubling rate, or as incidence per 100,000 persons. When the ", tags$b("Overlay Region-Specific NPIs"), 
         " is toggled on, each time series will be annotated with a vertical line and code for the data of NPI for that region.  The keys 
         for these NPI codes are in the table labeled ", tags$b("NPI Key"), "."),
  
  tags$h3("Similar Regions Tool", class='h3-c19'),
  tags$span("Identify similar regions based on COVID cases and demographics", class='italic-header'),
  tags$p(
    "Select a state and county by typing the name of the area into the text boxes.  This will allow you to filter and then select the appropriate 
    region. The tool will automatically generate a list of regions (counties, states, metro areas, and nations) that are similar to the 
    selected region. A graph of confirmed cases will appear to the right. Select a ", tags$b("Similarity Type"), 
    " from the dropdown list; definitions of these types are included in the table labeled ", tags$b("Types of Similarity"), 
    ". The ", tags$b("Begin Data"), " selection is automatically set to ", tags$b("From First 20 Cases, with Other Region Alignment"), 
    " which shifts the case curves of the most similar regions up to 1 week in either direction so that they are as closely aligned as 
    possible. You may select other ", tags$b("Begin Data"), " options. You can use the ", tags$b("Show Data As"), 
    " dropdown to view the data as: raw data, on a logarithmic axis, the 3-day change, the daily growth rate, the doubling rate, 
    or as incidence per 100,000 persons. When the ", tags$b("Overlay Region-Specific NPIs"), 
    " is toggled on, each time series will be annotated with a vertical line and code for the data of NPI for that region.  
    The keys for these NPI codes are in the table labeled ", tags$b("NPI Key"), 
    ". Regions with fewer than two weeks of at least 20 cases are excluded."
  ),
  
  tags$h3("Mortality Analysis Tool", class='h3-c19'),
  tags$span("Investigate trends in the virus's mortality rate over time", class='italic-header'),
  tags$p(
    "This view explores if and how the COVID-19 mortality rate is changing over time. As the risk of death due to COVID-19 
    increases with age, this analysis uses CDC datasets to estimate the distribution of cases and deaths across different age groups 
    over time, then stratifies the reported COVID-19 case and death counts into age cohorts using these distributions. 
    The mortality rate for each age group is then calculated after applying a lag between case deification and death."),
  tags$p("To explore this analysis, select the ", tags$b("Data to Graph"), 
  " from the following: Mortality Rate by Age Group, Distribution of Cases by Age Group, Distribution of Deaths by Age Group, 
  and Testing Availability."),
  
  tags$h3("State Comparison Analysis", class='h3-c19'),
  tags$span('Compare key COVID and behvaioral metrics across all states on a scatterplot', class='italic-header'),
  tags$p(
    "This tab allows you to visualize understand how changes in State-level policies are affecting trends in COVID-19 case trends
    and population behavior. We use these scatterplots to compare states according to population mobility, growth of COVID-19
    within the state, and percent positive testing rate. From these scatterplots, we can identify clusters, trends, and
    potential outlier states to better understand any potential relationship between a state’s policy and their epidemic outcomes."
  ),
  
  tags$h3("NPI Analysis Dashboard", class='h3-c19'),
  tags$span('Compare the Evolution of NPIs Across States', class='italic-header'),
  tags$p(
    "Another C19HCC dashboard that visualizes the impact of Non-Pharmaceutical Interventions (NPIs) in the United States over time.
    It allows for comparison of States with respect to the timing of NPI implementation and their impact on the spread of the virus. 
    Unlike our tool, this dashboard is purely data-driven (i.e. no simulation)" 
     ),
  
  tags$h3("Short-Term Forecast", class='h3-c19'),
  tags$span('Short-term estimate of COVID cases', class='italic-header'),
  tags$p(
    "Select a state and county by typing the name of the area into the text boxes.  This will allow you to filter and then select the appropriate 
    region. A graph of the estmated and actual number of cumulative cases will appear to the right. This forecast uses curve-fitting to generate these estimates,
    and should not be considered reliable for long-term estimates."
      ),
  
  tags$h3("Intervention Model", class='h3-c19'),
  tags$span('"What If" Modeling', class='italic-header'),
  tags$p(
    "This tab uses an SEIR model to understand the dynamics of COVID-19. The Intervention model used in this tool is built using an adaptation of 
    the ", tags$a(href= "https://alhill.shinyapps.io/COVID19seir/", "model created by Alison Hill"), 
    " and licensed under a ", tags$a(href="https://creativecommons.org/licenses/by-sa/4.0/", "Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0) License"), 
    ". SEIR models are a primary tool in epidemiology, providing insight into epidemiological dynamics, ", 
    tags$i("but are not well-suited for accurate near-term forecasts given their dependence on parameters that are difficult to estimate and assumptions about population mixing.")
  ),
  
  tags$p('We have attempted to "fit" transmission characteristics to local areas by finding optimized model parameters that best fit 
  the actual data for COVID-19 ', tags$span("deaths"), " for a given area; often however, these fits are not satisfactory."),
  
  # tags$p("As you use this tool, keep in mind that the model may not fit observed data. The purpose of these fits is to ensure the 
  #        model is somewhat accurate to a given area for purposes of What If evaluation of policies.  The structure and assumptions 
  #        necesary in an SEIR model does not make them well suited to forecasting.  If you desire a forecast, statistical approaches 
  #        perform better, but are typically only useful in the short term. A comparison of various forecast models can be found ", tags$a(href="https://reichlab.io/covid19-forecast-hub/", "here.")), 
  # 
  tags$h4("Using Model Interventions"),
  tags$p("Start by choosing a U.S. state from the top dropdown list. With this, the model is loaded with basic data describing that state, including the population, case history, hospital capacity, and the dates of any NPIs implemented by the state. Each state has different transmission characteristics.After selecting a State to model, select what metrics to graph.  The choices are:",
         tags$ul(
           tags$li("Critical Infections and ICU Bed Capacity"),
           tags$li("Critical Infections and Ventilator Capacity"),
           tags$li("Cases Needing Hospitalization and Hospital Bed Capacity"),
           tags$li("All Infections"),
           tags$li("Deaths")
         )),
  
  tags$h4("Inspect the Graph"),
  tags$p("The graph to the right plots the modeled value selected under ",
  tags$b("Data to Show"), ". The red line indicates the present day, and the purple shaded region is the time period during which NPIs are in place. If the value of the metric being graphed has a very high maximum value in the shown time period, the early numbers may be small and difficult to see. You can click and drag on a region of the chart to zoom in on that area. When you move the cursor over any data line, a tool tip will appear with the values at that date. More viewing options are available in the top right of the graph, such as zooming, panning, resetting the view, and tooltip style."),
  tags$p("Underneath the graph is a timeline of NPIs currently in place for a given state, starting on the actual date each went into effect. As most states have not yet announced when NPIs will be lifted, each NPI is kept in place for the duration of the simulation. The length of these NPIs can be changed by dragging the bars to the desired time, and then clicking on the timeline canvas to commit the change. Finer control on data selection can be achieved through the date picker in the NPI tab, described below."),
  
  tags$h4("Overlay Actual Data"),
  tags$p("Underneath ", tags$b("Data to Show"), " is an option to ", tags$b("Overlay Actual Data"), 
         ". Click the toggle to overlay actual data when available for deaths and confirmed cases. Actual data for deaths is only 
         overlaid when ", tags$i("Deaths"), " is selected as the metric to graph, while confirmed cases are only overlaid when ", 
         tags$i("All Infections"), " is selected as the metric. These models were fit to the number of deaths, so the modeled number 
         of all infections includes the many infections that go unconfirmed and may be substantially higher than the number of 
         confirmed cases due to limitations in the availability of testing."),
  
  tags$h4("Non-Pharmaceutical Interventions"),
  tags$p("This tab shows the five primary NPIs used. By default, all NPIs currently in place in that state are selected, beginning on the date they were enacted, and held in place for four months. The dates of each NPI can be changed using the date picker next to the name or the timeline canvas described above. As the NPIs change, the shaded region in the graph will also change."),
  tags$p(
    "Currently, the models of NPI effectiveness are rough estimates, chosen based upon observations from Google's ", 
    tags$a("COVID-19 Mobility Reports", href="https://www.google.com/covid19/mobility/"), 
    tags$a("SafeGraph's Geographic Response to Shelter in Place", href="https://www.safegraph.com/dashboard/covid19-shelter-in-place"), 
    ", and real world observation. This is an active area of model development, and it is known that different locations have varying 
    levels of effectiveness, and that this effectiveness varies over time.  The model does not yet account for this."),
  tags$p("The values for NPIs can be adjusted in the ", tags$b("Advanced Settings"), 
         " tab if desired.  An NPI with a value of 1.0 would mean that it is 100% effective at reducing contact between people; 0.5 would indicate a 50% reduction."),
  
  tags$h4("Health System Capacity"),
  tags$p("Under the NPI tab, actual state data for hospital beds, ICU beds, ventilators, and their average usage is shown. By varying these parameters, e.g., adding more capacity, you can observe the impact this has on the cumulative number of deaths."),
  tags$p("This data does not currently include information on local field hospitals that have been deployed in response to COVID-19."),
  
  tags$h3("Advanced Settings"),
  tags$p("This tab can be found in the left sidebar. In Alison Hill's original model, there are many parameters for the Intervention Model. 
         We have chosen default values based on current data becoming available, with a bias toward U.S. data. Additionally, we have 
         added more parameters for the effectiveness of NPIs, which can vary. All sources can be found in the ", 
         actionLink(inputId = "sources_link", label="Sources"), 
         " tab at the left. When fitting the model to individual states, four parameters were varied: population size, 
         the date of the first case, initial # infected, mild transmission rate, severe transmission rate, and critical 
         transmission rate. Please experiment with different values to build insight, especially for highly known parameters, 
         such as the percentage of asymptomatic carrriers there are in the population."),
  
  tags$h3("Model Comparison Tool", class='h3-c19'),
  tags$span('Compare COVID projections from other leading state-of-the-art forecasting models.', class='italic-header'),
  tags$p(
    "With this tool, you can compare forecasts from all the top models cited by the CDC as tools to help health policy officials direct responses to COVID-19. Each model
    has its strengths, limitations, and sets of assumptions, but when viewed together, they can give a more holistic forecast than one model alone."
  ),
  
  tags$h3("Hotspot Identification", class='h3-c19'),
  tags$span('View current and forecasted hotspots at the county level', class='italic-header'),
  tags$p(
    "This tool allows you to visualize current COVID hotspots across the country at the county level, and forecast where
    future hotspots will be based on current trends. By default, hotspots are defined by looking at the change in cases per capita over
    the entire duration of data, but you can adjust the criteria to look at totals, or shorten the time-frame to 1 or two weeks."
  ),
  
  tags$h3("PPE & Pharma Model", class='h3-c19'),
  tags$span('Forecast demand for PPE and pharmaceuticals at the state and county level', class='italic-header'),
  tags$p(
    "This tool generates short-term predictions of future demand for personal protective equipment (PPE) needed by healthcare and
    other essential workers when in contact with COVID-19 patients. Model predictions are driven by the confirmed number of COVID-19 cases
    within a chosen county. In addition, this tool can also predict future demand for pharmaceuticals, such as
    Asprin or AspirinDexmedetomidine."
  ),
  
  # tags$h3("System Dynamics Model", class='h3-c19'),
  # tags$span('Model COVID-19 spread at the state-level using pharmaceutial and non-pharmaceutial interventions', class='italic-header'),
  # tags$p(
  #     'This tool allows you to model the spread of COVID-19 using a highly flexible SEIR model. Unlike many existing models,
  #     it allows you to model both pharmaceutical interventions (vaccine effectiveness, quality of COVID treatment) and
  #     non-pharmaceutical interventions (improved hygine, mask wearing, and social distancing). While it is calibrated on
  #     state-level case data, it should not be used for forecasts.'
  # ),
  
  
  tags$h3("Contact"),
  tags$p("Questions about this tool can be directed to ",tags$a(href="cglazner@mitre.org", "Dr. Chris Glazner (cglazner@mitre.org)"), ".")
  
)

model_comp_ui <- fluidPage(
  fluidRow(
    column(width = 3,
           tags$h3("Model Comparison"),
           tags$div("The models below are cited by ", tags$a(href="https://www.cdc.gov/coronavirus/2019-ncov/covid-data/forecasting-us.html", "the CDC"),
                  " as tools to help health policy officials direct responses to COVID-19. Each model has strengths, limitations, and sets of assumptions that must be considered and vetted appropriately. Only statistically fit model forecasts are compared. "),
           hr(),
           h4("Select State"),
           selectizeInput(inputId = "model.comp.state", label = NULL, choices = c("United States", sort(STATE_NAMES$full)), selected = "California", multiple=FALSE),
           box(title = "Data to Graph",
               radioButtons(inputId = "model.comp.population.type", label = "Select one of the following.",
                              choices = c("Cumulative Hospitalizations"="C", "Cumulative Deaths"="D",
                                          "New Daily Deaths"="DeltaD", "New Hospitalizations" = "DeltaI2_I3"),
                              #choices = c("Critical Infections"="I3", "Cases Anticipated Needing Hospitalization" = "I2_I3", "All Cases" = "I1_I2_I3", "Deaths" = "D"),
                              selected = c("Cumulative Deaths" = "D")),
               width = "100%"
           ),
           box(title = "Forecast Models", width = "100%",
               tags$style(".checkbox { margin-top: 0px; margin-bottom: 3px }"),
               tags$b("Select one or more of the following."),
               checkboxInput(inputId = "model.comp.cu_80", label = tags$span("Columbia University (20% Contact Reduction)  ", tags$a(icon("info-circle"), href="https://columbia.maps.arcgis.com/apps/webappviewer/index.html?id=ade6ba85450c4325a12a5b9c09ba796c"), style="white-space: nowrap;"), value = TRUE),
               checkboxInput(inputId = "model.comp.ihme", label = tags$span("IHME  " , tags$a(icon("info-circle"), href="https://covid19.healthdata.org/united-states-of-america"), style="white-space: nowrap;"), value = TRUE),
               checkboxInput(inputId = "model.comp.mitre.curve", label = tags$span("C19HCC Short-term  ", tags$a(icon("info-circle"), href="http://dashboards.c19hcc.org/ppe"), style="white-space: nowrap;"), value = TRUE),
               checkboxInput(inputId = "model.comp.mitre.ens", label = tags$span("MITRE Ensemble  ", actionLink(inputId = "about_mitre_ens_link", label = "", icon = icon("info-circle")), style="white-space: nowrap;") , value = TRUE),
               #tags$span(style="display: none;", checkboxInput(inputId = "model.comp.mitre.curve", label = tags$span("MITRE CurveFit  ", tags$a(icon("info-circle"), href="http://dashboards.c19hcc.org/ppe"), style="white-space: nowrap;"), value = FALSE)),
               #checkboxInput(inputId = "model.comp.mitre.seir", label = tags$span("MITRE SEIR  ", actionLink(inputId = "about_mitre_seir_link", label = "", icon = icon("info-circle")), style="white-space: nowrap;") , value = FALSE),
               #checkboxInput(inputId = "model.comp.penn.chime", label = tags$span("Penn CHIME  " , tags$a(icon("info-circle"), href="http://www.google.com")), value = FALSE),
               checkboxInput(inputId = "model.comp.austin", label = tags$span("UT Austin  " , tags$a(icon("info-circle"), href="https://covid-19.tacc.utexas.edu/projections/"), style="white-space: nowrap;"), value = TRUE),
               checkboxInput(inputId = "model.comp.geneva", label = tags$span("University of Geneva  " , tags$a(icon("info-circle"), href="https://renkulab.shinyapps.io/COVID-19-Epidemic-Forecasting/"), style="white-space: nowrap;"), value = TRUE),
               checkboxInput(inputId = "model.comp.mobs", label = tags$span("Northeastern (MOBS)  " , tags$a(icon("info-circle"), href="https://covid19.gleamproject.org/"), style="white-space: nowrap;"), value = TRUE),
               checkboxInput(inputId = "model.comp.yyg", label = tags$span("Youyang Gu (YYG)  " , tags$a(icon("info-circle"), href="https://covid19-projections.com/about/"), style="white-space: nowrap;"), value = TRUE),
               #checkboxInput(inputId = "model.comp.rush", label = tags$span("Rush University  " , tags$a(icon("info-circle"), href="http://www.google.com"), style="white-space: nowrap;"), value = FALSE),
               #checkboxInput(inputId = "model.comp.stanford", label = tags$span("Stanford  " , tags$a(icon("info-circle"), href="http://www.google.com"), style="white-space: nowrap;"), value = FALSE),
               tags$i('Note: Some models do not compute all metrics (e.g. deaths/hospitalizations). Select alternative "Data to Graph" values to view other model outputs.')
               )
           ),
    column(width = 9,
           tags$div(
             withSpinner(plotlyOutput(outputId = "model.comp.plot", width = "100%", height = "40%"))
           ),
           dataTableOutput(outputId = "model.comp.info.table", width = "100%")
           )
  )

)

intervention_ui <- fluidPage(style="background-color: #ffffff;",
  fluidRow(
    box(
      tags$b(textOutput('interv_disclaimer')),
      title=NULL, footer=NULL, width = "100%", style = "background-color: #0D2F4F; color: #FFF601;")
  ),
  fluidRow(
    column(width = 6,
           uiOutput('interv_tab_title'),
           uiOutput('interv_tab_description'),
           style='margin-bottom:10px;'
           ),
    column(width = 6,
           div(
             div(id='interv_options',
                 # tags$h4(tags$b('Projection Model: ')), 
                 # div(
                 #   radioGroupButtons(inputId = "interv_model_toggle", label = NULL,
                 #                     choiceNames = c("NPI", "Mobility", "Statistical (PPE)", "Ensemble"),
                 #                     choiceValues = c('npi','mobility', 'ppe', 'ensemble'),
                 #                     selected = c('npi'), size = "normal")
                 # ),
                 # tags$h4(tags$b("State: ")),
                 div(id = "intervention_state_div",
                     selectizeInput(inputId = "inter_state", label=NULL, choices = STATE_NAMES$full, selected = c("California"), multiple = FALSE),
                     style='width: 120px; margin-left:10px; margin-right: 20px;'),
                 tags$h4(tags$b('County: ')),
                 div(id = "intervention_county_div",
                     uiOutput("intervention.selector.county.ui"),
                     style='width: 160px; margin-left:10px; margin-right: 20px'),
                 
                 div(id='per_capita_div2',
                     tags$h4(tags$b("Per-Capita Counts: ")),
                     #tags$i('For count measures (cases, deaths, tests), control for population.'),
                     div(
                       materialSwitch(inputId = "interv.per.capita", label = NULL, inline = TRUE, value = FALSE, status = "info"),
                       style='margin-left: 10px'
                     ),
                     style='display: flex;'
                 ),

                 style='display: flex;'
             ),
             tags$br(),
             fluidRow(
               box(
                 actionButton("run_model", "Run Models and Update Plots"),
                 tags$div(tags$b('Click the "Run Models and Update Plots" button above to update the plots below after selecting a region or updating the model parameters. 
                        The plots may take up to 10 seconds to update as the models run.')),
                 tags$br(),
                 title=NULL, footer=NULL, width = "100%",  style = "background-color: #f2f2f2; border: 3px solid #0D2F4F;"
               ),
               style='margin-left: 10px; margin-right: 20px; '
             ),
             style='margin-left: 40px; margin-top: 10px;'
             ),
           )
  ),
  fluidRow(
    column(width=6, 
    ),
    column(width=6)
  ),
  fluidRow(
    tabBox(
      width = "100%", selected = "Total Deaths",
      tabPanel(title = "All Active Cases", plotlyOutput("inter_model_plt_active_cases", height = "100%")),
      tabPanel(title = "Hospitalizations", plotlyOutput("inter_model_plt_hosp", height = "100%")),
      tabPanel(title = "Total Deaths", plotlyOutput("inter_model_plt_deaths", height = "100%")),
      tabPanel(title = "Daily Deaths", plotlyOutput("inter_model_plt_daily_deaths", height = "100%")),
      tabPanel(title = "Critical Cases", plotlyOutput("inter_model_plt_critical_cases", height = "100%"))
    )
  ),
  fluidRow(
    tags$div(style="background-color: white; width: 100%; position: relative;",
           #note to self - removed one nest layer of divs with this styling ^^ may need to add back in if not working
           tags$div(
             tags$div(
               timevisOutput("intervention.timeline"),#, height = "20%"),
               uiOutput("gantt.helper.text"),
               br(),
               style="position: relative; left: 55px; margin-right: 262px;"
             ),
             tags$div(
               style='position: absolute; right: -170px; top: 2px; float: right;'
             ),
             style="position: relative;"
           )
    )
  ),
  fluidRow(
    plotlyOutput(outputId = "mobility.beta.plt", width = "100%", height = "100%") #, height = "10%")
  ),
  tags$hr(),
  tags$br(),
  tags$h4("Additional Simulation Parameters"),
  tags$p('The settings below drive the underlying parameters that generate the figures and statistics above. 
         To update the model plots after adjusting these parameters, click the "Run Models and Update Plots" button at the top of the page.'),
  fluidRow(
    column(width = 6,
           box(title = div(icon("tasks"), " Non-Pharmaceutical Interventions (NPIs)"), status = "info", width = "100%",collapsible = TRUE,
               id = "npi_dates_area",
               fluidRow(style="margin-left: 1%; margin-right: 1%;",
                        column(width = 4,
                               tags$b("NPI Name")),
                        column(width = 4,
                               tags$b("NPI Dates")),
                        column(width = 4,
                               tags$b("NPI Effectiveness"))
               ),
               fluidRow(style="margin-left: 1%; margin-right: 1%; border-top:solid grey 1px;",
                        column(width = 4,
                               checkboxInput(inputId = "interv_bar", label = "Bar/Restaurant Limits", value = TRUE)
                        ),
                        column(width = 4,
                               dateRangeInput(inputId = "interv_bar_date", label = NULL, start = "2020-03-15", end="2020-07-14", format = "mm/dd/yyyy", min = "2020-01-01",max = "2021-12-31")
                        ),
                        column(width = 4,
                               radioGroupButtons(inputId = "npi.bar.prop.mild.button", label = NULL,
                                                 choiceNames = c("Small", "Medium", "Large"),
                                                 choiceValues = c(0.025, 0.05, 0.075), 
                                                 selected = c(0.05), size = "sm"),
                                                 #choiceValues = c(0.025, 0.05, 0.075), 
                                                 #selected = c(0.10), size = "sm"),
                               numericInput("npi.bar.prop.mild", label = NULL, min = 0, max=1, value=0.1, step = 0.01)
                               )
               ),
               fluidRow(style="border-top:solid grey 1px; margin-left: 1%; margin-right: 1%;",
                        column(width = 4,
                               checkboxInput(inputId = "interv_lgb", label = "Large Gatherings Ban", value = TRUE)
                        ),
                        column(width = 4,
                               dateRangeInput(inputId = "interv_lgb_date", label = NULL, start = "2020-03-12", end = "2020-07-20", format = "mm/dd/yyyy", min = "2020-01-01", max = "2021-12-31")
                        ),
                        column(width = 4,
                               radioGroupButtons(inputId = "npi.lgb.prop.mild.button", label = NULL,
                                                 choiceNames = c("Small", "Medium", "Large"),
                                                  choiceValues = c(0.075, 0.1, 0.125),
                                                  selected = c(0.1), size = "sm"),
                                                 #choiceValues = c(0.10, 0.15, 0.20),
                                                 #selected = c(0.15), size = "sm"),
                               numericInput("npi.lgb.prop.mild", label = NULL, min = 0, max=1, value=0.25, step = 0.01)
                        )
               ),
               fluidRow(style="border-top:solid grey 1px; margin-left: 1%; margin-right: 1%;",
                        column(width = 4,
                               checkboxInput(inputId = "interv_mm", label = "Mandatory Mask Usage", value = FALSE)
                        ),
                        column(width = 4,
                               dateRangeInput(inputId = "interv_mm_date", label = NULL, start = "2020-04-01", end = "2020-07-20", format = "mm/dd/yyyy", min = "2020-01-01", max = "2021-12-31")
                        ),
                        column(width = 4,
                               radioGroupButtons(inputId = "npi.mm.prop.mild.button", label = NULL,
                                                 choiceNames = c("Small", "Medium", "Large"),
                                                 choiceValues = c(0.025, 0.05, 0.075),
                                                 selected = c(0.05), size = "sm"),
                                                 #choiceValues = c(0.05, 0.10, 0.15),
                                                 #selected = c(0.10), size = "sm"),
                               
                               numericInput("npi.mm.prop.mild", label = NULL, min = 0, max=1, value=0.05, step = 0.01)
                        )
               ),
               fluidRow(style="border-top:solid grey 1px; margin-left: 1%; margin-right: 1%; display: none;",
                        column(width = 4,
                               checkboxInput(inputId = "interv_mqt", label = "Mandatory Quarantine for Travelors", value = FALSE)
                        ),
                        column(width = 4,
                               dateRangeInput(inputId = "interv_mqt_date", label = NULL, start = "2020-04-01", end = "2020-07-20", format = "mm/dd/yyyy", min = "2020-01-01", max = "2021-12-31")
                        ),
                        column(width = 4,
                               radioGroupButtons(inputId = "npi.mqt.prop.mild.button", label = NULL,
                                                 choiceNames = c("Small", "Medium", "Large"),
                                                 choiceValues = c(0.025, 0.05, 0.075),
                                                 selected = c(0.05), size = "sm"),
                                                 #choiceValues = c(0.05, 0.10, 0.15),
                                                 #selected = c(0.10), size = "sm"),
                               numericInput("npi.mqt.prop.mild", label = NULL, min = 0, max=1, value=0.05, step = 0.01)
                        )
               ),
               fluidRow(style="border-top:solid grey 1px; margin-left: 1%; margin-right: 1%;",
                        column(width = 4,
                               checkboxInput(inputId = "interv_nbc", label = "Non-Essential Business Closures", value = TRUE)
                        ),
                        column(width = 4,
                               dateRangeInput(inputId = "interv_nbc_date", label = NULL, start = "2020-03-19", end="2020-07-20", format = "mm/dd/yyyy",min = "2020-01-01", max = "2021-12-31")
                        ),
                        column(width = 4,
                               radioGroupButtons(inputId = "npi.nbc.prop.mild.button", label = NULL,
                                                 choiceNames = c("Small", "Medium", "Large"),
                                                 #choiceValues = c(0.2, 0.3, 0.35),
                                                 #selected = c(0.3), size="sm"),
                                                 choiceValues = c(0.075, 0.1, 0.125),
                                                 selected = c(0.1), size = "sm"),
                               numericInput("npi.nbc.prop.mild", label = NULL, min = 0, max=1, value=0.25, step = 0.01)
                        )
               ),
               fluidRow(style="border-top:solid grey 1px; margin-left: 1%; margin-right: 1%;",
                        column(width = 4,
                               checkboxInput(inputId = "interv_sc", label = "School Closures", value = TRUE)
                        ),
                        column(width = 4,
                               dateRangeInput(inputId = "interv_sc_date", label = NULL, start = "2020-03-19", end="2020-07-16", format = "mm/dd/yyyy",min = "2020-02-01",max = "2021-12-31")
                        ),
                        column(width = 4,
                               radioGroupButtons(inputId = "npi.sc.prop.mild.button", label = NULL,
                                                 choiceNames = c("Small", "Medium", "Large"),
                                                 choiceValues = c(0.05, 0.075, 0.1),
                                                 selected = c(0.075), size = "sm"),
                                                 #choiceValues = c(0.10, 0.15, 0.20),
                                                 #selected = c(0.15), size = "sm"),
                               numericInput("npi.sc.prop.mild", label = NULL, min = 0, max=1, value=0.15, step = 0.01)
                        )
               ),
               fluidRow(style="border-top:solid grey 1px; margin-left: 1%; margin-right: 1%;",
                        column(width = 4,
                               checkboxInput(inputId = "interv_sah", label = "Stay At Home Order", value = TRUE)
                        ),
                        column(width = 4,
                               dateRangeInput(inputId = "interv_sah_date", label = NULL, start="2020-03-19", end="2020-07-20", format = "mm/dd/yyyy",min = "2020-02-01",max = "2021-12-31")
                        ),
                        column(width = 4,
                               radioGroupButtons(inputId = "npi.sah.prop.mild.button", label = NULL,
                                                 choiceNames = c("Small", "Medium", "Large"),
                                                 choiceValues = c(0.3, 0.4, 0.45),
                                                 selected = c(0.4), size = "sm"),
                                                 #choiceValues = c(0.6, 0.7, 0.8),
                                                 #selected = c(0.7), size = "sm"),
                               numericInput("npi.sah.prop.mild", label = NULL, min = 0, max=1, value=0.85, step = 0.01)
                        )
               ),
               fluidRow(style="border-top:solid grey 1px; margin-left: 1%; margin-right: 1%;",
                        column(width = 4,
                               checkboxInput(inputId = "interv_rev", label = "Reopening Plan Reversal", value = TRUE)
                        ),
                        column(width = 4,
                               dateRangeInput(inputId = "interv_rev_date", label = NULL, start="2020-06-27", end="2020-07-27", format = "mm/dd/yyyy", min = "2020-02-01", max = "2021-12-31"),
                               #actionButton("q_per_cap", label = "", icon = icon("info-circle"), style='padding:1px; font-size:120%; border:0; color: #7e8284; background-color: transparent'),
                               bsTooltip(id="interv_rev_date", title="Note: Reopening Plan Reversal cannot start until Bar/Restaurant Limits, Large Gathering Bans, Non-Essential Business Closures, and Stay at Home Order have all ended or are inactive.", placement = "below", options = list(container = "body"))
                        ),
                        column(width = 4,
                               radioGroupButtons(inputId = "npi.rev.prop.mild.button", label = NULL,
                                                 choiceNames = c("Small", "Medium", "Large"),
                                                 choiceValues = c(0.05, 0.075, 0.1), #implicit assumption that people aren't going to take this as seriously
                                                 selected = c(0.075), size = "sm"),
                               numericInput("npi.rev.prop.mild", label = NULL, min = 0, max=1, value=0.075, step = 0.01)
                        )
               ),
               uiOutput("custom.npi.1.calendar.ui"),
               uiOutput("custom.npi.2.calendar.ui"),
               uiOutput("custom.npi.3.calendar.ui"),
               uiOutput("custom.npi.4.calendar.ui"),
               uiOutput("custom.npi.5.calendar.ui"),
               uiOutput("custom.npi.6.calendar.ui"),
               uiOutput("custom.npi.7.calendar.ui"),
               uiOutput("custom.npi.8.calendar.ui"),
               uiOutput("custom.npi.9.calendar.ui"),
               uiOutput("custom.npi.10.calendar.ui"),

               hr(),
               actionButton(inputId = "add.inter.form", icon = icon("plus"), label = "Add Intervention"),
               actionButton(inputId = "sub1proxy", label = "Remove Intervention", icon = icon("minus")),
               #actionButton(inputId = "check.dates.trigger", label = "update check.dates.dat"),
               actionButton(inputId = "reset.interv.info", label = "Reset Interventions", class = "btn-reset"),
               downloadButton(outputId='downloadData', label = 'Download Data', class = "pull-right")
               )
           ),
    column(width = 6,
           box(title = div(icon("hospital"), " Health System Capacity"), status = "danger",
               width = "100%", collapsible = TRUE, collapsed = FALSE,
               uiOutput("hosp_disclaimer_tag"),
               fluidRow(
                 column(2, tags$b("All hospital beds")),
                 column(5,  sliderInput(inputId = "HospBedper2", "Quantity", value = 85161, min=1, step=1, max=250000),
                        numericInput("HospBedper", label="", value=2.3, min = 0)
                 ),
                 column(5, sliderInput("HospBedOcc2","Occupied",value = 49605, min = 0, step = 1, max = 85161),
                        numericInput("HospBedOcc",label="", value = 65, min = 0, max = 100, step = 0.1)
                 ),
                 style="background-color: #f2f2f2; margin-left: 2px; margin-right: 2px;"
               ),
               fluidRow(
                 column(2, tags$b("ICU beds")),
                 column(5,  sliderInput("ICUBedper2","Quantity",value = 6925, min = 1, step = 1, max=20000),
                        numericInput("ICUBedper",label="",value = 1.2, min = 0, step = 0.01)
                 ),
                 column(5,sliderInput("ICUBedOcc2","Occupied",value = 4069, min = 1, step = 1, max=6925),
                        numericInput("ICUBedOcc",label="",value = 68, min = 0, step = 0.01)
                 ),
                 style = "margin-left: 2px; margin-right: 2px;"
               ),
               fluidRow(
                 column(2,
                        tags$b("Mechanical ventilators"),
                        numericInput(inputId = "ConvMVCap", label = "", value = 0.0619195),
                        numericInput(inputId = "ContMVCap", label = "", value = 0.154798),
                        numericInput(inputId = "CrisisMVCap", label = "", value = 0.4179567)),
                 column(10,
                        uiOutput("ConvVentCap2"),
                        uiOutput("ContVentCap2"),
                        uiOutput("CrisisVentCap2")
                 ),
                 style="background-color: #f2f2f2; margin-left: 2px; margin-right: 2px;"
               ),
               hr(),
               fluidRow(
                 column(12,
                        actionButton(inputId = "reset.hosp.params", label = "Reset Capacity Parameters", class = "btn-reset")
                 ),
                 style = "margin-top: 1px;"
                 )
               )
           )
  )
)
