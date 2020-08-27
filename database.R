##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: Loads all data for app shiny server
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

library(usmap)
library(hash)
library(data.table)
library(Hmisc)
library(leaflet)
source('code/pull_jhu_function.R')
source('code/dataProcessing.R')

#USAFACTS Data (for case and death time-series)

USFACTS_CONFIRMED = read.csv('data/usafacts_cases.txt', stringsAsFactors = F)
USFACTS_CONFIRMED$County.Name = lapply(USFACTS_CONFIRMED$County.Name, function(x) {gsub("city", "City", x)})

USFACTS_DEATHS = read.csv('data/usafacts_deaths.txt', stringsAsFactors = F)
USFACTS_DEATHS$County.Name = unlist(lapply(USFACTS_DEATHS$County.Name, function(x) {gsub("city", "City", x)}))

# default healthcare capacity params

HDATA <- data.frame(fread('data/database/hdata.csv', stringsAsFactors = F))

# staffing data at the hospital level

HOSPITALS <- read.csv("data/hospital_icu_bed_data.csv", stringsAsFactors = F)

#hospital staffing at the county level
HOSPITALS_COUNTY<- read.csv('data/database/hospitals_county.csv')

# hospitalization data
HOSPITALIZATION_DATA <- read.csv(file = "./data/database/hospitalization_data.csv", stringsAsFactors = FALSE)

# state name-abbreviation hash maps

STATE_NAMES = unique(statepop[,c('full','abbr')])
STATE_LOOKUP <- hash::hash(STATE_NAMES$full, STATE_NAMES$abbr)

# updated STATE_INF_STATS

STATE_INF_STATS <- read_data_from_file('data/database/state_inf_stats.csv')

# infection metrics pertaining to first infection dates/counts, total cases/deaths', and doubling time
STATE_INF_METRICS <- read_data_from_file('data/database/state_inf_metrics.csv', date_cols=c('fi_date','fd_date'))

# state covid tracking stats (testing, hospitalizations, positivity rates, etc.)
STATE_TRACKING_METRICS = data.table(fread(file='data/database/state_tracking_metrics.csv'))

# international data
COUNTRY_INF_STATS <- read_data_from_file('data/database/country_inf_stats.csv')

# metro statistical area data

MBSA_INF_STATS <- read_data_from_file('data/database/mbsa_inf_stats.csv')

# county data

COUNTY_INF_STATS <- read_data_from_file('data/database/county_inf_stats.csv')
COUNTY_NAMES = suppressWarnings(get_eligible_counties(metric='deaths', cutoff=14))


COUNTY_CLEANUP <- data.frame(fread('data/database/county_cleanup.csv', stringsAsFactors = F))

# population data

WORLD_POP <- data.frame(fread('data/database/world_pop.csv', stringsAsFactors = F))

# ode params at state level

#STATE_ODE_PARAMS <- get_state_ode_params()

# ode params at county level

#COUNTY_ODE_PARAMS <- suppressWarnings(get_county_ode_params())

#Gaussian Projections

#GAUSSIAN_PROJ <- data.frame(fread("data/database/gaussian_proj.csv", stringsAsFactors = FALSE))

# Other Models

MODEL_DESCR <- get_model_comp_descriptions()
MODEL_COMP_NAME_OPTS <- sort(levels(factor(MODEL_DESCR$Model)))

#STATE_CHIME_DAT <- get_state_chime_data()
#STATE_IHME_DAT <- get_state_ihme_data()

CDC_COMP_DAT <- data.frame(fread(file="./data/database/cdc_comp_dat.csv", stringsAsFactors = FALSE))

# clinical params
CLINICAL_PARAMS <- get_covid_clinical_params()

# hospital resource data (licensed beds, staffed beds and # of facilities) by state

STATE_OCCUPANCY = read.csv('./data/database/hosp_occupancy.csv', stringsAsFactors = F)
# write.csv(STATE_OCCUPANCY, file='./data/database/hosp_occupancy.csv', row.names=F)


CAPACITY_INFO_FILE <- './descriptions/capacity_plot_explanation.txt'
CAPACITY_TEXT <- readChar(CAPACITY_INFO_FILE, file.info(CAPACITY_INFO_FILE)$size)

# State testing data (from covid-tracking.com)

CUMULATIVE_STATE_TESTING <- read.csv('data/state_testing_results.csv', stringsAsFactors = F) %>%
  select(state, positive, negative, totalTestResults, hospitalized, pending)

DAILY_STATE_TESTING <- read_data_from_file('data/database/daily_state_testing.csv', date_cols=c('date'))

# NPI data at the state level

CURRENT_NPIS <- c("State-Mandated School Closures", "Non-Essential Business Closures",
                  "Bar/Restaurant Limits", "Large Gatherings Ban", "Mandatory Quarantine for Travelers",
                  "Stay At Home Order", "Mandatory Masks", "Reopening Plan Reversal")

NPI_Mild_Effectiveness_Map <- c("State-Mandated School Closures" = 0.2, "Non-Essential Business Closures" = 0.4,
                     "Bar/Restaurant Limits" = 0.1, "Large Gatherings Ban" = 0.2, "Stay At Home Order" = 0.7,
                     "Mandatory Quarantine for Travelers"=0.0, "Mandatory Masks"=0.05, "Reopening Plan Reversal"=0.075)

# NPI_Mild_Effectiveness_Map <- c("State-Mandated School Closures" = 0.075, "Non-Essential Business Closures" = 0.10,
#                      "Bar/Restaurant Limits" = 0.05, "Large Gatherings Ban" = 0.10, "Stay At Home Order" = 0.40,
#                                 "Mandatory Quarantine for Travelers"=0.0, "Mandatory Masks"=0.05)

#   Policy Prop_Reduction_Mild_Trans Prop_Reduction_Severe_Trans Prop_Reduction_Death
# 1     State-Mandated School Closures                     0.075                           0                    0
# 2    Non-Essential Business Closures                     0.100                           0                    0
# 3              Bar/Restaurant Limits                     0.050                           0                    0
# 4               Large Gatherings Ban                     0.100                           0                    0
# 5 Mandatory Quarantine for Travelers                     0.050                           0                    0
# 6                 Stay At Home Order                     0.400                           0                    0
# 7                    Mandatory Masks                     0.050                           0                    0

NPI_param_name_mapper <- c("State-Mandated School Closures"="npi.sc.prop.mild",
                           "Non-Essential Business Closures"="npi.nbc.prop.mild",
                           "Bar/Restaurant Limits"="npi.bar.prop.mild", 
                           "Large Gatherings Ban" = "npi.lgb.prop.mild", 
                           "Mandatory Quarantine for Travelers" = "npi.mqt.prop.mild",
                           "Stay At Home Order"="npi.sah.prop.mild",
                           "Reopening Plan Reversal" = "npi.rev.prop.mild")

NPI_DATA <- read_data_from_file('data/database/npi_data.csv', date_cols=c('start_date', 'end_date', 'exp_start_date','exp_end_date'))

# State-level Rt values (reproduction rates over time)

RT = read_data_from_file('data/database/rt.csv', date_cols=c('date'))

# State-level model params

STATE_TUNING_PARAMS <- get_state_tuning_params(NPI_Mild_Effectiveness_Map) %>%
  mutate(region=state)

# County-level model params

COUNTY_TUNING_PARAMS = read_data_from_file('data/database/all_counties_optimal_params.csv',
                                           date_cols=c('start_date')) %>%
  mutate(initial.infected=1, 
         state = sapply(region, function(x){str_split(x, ', ')[[1]][[2]]}))

DEF_PARAMS <- read.csv('data/database/default_params.csv', stringsAsFactors = F)
DEF_PARAM_LIST <- as.list(DEF_PARAMS$value)
names(DEF_PARAM_LIST) = DEF_PARAMS$param
PARAM_TYPES = as.list(DEF_PARAMS$ui_type)
names(PARAM_TYPES) = DEF_PARAMS$param 

# moving window betas
MOVING_BETA_DATA <- read_data_from_file('data/database/new_region_fits.csv', date_cols=c('date'))

  # Mobility Data
MOBILITY_DATA <- data.frame(fread('data/database/mobility_data.csv', stringsAsFactors = F))
BETA1_TRACKING <- read.csv(file="./data/database/tracked_beta1_2020_04_29.csv", stringsAsFactors = FALSE)

## Computed Similarity Data
suppressWarnings(load("./data/database/regional_sim_data.RData")) #krf added warning suppression b/c utf-8 with spanish n's
SIM_REGION_LIST <- SIM_REGION_LIST %>%
  filter(str_detect(mState.Providence, "Ship", negate = TRUE)) #removing grand princess cruise ship...
SIMILARITY_TABLE_VIEW <- read.csv('data/database/sim_table_view.csv')

# Demog Similarity

HRSA_DEMOG_CATS <- read.csv(file="./data/database/hrsa_demog_cats.csv", stringsAsFactors = FALSE) %>%
  select(Column=column,Source=source,Category=CATEGORY, Description=description)
  
HRSA_SIMILARITY_PCT <- read.csv(file = "./data/database/hrsa_similarity_percentile.csv", stringsAsFactors = FALSE, check.names=F)

# PPE model case and death predictions
PPE_PREDS = get_ppe_preds

# Map projection data and parameters
load('data/database/county_map_file.Rdata')
EPSG2163 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:2163",
  proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
  resolutions = round(2^seq(16,7, -0.2)))
FORECAST_DATA <- read.csv("data/predicted_cases.csv", colClasses=c("countyFIPS"="character")) %>%
  rename(date = dates_str) %>%
  strip_dates()

EPSG2163_2 <- leafletCRS(
  resolutions = 1.5^(25:-5))

# State Demographic data
STATE_DEMOGRAPHIC <- read.csv(file = "./data/state_demographic_data.csv", stringsAsFactors = FALSE, check.names=F)

# National Age distributions
CDC_AGE <- read.csv(file = "./data/database/cdc_age_data.csv", stringsAsFactors = FALSE, check.names=F)
