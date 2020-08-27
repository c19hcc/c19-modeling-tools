##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: Build database from data sources (run daily) to reduce server load time in app.
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

library(usmap)
library(hash)
library(Hmisc)
library(dplyr)
library(readxl)
library(data.table)
source('code/pull_jhu_function.R')
source('code/dataProcessing.R')

#USAFACTS Data (for case and death time-series)

print('reading in usafacts datasets...')
USFACTS_CONFIRMED = read.csv('data/usafacts_cases.txt', stringsAsFactors = F)
USFACTS_CONFIRMED$County.Name = lapply(USFACTS_CONFIRMED$County.Name, function(x) {gsub("city", "City", x)})
USFACTS_DEATHS = read.csv('data/usafacts_deaths.txt', stringsAsFactors = F)
USFACTS_DEATHS$County.Name = lapply(USFACTS_DEATHS$County.Name, function(x) {gsub("city", "City", x)})


# staffing data at the hospital level
print('Reading in Hospital dataset...')
HOSPITALS <- read.csv("data/hospital_icu_bed_data.csv")

# staffing data at the county level
print('Building County-level Hospital dataset...')
HOSPITALS_COUNTY <- hospital_county()
write.csv(HOSPITALS_COUNTY, file='data/database/hospitals_county.csv', row.names=F)

# default hospital capacity parameters

print('Pulling in Hospital params...')
HDATA <- get_hosp_data()
write.csv(HDATA, file='data/database/hdata.csv', row.names=F)

# clinical params

print('Building covid clinical params...')
CLINICAL_PARAMS <- get_covid_clinical_params()

# ode params at state level

# print('reading in default state ode params...')
# STATE_ODE_PARAMS <- get_state_ode_params()

# state name-abbreviation hash maps

STATE_NAMES = unique(statepop[,c('full','abbr')])
STATE_LOOKUP <- hash::hash(STATE_NAMES$full, STATE_NAMES$abbr)

# Update R_t data by state

print('Pulling latest Rt data by state...')
RT = get_all_rt_data(export=T)

print('Updating transmission rate estimates...')
MOVING_BETA_DATA = update_moving_beta_db(export_res=T)


# updated STATE_INF_STATS

print('Building state covid time series...')

MEX_DAT <- suppressWarnings(get_mexican_dat())

STATE_INF_STATS <- get_all_states_national_time_series() %>%
  union_all(select(MEX_DAT, -population) %>%
              filter(as.numeric(date-firstCaseDate)>=-2 | as.numeric(date-firstDeathDate)>=-2) %>%
              mutate(State = mState.Providence))

write.csv(STATE_INF_STATS, file='data/database/state_inf_stats.csv', row.names=F)

print('Calculating state covid statistics...')
STATE_INF_METRICS <- get_states_infection_stats(n=10, rt_df=RT)
write.csv(STATE_INF_METRICS, file='data/database/state_inf_metrics.csv', row.names=F)

# international data

print('Building International covid time series')

COUNTRY_INF_STATS <- get_international_jhu_data() %>%
  filter(as.numeric(date-firstCaseDate)>=-2 | as.numeric(date-firstDeathDate)>=-2)

write.csv(COUNTRY_INF_STATS, file='data/database/country_inf_stats.csv', row.names=F)

# metro statistical area data

print('Building metro-area time series...')
MBSA_INF_STATS <-  get_mbsa_time_series() %>%
  filter(as.numeric(date-firstCaseDate)>=-2 | as.numeric(date-firstDeathDate)>=-2)
write.csv(MBSA_INF_STATS, file='data/database/mbsa_inf_stats.csv', row.names=F)

# county data

print('Building county-level time series...')
COUNTY_INF_STATS <-  get_counties_time_series(daycutoff=2)
write.csv(COUNTY_INF_STATS, file='data/database/county_inf_stats.csv', row.names=F)

COUNTY_CLEANUP <- get_county_mapping()
write.csv(COUNTY_CLEANUP, file='data/database/county_cleanup.csv', row.names=F)

# population data

print('Building world population dataset...')
WORLD_POP <- get_world_population_data() %>%
  mutate(mState.Providence = as.character(mState.Providence),
         Country.Region = as.character(Country.Region),
         population = as.numeric(as.character(population))) %>%
  union_all(MEX_DAT %>%
              select(mState.Providence, Country.Region, population) %>%
              distinct())
  
write.csv(WORLD_POP, file='data/database/world_pop.csv', row.names=F)

#forecast data (Gaussian)

#print('Building short-tern forecasts...')
#GAUSSIAN_PROJ <- get_gaussian_dat() #method 2 not ready yet; errors in script #2(folder.pth = "./data/gaussianProj/2020_04_16/")
#write.csv(GAUSSIAN_PROJ, file = "data/database/gaussian_proj.csv", row.names = FALSE)

# Other Models

print('reading in data from other models...')
MODEL_DESCR <- get_model_comp_descriptions()
MODEL_COMP_NAME_OPTS <- sort(levels(factor(MODEL_DESCR$Model)))

CDC_COMP_DAT <- get_cdc_model_comp_data()
write.csv(CDC_COMP_DAT, file = "data/database/cdc_comp_dat.csv", row.names = FALSE)

#STATE_CHIME_DAT <- get_state_chime_data()
#STATE_IHME_DAT <- get_state_ihme_data()

# hospital resource data (licensed beds, staffed beds and # of facilities) by state

STATE_OCCUPANCY = read.csv('./data/hosp_occupancy.csv', stringsAsFactors = F)
write.csv(STATE_OCCUPANCY, file='./data/database/hosp_occupancy.csv', row.names=F) 

HOSPITALIZATION_DATA <- get_nhsn_hospitalization_data()
write.csv(HOSPITALIZATION_DATA, file = "./data/database/hospitalization_data.csv", row.names = FALSE)

# State testing data (from covid-tracking.com)

print('Building state testing dataset...')
CUMULATIVE_STATE_TESTING <- read.csv('data/state_testing_results.csv') %>% select(state, positive, negative, totalTestResults, hospitalized, pending)
DAILY_STATE_TESTING <- get_daily_state_testing_data()
write.csv(DAILY_STATE_TESTING, file='data/database/daily_state_testing.csv', row.names=F)
# NPI data at the state level

# state covid tracking stats (testing, hospitalizations, positivity rates, etc.)
STATE_TRACKING_METRICS = get_state_tracking_stats(DAILY_STATE_TESTING) 
write.csv(STATE_TRACKING_METRICS, file='data/database/state_tracking_metrics.csv', row.names=F)


print('Building NPI dataset...')
CURRENT_NPIS <- c("State-Mandated School Closures", "Non-Essential Business Closures",
                  "Bar/Restaurant Limits", "Large Gatherings Ban", "Mandatory Quarantine for Travelers",
                  "Stay At Home Order", "Mandatory Masks", "Reopening Plan Reversal")

NPI_Mild_Effectiveness_Map <- c("State-Mandated School Closures" = 0.2, "Non-Essential Business Closures" = 0.4,
                                "Bar/Restaurant Limits" = 0.1, "Large Gatherings Ban" = 0.2, "Stay At Home Order" = 0.7,
                                "Mandatory Quarantine for Travelers"=0.0, "Mandatory Masks"=0.05, "Reopening Plan Reversal"=0.075)

NPI_param_name_mapper <- c("State-Mandated School Closures"="npi.sc.prop.mild", 
                           "Non-Essential Business Closures"="npi.nbc.prop.mild",
                           "Bar/Restaurant Limits"="npi.bar.prop.mild", 
                           "Large Gatherings Ban" = "npi.lgb.prop.mild", 
                           "Mandatory Quarantine for Travelers" = "npi.mqt.prop.mild",
                           "Stay At Home Order"="npi.sah.prop.mild",
                           "Reopening Plan Reversal" = "npi.rev.prop.mild")

print('getting NPI data...')
NPI_DATA <- suppressWarnings(get_npi_data())
write.csv(NPI_DATA, file='data/database/npi_data.csv', row.names=F)

print('getting latest descartes mobility data...')
MOBILITY_DATA <- get_descartes_mobility_data()
write.csv(MOBILITY_DATA, file = "data/database/mobility_data.csv", row.names = FALSE)

BETA1_TRACKING <- read.csv(file="./data/tracked_beta1_by state_4.29.csv", stringsAsFactors = FALSE)
write.csv(BETA1_TRACKING, file="./data/database/tracked_beta1_2020_04_29.csv", row.names = FALSE)

#Similarity Data
suppressWarnings(region_similarity_vectors(mindays = 14, mincases = 20))

SIMILARITY_TABLE_VIEW <- get_similarity_table_view()
write.csv(SIMILARITY_TABLE_VIEW, file='data/database/sim_table_view.csv', row.names=F)



#HRSA Clustering
HRSA_SIMILARITY_PCT <- hrsa_similarity_percentiles()
write.csv(HRSA_SIMILARITY_PCT, file="./data/database/hrsa_similarity_percentile.csv", row.names=F)

print(paste0('DONE. Database files in ', getwd(), '/data/database'))

# CDC Age cohort analysis
#Todo: update this to update death data more frequently
old_cdc_data = read.csv(file = "./data/database/cdc_age_data.csv", stringsAsFactors = FALSE, check.names=F)
if (Sys.Date() >= as.Date(max(old_cdc_data$cases_update_date))+49 + 49) { #49 days after last week of data's start date
  CDC_AGE = tryCatch(expr = {get_cdc_age_rates()},
                     error = function(e){NULL})
  if (!is.null(CDC_AGE)) {
    write.csv(CDC_AGE, file = "./data/database/cdc_age_data.csv", row.names = F)
  }
}

