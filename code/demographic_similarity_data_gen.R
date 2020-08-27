##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: Generate demographic similarity data at the county level for all counties.
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

source('database.R')
source('code/similarity_functions.R')

fips_name_map = USFACTS_CONFIRMED %>%
  select(countyFIPS, county = County.Name, state=State) %>%
  mutate(fips = as.character(countyFIPS), county.state = paste(county, state, sep=", "))

county_list <- sort(as.character(unique(fips_name_map$county.state)))

df = suppressWarnings(region_similarity_calc_demographics(T,T,T,county_list[1])) %>% 
  mutate(original_region = county_list[1], rank = row_number() - 1) %>% 
  filter(original_region != region) %>%
  filter(rank < 11)

for (region_id in county_list[2:length(county_list)]) {
  
  tryCatch({
    temp_df = suppressWarnings(region_similarity_calc_demographics(T,T,T, region_id)) %>% 
      mutate(original_region = region_id, rank = row_number() - 1) %>%
      filter(original_region != region) %>% 
      filter(rank < 11)
    
    df <- df %>% union_all(temp_df)
  }, error=function(e){})
  
}

df <- df %>% 
  left_join(fips_name_map, by = c("region" = "county.state")) %>% 
  left_join(fips_name_map, by = c("original_region" = "county.state")) %>%
  select(original_region, original_region_fips = fips.y, region = region, region_fips = fips.x, similarity, rank)

write.csv(x = df, file = "./data/demographic_similarity_all_counties.csv", row.names=F)
