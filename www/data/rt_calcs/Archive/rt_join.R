library(dplyr)

rt_old_cty = read.csv("data/rt_counties.csv", stringsAsFactors = F) #keep this the same
rt_new_cty = read.csv("data/rt_calcs/data/rt_counties_0514.csv", stringsAsFactors = F) #change this to newest file

rt_old_us = read.csv("data/rt_national.csv", stringsAsFactors = F) #keep this the same
rt_new_us = read.csv("data/rt_calcs/data/rt_national_0514.csv", stringsAsFactors = F) #change this to newest file

rt_cty <- rbind(rt_old_cty, rt_new_cty)
rt_cty <- rt_cty[!duplicated(rt_cty[,c("region", "date")], fromLast = TRUE),]
rt_cty <- rt_cty %>% arrange(region, date)
rt_cty <- add_fips(rt_cty)
write.csv(rt_cty, "data/rt_counties.csv", row.names = FALSE) #rewrite data file

rt_us <- rbind(rt_old_us, rt_new_us)
rt_us <- rt_us[!duplicated(rt_us[,c("region", "date")], fromLast = TRUE),]
rt_us <- rt_us %>% arrange(region, date)
write.csv(rt_us, "data/rt_national.csv", row.names = FALSE) #rewrite data file


add_fips <- function(cty_df, cty_col='region'){
  
    cty_to_fips = read.csv('data/usafacts_cases.txt', stringsAsFactors = F) %>%
      select(countyFIPS, County.Name, State) %>% 
      rename(fips=countyFIPS) %>%
      mutate(county.state = paste(County.Name, State, sep=', ')) %>%
      select(county.state, fips) %>%
      distinct() 
    
    cty_df[,'cty_col'] = cty_df[,cty_col]
    cty_df = cty_df[which(!grepl('City, [A-Z]{2}', cty_df[,cty_col])),]
    cty_df <- cty_df  %>%
      left_join(cty_to_fips, by=c('cty_col'='county.state')) %>%
      select(-cty_col)
      
    old_cols = setdiff(colnames(cty_df), c('fips'))
    cty_df = cty_df[,c('fips', old_cols),]
    
    return(cty_df)
}
