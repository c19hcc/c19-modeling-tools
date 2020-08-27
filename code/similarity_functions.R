##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: Functions supporting the similar regions tab.
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

sim_logMSE <- function(vector1, vector2) {
  d = 1/(1+mean((log(vector1) - log(vector2))^2))
  return(d)
}

sim_MSE <- function(vector1, vector2) {
  d = 1/(1+mean((vector1 - vector2)^2))
  return(d)
}

sim_cosine <- function(vector1, vector2) {
  d = as.numeric((vector1 %*% vector2)/(norm(as.matrix(vector1), type = "2")*norm(as.matrix(vector2), type = "2")))
  return(d)
}

#similarity_type in c("case.cosine", "case.logMSE", "case.mse", "case.permil")
region_similarity_calc <- function(region1, similarity_type = "case.logMSE"){
  #region_list = unique(reg_data$identifier)
  nRegions = nrow(SIM_REGION_LIST)
  
  sim_mat = matrix(NA,nrow = nRegions, ncol = 1)
  shift_mat = matrix(0,nrow = nRegions, ncol = 1)
  
  region_num = which(SIM_REGION_LIST$mState.Providence == region1 & SIM_REGION_LIST$region_type != "Country")
  
  if (similarity_type == "case.permil") {
    v1 = REGIONAL_VECTORS_PERMIL[[region_num]]
  } else {
    v1 = REGIONAL_VECTORS[[region_num]]
  }
    
  len_reg1 = length(v1)
  
  if (length(v1) <=1) {
    sim_mat = matrix(NA,nrow = nRegions, ncol = 1)
    shift_mat = matrix(NA,nrow = nRegions, ncol = 1)
  } else {
    
    for (j in 1:nRegions) {
      if (similarity_type == "case.permil") {
        v2 = REGIONAL_VECTORS_PERMIL[[j]]
      } else {
        v2 = REGIONAL_VECTORS[[j]]
      }
      len_reg2 = length(v2)
      
      if (length(v2) <= 1) {
        sim_mat[j] = NA
        shift_mat[j] = NA 
      } else {
        d_max = 0
        start1 = 0
        start2 = 0
        
        for (startday2 in 0:7) {
          min_len = min(len_reg1, len_reg2-startday2)
          #d = as.numeric((v1[1:min_len] %*% v2[(startday2+1):(startday2+min_len)])/(norm(as.matrix(v1[1:min_len]), type = "2")*norm(as.matrix(v2[(startday2+1):(startday2+min_len)]), type = "2")))
          #d = 1/(1+mean((log(v1[1:min_len]) - log(v2[(startday2+1):(startday2+min_len)]))^2))
          if (similarity_type == "case.cosine") {
            d = sim_cosine(v1[1:min_len], v2[(startday2+1):(startday2+min_len)])
          } else if (similarity_type == "case.logMSE") {
            d = sim_logMSE(v1[1:min_len], v2[(startday2+1):(startday2+min_len)])
          } else if (similarity_type == "case.MSE") {
            d = sim_MSE(v1[1:min_len], v2[(startday2+1):(startday2+min_len)])
          } else if (similarity_type == "case.permil") {
            d = sim_MSE(v1[1:min_len], v2[(startday2+1):(startday2+min_len)])
          } else {
            print("ERROR: No case curve similarity measure selected.")
          }
          
          if (d > d_max) {
            d_max = d
            start2 = startday2
          }
        }
        for (startday1 in 0:7) {
          min_len = min(len_reg1-startday1, len_reg2)
          #d = as.numeric((v1[(startday1+1):(startday1+min_len)] %*% v2[1:min_len])/(norm(as.matrix(v1[(startday1+1):(startday1+min_len)]), type = "2")*norm(as.matrix(v2[1:min_len]), type = "2")))
          #d = 1/(1+mean((log(v1[(startday1+1):(startday1+min_len)]) - log(v2[1:min_len]))^2))
          
          if (similarity_type == "case.cosine") {
            d = sim_cosine(v1[(startday1+1):(startday1+min_len)], v2[1:min_len])
          } else if (similarity_type == "case.logMSE") {
            d = sim_logMSE(v1[(startday1+1):(startday1+min_len)], v2[1:min_len])
          } else if (similarity_type == "case.MSE") {
            d = sim_MSE(v1[(startday1+1):(startday1+min_len)], v2[1:min_len])
          } else if (similarity_type == "case.permil") {
            d = sim_MSE(v1[(startday1+1):(startday1+min_len)], v2[1:min_len])
          } else {
            print("ERROR: No case curve similarity measure selected.")
          }
          
          if (d > d_max) {
            d_max = d
            start1 = startday1
          }
        }
        
        sim_mat[j] = d_max
        shift_mat[j] = start2 - start1 
      }
    }
  }
  
  reg_sim_df = SIM_REGION_LIST
  reg_sim_df$similarity = as.vector(sim_mat)
  reg_sim_df$shift = as.vector(shift_mat)
  if (similarity_type == "case.permil") {
    reg_sim_df$data_len = lengths(REGIONAL_VECTORS_PERMIL) - reg_sim_df$shift
  } else {
    reg_sim_df$data_len = lengths(REGIONAL_VECTORS) - reg_sim_df$shift
  }
                               
  reg_sim_df = reg_sim_df[!is.na(reg_sim_df$similarity),]
  
  return(reg_sim_df)
}

## Demographic similarity
region_similarity_calc_demographics <- function(demog.flag, epid.flag, health.flag, county.name, testing.run=NULL){
  if (!is.null(testing.run)){
    demog.flag <- T
    epid.flag <- F
    health.flag <- F
    county.name <- "Washington, DC"
  }
  
  fips_name_map = USFACTS_CONFIRMED %>%
    select(countyFIPS, county = County.Name, state=State) %>%
    mutate(fips = as.character(countyFIPS), county.state = paste(county, state, sep=", "))
  
  selected.fips.char <- fips_name_map %>%
    filter(county.state == county.name) %>%
    distinct() %>%
    .$fips
  
  ## Set up data frame; ex: bronx 36005; westchester 36119
  
  tgt_fips <- aggregate_distance(demog.flag, epid.flag, health.flag, HRSA_DEMOG_CATS, 
                                 whichfips = as.character(selected.fips.char)[1], data = HRSA_SIMILARITY_PCT)
  
  tgt_fips <- tgt_fips[[1]]
  
  df_pop_county <- data.frame(region=as.numeric(names(tgt_fips)), cluster=tgt_fips, stringsAsFactors=F)
  df_pop_county$value <- names(tgt_fips)
  ordered_fips <- names(tgt_fips[order(tgt_fips)])
  
  df_pop_county$fips.rank <- sapply(df_pop_county$value, FUN = function(x){return(which(ordered_fips==x)-1)})#Top match is itself
  
  df_pop_county.final <- df_pop_county %>% 
    arrange(fips.rank) %>% 
    left_join(fips_name_map, by = c("value" = "fips")) %>% 
    filter(!is.na(countyFIPS)) %>%
    mutate(similarity = 1 - cluster/max(cluster, na.rm=TRUE)) %>%
    select(region = county.state, similarity)
  
  
  return(df_pop_county.final)
  
}

aggregate_distance <- function(d, e, h, mapping, whichfips, data){
  # d - > Demographics
  # e - > Epidemiology
  # h - > Healthcare System Characteristics
  # mapping - > dataframe with columns "Category" "Column"  
  # distances - > list of the individual column distances, dataframe inside list
  # whichfips - > the jurisdiction's FIPS as a character
  # ordered list of all fips for the list name
  # data - > dataframe
  
  row.names(data) <- data$fips
  data <- data %>%
    select(-fips)
  #data <- data[,1:(ncol(data)-1)]
  data_fips <- data[rownames(data) == whichfips, , drop=F]
  
  mapping <- mapping %>%
    filter(!is.na(Category), trimws(Category) != "")
  
  distances <-  abs(data - as.data.frame(lapply(data_fips, rep, nrow(data)), check.names=F))
  
  d_scores <- distances[, unique(mapping$Column[mapping$Category=="Population"]) ]
  e_scores <- distances[, unique(mapping$Column[mapping$Category=="Vulnerable Population Health Measures"])]
  h_scores <- distances[, unique(mapping$Column[mapping$Category=="Healthcare System Characteristics"])]
  
  if (d==T & e==T & h==T) {
    return(list(
      distance=rowSums(d_scores, na.rm = TRUE) + rowSums(e_scores, na.rm = TRUE) + rowSums(h_scores, na.rm = TRUE),
      breakdown=cbind(d_scores, e_scores, h_scores)))
  } else if (d==T & e==T & h==F) {
    return(list(
      distance=rowSums(d_scores, na.rm = TRUE) + rowSums(e_scores, na.rm = TRUE) ,
      breakdown=cbind(d_scores, e_scores)))
  } else if (d==T & e==F & h==T) {
    return(list(
      distance=rowSums(d_scores, na.rm = TRUE) + rowSums(h_scores, na.rm = TRUE),
      breakdown=cbind(d_scores, h_scores)))
  } else if (d==F & e==T & h==T) {
    return(list(
      distance=rowSums(e_scores, na.rm = TRUE) + rowSums(h_scores, na.rm = TRUE),
      breakdown=cbind(e_scores, h_scores)))
  } else if (d==T & e==F & h==F) {
    return(list(
      distance=rowSums(d_scores, na.rm = TRUE),
      breakdown=cbind(d_scores)))
  } else if (d==F & e==T & h==F) {
    return(list(
      distance=rowSums(e_scores, na.rm = TRUE),
      breakdown=cbind(e_scores)))
  } else if (d==F & e==F & h==T){
    return(list(
      distance=rowSums(h_scores, na.rm = TRUE),
      breakdown=cbind(h_scores)))
  }
  
}
