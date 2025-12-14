# Header -----------------------------------------------------------------------
# File name: posthoc_analysis.R
# Folder/Module name: code
# Version: v0.1
# Created Date: 2024-01-09
# Last Edited Date: 2024-01-09
# Editor/Creator: Akson Sam Varghese
# Description: Source file for implementing post hoc analysis. the intention is
# to subset the main dataset and create the cohort or groups of samples where
# the cohort would contain significant columns and rows from the values associated
# decision tree. 
# ------------------------------------------------------------------------------


# source -----------------------------------------------------------------------
source('code/common.r')


# Library ----------------------------------------------------------------------
library(tidyverse)
library(epitools)


# Functions --------------------------------------------------------------------

pha_load_data <- function(){
  #' @description 
  #' A function to apply decision rules
  #' 
  #' 
  #' 
  #' 
  #' @example 
  #'  load_data()
  #' 
  #' 
  #' @return 
  
  # step 1: load the data
  data_path <- 'data/wales_data/processed_imputed_file.csv'
  data <- read_csv(data_path)
  dim(data)
  colname_mapping <- read_csv("data/wales_data/column_name_mapping.csv")
  
  # step 2: convert to factors
  data <- convert_to_factors(data, colname_mapping)
  names(data)[35] <- "Y"
  
  # step 3: select only the required column
  data <- data %>%
    select(Y, var_6, var_25, var_29 , var_5, var_12, var_27, var_30, var_31)
  
  return(data)
}

calc_odds_ratio <- function(prob1, prob2){
  odds1 <- prob1 / (1 - prob1)
  odds2 <- prob2 / (1 - prob2)
  return(odds1/odds2)
}

get_odds_ratio <- function(dataset){
  #' @description 
  #' A function get odds ratio for all variables
  #'  var_6, var_25, var_29 , var_5, var_12, var_27, var_30, var_31
  #' 
  #' @param 
  #' 
  #' 
  #' @example 
  #'  oddsratio(dataset)
  #' 
  #' 
  #' @return 
  ggplot(data=dataset, aes(x=var_5)) + 
    geom_histogram(stat="count")
  
  subdata <- dataset %>%
    select(var_6, Y) %>%
    mutate(bins=cut(var_6, breaks=4))
  sub_tbl <- table(subdata$bins, subdata$Y)
  round(sub_tbl / 744, 3)
  
  calc_odds_ratio(0.121, 0.160)
  calc_odds_ratio(0.177, 0.214)
  calc_odds_ratio(0.120, 0.087)
  calc_odds_ratio(0.090, 0.031)
  
  subdata <- dataset %>%
    select(var_25, Y)
  sub_tbl <- table(subdata$var_25, subdata$Y)
  round(sub_tbl / 744, 3)
  
  calc_odds_ratio(0.434, 0.460)
  calc_odds_ratio(0.009, 0.007)
  calc_odds_ratio(0.000, 0.001)
  calc_odds_ratio(0.019, 0.009)
  calc_odds_ratio(0.028, 0.005)
  calc_odds_ratio(0.016, 0.009)
  calc_odds_ratio(0.001, 0.000)
  
  
  
  subdata <- dataset %>%
    select(var_5, Y)
  sub_tbl <- table(subdata$var_5, subdata$Y)
  round(sub_tbl / 744, 3)
  
  calc_odds_ratio(0.007, 0.007)
  calc_odds_ratio(0.206, 0.176)
  calc_odds_ratio(0.190, 0.172)
  calc_odds_ratio(0.069, 0.097)
  calc_odds_ratio(0.024, 0.032)
  calc_odds_ratio(0.009, 0.005)
  calc_odds_ratio(0.004, 0.001)
  
  
  subdata <- dataset %>%
    select(var_29, Y)
  sub_tbl <- table(subdata$var_29, subdata$Y)
  round(sub_tbl / 744, 3)
  
  calc_odds_ratio(0.22, 0.285) # 2
  calc_odds_ratio(0.255, 0.199) # 3
  calc_odds_ratio(0.013, 0.003) # 6
  
  
  subdata <- dataset %>%
    select(var_12, Y)
  sub_tbl <- table(subdata$var_12, subdata$Y)
  round(sub_tbl / 744, 3)
  
  calc_odds_ratio(0.292, 0.321) # 1
  calc_odds_ratio(0.137, 0.128) # 2
  calc_odds_ratio(0.050, 0.030) # 3
  calc_odds_ratio(0.030, 0.013) # 4
  
  
  subdata <- dataset %>%
    select(var_27, Y)
  sub_tbl <- table(subdata$var_27, subdata$Y)
  round(sub_tbl / 744, 3)
  
  calc_odds_ratio(0.292, 0.321) # 1
  calc_odds_ratio(0.137, 0.128) # 2
  calc_odds_ratio(0.065, 0.066) # 3
  calc_odds_ratio(0.056, 0.038) # 4
  
  
  subdata <- dataset %>%
    select(var_30, Y)
  sub_tbl <- table(subdata$var_30, subdata$Y)
  round(sub_tbl / 744, 3)
  calc_odds_ratio(0.159, 0.147) # 1
  calc_odds_ratio(0.349, 0.345) # 2
  
  
  subdata <- dataset %>%
    select(var_31, Y)
  sub_tbl <- table(subdata$var_31, subdata$Y)
  round(sub_tbl / 744, 3)
  calc_odds_ratio(0.003, 0.003) # 1
  calc_odds_ratio(0.220, 0.206) # 2
  calc_odds_ratio(0.221, 0.249) # 3
  calc_odds_ratio(0.024, 0.019) # 4
  calc_odds_ratio(0.039, 0.012) # 5
  calc_odds_ratio(0.003, 0.004) # 6
  calc_odds_ratio(0.005, 0.000) # 7
  calc_odds_ratio(0.001, 0.000) # 8
  
}




# Hanlder ----------------------------------------------------------------------
run_post_hoc_analysis <- function(){
  #' @description 
  #' A function to run survival analysis
  #' 
  #' 
  #' 
  #' 
  #' @example 
  #'  run_post_hoc_analysis()
  #' 
  #' 
  #' @return 
  
  # Step 1: load dataset
  dataset <- pha_load_data()
  
  # Step 2: get the odds ratio
  get_odds_ratio(dataset)
  
}









