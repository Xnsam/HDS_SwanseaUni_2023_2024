# Header -----------------------------------------------------------------------
# File name: demograph.R
# Folder/Module name: code
# Version: v0.1
# Created Date: 2024-01-09
# Last Edited Date: 2024-01-09
# Editor/Creator: Akson Sam Varghese
# Description: Source file for extracting the demographic information of data
# ------------------------------------------------------------------------------



# Source -----------------------------------------------------------------------
source('code/common.R')


# Library ----------------------------------------------------------------------
library(tidyverse)


options(tibble.print_min = Inf)
# calculate proportion of nationality in the data
# "British"=2,
# "Prefer not to say"=1,
# "Welsh"=3,
# "European"=4,
# "American"=5,
# "Indian"=6,
# "Irish"=7,
# "New Zealand"=8,
# "Dual British and other nationality"=9,
# "Greek"=10,
# "Brazilian"=11,
# "Filipino"=12

# Handler ----------------------------------------------------------------------


get_demograph_data <- function(){
  #' @description 
  #' A function to display the demographic information of the dataset
  #' 
  #' 
  #' @param None
  #' 
  #' 
  #' @example 
  #'  main_app()
  #' 
  #' 
  #' @return None
  
  data_path <- 'data/wales_data/preprocessed_file.csv'
  data <- read_csv(data_path)
  
  # calculate the proportion of nationality in the dataset
  tmp <- data %>%
    group_by(Nationality) %>%
    summarise(perc=round((n()/dim(data)[1]) * 100, 2)) %>%
    arrange(desc(perc))
  print(tmp)
  
  # calculate proportion of ethnicity as per nationality in the data
  tmp <- data %>%
    group_by(Nationality, `What is your ethnic group?`) %>%
    summarize(perc=round((n()/dim(data)[1]) * 100, 2))
  print(tmp)
  
  
  # calculate nationality and employment
  tmp <- data %>%
    group_by(Nationality, `Are you currently working?`) %>%
    summarize(perc=round((n()/dim(data)[1]) * 100, 2))
  print(tmp)
  
  # calculate nationality and education
  tmp <- data %>%
    group_by(Nationality, 
             `What is the highest level of education you have reached?`) %>%
    summarize(perc=round((n()/dim(data)[1]) * 100, 2))
  print(tmp)
  
  # calculate nationality and income
  # What is the number that best describes your TOTAL household income BEFORE TAX?
  
  tmp <- data %>%
    group_by(
      Nationality, 
      `What is the number that best describes your TOTAL household income BEFORE TAX?`) %>%
    summarize(perc=round((n()/dim(data)[1]) * 100, 2))
  print(tmp)
  
  
  # calculate nationality and intention to feed baby
  tmp <- data %>%
    group_by(Nationality, `How are you planning to feed your baby?`) %>%
    summarize(perc=round((n()/dim(data)[1]) * 100, 2))
  print(tmp)
  
}



















