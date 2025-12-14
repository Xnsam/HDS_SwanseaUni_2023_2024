# Header -----------------------------------------------------------------------
# File name: test_files.R
# Folder/Module name: tests
# Version: v0.1
# Created Date: 2024-01-10
# Last Edited Date: 2024-01-10
# Editor/Creator: Akson Sam Varghese
# Description: Source file for implementing the text files
# ------------------------------------------------------------------------------


# Library ----------------------------------------------------------------------
library(tidyverse)
library(here)

# Source -----------------------------------------------------------------------
source("code/common.R")


# Execution Code ---------------------------------------------------------------

test_files_fxn <- function(){
  # test for missing columns
  file_path = here("data/wales_data", "processed_born_in_wales_data_june_2022.csv")
  df <- read_csv(file_path)
  file_path = here("data/wales_data", "columns_mapping.csv")
  column_mapping <- read_csv(file_path)
  
  missed_list <- c()
  
  for(idx in 1:dim(column_mapping)[1]){
    row <- column_mapping[idx,]
    if(!(row$column_name %in% colnames(df))){
      missed_list <- c(row$column_name)
    }
  }
  
  if(length(missed_list) > 0){
    display_logs('Missing columns in data file', "debug")
    print(missed_list)
  }
}
