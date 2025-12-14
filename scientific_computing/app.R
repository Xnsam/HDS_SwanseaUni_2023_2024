# Header -----------------------------------------------------------------
# File name: app.R
# Folder/Module name: ./
# Version: v0.1
# Created Date: 2023-10-29
# Last Edited Date: 2023-11-24
# Editor/Creator: Akson Sam Varghese
# Description: Application file responsible for managing the components


# Setup ------------------------------------------------------------------
source('./setup.R')



# Library -----------------------------------------------------------------
library(tinsel)         # required for decorators functionality
library(tidyverse)      # required for data analysis
library(gridExtra)      # required for combined facet grid plots
library(ggforce)        # required for saving facet grid plots
library(RPostgreSQL)    # To access the database.
library(GetoptLong)     # To substitute variables into strings.
library(lubridate)      # used for coercing date formats
library(scales)         # used for custom scaling limits of axes
library(psych)          # used for pair plot correlation in Q5
library(Metrics)        # Used in custom analysis

# Source ------------------------------------------------------------------
source('code/custom_utils.R')                   # Custom utility functions
source_decoratees('code/question1.R')           # Question 1
source_decoratees('code/question2.R')           # Question 2
source_decoratees('code/question3.R')           # Question 3
source_decoratees('code/question4.R')           # Question 4
source_decoratees('code/open_end_analysis.R')   # EDA
source('tests/test_main.R')                     # Tests 


# Functions ---------------------------------------------------------------
exit_function <- function(){
  #' @description
  #' A function to exit the application
  #'
  #' @example 
  #' # exit_function()
  #'
  #' @return None

  display_logs("Clearing environment ...", "info")
  rm(list=ls()) # TODO: Need to resolve the bug rm sometimes does not work ####
  display_logs("App Stopped", "info")
}

set_data_limit <- function(config){
  #' @description
  #' A function to set the data limit for the gp database table
  #'
  #' @param config loaded config yaml file
  #' 
  #' @example
  #' # set_data_limit(config)
  #'  
  #' 
  #' @return None

  # set data limit
  display_txt <- "Enter the new default limit "
  minimum_limit <- 1000000
  maximum_limit <- 21596597 # MAX SIZE OF THE GP DATA IN DB
  
  data_limit <- get_user_input(
    display_txt, return_type="int", pattern=minimum_limit:maximum_limit)
  
  config$database$data_limit <- data_limit
  
  display_logs(paste("Data limit set to", data_limit), "success")
  # run the function
  run_app(config)
}

main_app <- function(config, option){
   #' @description
   #' A function to run the application
   #'
   #' @param config config data loaded from the config file of the project
   #' @param option an integer value for the selection option
   #' 
   #' @exmple
   #' # main_app(config, option) 
   #'
   #' @return None
  
  # list of the functions
  fxn_list <- c(
    run_question_1, run_question_2, run_question_3, 
    run_question_4, run_analysis
  )
  # sequence of available options from 1 - 5
  selection <- (1:5)
  
  if (option %in% selection){
    # if the selection are 1, 2, 3, 4, 5 run a function individually
    fxn_list[[option]](config)
    run_app(config)
  } else if (option == 6 ){
    # section to run all the functions sequentially
    for (fxn in fxn_list){
      fxn(config)
    }
  } else if (option == 7){
    set_data_limit(config)
  } else if (option == 8){
    exit_function()
  }
  else if (option == 9){
    test_main()
  }
}

get_user_option <- function(){
  #' @description
  #' A function to get the option value / function to execute from user
  #' 
  #' @param None
  #' 
  #' @example 
  #' #  get_user_option()
  #' 
  #' @return option an integer value of the user selection
  
  note_txt <- "
  In PostgreSQL, when you apply limit, the results are not consistent.
  The selection of limit N rows in PostgreSQL is random.
  "
  display_logs(note_txt, "info")
  
  display_txt <- "
  # ==========================
  Select the execution option
  
  1. Question 1
  2. Question 2
  3. Question 3
  4. Question 4
  5. Run open End Analysis
  6. Run all ( Approx. Time of Execution 7 - 10 mins)
  7. Set Data limit for gp_practice_upto_2015, default: 1000000 *
  8. Exit
  9. Run Tests
  
  Enter the number as input for corresponding task.
  Example: Type 9 and press `enter/ return` key
  
  "
  return (get_user_input(display_txt, return_type="int", pattern=1:10))
}


run_app <- function(config){
  #' @description
  #' A function to run the application 
  #' 
  #' @param config config file loaded from yaml file
  #' 
  #' @example
  #' #  run_app(config)
  #' 
  #' @return None

  # get the option corresponding to the function to execute
  option <- get_user_option()
  
  # run the options menu 
  main_app(config, option)
}

run <- function(){
  #' @description
  #' A handler function to run the application
  #' 
  #' @example
  #' #  run()
  #' 
  #' @return None
  
  # get the config data from the ./config.yaml file and install dependencies
  config <- set_up()
  
  # run the application
  run_app(config)
}


# Execution ---------------------------------------------------------------
run()



