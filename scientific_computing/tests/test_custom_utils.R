# Header -----------------------------------------------------------------------
# File name: custom_utils.R
# Folder/Module name: tests
# Version: v0.1
# Created Date: 2023-11-10
# Last Edited Date: 2023-11-10
# Editor/Creator: Akson Sam Varghese
# Description: File to test the functions in the custom utils
# ------------------------------------------------------------------------------


# Source ------------------------------------------------------------------
source('code/custom_utils.R')

# Library -----------------------------------------------------------------
library(testthat)

test_get_user_input <- function(){
  #' @description
  #' A function to test code/custom_utils::get_user_input function
  #'
  #' @return None
  
  display_logs(
    "Running tests for code/custom_utils::get_user_input ... ", "info")
  # test for character pattern Y|n single character input
  expect_equal(
    get_user_input("Enter : Y", return_type="str", pattern='[Y|n]'),
    "Y"
  )
  
  # test for character pattern given to the string
  expect_equal(
    get_user_input("enter format : 7A3-W12345", return_type="str",
                   pattern='7A[1-7]-W[0-9][0-9][0-9][0-9][0-9]'),
    "7A3-W12345")
  
  # test for character pattern given to the string
  expect_equal(
    get_user_input("enter integer : 1", return_type="int", pattern=1:5),
    1
  )
}


test_read_config_file <- function(){
  
  display_logs(
    "Running tests for code/custom_utils::get_user_input ... ", "info")
  config_file <- yaml.load_file('./config.yaml')
  expect_equal(
    read_config_file(),
    config_file
  )
}


run_tests <- function(){
  #' @description
  #' A function to run the tests for functions in code/custom_utils.R
  #'
  #'
  #' @return None
  
  test_get_user_input()
  test_read_config_file()
  
  
  display_logs("Tests Complete.")
}


