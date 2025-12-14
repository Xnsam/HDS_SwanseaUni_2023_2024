# Header -----------------------------------------------------------------------
# File name: custom_utils.R
# Folder/Module name: code
# Version: v0.1
# Created Date: 2023-10-29
# Last Edited Date: 2023-11-24
# Editor/Creator: Akson Sam Varghese
# Description: File to store utility / reusable functions in the project
# ------------------------------------------------------------------------------



# Library ----------------------------------------------------------------------
library(crayon)         # Used in logger for log level based outputs
library(yaml)           # used in loading the config file


# Source -----------------------------------------------------------------------
source('data/queries.R')


# Functions --------------------------------------------------------------------

get_db_con <- function(d_name, con_params){
  #' @description
  #' A function to connect with the database given the specific driver name
  #' and connection parameters. This function can be modified if there are
  #' other db connections
  #'
  #' @param d_name Name of the database driver
  #' @param con_params named list of parameters required in the connection
  #'
  #' @example 
  #'  d_name <- 'PostgreSQL'
  #'  con_params <- list(
  #'    db_name='gp_practice_data', host='localhost', port=5432, 
  #'    user_name='postgres', password=.rs.askForPassword('Password:')
  #'  )
  #'  get_db_connection(d_name, con_params)
  #'  
  #' @return db_con Database connection object
  
  if (d_name == "PostgreSQL"){
    driver <- dbDriver(d_name)
    db_con <- dbConnect(
      driver, dbname=con_params$name, host=con_params$host,
      port=con_params$port, user=con_params$user,
      password=con_params$password
    ) 
  } else if (d_name == "MongoDB") {
    # TODO need to implement mongodb connection! ####
    display_logs("Implementation pending", log_level = "error")
  }
  return (db_con)
}


get_db_connection <- function(config){
  #' @description
  #' A function to connect to db and return the db connection
  #' 
  #' @param config loaded config yaml object
  #' 
  #' @example
  #'  get_db_connection(config)
  #' 
  #' @return db_con db connection object
  
  # database details from the config file
  db_config <- config$database
  tryCatch(
    expr = {
      db_con <- get_db_con(db_config$driver_name, db_config)
      text <- "DB connection Established"
      log_lvl <- "success"
    },
    error = function(e) {
      text <- "DB connection failed"
      log_lvl <- "debug"
    },
    warning = function(e){
      text <- e
      log_lvl <- "error"
    },
    error = function(e){
      display_logs(text_value=text, log_level=log_lvl)
      stop()
    }
  )
  return (db_con)
}

display_logs <- function(text_value, log_level="success"){
  #' @description
  #' A function to display the logs with respect to the log levels
  #' 
  #' @param text_value string value to be displayed
  #' @param log_level level of the log to be displayed
  #' 
  #' 
  #' @example 
  #'  display_logs(text_value="this is a warning", log_level="warning)
  #' 
  #' 
  #' @return None
  
  log_colour <- white
  
  if (log_level == "info"){
    log_colour <- blue
  } else if (log_level == "debug"){
    log_colour <- magenta
  } else if (log_level == "success"){
    log_colour <- green
  } else if (log_level == "error"){
    log_colour <- red
  } else if (log_level == "warning") {
    log_colour <- yellow
  } else if (log_level == "process"){
    log_colour <- cyan
  } else {
    log_colour <- silver
  }
  
  log_colour <- log_colour $ bold
  cat(paste("[", log_colour(log_level), "]", log_colour(text_value), "\n"))
}

set_up <- function(){
  #' @description
  #' A function to set up the application by installing and clearing the 
  #' work space and setting options to the environment
  #' 
  #' @param None
  #' 
  #' @example
  #' # set_up()  
  #' 
  #' @return None
  
  display_logs('Running set up', 'process')
  # set options for seconds
  op <- options(digits.secs = 4) 
  
  # environment management
  # renv::activate() 
  # renv::restore()
  # renv::status()
  # TODO : Fix bug for renv commands inconsistency ####
  
  # create db indexes for faster references
  create_db_indexes(config)
  
  # create views
  create_query_views(config)
  
  #create sub folders in the output
  create_question_folders()
  return (config)
}

timer <- function(f){
  #' @description
  #' A decorator to the functions to track time and logs
  #' 
  #' 
  #' @param f function to be executed
  #' 
  #' @return wrapper function object
  
  wrapper <- function(...) {
    #' @description
    #' A wrapper function for wrapping on functions
    #' 
    #' 
    #' @param ... ellipsis as defined in the function
    #' 
    #' @return f return object
    
    start_time <- Sys.time()
    res <- f(...)
    display_logs(
      paste("Fxn execution time:", round(Sys.time() - start_time, 4), "secs"), 
      log_level="info"
    ) 
    print("-----------------------------------------------------------")
    return(res)
  }
  return(wrapper)
}

execute_query <- function(config, query){
  #' @description
  #' A function to execute the query
  #'
  #' @param config loaded yaml config file
  #' @param query query to be executed
  #' 
  #' @example
  #' #  execute_query(config, query)
  #'
  #' @return result result of the executed query
 
  # connect to db
  db_con <- get_db_connection(config)

  # execute the query
  # display_logs("Executing Query ...", "process")
  result <- dbGetQuery(db_con, query)
  # display_logs("Query Execution complete ...", "success")
  
  # best practice to disconnect db after running the script
  # and not leave the connection open
  dbDisconnect(db_con)
  return (result)
}


create_db_indexes <- function(config){
  #' @description
  #' A function to create indexes in the db for faster query performance
  #'
  #' @param config loaded yaml config file
  #' 
  #' @example
  #' # create_db_indexes(config)
  #'
  #' @return None
  
  query <- "
  create index if not exists idx_practiceid
  on
    gp_data_up_to_2015(practiceid);
    
  create index if not exists idx_hb
  on 
    gp_data_up_to_2015(hb);
  "
  execute_query(config, query)
  display_logs("Created SQL Indexes.", "info")
}

get_practice_list_from_hb <- function(config, hb){
  #' @description
  #' A function to get the list of practice id as per the health board
  #' 
  #' @param config loaded config yaml object
  #' @param hb name of the health board
  #' 
  #' 
  #' @example
  #' # get_practice_list_from_hb(config, hb)
  #' 
  #' 
  #' @retun list of gp practice list
  
  query <- paste("
  select  distinct gdut.practiceid
  from    (
           select hb,
                  practiceid
           from gp_data_up_to_2015
           limit ",config$database$data_limit,"
          ) as gdut
  where
      gdut.hb = '", hb, "'
  ", sep="")
  
  df <- execute_query(config, query)
  return (df)
}


get_user_input <- function(display_txt="", return_type="str", pattern=NULL){
  #' @description
  #' A function to the user input 
  #' 
  #' @param display_txt text to be displayed to user
  #' @param return_type data type to which the user input should be typed cast
  #' @param pattern the pattern to be extracted / checked in the given string
  #' 
  #' @example
  #' #  get_user_input('this is awesome', return_type='str')
  #' #  get_user_input('Enter a number', return_type='int', pattern=1:5)
  #' 
  #' 
  #' @retun user_input typed cast user input
  
  if (display_txt != ""){
    cat(display_txt)
  }
  
  user_option <- readline("Please enter your input: ")
  
  # logic for handling when the user input is empty when enter is pressed
  if (user_option == ""){
    display_logs(
      "Incorrect input, you pressed enter / return key, rerun the app", 
      "error")
    stop()
  }
  
  # logic for handling when the data type is integer
  if (return_type == "int") {
    # try coercing the input to required int
    tryCatch(
      expr = {
        user_option <- as.integer(user_option)
      },
      error = function(e){
        display_logs("Incorrect input, expected integer", "error")
        stop()
      }
    )
  
    # check if the user number is na, if yes then stop
    if (is.na(user_option)){
      display_logs("Incorrect input, expected integer", "error")
      stop()
    }
    
    # check if the user number is with in the accepted range
    if (!user_option %in% pattern){
      display_txt <- paste(
        "Incorrect input, expected value out of option range, please try again"
      )
      display_logs(display_txt, "error")
      stop()
    }
    
  } else if(return_type == "str"){  
    # logic for handling when the data type is string
    # try coercing the input to required string
    tryCatch(
      expr = {
        # if the user input should match a pattern
        if (!is.null(pattern)){
          # extract the string as per the given pattern
          user_option <- str_extract(user_option, pattern)
          # check of the extracted string is null
          if (is.na(user_option)) {
            display_txt <- paste(
              "Incorrect input, expected string of pattern ", pattern
            )
            display_logs(display_txt, "error")
          }
        } else {
          user_option <- user_option
        }
      },
      error = function(e){
        display_logs("Incorrect input, expected string", "error")
        stop()
      }
    )
  }
  
  return (user_option)
}

export_data <- function(df, path, format="csv"){
  #' @description
  #' A function export data
  #' 
  #' @param df data frame to be exported
  #' @param path path to save the data
  #' @param format the format of the exported data
  #' 
  #' @example
  #' #  export_data(df, path, format)
  #' 
  #' 
  #' @retun None 
  
  if (format == "csv"){
    write.csv(df, path, row.names=TRUE)
  } else {
    print("format not implemented yet")
  }
}


create_data_bins <- function(data_list, stride=20){
  #' @description
  #' A function to create data bins
  #' 
  #' @param data_list list of data to be binned
  #' @param stride number of strides
  #' 
  #' @example
  #' # create_data_bins(data_list, stride)
  #' 
  #' @retun list_of_bins
  
  data_list <- as.vector(data_list)
  start <- 1
  stop <- start + stride
  data_length <- length(data_list)
  steps <- round(data_length / stride)
  
  list_of_bins <- list()
  for (i in 1:steps){
    list_of_bins[[i]] <- data_list[start:stop]
    start <- stop
    stop <- stop + stride
    if(stop >= data_length){
      stop <- data_length
    } 
  }
  return (list_of_bins)
}

custom_create_dir <- function(file_path){
  #' @description
  #' A function to create dir
  #' 
  #' @param file_path path of the file
  #' 
  #' @example
  #' # custom_create_dir(file_path)
  #' 
  #' @retun none
  
  if (!file.exists(file_path)){
    dir.create(file_path, showWarnings = FALSE)
    display_logs(paste("Folder created ", file_path))
  } else {
    display_logs(paste("Folder already exists...skipping...", file_path),
                 "info")
  }
}

get_scaled_lim <- function(vec_x){
  #' @description
  #' A function to calculate the scaled limits of given vector to zoom on plots
  #' extreme values
  #' 
  #' @param vec_x vector containing the values
  #' 
  #' @example
  #' # get_scaled_lim(c(4, 5, 6, 7, 8, 9, 10))
  #' 
  #' @retun limits vector with min and max value set from given vector
  
  max_val <- max(vec_x)
  min_val <- min(vec_x)
  delta <- max_val - min_val
  limits <- c(min_val - delta * 0.1, max_val + delta * 0.1)
  return (limits)
}

get_county_from_practice_id <- function(practice_id) {
  #' @description
  #' A function to fetch the county given practice id
  #' 
  #' @param practice_id to look up
  #' 
  #' @example 
  #' # get_county_from_practice_id('W12345')
  #' 
  #' 
  #' @retun county county value of the practice id
  
  query <- sprintf("
  SELECT    county
  FROM      address
  WHERE     practiceid = '%s'
  ", practice_id)
  
  df <- execute_query(config, query)
  return (df$county)
}


create_query_views <- function(config){
  #' @description
  #' A function to create query views for re-purposing the queries
  #' 
  #' @param config loaded config yaml object
  #' 
  #' @example
  #' # create_query_views(config)
  #' 
  #' @retun None
  
  execute_query(config, prev_table_view)
  execute_query(config, address_county_view)
  execute_query(config, qof_practiceid_view)
  execute_query(config, base_table_view)
  execute_query(config, base_table_view0)
  execute_query(config, cat_table_view)
  display_logs("Created SQL views.", "info")
}


wait_for_user_input <- function(){
  #' @description
  #' A function to wait for the user to put the input
  #' 
  #' @param None
  #' 
  #' 
  #' @example
  #' # wait_for_user_input()
  #' 
  #' 
  #' @retun None
  
  invisible(readline(prompt="Press [enter] to continue"))
}


create_question_folders <- function(){
  #' @description
  #' A function to create folders during set up
  #' 
  #' @param None
  #' 
  #' 
  #' @example
  #' # create_question_folders()
  #' 
  #' 
  #' @retun None
  
  folder_list <- c('question1', 'question2', 'question3', 'question4', 
                   'question5')
  output_path <- './output'
  custom_create_dir(file_path = output_path)
  for (folder in folder_list){
    custom_create_dir(file_path = paste(output_path,'/',folder,sep=""))
  }
}




