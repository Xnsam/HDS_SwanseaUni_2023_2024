# Header ------------------------------------------------------------------
# File name: question1.R
# Folder/Module name: code
# Version: v0.1
# Created Date: 2023-10-29
# Last Edited Date: 2023-11-25
# Editor/Creator: Akson Sam Varghese
# Description: Source file for implementing question 1 from assignment


# Sources -----------------------------------------------------------------
source('code/custom_utils.R')    # Custom utility functions


# Functions ---------------------------------------------------------------

get_health_board_and_gp <- function(config){
  #' @description
  #'  A function to get the list of health board and number of gp practices 
  #'  registered under the HB
  #' 
  #' @param config loaded config yaml object
  #' 
  #' @example 
  #' # get_health_board_and_gp(config)
  #' 
  #' @return list of health boards
  
  # NOTE:
  # The query only extracts data from 2015 as in the second part of the 
  # question, requires to calculate patient counts, Patient counts are only
  # available in the field4 of the qof_achievement table which is limited to
  # year 2015. It only makes sense to put consistent results from one time 
  # period.
  
  query <- sprintf("
  SELECT    gp_sample.hb as health_board_name,
            COUNT(distinct gp_sample.practiceid) AS number_of_gp
  FROM      (SELECT    gdut.hb,
                       gdut.practiceid
             FROM      gp_data_up_to_2015 AS gdut
             WHERE     CAST(gdut.period / 100 AS integer) = 2015
             LIMIT     %s
             ) AS gp_sample
  GROUP BY  gp_sample.hb
  ", config$database$data_limit)
  df <- execute_query(config, query)
  return (df)
}


get_registered_patients_count <- function(config, practice_list){
  #' @description
  #' A function to get the registered patients count as per the practice
  #' under a health board
  #' 
  #' 
  #' @param config loaded config yaml object
  #' @param practice_list list of the practices in HB
  #' 
  #' @example
  #' # get_registered_patients_count(config, practice_list)
  #' 
  #' @retun count of registered patients
  
  # ASSUMPTION -----------------------------------------
  # Decision made on research that, by definition, denominator is the number of 
  # patients registered for a QOF in disease register.
  # A patient could be registered in multiple disease register, so the sum
  # could be inflated. The number of patients could change from year to year, 
  # either increase or decrease. 
  # 
  # So the Assumptions are 
  #     1. The disease register is unique and does not contain overlapping
  #        patients ( likely to happen )
  #     2. The number of patients is limited to year 2015
  #     3. For some indicators, there is no disease register, example 
  #        Cervical Screening, so the count is limited to what data is available
  # ----------------------------------------------------
  
  query <- sprintf(
    "SELECT    SUM(qa.field4) AS patient_count
     FROM      qof_achievement AS qa
     WHERE     qa.orgcode IN (%s)
    ",
    paste0("'",practice_list,"'", collapse=","))
  
  
  df <- execute_query(config, query)
  return (df$patient_count)
}


get_qof_ind_report_count <- function(config, practice_list){
  #' @description
  #' A function to get the average QOF indicator report count
  #' in the practices under a health board
  #' 
  #' 
  #' @param config loaded config yaml object
  #' @param practice_list list of the practices in HB
  #' 
  #' @example 
  #' # get_qof_ind_report_count(config, practice_list)
  #' 
  #' 
  #' @retun average count of the QOF indicator
  
  
  # *** DECISION AND ASSUMPTION *** 
  # From research, it is found that the QOF indicators are introduced in a
  # particular year. In the DB there is only information of qof_achievement
  # for the year 2015
  # 
  # So the Assumptions are 
  #     1. The qof indicator count limits only year 2015
  
  query <- sprintf(
    "SELECT    AVG(practice_tbl.count) as average_count
     FROM      (SELECT     qa.indicator,
	                         COUNT(qa.indicator)
		            FROM       qof_achievement AS qa  
		            WHERE
			              qa.orgcode IN (%s)
			          GROUP BY
			              qa.indicator
			          ) AS practice_tbl",
    paste0("'",practice_list,"'",collapse=",")
  )
  
  df <- execute_query(config, query)
  return (df$average_count)
}


get_avg_spend <- function(config){
  #' @description
  #' A function to get the average spend in the practices under a health board
  #'  
  #' 
  #' @param config loaded config yaml object
  #' 
  #' @example
  #' # get_avg_spend(config)
  #' 
  #' 
  #' @retun average spend
  
  query <- sprintf("
      SELECT    spend_per_practice_tbl.hb,
                AVG(spend_per_practice_tbl.avg_spend_per_practice) 
                  AS avg_spend_per_hb
      FROM      (SELECT    gp2015_sample.hb,
	                         gp2015_sample.practiceid,
	                         AVG(actcost) AS avg_spend_per_practice
	               FROM      (SELECT    gdut.hb,
	                                    gdut.actcost,
	                                    gdut.practiceid
	                           FROM     gp_data_up_to_2015 AS gdut
	                           WHERE    CAST(gdut.period / 100 AS integer) = 2015
	                           LIMIT    %s
	                         ) as gp2015_sample
	               GROUP BY  gp2015_sample.hb, gp2015_sample.practiceid
	              ) AS spend_per_practice_tbl
	    GROUP BY  spend_per_practice_tbl.hb;
  ", config$database$data_limit)
  avg_spend <- execute_query(config, query)
  return (avg_spend)
}


# Handler -----------------------------------------------------------------

#. timer
run_question_1 <- function(config){
  #' @description
  #' A function to implement the Question 1
  #' Function execution time: 6-9 secs on default limit 
  #' 
  #' @param config loaded config yaml object
  #' 
  #' @example
  #' #  run_question_1(config)
  #' 
  #' @return None
  
  display_logs(" -------- Running Question 1", "info")
  display_logs("Estimated function execution time: 6-9 secs on default limit ", 
               "info")
  display_logs("Preparing Table ...", "process")
  # get list of unique Health Board (HB)
  tryCatch(
    expr = {
      hb_data <- get_health_board_and_gp(config)
    },
    error = function(e){
      display_txt <- "
      Failed to get health board data, please check if DB is correct
      "
      display_logs(display_txt, "debug")
      stop()
    }
  )
  
  hb_registered_count <- c()
  qof_count <- c()
  for (row in 1:nrow(hb_data)){
    hb <- hb_data[row, "health_board_name"]
    
    # get the list of practice list
    tryCatch(
      expr = {
        practice_list <- get_practice_list_from_hb(config, hb)
        practice_list <- practice_list$practiceid
      },
      error = function(e){
        display_txt <- " Failed to get practice list "
        display_logs(display_txt, "debug")
        stop()
      }
    )
    
    # get total number of registered patients in all practices w.r.t HB
    tryCatch(
      expr = {
        count <- get_registered_patients_count(config, practice_list)
        hb_registered_count <- c(hb_registered_count, count)
      },
      error = function(e){
        display_txt <- "
        failed to get total number of registered patients in all 
        practices w.r.t HB
        "
        display_logs(display_txt, "debug")
        stop()
      }
    )
    
    # get avg number of QOF indicators reported for all practices in HB
    tryCatch(
      expr = {
        count <- get_qof_ind_report_count(config, practice_list)
        qof_count <- c(qof_count, count)
      },
      error = function(e){
        display_txt <-  "
        failed toget avg number of QOF indicators reported for 
        all practices in HB
        "
        display_logs(display_txt, "debug")
        stop()
      }
    )
  }
  
  # get average spend per practice per month on medications in HB
  tryCatch(
    expr = {
      avg_spend_data <- get_avg_spend(config)
    },
    error = function(e){
      display_txt <- "
      failed to get average spend per practice per month on medications in HB
      "
      display_logs(display_txt, "debug")
      stop()
    }
  )
  
  # add to the table
  tryCatch(
    expr = {
      hb_data <- cbind(hb_data, patient_count = hb_registered_count)
      hb_data <- cbind(hb_data, average_qof_count = qof_count)
      
      hb_data <- merge(
        hb_data, avg_spend_data, by.x="health_board_name", by.y="hb", 
        all.x=TRUE)
      
      hb_data <- hb_data %>%
        rename(average_spend_per_hb=avg_spend_per_hb)
      
      cols <- c('average_qof_count', 'average_spend_per_hb')
      hb_data[,cols] <- round(hb_data[,cols], digits = 4)
    },
    error = function(e){
      display_txt <- "
      failed to add to table
      "
      display_logs(display_txt, "debug")
      stop()
    }
  )
  
  # display table
  View(hb_data)
  print(paste('Result Data Period : 2015 | Data Limit :', 
              config$database$data_limit))
  display_logs("Question 1 complete", "success")
  print("-----------------------------------------------------------")
}
