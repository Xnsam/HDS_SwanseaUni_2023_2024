# Header -----------------------------------------------------------------------
# File name: app.R
# Folder/Module name: 
# Version: v0.1
# Created Date: 2024-01-09
# Last Edited Date: 2024-01-09
# Editor/Creator: Akson Sam Varghese
# Description: Source file for implementing the main application
# ------------------------------------------------------------------------------



# Source -----------------------------------------------------------------------
source('code/preprocessing.R')
source('code/demograph.R')
source('code/data_modelling.R')
# source('code/secondary_analysis.R')
source('code/survival_analysis.R')
source('code/posthoc_analysis.R')


main_app <- function(){
  #' @description 
  #' A main function to run the main app
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
 
  
  # step 1: run preprocessing and created a processed file 
  run_preprocessing()
  
  # step 2: display demographic data
  get_demograph_data()
  
  # step 3: create logistic regression base model
  run_initial_modelling()
  
  # step 4: select variables based on initial logistic regression, decision tree
  # and research
  # run_sec_analysis()
  # secondary analysis should be run by running the file as it has some 
  # technical issues associated to packages used in the code that does not
  # allow the file to be run in the function

  
  # step 6: Survival analysis
  run_survival_analysis()
  
  
  # step 7: post hoc analysis
  run_post_hoc_analysis()
  
  
}


main_app()

