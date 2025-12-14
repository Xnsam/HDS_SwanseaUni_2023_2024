# Header -----------------------------------------------------------------------
# File name: survival_analysis.R
# Folder/Module name: code
# Version: v0.1
# Created Date: 2024-01-09
# Last Edited Date: 2024-01-09
# Editor/Creator: Akson Sam Varghese
# Description: Source file for survival analysis
# ------------------------------------------------------------------------------


# source -----------------------------------------------------------------------
source('code/common.r')


# Library ----------------------------------------------------------------------
library(tidyverse)
library(finalfit)
library(survival)
library(naniar)



# Functions --------------------------------------------------------------------
survan_load_data <- function(){
  #' @description 
  #' A function to load the data
  #' 
  #' 
  #' 
  #' 
  #' @example 
  #'  data <- load_data()
  #' 
  #' 
  #' @return dataset, in this case  the dataset used for survival analysis
  
  # load born in wales
  target_column <- "How are you planning to feed your baby?"
  data_path <- "data/wales_data/processed_born_in_wales_data_june_2022_survival.csv"
  data <- read_csv(data_path)
  
  # remove  the columns with no target variable data
  plt <- data %>%
    select(all_of(target_column)) %>%
    vis_miss()
  print(plt)
  # drop the rows with no target columns
  subset_df <- subset(data, !(is.na(data[[target_column]])))
  # check again
  plt <- subset_df %>%
    select(all_of(target_column)) %>%
    vis_miss()
  print(plt)
  # convert into correct date format
  subset_df$`Start time` <- as.Date(subset_df$`Start time`, format="%d-%m-%Y")
  # select the dataset with columns of interest
  # var_6, var_25, var_29 , var_5, var_12, var_27
  dataset <- subset_df %>%
    select(
      `SYSTEM_ID`,
      `Start time`, `Expected date of delivery of your baby`, 
      `How are you planning to feed your baby?`,
      `Do you work`, `Are you currently working?`,
      `What is your current relationship status ?`,
      `My weight (before pregnancy) in Kg is:`,  # var_6
      `Do you smoke?`, # var_25
      `What type of maternity care are you receiving now?`, # var_29
      `Being so restless that it is hard to sit still?`, # var_12
      `How many people live in your home (not including you)?`, # var_5
      `Feeling nervous, anxious or on edge?` # var_27
    )
  
  # load survival analysis
  data_path <- "data/wales_data/survival_data.csv"
  survival_data <- read_csv(data_path)
  survival_data <- subset(survival_data, 
                          !(is.na(survival_data[['Started breastfeeding']])))
  
  dataset <- survival_data %>%
    left_join(dataset, by=c("STUDY_ID"="SYSTEM_ID"), keep=FALSE)
  
  
  data_columns <- colnames(dataset)
  
  dataset <- dataset %>%
     select(-STUDY_ID)
  #  select(-SYSTEM_ID)
  
  plt <- dataset %>%
    vis_miss()
  print(plt)
  
  
  return(dataset)
}


assign_encoding_intention <- function(text){
  #' @description 
  #' A function to assign encoding as per value
  #' 
  #' 
  #' @param val value to be looked up
  #' 
  #' 
  #' @example 
  #'  result <- lapply(
  #'         subset_df[[column_name]], assign_encoding, 
  #'         list_mapping = list_mapping)
  #' 
  #' 
  #' @return  encoded value
  
  if(grepl("Breast milk only", text)){
    return(1)
  } else {
    return(0)
  }
}

survan_preprocess <- function(dataset){
  #' @description 
  #' A function to run preprocess dataset 
  #' Transform and encode the dataset columns of choice
  #' 
  #' 
  #' 
  #' @example 
  #'  run_survival_analysis()
  #' 
  #' 
  #' @return processed_data, 
  
  # ----------------------------------------------------------------------------
  # Step 1: convert the date to number of days for survival time analysis
  dataset <- dataset %>%
    mutate(time_days=`Stopped breastfeeding` - `Started breastfeeding`)
  
  dataset[["time_days"]] <- as.numeric(dataset[["time_days"]])
  dataset[["status"]] <- lapply(
    dataset[['Stopped breastfeeding']],
    function(x) ifelse(is.na(x), 0, 1)
  )
  dataset <- unnest(dataset, "status")
  # ----------------------------------------------------------------------------
  
  
  # ----------------------------------------------------------------------------
  # Step 2: Encode `How are you planning to feed your baby?`
  column_name <- "How are you planning to feed your baby?"
  dataset[[column_name]] <- lapply(dataset[[column_name]], 
                                   assign_encoding_intention)
  dataset <- unnest(dataset, column_name)
  
  idx <- which(colnames(dataset) == column_name)
  names(dataset)[idx] <- "intention"
  # ----------------------------------------------------------------------------
  
  # ----------------------------------------------------------------------------
  # Step 3: Encode `Do you work`
  column_name <- "Do you work"
  dataset[[column_name]] <- lapply(dataset[[column_name]],
                                   assign_encoding,
                                   list_mapping=list_mapping_doyouwork)
  dataset <- unnest(dataset, column_name)
  idx <- which(colnames(dataset) == column_name)
  names(dataset)[idx] <- "work_type"
  # ----------------------------------------------------------------------------
  
  # ----------------------------------------------------------------------------
  # Step 4: Encode `Are you currently working?`
  column_name <- "Are you currently working?"
  dataset[[column_name]] <- lapply(dataset[[column_name]],
                                   assign_encoding4)
  dataset <- unnest(dataset, column_name)
  dataset[[column_name]] <- as.factor(dataset[[column_name]])
  idx <- which(colnames(dataset) == column_name)
  names(dataset)[idx] <- "work_status"
  # ----------------------------------------------------------------------------
  
  # ----------------------------------------------------------------------------
  # Step 5: Encode `What is your current relationship status ?`
  column_name <- "What is your current relationship status ?"
  dataset[[column_name]] <- lapply(dataset[[column_name]], 
                                   assign_encoding,
                                   list_mapping=list_mapping_relation)
  dataset <- unnest(dataset, column_name)
  dataset[[column_name]] <- as.factor(dataset[[column_name]])
  idx <- which(colnames(dataset) == column_name)
  names(dataset)[idx] <- "relationship"
  # ----------------------------------------------------------------------------
  
  # ----------------------------------------------------------------------------
  # Step 6: Encode `My weight (before pregnancy) in Kg is:`
  column_name <- "My weight (before pregnancy) in Kg is:"
  dataset <- convert_wt_to_kg(dataset, column_name)
  idx <- which(colnames(dataset) == column_name)
  names(dataset)[idx] <- "weight"
  # ----------------------------------------------------------------------------
  
  # ----------------------------------------------------------------------------
  # Step 7: Encode `Do you smoke?`
  column_name <- "Do you smoke?"
  list_mapping_smoke
  dataset[[column_name]] <- lapply(dataset[[column_name]], 
                    assign_encoding,
                    list_mapping=list_mapping_smoke)
  dataset <- unnest(dataset, column_name)
  idx <- which(colnames(dataset) == column_name)
  names(dataset)[idx] <- "smoke"
  # ----------------------------------------------------------------------------
  
  # ----------------------------------------------------------------------------
  # Step 8: Encode `What type of maternity care are you receiving now?`
  column_name <- "What type of maternity care are you receiving now?"
  dataset[[column_name]] <- lapply(dataset[[column_name]], normalize_text)
  dataset[[column_name]] <- lapply(dataset[[column_name]], 
                                   assign_encoding,
                                   list_mapping=list_mapping_care)
  dataset <- unnest(dataset, column_name)
  dataset[[column_name]] <- as.factor(dataset[[column_name]])
  idx <- which(colnames(dataset) == column_name)
  names(dataset)[idx] <- "maternitycare"
  # ----------------------------------------------------------------------------
  
  # ----------------------------------------------------------------------------
  # Step 9: Encode `Being so restless that it is hard to sit still?` 
  column_name <- "Being so restless that it is hard to sit still?"
  dataset[[column_name]] <- lapply(dataset[[column_name]], 
                                   assign_encoding,
                                   list_mapping=list_mapping_days)
  dataset <- unnest(dataset, column_name)
  idx <- which(colnames(dataset) == column_name)
  names(dataset)[idx] <- "restless"
  # ----------------------------------------------------------------------------
  
  # ----------------------------------------------------------------------------
  # Step 10: Encode `How many people live in your home (not including you)?`
  column_name <- "How many people live in your home (not including you)?"
  dataset[[column_name]] <- as.numeric(dataset[[column_name]])
  dataset <- unnest(dataset, column_name)
  idx <- which(colnames(dataset) == column_name)
  names(dataset)[idx] <- "family"
  # ----------------------------------------------------------------------------
  
  # ----------------------------------------------------------------------------
  # Step 11: Encode `Feeling nervous, anxious or on edge?`
  column_name <- "Feeling nervous, anxious or on edge?"
  dataset[[column_name]] <- lapply(dataset[[column_name]], 
                                   assign_encoding,
                                   list_mapping=list_mapping_days)
  dataset <- unnest(dataset, column_name)
  idx <- which(colnames(dataset) == column_name)
  names(dataset)[idx] <- "anxiety"
  # ----------------------------------------------------------------------------
  
  return(dataset)
}


plot_kaplan_meier_plot <- function(dataset){
  #' @description 
  #' A function to plot kaplan meier plot
  #' 
  #' @param dataset 
  #' 
  #' 
  #' @example 
  #'  plot_kaplan_meier_plot()
  #' 
  #' @return None
  
  
  ggplot(data=dataset, aes(x=`Stopped breastfeeding`)) +
    geom_histogram(stat="bin", fill="blue", alpha=0.3) +
    geom_histogram(data=dataset, aes(x=`Started breastfeeding`), stat="bin", 
                   fill="green", alpha=0.3) +
    labs(title="histogram plot", x="Start / Stop Feeding", y="count")
  
  
  survival_object <- dataset %$%
    Surv(time_days, status)
  
  # Overall survival in whole cohort
  my_survfit <- survfit(survival_object ~ 1, data = dataset)
  my_survfit # 205 patients, 71 events
  
  dependent_os <- "Surv(time_days/(365/12), status)"
  explanatory  <- c("intention")
  
  plt <- dataset %>% 
    surv_plot(dependent_os, explanatory, pval = TRUE, 
              surv.median.line = "hv", xlab="Time (months)",
              title="Kaplan-Meier Survival Analysis")
  
  print(plt)
  
}



run_cox_regression <- function(dataset){
  #' @description 
  #' A function to run cox regression and plot the hazard ratio plot
  #' 
  #' 
  #' 
  #' 
  #' @example 
  #'  run_cox_regression()
  #' 
  #' 
  #' @return None
  
  tmp_data <- dataset %>%
    select(time_days, status, work_type, work_status, 
           relationship, smoke, restless, 
           weight, maternitycare, family, anxiety, intention)
  
  dependent_os <- "Surv(time_days, status)"
  explanatory <- c(
    "work_type", "work_status", "relationship", "smoke", "restless", "weight", 
    "maternitycare", "family", "anxiety"
  )
  
  
  result <- tmp_data %>%
    finalfit(dependent_os, explanatory, add_dependent_label = FALSE) %>%
    rename("Overall survival" = label) %>% 
    rename(" " = levels) %>% 
    rename("  " = all)
  
  print(result)
  
  
  plt <- tmp_data %>%
    hr_plot(dependent_os, explanatory)
  
  print(plt)
}


# Handler ----------------------------------------------------------------------

run_survival_analysis <- function(){
  #' @description 
  #' A function to run survival analysis
  #' 
  #' 
  #' 
  #' 
  #' @example 
  #'  run_survival_analysis()
  #' 
  #' 
  #' @return dataset, in this case  the dataset used for survival analysis
  
  # Step 1: load the data
  # merge the data based on the start time column for survival dataset
  # and processed born in wales dataset
  dataset <- survan_load_data()
  
  # step 2: preprocess the columns of interest
  dataset <- survan_preprocess(dataset)
  
  # step 3: plot kaplan meier plot
  plot_kaplan_meier_plot(dataset)
  
  # step 4: cox regression - time - status - relationship - work hours - all
  # the factors significant from the logistic regression model
  run_cox_regression(dataset) 
}





