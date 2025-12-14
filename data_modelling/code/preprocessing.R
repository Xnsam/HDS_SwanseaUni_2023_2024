# Header -----------------------------------------------------------------------
# File name: preprocessing.R
# Folder/Module name: code
# Version: v0.1
# Created Date: 2024-01-09
# Last Edited Date: 2024-01-09
# Editor/Creator: Akson Sam Varghese
# Description: Source file for creating the processed file for modelling
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
source('code/common.R')


# Library ----------------------------------------------------------------------
library(tidyverse)
library(naniar)
library(tidytext)
library(mice)

set.seed(143)

# Functions --------------------------------------------------------------------

assign_encoding2 <- function(val){
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
  
  if(is.na(val)){
    return(NA)
  } else {
    simple_text <- normalize_text(val)
    if(grepl("not sure", simple_text) | grepl("not decided", simple_text) | 
       grepl("dont", simple_text)
    ){
      return(2)
    } else if(grepl("no", simple_text)){
      return(0)
    } else if(grepl("yes", simple_text)){
      return(1)
    } else {
      return(-1)
    }
  }
}

assign_encoding3 <- function(val){
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
  
  if(is.na(val)){
    return(NA)
  } else {
    simple_text <- normalize_text(val)
    if(grepl("work sitting", simple_text)){
      return(2)
    } else if(grepl("definite physical effort", simple_text) | 
              grepl("work standing", simple_text)){
      return(3)
    } else if(grepl("retired", simple_text) | grepl("unemployed", simple_text) |
              grepl("not in employment", simple_text)){
      return(1)
    } else if(grepl("vigorous physical", simple_text)){
      return(4)
    } else {
      return(-1)
    }
  }
}


check_variable <- function(
    subset_df, final_data, column_name, list_mapping=NULL, encoding_fxn=NULL){
  #' @description 
  #' A function to check work status column
  #' 
  #' 
  #' @param subset_df data frame to work on
  #' @param final_data data frame to be modified
  #' @param column_name name of the column
  #' @param list_mapping mapping of the list
  #' @param encoding_fxn encoding function to use
  #' 
  #' 
  #' @example 
  #'  final_data <- check_variable(
  #'          subset_df, final_data, column_name, list_mapping, encoding_fxn)
  #' 
  #' 
  #' @return final_data modified data frame
  data_preview(final_data, column_name)
  if(is.null(list_mapping) == TRUE){
    final_data[[column_name]] <- lapply(
      subset_df[[column_name]], encoding_fxn)
  } else {
    final_data[[column_name]] <- lapply(
      subset_df[[column_name]], encoding_fxn, list_mapping = list_mapping)
  }
  
  final_data <- final_touch(final_data, column_name)
  return(final_data)
}


check_variable_missingness <- function(subset_df){
  #' @description 
  #' A function to check missingness is all columns
  #' 
  #' 
  #' @param subset_df data frame to work on
  #' 
  #' 
  #' @example 
  #'  result <- check_variable_missingness(subset_df)
  #' 
  #' 
  #' @return subset_df filtered data frame
  
  na_count <- miss_var_summary(subset_df)
  # > 40 - 32
  # < 40 - 1
  # < 30 - 3
  # < 20 - 2
  # < 10 - 30
  for(val in c(40, 40, 30, 20, 10)){
    print(paste("percent ", val))
    print(dim(na_count %>% filter(pct_miss < val))[1])
  }
  
  # create a variable list with only < 40% of the missing-ness
  variable_list <- na_count %>%
    filter(pct_miss < 40) %>%
    select(variable)
  
  subset_df <- subset_df %>%
    select(variable_list$variable)
  head(subset_df)
  
  variable_list <- colnames(subset_df)
  final_data <- as_tibble(subset_df)
  result <- list(
    final_data=final_data,
    variable_list=variable_list
  )
  return(result)
}


check_occupation <- function(
    subset_df, final_data, column_name, target_column){
  #' @description 
  #' A function to check occupation column
  #' 
  #' 
  #' @param subset_df data frame to work on
  #' @param final_data data frame to be modified
  #' @param column_name name of the columnt 
  #' @param target_column name of the target column
  #' 
  #' 
  #' @example 
  #'  final_data <- check_occupation(
  #'     subset_df, final_data, column_name, target_column)
  #' 
  #' 
  #' @return final_data modified dataframe
  
  data_preview(final_data, column_name)
  
  # create model data frame
  model_data <- final_data %>%
    select(column_name, target_column)
  # convert to factor
  model_data[[column_name]] <- as.factor(model_data[[column_name]])
  names(model_data)[1] <- "X"
  names(model_data)[2] <- "Y"
  model_data <- subset(model_data, !(is.na(model_data$X)))
  
  # logistic regression model
  log_reg_model <- glm(Y ~ X, data=model_data, family=binomial) 
  
  # assign the residuals to the group
  grp_number <- round(sqrt(length(unique(model_data$X))))
  res_data <- model_data %>%
    mutate(res_id=residuals(log_reg_model))  %>%
    group_by(X) %>%
    summarize(med_res_id=median(res_id), cnt=n())  %>%
    arrange(med_res_id) %>%
    mutate(cum_cnt=cumsum(cnt), name_group=ntile(cum_cnt, grp_number))
  
  assign_res_encoding <- function(text, res_data){
    if(is.na(text)){
      return(NA)
    }
    
    tmp_df <- res_data[which(res_data$X == text),]
    if(dim(tmp_df)[1] > 0){
      name_group <- tmp_df$name_group
      return(name_group)
    } else {
      return(NA)
    }
  }
  
  final_data[[column_name]] <- lapply(
    subset_df[[column_name]], assign_res_encoding, res_data=res_data)
  
  final_data <- final_touch(final_data, column_name)
  
  return(final_data)
}

check_occupation_catboost <- function(
    subset_df, final_data, column_name, target_column){
  #' @description 
  #' A function to check occupation column target encoding using catboost
  #' 
  #' 
  #' @param subset_df data frame to work on
  #' @param final_data data frame to be modified
  #' @param column_name name of the columnt 
  #' @param target_column name of the target column
  #' 
  #' 
  #' @example 
  #'  final_data <- check_occupation(
  #'     subset_df, final_data, column_name, target_column)
  
  
  data_preview(final_data, column_name)
  
  # create model data frame
  model_data <- final_data %>%
    select(column_name, target_column)
  # convert to factor
  model_data[[column_name]] <- as.factor(model_data[[column_name]])
  names(model_data)[1] <- "X"
  names(model_data)[2] <- "Y"
  model_data <- subset(model_data, !(is.na(model_data$X)))
  
  
}

perform_imputation <- function(processed_file_path){
  #' @description 
  #' A function to implement the mice approach
  #' 
  #' 
  #' @param processed_file_path file path to load data and work on.
  #' 
  #' 
  #' @example 
  #'  result <- perform_imputation(processed_file_path)
  #' 
  #' 
  #' @return result imputed data
  
  # load the data
  processed_file <- read_csv(processed_file_path)
  
  # create reference columns for convenient column names and data frame access
  col_names <- data.frame(name_1=c(), name_2=c())
  for(idx in 1:length(colnames(processed_file))){
    old_name <- colnames(processed_file)[[idx]]
    new_name <- paste("var_", idx, sep="")
    names(processed_file)[idx] <- new_name
    
    colname <- data.frame(name_1=old_name, name_2=new_name)
    col_names <- rbind(col_names, colname)
  }
  
  
  # visualize missing values in the processed file
  plt <- processed_file %>%
    vis_miss()
  print(plt)
  
  # impute
  df_imputed <- mice(processed_file)
  df_complete <- complete(df_imputed)
  
  # visualize the missing values in the inputed data frame
  plt <- df_complete %>%
    vis_miss()
  print(plt)
  
  result <- list(
    imputed_data=df_complete,
    col_names_df=col_names
  )
  
  return(result)
}


create_bmi_column <- function(df_imputed, col_names_df){
  #' @description 
  #' A function to create bmi column
  #' 
  #' 
  #' @param df_imputed data frame to be imputed
  #' @param col_names_df column names data frame
  #' 
  #' 
  #' @example 
  #'  result <- create_bmi_column(df_imputed, col_names_df)
  #' 
  #' 
  #' @return result
  
  # create BMI column
  # var_6 : My weight (before pregnancy) in Kg is:
  column_name1 <- col_names_df[
    which(col_names_df$name_1 == 'My weight (before pregnancy) in Kg is:'),]
  
  column_name1 <- column_name1$name_2
  
  # var_7 : My height in centimetres is :
  column_name2 <- col_names_df[
    which(col_names_df$name_1 == 'My height in centimetres is :'),]
  column_name2 <- column_name2$name_2
  
  # calculate bmi
  # wt / ht^2
  df_imputed <- df_imputed %>%
    mutate(bmi=df_imputed[[column_name1]] / (df_imputed[[column_name2]] ** 2))
  
  head(df_imputed['bmi'])
  
  idx <- dim(col_names_df)[1] + 1
  var_name <- paste('var_', idx, sep="")
  
  names(df_imputed)[idx] <- var_name
  col_names_df <- rbind(col_names_df, 
                        data.frame(name_1="bmi", name_2=var_name))
  
  result <- list(
    df_imputed=df_imputed,
    col_names_df=col_names_df
  )
  
  return(result)
}


convert_ht_to_mtrs <- function(data, col_name){
  #' @description 
  #' A function to ht to metres
  #' 
  #' @param data data to work on
  #' @param col_name data to use col_name
  #' 
  #' 
  #' @example 
  #'  result <- convert_ht_to_mtrs(data, col_name)
  #' 
  #' 
  #' @return data, modified data
  
  data[[col_name]] <- round(data[[col_name]] / 100, 2)
  return(data)
}

perform_outlier_treatment <- function(processed_file_path){
  #' @description 
  #' A function to perform outlier treatment
  #' 
  #' 
  #' @param processed_file_path file path to load data and work on.
  #' 
  #' 
  #' @example 
  #'  result <- perform_outlier_treatment(processed_file_path)
  #' 
  #' 
  #' @return none, write back the outlier treated path
  
  # load data
  data <- read_csv(processed_file_path)
  
  simplify <- function(x){
    qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
    caps <- quantile(x, probs=c(.05, .95), na.rm = T)
    H <- 1.5 * IQR(x, na.rm = T)
    x[x < (qnt[1] - H)] <- caps[1]
    x[x > (qnt[2] + H)] <- caps[2]
    
    return(x)
  }
  
  column_name <- "How many people live in your home (not including you)?"
  data[[column_name]] <- lapply(
    data[[column_name]], function(x) ifelse(x > 10, NA, x)
  )
  data <- unnest(data, column_name)
  
  column_name <- "How would you describe your experience of this pregnancy (support from midwife, how you feel about being pregnant)?"
  data[[column_name]] <- simplify(data[[column_name]])
  
  write.csv(data, processed_file_path, row.names=FALSE)
}

recheck_working_columns <- function(imputed_data){
  #' @description 
  #' A function recheck working columns for consistency
  #' 
  #' @param imputed_data data to be modified
  #' 
  #' 
  #' @example 
  #'  result <- recheck_working_columns(imputed_data)
  #' 
  #' 
  #' @return data, modified data
  
  idx <- which(imputed_data$var_30 == 0)
  imputed_data$var_1[idx] <- 0
  
  return(imputed_data)
}

create_processed_file <- function(){
  #' @description 
  #' A workflow function to check each variable and create a processed file
  #' 
  #' 
  #' @param subset_df data frame to work on
  #' @param final_data data frame to be modified
  #' @param column_name name of the column 
  #' 
  #' 
  #' @example 
  #'  result <- create_processed_file()
  #' 
  #' 
  #' @return result, list of final data and columns categorized in columns

  # GLOBAL VARIABLES -----------------------------------------------------------
  nominal_columns <- c()
  ordinal_columns <- c()
  numeric_columns <- c()
  
  
  data_path <- "data/wales_data/processed_born_in_wales_data_june_2022.csv"
  target_column <- "How are you planning to feed your baby?"
  
  # ----------------------------------------------------------------------------

  
  # check for missing values in target_column ----------------------------------
  subset_data <- check_missing_target_column(data_path, target_column)
  # ----------------------------------------------------------------------------
  
  # checking all columns for missing data --------------------------------------
  result <- check_variable_missingness(subset_data)
  final_data <- result$final_data
  variable_list <- result$variable_list
  # ----------------------------------------------------------------------------
  
  # tidy each column in the data -----------------------------------------------
  column_name <- variable_list[1]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping = list_mapping_doyouwork,
    encoding_fxn = assign_encoding)
  
  # ---------------------------------------------
  # Variable : "Have you had any periods of bad stress in your pregnancy ?"
  
  column_name <- variable_list[2]
  nominal_columns <- c(nominal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping = list_mapping_binary,
    encoding_fxn = assign_encoding)
  
  # ---------------------------------------------
  # Variable : "Has COVID19 change the type of birth you feel you will have?"
  
  column_name <- variable_list[3]
  nominal_columns <- c(nominal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    encoding_fxn=assign_encoding2)
  
  
  # ---------------------------------------------
  # Variable : "Would you consider yourself to be:" 
  
  column_name <- variable_list[4]
  nominal_columns <- c(nominal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping = list_mapping_genderorientation,
    encoding_fxn = assign_encoding)
  
  
  # ---------------------------------------------
  # Variable : "How many people live in your home (not including you)?"
  
  column_name <- variable_list[5]
  numeric_columns <- c(numeric_columns, column_name)
  data_preview(final_data, column_name)
  
  
  # ---------------------------------------------
  # Variable : "My weight (before pregnancy) in Kg is:"
  
  column_name <- variable_list[6]
  numeric_columns <- c(numeric_columns, column_name)
  data_preview(final_data, column_name)
  final_data <- convert_wt_to_kg(
      final_data, column_name
  )
  
  
  # ---------------------------------------------
  # Variable : "My height in centimetres is :"
  
  column_name <- variable_list[7]
  numeric_columns <- c(numeric_columns, column_name)
  data_preview(final_data, column_name)
  final_data <- convert_ht_to_mtrs(
    final_data, column_name 
  )
  
  # ---------------------------------------------
  # Variable : "How would you describe your experience of this pregnancy 
  #            (support from midwife, how you feel about being pregnant)?"
  
  column_name <- variable_list[8]
  numeric_columns <- c(numeric_columns, column_name)
  final_data <- check_midwife_experience(subset_data, final_data, column_name)
  
  
  # ---------------------------------------------
  # Variable : "Trouble relaxing?"
  
  column_name <- variable_list[9]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_days,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "Gardening/DIY"

  column_name <- variable_list[10]
  ordinal_columns <- c(ordinal_columns, variable_list[10])
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_hours,
    encoding_fxn=assign_encoding)

  # ---------------------------------------------
  # Variable : "Cycling, including cycling to work and during leisure time"
  
  column_name <- variable_list[11]
  ordinal_columns <- c(ordinal_columns, variable_list[11])
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_hours,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "Being so restless that it is hard to sit still?"
  
  column_name <- variable_list[12]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_days,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "Feeling afraid as if something awful might happen?"
  
  column_name <- variable_list[13]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_days,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "Becoming easily annoyed or irritable?"
  
  column_name <- variable_list[14]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_days,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "Worrying too much about different things?"
  
  column_name <- variable_list[15]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_days,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "Not being able to stop or control worrying?"
  
  column_name <- variable_list[16]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_days,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "Housework/Childcare"
  
  column_name <- variable_list[17]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_hours,
    encoding_fxn=assign_encoding)

  
  # ---------------------------------------------
  # Variable : "Walking, including walking to work, shopping, for pleasure etc."
  
  column_name <- variable_list[18]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_hours,
    encoding_fxn=assign_encoding)
  
  
  # ---------------------------------------------
  # "Physical exercise such as swimming, jogging, aerobics, football, 
  #  tennis, gym workout etc."
  
  column_name <- variable_list[20]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_hours,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "I feel satisfied with the support and care I 
  # have received in my pregnancy from my health care team (1 is strongly 
  # disagree, 5 is strongly agree)"
  
  column_name <- variable_list[21]
  ordinal_columns <- c(ordinal_columns, column_name)
  data_preview(subset_data, column_name)
  
  # ---------------------------------------------
  # Variable : "Please tell us the type and amount of physical activity involved 
  #             in your work" 
  
  column_name <- variable_list[22]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    encoding_fxn=assign_encoding3)
  
  # ---------------------------------------------
  # Variable : "How would you describe your usual walking pace?  Please mark 
  #             one box only."
  
  column_name <- variable_list[23]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_pace,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "What is the highest level of education you have reached?"
  
  column_name <- variable_list[24]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_edu,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "Do you smoke?"
  
  column_name <- variable_list[25]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_smoke,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "Do you drink alcohol?"
  
  column_name <- variable_list[26]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_alcohol,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "Feeling nervous, anxious or on edge?"
  
  column_name <- variable_list[27]
  ordinal_columns <- c(ordinal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_days,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "What is the number that best describes your TOTAL household 
  #             income BEFORE TAX?"
  
  column_name <- variable_list[28]
  ordinal_columns <- c(ordinal_columns, column_name)
  subset_data[[column_name]] <- lapply(
    subset_data[[column_name]], normalize_text
  )
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_income,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "What type of maternity care are you receiving now?"
  
  column_name <- variable_list[29]
  nominal_columns <- c(nominal_columns, column_name)
  subset_data[[column_name]] <- lapply(
    subset_data[[column_name]], normalize_text
  )
  final_data <- check_variable(
    subset_data, final_data, column_name, 
    list_mapping=list_mapping_care,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "Are you currently working?" 
  
  column_name <- variable_list[30]
  nominal_columns <- c(nominal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name,
    encoding_fxn=assign_encoding4)
  
  # ---------------------------------------------
  # Variable : "What is your current relationship status ?" 
  
  column_name <- variable_list[31]
  nominal_columns <- c(nominal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name,
    list_mapping=list_mapping_relation,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "What is the main language spoken in your home?"
  
  column_name <- variable_list[32]
  nominal_columns <- c(nominal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name,
    list_mapping=list_mapping_lang,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "What is your ethnic group?"
  column_name <- variable_list[33]
  nominal_columns <- c(nominal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name,
    list_mapping=list_mapping_ethnicity,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "Nationality"
  column_name <- variable_list[34]
  nominal_columns <- c(nominal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name,
    list_mapping=list_mapping_nationality,
    encoding_fxn=assign_encoding)
  
  # ---------------------------------------------
  # Variable : "How are you planning to feed your baby?"
  
  column_name <- variable_list[35]
  final_data <- check_variable(
    subset_data, final_data, column_name,
    encoding_fxn=assign_encoding5)
  
  # ---------------------------------------------
  # Variable : "What is  your occupation ?"
  
  column_name <- variable_list[19]
  nominal_columns <- c(nominal_columns, column_name)
  final_data <- check_occupation(
    subset_data, final_data, column_name, target_column
  )
  
  # ---------------------------------------------
  # Variable :  "Do you have other children ?"
  
  column_name <- variable_list[36]
  nominal_columns <- c(nominal_columns, column_name)
  final_data <- check_variable(
    subset_data, final_data, column_name,
    list_mapping=list_mapping_binary,
    encoding_fxn=assign_encoding)
  
  result <- list(
    "final_data"=final_data,
    "nominal_columns"=nominal_columns,
    "ordinal_columns"=ordinal_columns,
    "numeric_columns"=numeric_columns
  )
  
  return(result)
}


# Handler ----------------------------------------------------------------------

run_preprocessing <- function(){
  #' @description 
  #' A function to run the preprocessing
  #' 
  #' 
  #' 
  #' 
  #' @example 
  #'    run_preprocessing()
  #' 
  #' 
  #' @return None
  
  # Step 1: Create a processed file by apply preprocessing / data cleaning by
  # manually inspecting and reviewing each column
  
  result <- create_processed_file()
  final_data <- result$final_data
  nominal_columns <- result$nominal_columns
  ordinal_columns <- result$ordinal_columns
  numeric_columns <- result$numeric_columns
  
  processed_file_path <- "data/wales_data/preprocessed_file.csv"
  write.csv(final_data, file=processed_file_path, row.names=FALSE)
  
  # Step 2: performing outlier treatment
  # performing outlier treatment before and after the imputation to ensure extreme
  # values are capped
  perform_outlier_treatment(processed_file_path)
  
  # Step 3: perform imputation on the missing values using MICE approach
  result <- perform_imputation(processed_file_path)
  imputed_data <- result$imputed_data
  col_names_df <- result$col_names_df
  col_names_mapping_path <- "data/wales_data/column_name_mapping.csv"
  write.csv(col_names_df, file=col_names_mapping_path, row.names=FALSE)
  
  # step 4: check working columns consistency
  # when doing a cross tab with are you currently working and do you work,
  # when former variable = 0 then the later variable should also be = 0
  # this is just to double check and ensure that, it is the case
  imputed_data <- recheck_working_columns(imputed_data)
  print(dim(imputed_data))

  # step 5: check the target variable ratio
  # check the proportion of target variable in the data
  # 'How are you planning to feed your baby?'
  column_name <- col_names_df[
    which(col_names_df$name_1 == 'How are you planning to feed your baby?'),]
  column_name <- column_name$name_2
  target_sample_ratio <- sum(imputed_data[column_name]) / dim(imputed_data)[1]
  display_logs(paste("target variable ratio", target_sample_ratio) ,"info")
  
  
  # step 6: create the bmi column
  # create a bmi column using the height and weight column
  result <- create_bmi_column(imputed_data, col_names_df)
  processed_file <- result$df_imputed
  column_name_mapping <- result$col_names_df
  

  # step 7: Add encoding type to the column_name_mapping
  # add a new column in the column mapping data
  data_type_list <- c()
  for(idx in 1:dim(column_name_mapping)[1]){
    row <- column_name_mapping[idx,]
    column_name <- row$name_1
    if(column_name %in% nominal_columns){
      data_type_list <- c(data_type_list, "nominal")
    } else if(column_name %in% ordinal_columns){
      data_type_list <- c(data_type_list, "ordinal")
    } else if(
      column_name %in% numeric_columns | column_name == "bmi"){
      data_type_list <- c(data_type_list, "numeric")
    } else if(column_name == "How are you planning to feed your baby?"){
      data_type_list <- c(data_type_list, "target")
    }
  }
  column_name_mapping$data_type_val <- data_type_list
  
  
  # Step 8: save the files 
  imputed_file_path <- "data/wales_data/processed_imputed_file.csv"
  write.csv(column_name_mapping, file=col_names_mapping_path, row.names=FALSE)
  write.csv(processed_file, file=imputed_file_path, row.names=FALSE)
  
  # Step 9: performing outlier treatment after the imputation to ensure extreme
  # values are capped
  # perform_outlier_treatment(imputed_file_path)
  
  display_logs("Completed running preprocessing", "info")
}



