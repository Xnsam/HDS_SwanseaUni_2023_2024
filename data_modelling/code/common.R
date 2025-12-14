# Header -----------------------------------------------------------------------
# File name: common.R
# Folder/Module name: code
# Version: v0.1
# Created Date: 2024-01-09
# Last Edited Date: 2024-01-09
# Editor/Creator: Akson Sam Varghese
# Description: Source file for implementing the data preprocessing
# ------------------------------------------------------------------------------


# Library ----------------------------------------------------------------------
library(tidyverse)
library(crayon)
library(utf8)
library(naniar)
library(stringr)
library(tidyverse)
library(tidytext)
library(mice)




# Source -----------------------------------------------------------------------



# Variables --------------------------------------------------------------------
list_mapping_doyouwork <- list(
  'Fixed regular hours part-time'=3,
  'Full time'=4,
  'Variable hours part-time'=3,
  'Usually full time but signed off with hyperemisis gravidarum currently'=3,
  'When required with no guarantee of hours'=3,
  'I choose my own hours'=2,
  'Self employed. Whenever I.l can'=2,
  '35 hours a week'=4,
  'Zero hour contract'=1
)
list_mapping_binary <- list(
  'No'=0,
  'Yes'=1,
  'Prefer not to say'=2
)
list_mapping_genderorientation <- list(
  'Heterosexual or straight?'=1,
  'Bisexual?'=2,
  'Homosexual or lesbian?'=3,
  'Prefer not to say'=4,
  'Pansexual'=5
)
list_mapping_days <- list(
  "Not at all"=1,
  "Several days"=2,
  "Nearly everyday"=4,
  "More than half the days"=3
)
list_mapping_hours <- list(
  "None"=1,
  "Some but less than 1 hour"=2,
  "1 hour but less than 3 hours"=3,
  "3 hours or more"=4
)
list_mapping_pace <- list(
  "Steady average pace"=2,
  "Brisk pace"=3,
  "Fast pace"=4,
  "Slow pace"=1
)  
# higher education - 4
# furthur education - 3
# secondary education - 2
# primary education - 1

list_mapping_edu <- list(
  "University higher degree"=4,
  "University degree"=4,
  "Higher national diploma"=4,
  "Exams at age 16 (GCSE or equivalent)"=2,
  "Exams at age 18 (A level or equivalent)"=2,
  "Vocational qualifications"=3,
  "None"=0,
  "Phd"=4,
  "PhD"=4,
  "Diploma"=4,
  "2 nvqs"=3,
  "PGCE"=4,
  "Dip he"=4,
  "NVQ via work"=3,
  "Masters"=4,
  "CACHE L5"=4, 
  "Currently studying degree"=4,
  "DipHE"=4, 
  "Doctorate"=4,
  "Post grad"=4,
  "Bachelor of Science in nursing. Qualified nurse in the Philippines and UK"=4,
  "Clinical doctorate"=4,
  "Diploma of Higher Education"=4,
  "Diploma of higher education"=4,
  "Currently studying Nursing Degree"=4,
  "Master?s Degree"=4
)

list_mapping_smoke <- list(
  "I used to, but I stopped before I was pregnant"=0,
  "No, never smoked"=0,
  "Yes I smoke, more than 5 cigarettes a week"=4,
  "I used to, but I stopped when I knew I was pregnant"=0,
  "Quit 5 years ago"=0,
  "I smoke e-cigarettes"=6,
  "Yes I smoke, less than 5 cigarettes a day"=5,
  "I have smoked but occasionally when younger."=0,
  "Used to casually smoke when drinking but not for many years"=0,
  "I smoke aroung 15 a day"=7,
  "I used to but I stopped when I knew I was pregnant"=2,
  "I used but I stopped before I was pregnant"=2,
  "Yes I smoke, less than 5 cigarettes a week"=3,
  "Stopped smoking 17 years ago."=0
)
list_mapping_alcohol <- list(
  "Yes, but I stopped before I was pregnant"=1,
  "Yes, I stopped as soon as I knew I was pregnant"=1,
  "No, I have never drunk alcohol"=1,
  "Very rarely before pregnancy"=1,
  "Yes, about once per week"=3,
  "Not for a few years"=1,
  "Yes, very occasionally now"=2,
  "Yes but maybe 2/3 a year"=2,
  "Occasionally before pregancy"=2,
  "Never during pregnant but never a massive drinker anway"=1,
  "Hardly drant prior to pregnancy. Had none during pregnancy"=1,
  "Yes, I stopped as soon as I knew I was pregant"=1,
  "Just on the odd night out, hardly anything"=2,
  "On very few occasions, but not since I have been pregnant"=1,
  "I only drink once or twice a year, I doesn't appeal to me but if I'm in the mood I would drink when not pregnant"=1,
  "Rarely, perhaps 3-4 times a year"=2,
  "Only on special occasions"=2,
  "Occasionally before pregnancy"=1,
  "No, have in the past"=1
)

list_mapping_income <- list(
  "between 40,000-49,999"=6,
  "between 20,000-29,999"=4,
  "50,000"=7,
  "prefer not to say"=0,
  "less than 10,000"=2,
  "between 30,000-39,999"=5,
  "between 10,000-19,999"=3,
  "200,000"=10,
  "1,50,000"=9,
  "perfer not to say"=0,
  "monthly less than 5k"=1,
  "both me and my husband are nurses earning 31,534 each before tax yearly"=8
)

list_mapping_care <- list(
  "midwife led care"=2,
  "consultant led care"=3,
  "i dont know yet"=1,
  "dont know yet"=1,
  "n/a"=1,
  "community midwife/health visitor"=4,
  "already have baby"=5,
  "health visitor"=4,
  "both midwife and consultant"=6,
  "awaiting decision based on previous birth"=7,
  "consultant on occassion but midwife led care"=2,
  "shared care"=6,
  "midwife and consultant"=6,
  "dual care plus fetal medicine care"=8,
  "but both parties believe the other should be caring"=6,
  "currently both will be confirmed which one leads at next scan"=6,
  "diabetic midwife led care" =2,
  "first appointment next week but will be consultant lead"=3,
  "early 5 weeks yet to meet midwife"=2,
  "was consultant care"=3,
  "rainbow clinic"=3,
  "not sure yet, i'm yet to have my 12 week scan"=1,
  "mainly midwife with consultant towards the end"=2,
  "both started with midwife, then referred to consultant and then discharged back to midwife"=6,
  "gave birth 10 months ago"=5,
  "both"=6,
  "none"=1,
  "too early not yet booked will be clc"=1,
  "consultant led but at 30 weeks yet to see a consultant"=3,
  "mixture of both"=6,
  "midwife and consultant led care" =6,
  "consultant led then midwife led in last weeks"=2,
  "midwife and present but meeting consultant in june"=2,
  "given birth already"=5,
  "not sure yet"=1
)

list_mapping_relation <- list(
  "Living with partner/civil partnership"=2,
  "Married"=3,
  "Dating"=4,
  "Single"=5,
  "Engaged"=6,
  "Separated"=7,
  "Prefer not to say"=1,
  "Partner- lives in army accommodation and at home when off"=8
)
list_mapping_lang <- list(
  "English"=1,
  "Swahili"=2,
  "Welsh"=3,
  "Italian"=4,
  "Spanish"=5,
  "Bilingual Welsh and English"=6,
  "Polish"=7,
  "Greek"=8,
  "Bilingual Wel/Eng"=6,
  "Portuguese"=9,
  "Waray waray"=10,
  "English and Welsh"=6,
  "Equal welsh/English"=6
)
list_mapping_ethnicity <- list(
  "White"=2,
  "Prefer not to say"=1,
  "Mixed"=3,
  "Asian"=4,
  "Chinese"=5
)
list_mapping_nationality <- list(
  "British"=2,
  "Prefer not to say"=1,
  "Welsh"=3,
  "European"=4,
  "American"=5,
  "Indian"=6,
  "Irish"=7,
  "New Zealand"=8,
  "Dual British and other nationality"=9,
  "Greek"=10,
  "Brazilian"=11,
  "Filipino"=12
)


# Functions --------------------------------------------------------------------


# A pair of functions that set-up a 1x1 and 2x2 plotting grid.
plot1x1 <- function(){return(par(mfrow=c(1,1)))}
plot2x2 <- function(){return(par(mfrow=c(2,2)))}

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



normalize_text <- function(text_value){
  #' @description 
  #' A function to normalize and clean the text
  #' 
  #' 
  #' @param text_value string value to clean
  #' 
  #' 
  #' @example 
  #'  cleaned_string <- normalize_text(string_to_clean)
  #' 
  #' 
  #' @return cleaned_string
  #' 
  if(is.na(text_value) | !(is.character(text_value))){
    return(text_value)
  }
  
  # replace all hex values
  text_value <- str_replace_all(text_value, "[(.\x9c.)|(.\xe5.)]", "")
  
  # decode all UTF8 encoding
  str_encoding <- Encoding(text_value)
  
  if(str_encoding != "unknown"){
    text_value <- iconv(text_value, from = str_encoding, to="ASCII//TRANSLIT")
  }
  
  # convert to lower
  text_value <- tolower(text_value)
  
  # Remove all special characters
  text_value <- gsub("[^0-9A-Za-z,-///' ]", "", text_value, ignore.case = TRUE)
  
  # trim the white spaces
  text_value <- str_trim(text_value)
  
  return(text_value)
}



filter_nan_values <- function(df, columns_mapping){
  #' @description 
  #' A function to filter NA values from the dataset based on the threshold
  #' 
  #' 
  #' @param df dataframe to work on
  #' 
  #' 
  #' @example 
  #'  filtered_dataset <- filter_nan_values(df)
  #' 
  #' 
  #' @return filtered_dataset 
  
  # check count of NA's
  tmp <- round((colSums(is.na(df)) / dim(df)[1] ) * 100, 2)
  
}



encode_ordinal <- function(x, order = unique(x)) {
  #' @description 
  #' A function to encode ordinal values
  #' 
  #' 
  #' @param x column to work on
  #' @param order order of the encoding to be considered
  #' 
  #' 
  #' @example 
  #'  encoded_column <- encode_ordinal(x, order=c("low", "medium", "high"))
  #' 
  #' 
  #' @return x encoded column
  
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  return(x)
}


encode_nominal <- function(df, cols){
  #' @description 
  #' A function to encode nominal values
  #' 
  #' 
  #' @param df the column to work on
  #' @param cols the columns to encode
  #' 
  #' 
  #' 
  #' @example 
  #'  df <- encode_nominal(x)
  #' 
  #' 
  #' @return df one hot encoded data frame 
  
  df <- dummy_cols(df, select_columns=cols, remove_selected_columns = TRUE,
                   ignore_na=TRUE)
  return(df)
}


convert_to_factors <- function(data, col_mapping){
  #' @description 
  #' A function to convert nominal columns to factors
  #' 
  #' 
  #' @param data data to be processed
  #' @param column_mapping mapping of column names
  #' 
  #' 
  #' @example 
  #'  data <- convert_to_factors(data, col_mapping)
  #' 
  #' 
  #' @return data data frame with modified columns
  
  sub_data <- col_mapping %>%
    filter(data_type_val == "nominal")
  
  for(column in sub_data$name_2){
    data[[column]] <- as.factor(data[[column]])
  }
  
  data <- data %>%
    mutate(across(where(is.factor), 
                  ~replace_na(., names(sort(-table(.)))[1]))) %>%
    mutate(across(where(is.numeric), ~replace_na(., median(., na.rm=TRUE))))
  
  return(data)
}


check_missing_target_column <- function(data_path, target_column){
  #' @description 
  #' A function to check missing values in target column
  #' 
  #' 
  #' @param data_path file path of the data
  #' @param target_column string value of the target column
  #' 
  #' 
  #' @example 
  #'  subset_df <- check_missing_target_column(data_path, target_column)
  #' 
  #' 
  #' @return subset_df filtered data frame
  
  data <- read_csv(data_path)
  head(data)
  
  plt <- data %>%
    select(all_of(target_column)) %>%
    vis_miss()
  
  print(plt)
  
  # drop the rows with no target columns
  subset_df <- subset(data, !(is.na(data[[target_column]])))
  
  #check
  plt <- subset_df %>%
    select(all_of(target_column)) %>%
    vis_miss()
  print(plt)
  
  return(subset_df)
}

assign_encoding <- function(val, list_mapping){
  #' @description 
  #' A function to assign encoding as per value and list mapping
  #' 
  #' 
  #' @param val value to be looked up
  #' @param list_mapping list mapping of the value
  #' 
  #' 
  #' @example 
  #'  result <- lapply(
  #'         subset_df[[column_name]], assign_encoding, 
  #'         list_mapping = list_mapping)
  #' 
  #' 
  #' @return encoded value
  
  if(is.na(val) | is.null(val)){
    return(NA)
  } else {
    return(as.numeric(list_mapping[[trimws(val)]]))
  }
}


assign_encoding5 <- function(text){
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
    return(0)
  } else {
    return(1)
  }
}

convert_wt_to_kg <- function(data, col_name){
  #' @description 
  #' A function to convert wt to kg
  #' 
  #' @param data data to work on
  #' @param col_name data to use col_name
  #' 
  #' 
  #' @example 
  #'  result <- convert_wt_to_kg(data, col_name)
  #' 
  #' 
  #' @return data, modified data
  
  # data[[col_name]] <- round(data[[col_name]] / 2.206, 2)
  
  # capping the weight due to data error in dataset, unable to figure out
  # if the dataset is wrong or not.
  data[[col_name]] <- lapply(data[[col_name]], 
                             function(x) ifelse(x < 45, 45, 
                                                ifelse(x > 120, 120, x))
    )
  data <- final_touch(data, col_name)
  return(data)
}

data_preview <- function(final_data, column_name){
  #' @description 
  #' A function to preview the data
  #' 
  #' @param final_data data frame to be modified
  #' @param column_name name of the columnt 
  #' 
  #' 
  #' @example 
  #'  result <- data_preview(final_data, column_name)
  #' 
  #' 
  #' @return final_data modified data frame
  
  # display the column name
  display_logs(column_name, "info")
  
  # check unique values
  print(unique(final_data[column_name]))
}

final_touch <- function(final_data, column_name, unnest_col=TRUE){
  #' @description 
  #' A function to unnest and print summary of the data
  #' 
  #' 
  #' @param final_data data frame to be used
  #' @param column_name column to be used
  #' @param unnest_col flag to apply unnest column
  #' 
  #' 
  #' @example 
  #'    final_data <- final_touch(final_data, column_name)
  #' 
  #' 
  #' @return  final_data modified data frame
  
  # to remove the list in the column name
  if(unnest_col){
    final_data <- unnest(final_data, column_name) 
  }
  
  # check
  summary(final_data[[column_name]])
  print(dim(final_data))
  
  return(final_data)
}


check_midwife_experience <- function(subset_data, final_data, column_name){
  #' @description 
  #' A function to check midwife experience 
  #' 
  #' 
  #' @param subset_df data frame to work on
  #' @param final_data data frame to be modified
  #' @param column_name name of the columnt 
  #' 
  #' 
  #' @example 
  #'  final_data <- check_midwife_experience(subset_df, final_data, column_name)
  #' 
  #' 
  #' @return final_data modified dataframe
  
  data_preview(final_data, column_name)
  
  # simplify text
  subset_data[[column_name]] <- lapply(subset_data[[column_name]], 
                                       normalize_text)
  corpus <- subset_data[[column_name]]
  
  display_logs("Running ...", "process")
  # create corpus from all the sentences in the column
  corpus_df <- data.frame()
  for(idx in 1:length(corpus)){
    text <- corpus[[idx]]
    sent_id <- paste("sent_", idx, sep="")
    if(is.na(text)){
      tmp <- data.frame(list(sent_id=sent_id, word=NA, n=NA, total=NA))
      corpus_df <- rbind(corpus_df, tmp)
    }
    
    words <- unique(strsplit(text, "\\s+")[[1]])
    sent_word_count <- length(words)
    
    for(word in words){
      word_count <- sum(str_count(words, sprintf("\\b%s\\b", word)))
      tmp <- data.frame(list(
        sent_id=sent_id, word=word, n=word_count, total=sent_word_count))
      corpus_df <- rbind(corpus_df, tmp)
    }
  }
  
  # create term frequency and idf values and calculate sent values
  corpus_df2 <- corpus_df %>%
    group_by(sent_id) %>%
    mutate(rank=row_number(), `term_frequency`=n/total) %>%
    ungroup() %>%
    bind_tf_idf(word, sent_id, n) %>% 
    group_by(sent_id) %>%
    summarise(sent_val=prod(tf_idf))
  
  sent_val_list <- c()
  for(idx in 1:dim(final_data)[1]){
    idx <- paste("sent_", idx, sep="")
    tmp_df <- corpus_df2[which(corpus_df2$sent_id == idx),]
    if(dim(tmp_df)[1] > 0){
      sent_value <- tmp_df$sent_val
    } else {
      sent_value <- NA
    }
    sent_val_list <- c(sent_val_list, sent_value)
  }
  
  final_data[[column_name]] <- sent_val_list
  
  final_data <- final_touch(final_data, column_name, unnest_col=FALSE)
  
  return(final_data)
}

assign_encoding4 <- function(val){
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
    if(grepl("yes,", simple_text)){
      return(1)
    }else{
      return(0)
    }
  }
}
