# Header -----------------------------------------------------------------------
# File name: log_reg_base.R
# Folder/Module name: code
# Version: v0.1
# Created Date: 2024-01-09
# Last Edited Date: 2024-01-09
# Editor/Creator: Akson Sam Varghese
# Description: source file to implement logistic regression based model
# ------------------------------------------------------------------------------



# Source -----------------------------------------------------------------------
source('code/common.R')

# Library ----------------------------------------------------------------------
library(broom)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(stats)



set.seed(143)

# Functions --------------------------------------------------------------------
plot_coeffs <- function(mlr_model) {
  coeffs <- coefficients(mlr_model)
  mp <- barplot(coeffs, col="#3F97D0", xaxt='n', main="Regression Coefficients")
  lablist <- names(coeffs)
  text(mp, par("usr")[3], labels = lablist, srt = 45, adj = c(1.1,1.1), 
       xpd = TRUE, cex=0.6)
}


build_stepwise_logreg_base_model <- function(data, colname_mapping){
  #' @description 
  #' A function to build logistic regression based model
  #' 
  #' 
  #' @param data data to be modelled
  #' @param colname_mapping name of the column mapping
  #' 
  #' 
  #' @example 
  #'  result <- build_logreg_base_model(data, colname_mapping)
  #' 
  #' 
  #' @return significant variables in the model
  
  logreg_model_intercept <- glm(Y ~ 1, data=data, family=binomial)
  logreg_model_all <- glm(Y ~ ., data=data, family=binomial)
  
  stepwise_model <- stats::step(
    logreg_model_intercept, 
    direction = "forward" , 
    scope=formula(logreg_model_all),
    trace=0)
  
  # step 3: summarize model
  print(stepwise_model$anova)
  print(summary(stepwise_model))
  
  # step 4: inspect the significant coefficients
  # plot_coeffs(stepwise_model)
  coeff_df <- tidy(stepwise_model)
  
  significant_variables <- coeff_df[which(coeff_df$p.value < 0.05),]
  
  significant_variables <- significant_variables %>%
    left_join(colname_mapping, by=c("term" = "name_2"))
  
  
  return(significant_variables)
}

build_dtree_base_model <- function(data, colname_mapping){
  #' @description 
  #' A function to build decision tree model
  #' 
  #' 
  #' @param data data to build the model
  #' @param colname_mapping name of the column mappin
  #' 
  #' 
  #' @example 
  #'  result <- build_dtree_base_model(data, colname_mapping)
  #' 
  #' 
  #' @return significant variables in the model
  
  model <- rpart(Y ~ ., data=data)
  plot(model)
  text(model, pretty=1, digits=3, use.n=TRUE)
  printcp(model)
  return(rpart.rules(model))
  
}


# Handlers ---------------------------------------------------------------------

run_initial_modelling <- function(){
  #' @description 
  #' A function to build initial modelling to extract important  variables
  #' 
  #' 
  #' @example 
  #'  result <- run_initial_modelling()
  #' 
  #' 
  #' @return result significant variables in the model
  
  # step 1: load data
  data_path <- 'data/wales_data/processed_imputed_file.csv'
  data <- read_csv(data_path)
  dim(data)
  colname_mapping <- read_csv("data/wales_data/column_name_mapping.csv")
  
  # step 2: convert to factors
  data <- convert_to_factors(data, colname_mapping)
  names(data)[35] <- "Y"
  # var_8 is dropped due to lack of interpret-ability during model diagnostics
  data <- data %>% select(-var_8) 
  data_without_var_19 <- data %>%  select(-var_19)
  
  
  
  # step 3: extract variables  from log reg model
  sign_var1 <- build_stepwise_logreg_base_model(
    data_without_var_19, colname_mapping)
  sign_var1$model_name <- replicate(dim(sign_var1)[1], "bmWO_var19")
  
  
  sign_var2 <- build_stepwise_logreg_base_model(
    data, colname_mapping)
  sign_var2$model_name <- replicate(dim(sign_var2)[1], "bmWvar19")
  significant_variables <- list(
    m1=sign_var1,
    m2=sign_var2
  )
  display_logs("significant variables from log reg model", "info")
  print(significant_variables)
  # ============================== 
  # significant variables from log reg
  # var_6, var_25, var_29 , var_5, var_12, var_27
  # var_19, var_6, var_21, var_5, var_15, var_13
  # ==============================
  
  
  # step 4: extract variables  from decision tree model
  model_rules1 <- build_dtree_base_model(data, colname_mapping)
  model_rules2 <- build_dtree_base_model(data_without_var_19, colname_mapping)
  
  model_rules <- list(
    model_rules1=model_rules1,
    model_rules2=model_rules2
  )
  display_logs("significant rules from decision tree model", "info")
  print(model_rules)
  # =============================
  # significant decision rules from decision tree model
  # var_19, var_6, var_27, var_21, var_12
  # var_6, var_25, var_31, var_29, var_30
  # ============================
  results <- list(significant_variables=significant_variables, 
                  model_rules=model_rules)
  print(results)
  return(results)
}

















