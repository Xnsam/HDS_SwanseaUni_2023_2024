# Header -----------------------------------------------------------------------
# File name: secondary_analysis.R
# Folder/Module name: code
# Version: v0.1
# Created Date: 2024-01-09
# Last Edited Date: 2024-01-09
# Editor/Creator: Akson Sam Varghese
# Description: Source file for implementing secondary analysis for 
#              model diagnostics
# ------------------------------------------------------------------------------


# Source -----------------------------------------------------------------------
source('code/common.R')



# Library ----------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(caret)
library(pROC)
library(yardstick)
library(broom)
library(car)

set.seed(143)

# Functions --------------------------------------------------------------------

get_bmi_group <- function(x){
  #' @description 
  #' A function to the bmi group w.r.t to the bmi value
  #' 
  #' @param x, bmi value
  #'
  #' @example 
  #'  get_bmi_group(data)
  #' 
  #' 
  #' @return bmi_group, the group of the bmi value
  
  if(between(x, 18, 24.9)){
    return('healthy')
  } else if(between(x, 25, 29.9)){
    return('overweight')
  } else if( x >= 30){
    return("obese")
  } else if( x < 18){
    return("underweight")
  }
}


get_model <- function(data){
  #' @description 
  #' A function to get the model from caret pkg
  #' 
  #' @param data, the data to be modelled
  #'
  #' @example 
  #'  model <- get_model(data)
  #' 
  #' 
  #' @return model, caret model
  
  ctrl <- trainControl(method="cv", number=5)
  data$Y <- as.factor(data$Y)
  model <- train(
    Y ~ ., 
    data=data, 
    method="glm", family="binomial", trControl=ctrl)
  print(model)
  
  return(model)
}


display_confusion_matrix <- function(data, model, model_type="caret"){
  #' @description 
  #' A function to display the confusion matrics
  #' 
  #' @param data, the data to be modelled
  #' @param model, the model to be used for predictions
  #' @param model_type, type of the model, caret / glm
  #'
  #' @example 
  #'  display_confusion_matrix(data, model, model_type)
  #' 
  #' 
  #' @return model, caret model
  
  if(model_type == "caret"){
    preds <- predict(model, newdata=data %>% select(-Y), type="raw")
  } else {
    preds <- predict(model, newdata=data %>% select(-Y), type="response")
  }
  
  pred_df <- data.frame()
  for(idx in 1:length(preds)){
    pred <- preds[idx]
    actual <- data[idx, "Y"]$Y
    pred_df <- rbind(pred_df, data.frame(
      idx=idx,
      pred=pred,
      actual=actual
    ))
  }
  
  cm <- confusionMatrix(as.factor(pred_df$actual), as.factor(pred_df$pred))
  
  print(cm)
  
}


get_predictions <- function(data, model, model_name='Model1'){
  #' @description 
  #' A function to get the predictions
  #' 
  #' @param data, the data to be modelled
  #' @param model, the model to be used for predictions
  #' @param model_name, name of the model to register against predictions
  #'
  #' @example 
  #'  test_scores <- get_predictions(data, model, model_name)
  #' 
  #' 
  #' @return test_scores
  
  class <- predict(model, data %>% select(-Y))
  probs <- predict(model, data %>% select(-Y),'prob')
  
  
  TEST.scored <- cbind(data$Y, class,probs) %>% 
    mutate(model_name = model_name)
  
  return(TEST.scored)
}


get_transformation_model <- function(data){
  #' @description 
  #' A function to get the model for transformed variables in the linear 
  #' equation
  #' 
  #' @param data, the data to be modelled
  #'
  #' @example 
  #'  model <- get_transformation_model(data)
  #' 
  #' 
  #' @return model
  
  data$Y <- as.factor(data$Y)
  
  folds <- 4
  cvIndex <- createFolds(factor(data$Y), folds, returnTrain = T)
  ctrl <- trainControl(index = cvIndex,
                       method = 'cv', 
                       number = folds)
  
  # interaction 1
  model <- train(
    Y ~ var_6 + var_25 + var_29 + var_5 + var_12 + var_27,
    data=data, 
    method="glm", family="binomial", trControl=ctrl)
  print(model)
  
  return(model)
}

get_glm_model <- function(data){
  #' @description 
  #' A function to get glm model
  #' 
  #' @param data, data to be modeled 
  #'
  #' @example 
  #'  model <- get_glm_model(data)
  #' 
  #' 
  #' @return model, fitted logistic regression model
  
  model <- glm(
    Y ~ var_6 + var_25 + var_29 + var_5 + var_12 + var_27,
    data = data, 
    family = binomial)
  
  return(model)
}


sa_load_data <- function(data_var=1){
  #' @description 
  #' A function to load the data 
  #' 
  #' 
  #' @example 
  #'  load_data()
  #' 
  #' 
  #' @return data
  
  # step 1: load the data
  data_path <- 'data/wales_data/processed_imputed_file.csv'
  data <- read_csv(data_path)
  dim(data)
  colname_mapping <- read_csv("data/wales_data/column_name_mapping.csv")
  
  # step 2: convert to factors
  data <- convert_to_factors(data, colname_mapping)
  names(data)[35] <- "Y"
  
  # step 3: create separate datasets with separate  variables to select
  if(data_var == 1){
    data <- data %>%
      select(var_6, var_25, var_29 , var_5, var_12, var_27, Y)
  } else {
    data <- data %>%
      select(var_19, var_6, var_21, var_5, var_15, var_13, Y)
  }
  
  return(data)
}


run_sec_analysis_logreg <- function(){
  #' @description 
  #' A function to run secondary analysis on significant variables only for 
  #' logistic regression model
  #' 
  #' 
  #' @example 
  #'  run_sec_analysis()
  #' 
  #' 
  #' @return None
  
  
  # Step 1: load data
  sub_data1 <- sa_load_data(data_var=1)
  sub_data2 <- sa_load_data(data_var=2)
  
  # step 2: get the respective models with different variables
  model1 <- get_model(sub_data1)
  model2 <- get_model(sub_data2)
  
  
  # step 3: display the confusion matrix
  display_logs("Confusion Matrix Model1", "info")
  display_confusion_matrix(sub_data1, model1)
  display_logs("Confusion Matrix Model2", "info")
  display_confusion_matrix(sub_data2, model2)
  
  # step 4: display the ROC Curve
  predictions1 <- get_predictions(sub_data1, model1, "Model1")
  predictions2 <- get_predictions(sub_data2, model2, "Model2")
  
  predictions <- rbind(predictions1, predictions2)
  predictions$`data$Y` <- as.factor(predictions$`data$Y`)
  roc_plot <- predictions %>%
    group_by(model_name) %>%
    roc_curve(truth=`data$Y`, "1") %>%
    autoplot()
  print(roc_plot)
  
}



# Handler ----------------------------------------------------------------------


run_sec_analysis <- function(){
  # step 1: run logistic regression model
  run_sec_analysis_logreg()
}


run_sec_analysis()

# ==================================================================|
# Favoring MODEL 1 has it seems better than Model 2 with lower FPR  |
# ==================================================================|

# This code is out because the functions used for diagnostic plots for some
# reason is not working in functions
# it always looks for 'data' named variable that contains the data frame 
# requiring the variable to be global level

# step 5: run model diagnostics
# to check for the model quality
data <- sa_load_data()
glm_model <- get_glm_model(data)
summary(glm_model)
residualPlot(glm_model)
marginalModelPlots(glm_model)
outlierTest(glm_model)
data <- augment(glm_model) %>% 
  mutate(index = 1:n())
data <- data %>%
  filter(abs(.cooksd) < 0.01) %>% 
  filter(abs(.std.resid) < 30000)

glm_model <- get_glm_model(data)
summary(glm_model)
residualPlot(glm_model)
marginalModelPlots(glm_model)
influenceIndexPlot(glm_model, id.n=3)
car::vif(glm_model)



## Step 6: Lastly, we can plot the predicted probabilities for each sample having
## intention to breastfeed and color by actual values

predicted.data <- data.frame(
  probability=glm_model$fitted.values,
  intention=data$Y)

predicted.data <- predicted.data[
  order(predicted.data$probability, decreasing=FALSE),]

predicted.data$rank <- 1:nrow(predicted.data)


ggplot(data=predicted.data, aes(x=rank, y=probability)) +
  geom_point(aes(color=intention), alpha=1, shape=4, stroke=2) +
  labs(
    title="S curve for Logistic Regression Predictions",
    x="index",
    y="Predicted probability of Intention to not BreastFeed"
  ) + 
  theme(plot.title = element_text(hjust = 0.5))







