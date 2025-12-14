library(tidyverse)
library(ggplot2)
library(factoextra)
library(caret)
library(e1071)
library(randomForest)
library(pROC)


# load the dataset
data <- read_csv("./code/heart_disease_modified.csv")

head(data)


# descriptive statistics
summary(data)


# remove columns
data <- data %>%
  select(-c("...1", "Patient_ID", "pace_maker"))

# ...1 column is removed because it is just a serial number to identify the rows
# Patient_Id is removed because patient id provides no specific value towards 
# the class
# pace maker if left un removed it gives error in the algorithm training as
# pace maker is statistical a categorical variable and has only 1 level 'NO'


# identify numeric columns 
num_cols <- sapply(data, is.numeric)
num_count <- sum(num_cols)
print(paste("Number of numerical columns:", num_count))


# identify categorical columns
cat_cols <- sapply(data, function(col) is.factor(col) || is.character(col))
cat_count <- sum(cat_cols)
print(paste("Number of categorical columns:", cat_count))


# convert the columns into factors
data <- data %>%
  mutate(
    sex=as.factor(sex),
    cp=as.factor(cp),
    fbs=as.factor(fbs), 
    restecg=as.factor(restecg),
    slope=as.factor(slope),
    ca=as.factor(ca),
    thal=as.factor(thal),
    smoker=as.factor(smoker),
    drug=as.factor(drug),
    fam_hist=as.factor(fam_hist),
    class=as.factor(class),
    exang=as.factor(exang),
    perfusion=as.factor(perfusion),
    oldpeak=as.factor(oldpeak)
  )

head(data)

# plot class distribution
data %>%
  ggplot(aes(x=class)) +
    geom_histogram(stat="count") +
    labs(x="Class", title="Class Distribution")


# check the distribution of the numeric cols
data %>%
  select(where(is.numeric)) %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x=value)) + 
    geom_histogram(binwidth=1, fill="blue", color="black") +
    facet_wrap(~variable, scales="free") +
    labs(title="Distribution of numeric values by numeric columns ", x="value")



remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[x >= lower_bound & x <= upper_bound]
  return(x)
}

# from the distribution plot
data <- data %>%
  mutate(
    # from google search identified that traponin column cannot be negative
    traponin=ifelse(traponin < 0, 0, traponin),
    # numeric columns has multiple peaks in the distribution,
    # indicating possible outliers, so to center the dataset and remove outliers
    thalach=remove_outliers(thalach),
    chol=remove_outliers(chol),
    traponin=remove_outliers(traponin),
    trestbps=remove_outliers(trestbps)
  )

# check for missing values in the data
# Assuming df is your dataframe
missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)
# no missing values found


# Classification algorithm
set.seed(123)

# split the dataset for train and test
data <- data %>%
  mutate(class=relevel(class, ref="1"))

unique(data$class)

trainIndex <- createDataPartition(data$class, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

dim(trainData)
dim(testData)

# Support Vector Machines
svm_model <- svm(
  class ~ .,
  data=trainData,
  probability=TRUE
)

# model training takes time 
rf_model <- train(class ~ ., 
                  data = trainData, 
                  method = "rf", 
                  trControl = trainControl(method = "cv", number = 5))

svm_predictions <- predict(svm_model, newdata = testData, probability=TRUE)
svm_prob <- attr(svm_predictions, "probabilities")
svm_prob <- svm_prob[, 1]
head(svm_prob)


rf_predictions <- predict(rf_model, newdata = testData, type="prob")
head(rf_predictions)
rf_prob <- rf_predictions[,1]

rf_predictions <- rf_predictions %>%
  mutate(label=as.factor(ifelse(`1` > 0.5, 1, 0)))

head(rf_predictions)

# Confusion matrix for SVM
svm_cm <- confusionMatrix(
  relevel(as.factor(svm_predictions), ref="1"), 
  relevel(as.factor(testData$class), ref="1"))

print(svm_cm)

# Confusion matrix for Random Forest
rf_cm <- confusionMatrix(
  relevel(rf_predictions$label, ref="1"), 
  relevel(testData$class, ref="1"))

print(rf_cm)

# Function to plot confusion matrix
plot_confusion_matrix <- function(cm, title) {
  cm_table <- as.data.frame(cm$table)
  ggplot(cm_table, aes(Prediction, Reference, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq)) +
    scale_fill_gradient(low = "white", high = "blue") +
    ggtitle(title) +
    theme_minimal()
}

# Plot confusion matrices
plot_confusion_matrix(svm_cm, "SVM Confusion Matrix")
plot_confusion_matrix(rf_cm, "Random Forest Confusion Matrix")


# Calculate AUC for SVM
svm_roc <- roc(testData$class, svm_prob)
svm_auc <- auc(svm_roc)

# Calculate AUC for Random Forest
rf_roc <- roc(testData$class, rf_prob)
rf_auc <- auc(rf_roc)

# Plot ROC curves
ggroc(list(SVM = svm_roc, RandomForest = rf_roc)) +
  ggtitle("ROC Curves for SVM and Random Forest") +
  theme_minimal()


# random forest performs better in ROC
# thus random forest is selected

# hyper parameter tuning
# Define a more comprehensive grid
tuneGrid <- expand.grid(.mtry = c(1:4), .ntree = c(100, 200, 300))

head(tuneGrid)

# Train the model with the new grid
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3, 
                        search='grid')

tunegrid <- expand.grid(.mtry = (1:15)) 

rf_gridsearch <- train(class ~ ., 
                       data = trainData,
                       method = 'rf',
                       metric = 'Accuracy',
                       tuneGrid = tunegrid)
print(rf_gridsearch)


rf_predictions <- predict(rf_model2, newdata = testData, type="prob")
head(rf_predictions)
rf_prob <- rf_predictions[,1]

rf_predictions <- rf_predictions %>%
  mutate(label=as.factor(ifelse(`1` > 0.5, 1, 0)))


rf_cm <- confusionMatrix(
  relevel(rf_predictions$label, ref="1"), 
  relevel(testData$class, ref="1"))

print(rf_cm)

plot_confusion_matrix(rf_cm, "Random Forest Confusion Matrix Hyper tuned")





  


