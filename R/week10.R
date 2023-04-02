# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
set.seed(67)

# Data Import and Cleaning
## Create a dataset called gss_tbl_original for basic data cleaning and filtering 
gss_tbl_original <- read_sav("../data/GSS2016.sav") %>%
  ## Change all "don't know" and "non-applicable" responses to NA in the whole dataset
  mutate_all(~na_if(., 0),
             ~na_if(., 98),
             ~na_if(., 99)) %>%
  ## Remove all missing values from the response variable, workhours 
  filter(!is.na(HRS1))

## Use a separate pipe to create the cleaned dataset as combining it with the above pipe will mess up with data cleaning 
gss_tbl <- select(gss_tbl_original,
                  ## Select only the variables that contained less 75% missingness through calculating the percentage of NA in each column
                  which(colSums(is.na(gss_tbl_original))/nrow(gss_tbl_original) < 0.75)) %>%
  ## Renamed the response as workhours 
  rename(workhours = HRS1)

# Visualization
## Create a histogram plot using gss_tbl
gss_tbl %>%
  ## Specify the variable for frequency counting 
  ggplot(aes(workhours)) +
  ## Add histogram to the plot 
  geom_histogram() +
  ## Added labels to the x- and y-axis
  labs(x = "Number of Hourse Worked Last Week", y = "Frequency")

# Analysis

## OLS regression model
### Shuffle row of gss_tbl for a random split of training and testing datasets  
shuffle_row <- sample(nrow(gss_tbl))
### Rearrange the dataset based on shffled row numbers 
shuffled_gss <- gss_tbl[shuffle_row,]
### Split the dataset into training and testing sets based on a 75/25 split
split <- round(nrow(shuffled_gss)*0.75)
### Select 75% of the original dataset to be the training set
train <- shuffled_gss[1:split,]
### Select 25% of the original dataset to be the testing set
test <- shuffled_gss[(split + 1):nrow(shuffled_gss),]
### Create the number of folds and holdout samples for models
folds <- createFolds(train$workhours, k = 10)

### Need to convert all data values into numeric for models to run 
train <- sapply(train, as.numeric)

### Train the OLS model which predicts workhours with all other variables in the dataset 
ols_model <- train(
  workhours ~.,
  data = train,
  method = "lm",
  ### Pass all NA values when running the models
  na.action = na.pass,
  ### Estimate NA values using medians
  preProcess = "medianImpute",
  ### Set up the folds, holdout sample, and grid search and training method (cross-validation)
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds
  )
)

### Test the trained model on the test set 
ols_test <- predict(ols_model, test, na.action = na.pass)
### Correlate the predicted value and the observed values to examine the model's predictive accuracy in the test sample 
ols_test_r2 <- cor(ols_test, test$workhours)^2

## Elastic net model 

### Follow the same process above to train the elastic net model 
glmnet_model <- train(
  workhours ~.,
  data = train,
  method = "glmnet",
  na.action = na.pass,
  preProcess =  "medianImpute",
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds
  )
)

### Tested the predictive accuracy of the elastic net model using similar steps as above 
glmnet_test <- predict(glmnet_model, test, na.action = na.pass)
glmnet_test_r2 <-cor(glmnet_test, test$workhours)^2

## Random forest model
### Follow the process above to train the random forest model 
rf_model <- train(
  workhours ~.,
  data = train,
  method = "ranger",
  na.action = na.pass,
  preProcess =  "medianImpute",
  ### The only difference lies on the tuning of hyperparameters, I tried out several combination of hyperparameters
  ### And this combination seems to be the one that reduces computation time yet does not sacrifice predictive accuracy
  tuneGrid = expand.grid(
    min.node.size = 5,
    splitrule = "variance",
    mtry = c(10, 50, 200)
  ),
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds,
  )
)

### Follow the same process above for testing the random forest model in the test set
rf_test <- predict(rf_model, test, na.action = na.pass)
rf_test_r2 <-cor(rf_test, test$workhours)^2

## eXtreme Gradient Boosting
### Follow the process above to train the eXtreme gradient boosting model 
gbm_model <- train(
  workhours ~.,
  data = train,
  method = "xgbLinear",
  na.action = na.pass,
  preProcess =  "medianImpute",
  ### Also tuned the hyperparameters to reduce the comptuation time yet retain a similar level of 
  ### predictive accuracy 
  tuneGrid = expand.grid(
    lambda = c(0, 0.01),
    alpha = c(0.1, 0.00001),
    nrounds = c(50, 150),
    eta = 0.3
  ),
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds,
  )
)

### Follow the same process above for testing the eXtreme gradient boosting model in the test set
gbm_test <- predict(gbm_model, test, na.action = na.pass)
gbm_test_r2 <-cor(gbm_test , test$workhours)^2


# Publication 
## Create an object algo that contains all model names 
algo <- c("OLS Regression Model", "Elastic Net Model",
          "Random Forest Model", "eXtreme Gradient Boosting Model")

## Create an object cv_rsq1 that contains all models' r-square values in the training set and rounded them to two decimal places 
cv_rsq1 <- c(round(ols_model$results$Rsquared,2), round(glmnet_model$results$Rsquared[2],2), 
            round(rf_model$results$Rsquared[3],2), round(gbm_model$results$Rsquared[7],2))

## Convert the format of cv_rsq1 values to always show two decimal places and without the leading zero and save it as cv_rsq
cv_rsq2 <- sapply(cv_rsq1, format, nsmall = 2)
cv_rsq <- str_remove(cv_rsq2, pattern = "^0")

## Create an object ho_rsq1 that contains all models' r-square values in the test set and rounded them to two decimal places 
ho_rsq1 <- c(round(ols_test_r2,2), round(glmnet_test_r2,2), round(rf_test_r2, gbm_test_r2),2)

## Convert the format of ho_rsq1 values to always show two decimal places and without the leading zero and save it as ho_rsq
ho_rsq2 <- sapply(ho_rsq1, formatC, format = "f", digits = 2)
ho_rsq <- str_remove(ho_rsq2, pattern = "^0")

## Combine the model names, 10-folde CV R2 and holdout CV R2 into a table
table1_tbl <- tibble(algo, cv_rsq, ho_rsq) 

# Qualitative Questions:
# 1. How did your results change between models? Why do you think this happened, specifically?
# Based on r-square values, the random forest and the eXtreme Gradient Boosting models performed better than OLS and elastic net models 
# because the former two models combine predictors or results from many models to decrease variance and bias without sacrificing the other,
# thus help alleviate the issue of overfitting. 

# 2. How did you results change between k-fold CV and holdout CV? Why do you think this happened, specifically?
# The r-square values were much larger in the training model than in the prediction mode, which is due to the issue of overfitting: 
# models try to maximize predictive accuracy in the given sample yet the accuracy does not hold up in a different sample. 

# 3. Among the four models, which would you choose for a real-life prediction problem, and why? Are there tradeoffs? Write up to a paragraph.
# I would pick the random forest model because of its high r-square value in the training set than the OLS and elastic net models.
# More importantly, it also the highest r-square value in the test sample among the four models. 
# Although the random forest model has a lower r-square value in the training set than the gradient boosting model, it does perform better than the latter in terms of generalizability
# And this touches on the bias-variance trade-off in model building: While the gradient boosting model has less bias in predicting the training set due to its high complexity,
# the variance of the gradient boosting model is higher than the random forest model as a result of the model complexity. 
# And despite the lower complexity and r-square value (in the training set) of the random forest model, it has the benefit of low variance, and thus higher generalizability.
# As our goal is to train models that can make accurate predictions in different datasets, the random forest model is more favorable.
# Another practical trade-off when comparing the random forest and gradient boosting models to the OLS and elastic net models is between prediction quality and computation time:
# While the former two models had larger r-square values in both training and testing sets, they do take substantially more time to run given their larger number of parameters. 





