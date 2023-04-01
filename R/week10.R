# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
set.seed(67)

# Data Import and Cleaning
gss_tbl_original <- read_sav("../data/GSS2016.sav") %>%
  mutate_all(~na_if(., 0),
             ~na_if(., 98),
             ~na_if(., 99)) %>%
  filter(!is.na(HRS1))

gss_tbl1 <- select(gss_tbl_original,
                  which(colSums(is.na(gss_tbl_original))/nrow(gss_tbl_original) < 0.75)) %>%
  rename(workhours = HRS1)

# Visualization
gss_tbl %>%
  ggplot(aes(workhours)) +
  geom_histogram() +
  labs(x = "Number of Hourse Worked Last Week", y = "Frequency")

# Analysis
# OLS regression model
shuffle_row <- sample(nrow(gss_tbl))
shuffled_gss <- gss_tbl[shuffle_row,]
split <- round(nrow(shuffled_gss)*0.75)
train <- shuffled_gss[1:split,]
test <- shuffled_gss[(split + 1):nrow(shuffled_gss),]
folds <- createFolds(train$workhours, k = 10)


train <- sapply(train, as.numeric)


ols_model <- train(
  workhours ~.,
  data = train,
  method = "lm",
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

ols_test <- predict(ols_model, test, na.action = na.pass)
ols_test_r2 <- cor(ols_test, test$workhours)^2

# Elastic net model 

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

glmnet_test <- predict(glmnet_model, test, na.action = na.pass)
glmnet_test_r2 <-cor(glmnet_test, test$workhours)^2

# Random forest model
rf_model <- train(
  workhours ~.,
  data = train,
  method = "ranger",
  na.action = na.pass,
  preProcess =  "medianImpute",
  tuneGrid = expand.grid(
    mtry = c(100, 200, 400, 600),
    splitrule = c("variance", "extratrees"),
    min.node.size = 5
  ),
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds,
  )
)

rf_test <- predict(rf_model, test, na.action = na.pass)
rf_test_r2 <-cor(rf_test, test$workhours)^2

# eXtreme Gradient Boosting

gbm_model <- train(
  workhours ~.,
  data = train,
  method = "xgbLinear",
  na.action = na.pass,
  preProcess =  "medianImpute",
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds,
  )
)

gbm_test <- predict(gbm_model, test, na.action = na.pass)
gbm_test_r2 <-cor(gbm_test , test$workhours)^2

summary(resamples(list("lm"=ols_model, "glmnet"=glmnet_model, "ranger"=rf_model)))
