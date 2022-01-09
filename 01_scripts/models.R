################################################################################
# Model building for New Const. Home Analysis                                  #
# Nolan Peterson                                                               #
# 12/28/2021                                                                   #
# https://github.com/NolanPeterson453/New-Construction-Analysis-               #
################################################################################

# Library statements 
library(tidyverse)
library(hablar)
library(splitstackshape)
library(leaps)
library(ranger)
library(caret)

# Read in data 
soc_data <- read_csv(file = "https://raw.githubusercontent.com/NolanPeterson453/New-Construction-Analysis-/main/02_data/cleaned_soc.csv")

# Assign column types 
factor_vars <- c("ACS", "AGER", "ASSOC", "BASE", "CON", "DECK", "DET", 
                 "DIV", "FNBS", "FOYER", "FRAME", "GAR", "HEAT", "HEAT2", 
                 "LNDR", "MFGS", "PATI", "PRCH", "SEWER", "STOR", "WAL1", 
                 "WAL2", "WALS","WATER", "BEDR", "FPLS", "FULB", "HAFB", "FUEL",
                 "FUEL2")

contin_vars <- c("FSLPR", "FSQFS", "LOTV", "FFNSQ", "AREA")

soc_data <- soc_data %>% 
  convert(fct(factor_vars),
          num(contin_vars))

# Expand data set by replicate weight 
soc_data_expanded <- expandRows(soc_data, 'WEIGHT')

# Create training and test splits 
set.seed(42)
sample <- sample(c(TRUE, FALSE), 
                 nrow(soc_data), 
                 replace=TRUE, 
                 prob=c(0.8,0.2))
train <- soc_data[sample, ]
test <- soc_data[!sample, ]

# Create training and test splits for expanded data  
set.seed(42)
sample <- sample(c(TRUE, FALSE), 
                 nrow(soc_data_expanded), 
                 replace=TRUE, 
                 prob=c(0.8,0.2))
train_expanded <- soc_data_expanded[sample, ]
test_expanded <- soc_data_expanded[!sample, ]



################################################################################
# Ordinary Linear Regression 
################################################################################

## Testing assumptions of linear regression 

# Correlation matrix of continous predictors and response
  # No apparent multicollinearity 
cor(select(train, contin_vars))

# Null model 
H_0_model <- lm(FSLPR ~ 1, data = train, weights = WEIGHT)
summary(H_0_model)

# Full model 
H_a_model_1 <- lm(FSLPR ~ . - WEIGHT, data = train, weights = WEIGHT)
summary(H_a_model_1) # Adjusted R-squared:  0.579

# Analyzing plots of full model 
  # potential issue with normality of residuals 
  # potnetial high leverage points 
plot(H_a_model_1)

## Testing log transformations in order to correct 

# Analyzing scatter plots of continous predictors and response 

  # FSQFS without log transformation 
plot(soc_data$FSQFS, soc_data$FSLPR)
  # FSQFS with log transformation 
plot(log(soc_data$FSQFS), soc_data$FSLPR)

  # LOTV without log transformation 
plot(soc_data$LOTV, soc_data$FSLPR)
  # LOTV with log transformation 
plot(log(soc_data$LOTV), soc_data$FSLPR)

  # AREA without log transformation 
plot(soc_data$AREA, soc_data$FSLPR)
  # AREA with log transformation 
plot(log(soc_data$AREA), soc_data$FSLPR)

# Fit model with log transformations  
H_a_model_2 <- lm(FSLPR ~ . + log(FSQFS) + log(LOTV) + log(AREA) 
                  - WEIGHT - FSQFS - LOTV - AREA, 
                  data = train,
                  weights = WEIGHT)
summary(H_a_model_2) #  Adjusted R-squared:  0.5555

# Analyzing plots of transformed model 
  # potential issue with normality of residuals 
  # potnetial high leverage points 
plot(H_a_model_2)

## Model Selection 

# Choose best model using stepwise selection with AIC as criterion 
n <- dim(soc_data)[1]
H_a_model_3 <- step(H_a_model_1, direction = "both", k = log(n))
summary(H_a_model_3) # Adjusted R-squared:  0.5754

# Find model MSE on train data 
  # Number of regression parameters for calculating degrees of freedom in MSE calculation 
num_reg_prams <- 40 

   # Number of training examples and number of test examples for n in MSE calculation 
n_train <- dim(train)[1]
n_test <- dim(test)[1]

   # write function for repeated MSE calculation 
MSE_calc <- function(predicted, actual, n, p){
  sum((predicted - actual)^2, na.rm = TRUE) / (n - p)
}

MSE_train_linear <- MSE_calc(predicted = H_a_model_3$fitted.values,
                             actual = train$FSLPR,
                             n = n_train, 
                             p = num_reg_prams) #2.5601e+10

# Find model MSE on test data 
linear_preds <- predict(H_a_model_3, newdata = test)
MSE_test_linear <- MSE_calc(predicted = linear_preds,
                            actual = test$FSLPR,
                            n = n_test, 
                            p = num_reg_prams) #2.0244e+10

################################################################################
# Random Forest Model 
################################################################################

## Using ranger Package ##

# Fit random forest model on all predictors (weighted)
ranger_model <- ranger(FSLPR ~ . - WEIGHT, 
                       data = train, 
                       case.weights =  train$WEIGHT,
                       verbose = TRUE)

# Find model MSE on train data 
MSE_train_ranger <- MSE_calc(predicted = ranger_model$predictions, 
                             actual = train$FSLPR,
                             n = n_train, 
                             p = num_reg_prams) #1.5188e+10

# Find model MSE on test data 
ranger_pred <- predict(ranger_model,
                              data = test,
                              case.weights = test$WEIGHT)

MSE_test_ranger <- MSE_calc(predicted = ranger_pred$predictions,
                            actual = test$FSLPR,
                            n = n_test,
                            p = num_reg_prams) #1.2325e+10

# Tune model using caret package 
tunned_ranger <- train(FSLPR ~ . - WEIGHT, 
                    data = train, 
                    weights =  train$WEIGHT,
                    method = 'ranger')

tunned_ranger_model <- tunned_ranger$finalModel

# Find model MSE on train data
MSE_train_tunned_ranger <- MSE_calc(predicted = tunned_ranger_model$predictions,
                                    actual = train$FSLPR,
                                    n = n_train,
                                    p = num_reg_prams) #1.3509e+10

# Find model MSE on test data
tunned_ranger_pred <- predict(tunned_ranger_model,
                       data = test,
                       case.weights = test$WEIGHT)
MSE_test_ranger <- sum((tunned_ranger_pred$predictions - test$FSLPR)^2) / (n_test - num_reg_prams)
