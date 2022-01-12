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
library(dummies)

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
null_linear_model <- lm(FSLPR ~ 1, data = train, weights = WEIGHT)
summary(null_linear_model)

# Full model 
full_linear_model <- lm(FSLPR ~ . - WEIGHT, data = train, weights = WEIGHT)
summary(full_linear_model) # Adjusted R-squared:  0.579

# Analyzing plots of full model 
  # potential issue with normality of residuals 
  # potnetial high leverage points 
plot(full_linear_model)

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
log_trans_linear_model <- lm(FSLPR ~ . + log(FSQFS) + log(LOTV) + log(AREA) 
                  - WEIGHT - FSQFS - LOTV - AREA, 
                  data = train,
                  weights = WEIGHT)
summary(log_trans_linear_model) #  Adjusted R-squared:  0.5555

# Analyzing plots of transformed model 
  # potential issue with normality of residuals 
  # potnetial high leverage points 
plot(log_trans_linear_model)

## Model Selection 

# Choose best model using stepwise selection with AIC as criterion 
n <- dim(soc_data)[1]
AIC_linear_model <- step(full_linear_model, direction = "both", k = log(n))
summary(AIC_Linear_model) # Adjusted R-squared:  0.5754

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

MSE_train_linear <- MSE_calc(predicted = AIC_Linear_model$fitted.values,
                             actual = train$FSLPR,
                             n = n_train, 
                             p = num_reg_prams) #2.5601e+10

# Find model MSE on test data 
linear_preds <- predict(AIC_Linear_model, newdata = test)
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
# tunned_ranger <- train(FSLPR ~ . - WEIGHT, 
#                     data = train, 
#                     weights =  train$WEIGHT,
#                     method = 'ranger')

# Refit tuned model using tunned_ranger parameters for increased speed
tunned_ranger_model <- ranger(FSLPR ~ . - WEIGHT, 
                              data = train, 
                              case.weights =  train$WEIGHT,
                              num.trees = 500,
                              mtry = 34,
                              min.node.size = 5,
                              verbose = TRUE)

# Find model MSE on train data
MSE_train_tunned_ranger <- MSE_calc(predicted = tunned_ranger_model$predictions,
                                    actual = train$FSLPR,
                                    n = n_train,
                                    p = num_reg_prams) #1.3509e+10

## Make test set with dummy encoding matching best ranger model dummy encoding 

# Get column names encoding from ranger model 
ranger_dummy_names <- tunned_ranger_model[["xNames"]]

# Convert BASE variable to dummy encoding and match names with ranger model encoding
base_dum <- dummy_cols(test$BASE, remove_first_dummy = TRUE) %>% 
  select(-1) 
colnames(base_dum) <- ranger_dummy_names[4:7]

# Convert HEAT variable to dummy encoding and match names with ranger model encoding
heat1_dum <- dummy_cols(test$HEAT, remove_first_dummy = TRUE) %>% 
  select(-1)
colnames(heat1_dum) <- ranger_dummy_names[16:19]

# Convert HEAT2 variable to dummy encoding and match names with ranger model encoding
heat2_dum <- dummy_cols(test$HEAT2, remove_first_dummy = TRUE) %>% 
  select(-1)
colnames(heat2_dum) <- ranger_dummy_names[20:23]

# Convert WAL1 variable to dummy encoding and match names with ranger model encoding
wal1_dum <- dummy_cols(test$WAL1, remove_first_dummy = TRUE) %>% 
  select(-1)
colnames(wal1_dum) <- ranger_dummy_names[30:37]

# Convert WAL2 variable to dummy encoding and match names with ranger model encoding
wal2_dum <- dummy_cols(test$WAL2, remove_first_dummy = TRUE) %>% 
  select(-1)
colnames(wal2_dum) <- ranger_dummy_names[38:45]

# Convert FUEL variable to dummy encoding and match names with ranger model encoding
# fill unused level with 0 to match train set 
fuel1_dum <- dummy_cols(test$FUEL, remove_first_dummy = TRUE) %>% 
  select(-1)
fuel1_dum$.data_04 <- rep(0, dim(fuel1_dum)[1])
colnames(fuel1_dum) <- ranger_dummy_names[55:59]

# Convert FUEL2 variable to dummy encoding and match names with ranger model encoding
fuel2_dum <- dummy_cols(test$FUEL2, remove_first_dummy = TRUE) %>% 
  select(-1)
colnames(fuel2_dum) <- ranger_dummy_names[60:63]

# Construct matching test set by replacing variables with their corresponding dummy data frame
test_dum <- test %>% 
  select(-BASE, -HEAT, -HEAT2, -WAL2, -FUEL, -FUEL2, -WEIGHT, -FSLPR) %>% 
  cbind(base_dum, heat1_dum, heat2_dum, wal1_dum, wal2_dum, fuel1_dum, fuel2_dum)

# Find model MSE on test data
tunned_ranger_pred <- predict(tunned_ranger_model,
                       data = test_dum,
                       case.weights = test$WEIGHT)

MSE_test_tunned_ranger <- MSE_calc(predicted = tunned_ranger_pred$predictions,
                                   actual = test$FSLPR,
                                   n = n_test,
                                   p = num_reg_prams) #1.1067e+10
