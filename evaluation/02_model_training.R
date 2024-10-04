# 02_model_training.R

library(tidyverse)
library(caret)

# Function to train individual models
train_individual_models <- function(data) {
  # Implement individual model training here
  # For simplicity, we'll use logistic regression for all models
  # In a real-world scenario, you'd implement each model separately
  
  models <- list()
  
  # BTM Model
  models$BTM <- glm(home_win ~ home_win_rate + away_win_rate, 
                    data = data, family = binomial())
  
  # TOOR Model
  models$TOOR <- glm(home_win ~ home_win_rate + away_win_rate, 
                     data = data, family = binomial())
  
  # GSSD Model
  models$GSSD <- glm(home_win ~ home_win_rate + away_win_rate, 
                     data = data, family = binomial())
  
  # ZSD Model
  models$ZSD <- glm(home_win ~ home_win_rate + away_win_rate, 
                    data = data, family = binomial())
  
  # PRP Model
  models$PRP <- glm(home_win ~ home_win_rate + away_win_rate, 
                    data = data, family = binomial())
  
  return(models)
}

# Function to train ensemble model
train_ensemble_model <- function(data, individual_predictions) {
  # Combine individual model predictions with actual results
  ensemble_data <- cbind(data$home_win, individual_predictions)
  colnames(ensemble_data)[1] <- "actual_result"
  
  # Train logistic regression ensemble
  ensemble_model <- glm(actual_result ~ ., 
                        data = as.data.frame(ensemble_data), 
                        family = binomial())
  
  return(ensemble_model)
}

# Example usage:
# individual_models <- train_individual_models(historical_data)
# individual_predictions <- predict_individual_models(individual_models, historical_data)
# ensemble_model <- train_ensemble_model(historical_data, individual_predictions)