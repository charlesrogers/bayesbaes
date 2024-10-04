library(tidyverse)
library(caret)
library(pROC)

# Calculate team strength based on average goal difference
team_strength <- df.seasons %>%
  gather(key = "team_type", value = "team", Home, Away) %>%
  mutate(goals_for = ifelse(team_type == "Home", HomeGoals, AwayGoals),
         goals_against = ifelse(team_type == "Home", AwayGoals, HomeGoals)) %>%
  group_by(team) %>%
  summarize(strength = mean(goals_for - goals_against))

# Prepare the data
df_model <- df.seasons %>%
  mutate(Home.Year=paste(Home,Season_End_Year,sep = "."),
         Away.Year=paste(Away,Season_End_Year,sep = ".")) %>% 
  left_join(df.isa.summary.19_23, by = c("Home.Year" = "team.year")) %>%
  View()
  mutate(Home.Year=paste(Home,Season_End_Year,sep = "."),
         Away.Year=paste(Away,Season_End_Year,sep = ".")) %>% 
  left_join(df.isa.summary.19_23, by = c("Home.Year" = "team.year")) %>%
  View()
  mutate(
    close_game = abs(HomeGoals - AwayGoals) == 1,
    xG_diff = abs(Home_xG - Away_xG)
  ) %>%
  left_join(team_strength, by = c("Home" = "team")) %>%
  rename(home_strength = strength) %>%
  left_join(team_strength, by = c("Away" = "team")) %>%
  rename(away_strength = strength) %>%
  mutate(strength_diff = home_strength - away_strength) %>%
  select(close_game, Home_xG, Away_xG, xG_diff, home_strength, away_strength, strength_diff) %>%
  na.omit()  # Remove rows with NA values

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(df_model$close_game, p = 0.8, list = FALSE)
train_data <- df_model[train_index, ]
test_data <- df_model[-train_index, ]

# Train a logistic regression model
model <- glm(close_game ~ Home_xG + Away_xG + xG_diff + home_strength + away_strength + strength_diff, 
             data = train_data, family = "binomial")

# Make predictions on the test set
predictions <- predict(model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, TRUE, FALSE)

# Evaluate the model
confusion_matrix <- confusionMatrix(factor(predicted_classes), factor(test_data$close_game))
print(confusion_matrix)

# Print model summary
summary(model)

# Plot ROC curve
roc_obj <- roc(test_data$close_game, predictions)
plot(roc_obj, main = "ROC Curve")
auc_value <- auc(roc_obj)
print(paste("AUC:", auc_value))

# Show top 10 games most likely to be close games
test_data_with_probs <- test_data %>%
  mutate(close_game_prob = predictions) %>%
  arrange(desc(close_game_prob))

print("Top 10 games most likely to be close games:")
print(head(test_data_with_probs, 10))

# Feature importance
importance <- varImp(model, scale = FALSE)
print(importance)