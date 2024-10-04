# Load libraries
library(performance)  # Package for model performance evaluation
library(broom)        # Package for tidying model output

# Load data
df.points.19_23 <- df.isa.summary.19_23 %>%
  # Calculate Pythagorean winning percentage using Pythagorean formula with data from Statsbomb
  # See https://statsbomb.com/articles/soccer/improving-soccers-version-of-the-bill-james-pythagorean/
  mutate(
    points_pyt.pct = total.goals.scored^1.2 / (total.goals.scored^1.2 + total.goals.conceded^1.2),
    points_pyt.sb = ((2 / 3 * total.goal_differential) + 52.39) * (38 / 38),
    # Calculate overfit model with higher exponent for goals scored and conceded
    points.overfit = (total.goals.scored^1.331 / (total.goals.scored^1.331 + total.goals.conceded^1.331))
  )

# Fit simple model
# RMSE = 4.005
model_all.simple <- lm(points ~ total.goal_differential, data = df.points.19_23)
# Evaluate model performance
performance::model_performance(model_all.simple)

# Fit Statsbomb model
# RMSE =
model_all.sb <- lm(points ~ I((2 / 3 * total.goal_differential) + 52.39), data = df.points.19_23)
# Evaluate model performance
performance::model_performance(model_all.sb)

# Fit overfit model
model_all.overfit <- lm(points ~ I(total.goals.scored^1.331 / (total.goals.scored^1.331 + total.goals.conceded^1.331)), data = df.points.19_23)
# Evaluate model performance
performance::model_performance(model_all.overfit)

# Compare models
performance::compare_performance(model_all.overfit, model_all.simple, model_all.sb, rank = TRUE)

# Make predictions with overfit model
predictions.overfit <- predict(model_all.overfit, df.points.19_23)
predictions_sb <- predict(model_all.sb, df.points.19_23)
predictions_simple <- predict(model_all.simple, df.points.19_23)

# Plot predictions for overfit model and Statsbomb model
plot(predictions_sb, predictions.overfit,
  main = "Comparison of Model Predictions",
  xlab = "Statsbomb Model Predictions",
  ylab = "Overfit Model Predictions"
)
abline(a = 0, b = 1, col = "red") # Add line of best fit

# Calculate correlation between predictions
cor_pred <- cor(predictions_sb, predictions.overfit)
print(paste("Correlation between predictions:", round(cor_pred, 4)))

# Calculate mean absolute difference between predictions
mean_diff <- mean(abs(predictions_sb - predictions.overfit))
print(paste("Mean absolute difference between predictions:", round(mean_diff, 4)))

# Define function to calculate RMSE
calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Calculate RMSE for both models
rmse_sb <- calculate_rmse(df.points.19_23$points, predictions_sb)
rmse_simple <- calculate_rmse(df.points.19_23$points, predictions_simple)
rmse_overfit <- calculate_rmse(df.points.19_23$points, predictions.overfit)

print(paste("RMSE for Statsbomb model:", round(rmse_sb, 4)))
print(paste("RMSE for Simple model:", round(rmse_simple, 4)))
print(paste("RMSE for Overfit model:", round(rmse_overfit, 4)))

# Augment model output with residuals
df.points.19_23.aug <- broom::augment(model_all.overfit, data = df.points.19_23)

# Plot residuals
# Note: The hline at y=0 represents the mean of the residuals
base_plot <- ggplot(df.points.19_23.aug, aes(x = total.goal_differential, y = .resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = 3) +
  xlab("Goal difference") + ylab("Residual")

# Identify teams with large residuals
highlight_teams <- df.points.19_23.aug %>%
  filter(abs(.resid) > 6) 
# Plot teams with large residuals with labels
library(ggrepel)
base_plot +
  geom_point(data = highlight_teams, color = "red") +
  geom_text_repel(data = highlight_teams, color = "red",
                  aes(label = paste(team.year,round(.resid,1))))


# EXPLORING WHY THESE TEAMS HAVE LARGE RESIDUALS
# Filter games decided by one run
## Subset games where the difference in points is exactly 1
df.points.19_23.1run_wins <- df.points.19_23 %>%
  filter(diff == 1) %>%
  group_by(winner) %>%
  summarise(
    # Count the number of one run wins for each team
    one_run_wins = n(),
    # Calculate the percentage of wins that were won by one run for each team
    one_run_wins_pct = n() / sum(df.points.19_23$winner == winner)
  )

# Plot the relationship between number of one run wins and percentage of wins won by one run
df.season.2019.1run_wins %>%
  ggplot(aes(x = one_run_wins, y = one_run_wins_pct)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Number of Wins", y = "Percent of Wins Won by 1 Run")

# Calculate the correlation between number of one run wins and percentage of one run wins
# Note: The correlation is low, indicating a weak relationship between the two variables
correlation <- cor(df.season.2019.1run_wins$one_run_wins, df.season.2019.1run_wins$one_run_wins_pct)
print(paste0("The correlation between number of one run wins and the percent of one run wins is: ", correlation))

