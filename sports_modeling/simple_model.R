# Soccer Pythagorean Model
# Calculate the Pythagorean winning percentage for each team in the 2019 season
points_data.2019 <- df.season.2019.summary %>%
  mutate(
    # Calculate the Pythagorean winning percentage using the Pythagorean formula with data from statsbomb: https://statsbomb.com/articles/soccer/improving-soccers-version-of-the-bill-james-pythagorean/
    points_pyt.pct = total.goals.scored^1.2 / (total.goals.scored^1.2 + total.goals.conceded^1.2),
    # Calculate the total points expected based on the Pythagorean winning percentage
    total_points.pythagorean = points_pyt.pct * (38 * 3),
    # Calculate the percentage difference between the actual points and Pythagorean points
    diff.pct = abs(points.pct - points_pyt.pct),
    # Calculate the absolute difference between the total Pythagorean points and actual points
    diff = abs(total_points.pythagorean - points)
  ) %>%
  select(team, points, points_pyt.pct, points.pct, total_points.pythagorean, diff, total.goals.scored, total.goals.conceded, everything())

# Calculate the Root Mean Squared Error (RMSE) for the Pythagorean winning percentage
points_data.2019 %>%
  summarize(rmse = sqrt(mean(diff.pct^2)))

# Calculate the RMSE for the total points
points_data.2019 %>%
  summarize(rmse = sqrt(mean(diff^2)))

# Linear Regression Model: Points Percent
# Fit a linear regression model to predict the Pythagorean winning percentage based on goals scored and conceded
model <- lm(points_pyt.pct ~ total.goals.scored + total.goals.conceded, data = points_data.2019)
# Extract model statistics using broom package
broom::tidy(model, conf.int = TRUE, conf.level = 0.95)

# Calculate a better exponent
# Calculate a new Pythagorean winning percentage using an optimized exponent
points_data.2019.calc <- points_data.2019 %>%
  mutate(
    # Calculate the log ratio of actual points to maximum possible points
    logPointsRatio = log(points / ((38 * 3)-points)),
    # Calculate the log ratio of goals scored to goals conceded
    logGoalsRatio = log(total.goals.scored / total.goals.conceded)
  ) 

# Fit a linear regression model to find the optimal exponent for the Pythagorean formula
pytFit <- lm(logPointsRatio ~ 0 + logGoalsRatio, data = points_data.2019.calc)
pytFit

# Update data with new Pythagorean model
points_data.2019.new <- df.season.2019.summary %>%
  mutate(
    # Calculate the Pythagorean winning percentage with the new exponent
    points_pyt.pct = total.goals.scored^1.331 / (total.goals.scored^1.331 + total.goals.conceded^1.331),
    # Calculate the total points expected based on the new Pythagorean winning percentage
    total_points.pythagorean = points_pyt.pct * (38 * 3),
    # Calculate the percentage difference between the actual points and new Pythagorean points
    diff.pct = abs(points.pct - points_pyt.pct),
    # Calculate the absolute difference between the total new Pythagorean points and actual points
    diff = abs(total_points.pythagorean - points)
  ) %>%
  select(team, points, points_pyt.pct, points.pct, total_points.pythagorean, diff, total.goals.scored, total.goals.conceded, everything())

# Calculate the RMSE for the new Pythagorean winning percentage
points_data.2019.new %>%
  summarize(rmse = sqrt(mean(diff.pct^2))

# Filter games decided by one run
# Subset games where the difference in points is exactly 1
df.season.2019.1run_wins <- df.season.2019.simple %>%
  filter(diff == 1) %>%
  group_by(winner) %>%
  summarise(
    # Count the number of one run wins for each team
    one_run_wins = n(),
    # Calculate the percentage of wins that were won by one run for each team
    one_run_wins_pct = n() / sum(df.season.2019.simple$winner == winner)
  )

# Plot the relationship between number of one run wins and percentage of wins won by one run
df.season.2019.1run_wins %>%
  ggplot(aes(x = one_run_wins, y = one_run_wins_pct)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Number of Wins", y = "Percent of Wins Won by 1 Run")

# Calculate the correlation between number of one run wins and percentage of one run wins
# Note: The correlation is low, indicating a weak relationship between the two variables
correlation <- cor(df.season.2019.1run_wins$one_run_wins, df.season.2019.1run_wins$one_run_wins_pct, method = "pearson")
print(paste0("The correlation between number of one run wins and the percent of one run wins is: ", correlation))

# Pythagorean Model for Soccer
# Define functions to calculate Pythagorean winning percentage and season points
library(tidyverse)

soccer_pythagorean_expectation <- function(goals_for, goals_against, exponent = 2) {
  # Calculate the Pythagorean winning percentage based on goals scored and conceded
  points_ratio <- goals_for^exponent / (goals_for^exponent + goals_against^exponent)
  expected_points <- points_ratio * 3
  return(expected_points)
}

calculate_season_points <- function(expected_points_per_game, games_played, max_games = 38) {
  # Calculate the total points for the season based on expected points per game and games played
  total_points <- expected_points_per_game * games_played
  remaining_games <- max_games - games_played
  projected_total_points <- total_points + (expected_points_per_game * remaining_games)

  tibble(
    games_played = games_played,
    total_points = total_points,
    projected_total_points = projected_total_points
  )
}

# Example usage of functions
team_data <- tibble(
  team_name = c("Team A", "Team B", "Team C"),
  goals_for = c(50, 40, 30),
  goals_against = c(30, 35, 45),
  games_played = c(20, 25, 30)
)

results <- team_data %>%
  mutate(
    expected_points_per_game = map2_dbl(goals_for, goals_against, soccer_pythagorean_expectation),
    season_projection = map2(expected_points_per_game, games_played, calculate_season_points)
  ) %>%
  unnest(season_projection, names_sep = "_")

print(results)

# Visualize the projected total points by team
ggplot(results, aes(x = games_played, y = projected_total_points, color = team_name)) +
  geom_point(size = 3) +
  geom_text(aes(label = round(projected_total_points, 1)), vjust = -1, hjust = 0.5) +
  labs(
    title = "Projected Total Points by Team",
    x = "Games Played",
    y = "Projected Total Points",
    color = "Team"
  ) +
  theme_minimal()

