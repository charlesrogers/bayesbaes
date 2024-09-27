# Soccer Pythagorean Model
# Calculate the Pythagorean winning percentage for each team in the 2019 season
points_data.2019 <- df.season.2019.summary %>%
  mutate(
    # Calculate the Pythagorean winning percentage using the Pythagorean formula with data from statsbomb: https://statsbomb.com/articles/soccer/improving-soccers-version-of-the-bill-james-pythagorean/
    points_pyt.pct = total.goals.scored^1.2 / (total.goals.scored^1.2 + total.goals.conceded^1.2),
    points_pyt.sb= ((2/3 * total.goal_differential) + 52.39) * (38 / 38),
    # Calculate the total points expected based on the Pythagorean winning percentage
    total_points.pythagorean = points_pyt.pct * (38 * 3),
    # Calculate the percentage difference between the actual points and Pythagorean points
    diff.pct = abs(points.pct - points_pyt.pct),
    # Calculate the absolute difference between the total Pythagorean points and actual points
    diff = abs(total_points.pythagorean - points),
    diff.sb= abs(points_pyt.sb - points)
  ) %>%
  select(team, points, points_pyt.pct, points.pct, total_points.pythagorean, diff, total.goals.scored, total.goals.conceded, everything())

# Calculate the Root Mean Squared Error (RMSE) for the Pythagorean Expectations Theorum
points_data.2019 %>%
  summarize(rmse = sqrt(mean(diff.sb^2)))

# Linear Regression Models: 
## Points Percent
##  Fit a linear regression model to predict the Pythagorean winning percentage based on goals scored and conceded
model.simple <- lm(points ~ total.goal_differential, data = points_data.2019)
performance::model_performance(model.simple)
# RMSE: 4.239 (percent)
# Statsbomb Model: 
## https://statsbomb.com/articles/soccer/improving-soccers-version-of-the-bill-james-pythagorean/
model.sb <- lm(points ~ I((2/3 * total.goal_differential) + 52.39), data = points_data.2019)
performance::model_performance(pytFit)
4.239
## Extract model statistics using broom package
broom::tidy(model, conf.int = TRUE, conf.level = 0.95)

points_data.2019 <- df.season.2019.summary %>%
  mutate(
    # Calculate the Pythagorean winning percentage using the Pythagorean formula with data from statsbomb: https://statsbomb.com/articles/soccer/improving-soccers-version-of-the-bill-james-pythagorean/
    points_pyt.pct = total.goals.scored^1.2 / (total.goals.scored^1.2 + total.goals.conceded^1.2),
    points_pyt.sb= ((2/3 * total.goal_differential) + 52.39) * (38 / 38),
    # Calculate the total points expected based on the Pythagorean winning percentage
    total_points.pythagorean = points_pyt.pct * (38 * 3),
    # Calculate the percentage difference between the actual points and Pythagorean points
    diff.pct = abs(points.pct - points_pyt.pct),
    # Calculate the absolute difference between the total Pythagorean points and actual points
    diff = abs(total_points.pythagorean - points),
    diff.test= abs(points_pyt.test - points)
  ) %>%
  select(team, points, points_pyt.pct, points.pct, total_points.pythagorean, diff, total.goals.scored, total.goals.conceded, everything())


# Calculate a better exponent
## Calculate a new Pythagorean winning percentage using an optimized exponent
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




# The corrected equation is:
# The coefficients of the equation are the result of a linear regression fit of the Pythagorean winning percentage
# to the actual points earned. The equation is of the form:
# points_pyt.pct ~ I(total.goals.scored^1.331 / (total.goals.scored^1.331 + total.goals.conceded^1.331))
# The I() function is used to ensure that the exponentiation is done element-wise.
pyfit.live <- lm(points_pyt.pct ~ I(total.goals.scored^1.331 / (total.goals.scored^1.331 + total.goals.conceded^1.331)), data = points_data.2019.new)
performance::model_performance(pyfit.live) %>%
  format(scientific = FALSE)
# The summary() function is used to get a summary of the fit of the model.
# The output includes the coefficients of the equation, the standard errors of these coefficients,
# the t-statistic, the p-value, the R-squared value, and the F-statistic.
summary(pyfit.live)

# The broom package is used to augment the data with the predicted values and residuals.
# The augment() function takes the model and the data as arguments and returns a data frame with the predicted values and residuals.
library(broom)
# The data is augmented with the predicted values and residuals.
my_teams_aug <- augment(pyfit.live, data = points_data.2019.new)

# A ggplot object is created to visualize the residuals.
# The x-axis is the run differential, and the y-axis is the residual.
# The geom_point() function is used to plot the points, and the geom_hline() function is used to add a horizontal line at y=0.
base_plot <- ggplot(my_teams_aug, aes(x = total.goal_differential, y = .resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = 3) +
  xlab("Run differential") + ylab("Residual")

# The top 4 teams with the largest residuals are highlighted.
# The arrange() function is used to sort the data by the absolute value of the residual in descending order.
# The head() function is used to select the top 4 teams.
highlight_teams <- my_teams_aug %>%
  arrange(desc(abs(.resid))) %>%
  head(4)

# The ggrepel package is used to add labels to the plot.
# The geom_text_repel() function is used to add labels to the plot.
# The aes() function is used to specify the label for each point.
# The data argument is used to specify the data for the labels.
install.packages("ggrepel")
library(ggrepel)
# The plot is modified to add the labels.
base_plot +
  geom_point(data = highlight_teams, color = "red") +
  geom_text_repel(data = highlight_teams, color = "red")+
  aes(label = paste(team))


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
  summarize(rmse = sqrt(mean(diff.pct^2)))

# Filter games decided by one run
# Subset games where the difference in points is exactly 1
df.seasons.1run_wins <- df.seasons.games %>%
  filter(abs(goal_differential) == 1) %>%
  group_by(team.year) %>%
  summarise(
    # Count the number of one run wins and losses for each team
    one_run_wins = sum(outcome == "win"),
    one_run_losses = sum(outcome == "loss")
  ) %>%
  mutate(
    one_run_wins_pct = one_run_wins / (one_run_wins + one_run_losses),
    one_run_losses_pct = one_run_losses / (one_run_wins + one_run_losses),
    one_run_wins_loss_diff = one_run_wins - one_run_losses
  ) %>% 
  ungroup() %>%
  arrange(desc(one_run_wins_loss_diff))


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


# Goal Diffential Vs Total Points
# Plot the relationship between goal differential and total points
 goal_diff <- points_data.2019 %>%
  ggplot(aes(x = total.goal_differential, y = points)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Goal Differential", y = "Total Points")




