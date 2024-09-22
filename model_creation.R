
write.csv(serieA_2023, "serieA_2023.csv")
write.csv(df.summary.all, "df.summary.all.csv")
df_long <- df.summary.short %>%
  pivot_longer(
    cols = c(starts_with("points"), starts_with("total_xg"), starts_with("total_goals")),
    names_to = c(".value", "year"),
    names_pattern = "(.+)\\.?(\\d{4})",
    values_drop_na = TRUE
  ) %>%
  mutate(year = as.integer(year))

str(df_long)

predict_next_year_points <- function(df) {
  df %>%
    arrange(team, year) %>%
    group_by(team) %>%
    mutate(
      next_year_points = lead(points.),
      prev_year_xg_for = lag(total_xg.for.),
      prev_year_xg_against = lag(total_xg.against.)
    ) %>%
    ungroup() %>%
    filter(!is.na(next_year_points) & !is.na(prev_year_xg_for) & !is.na(prev_year_xg_against))
}

df_prediction <- predict_next_year_points(df_long)

fit_xg_all <- lm(next_year_points ~ prev_year_xg_for + prev_year_xg_against, data = df_prediction)
summary(fit_xg_all)
install.packages("broom")
library(broom)

df_prediction %>%
  group_by(year) %>%
  do(model = lm(next_year_points ~ prev_year_xg_for + prev_year_xg_against, data = .)) %>%
  summarise(year = year, tidy_model = list(tidy(model)), glance_model = list(glance(model)))


# OLD
# Create data frame for linear regression of XG vs Goals
df.predict.2023 <- df.summary.short %>%
  filter(!is.na(points.2023) & !is.na(points.2022)) %>%
  select(team, starts_with("points"), starts_with("total_xg"), starts_with("total_goals"))

# Linear regression to predict next year's points from xg
fit_xg <- lm(points.2023 ~ total_xg.for.2022 * total_xg.against.2022, data = df.predict.2023)
summary(fit_xg)
# Linear regression to predict next year's points from xg for
fit_xg.for <- lm(points.2023 ~ total_xg.for.2022 , data = df.predict.2023)
summary(fit_xg.for)
# Linear regression to predict next year's points from xg agasinst
fit_xg.against <- lm(points.2023 ~ total_xg.against.2022 , data = df.predict.2023)
summary(fit_xg.against)

# Linear regression to predict next year's points from goals
fit_goals <- lm(points.2023 ~ total_goals.for.2022 + total_goals.against.2022, data = df.predict.2023)
summary(fit_goals)

# Linear regression to predict next year's points from goals for
fit_goals.for <- lm(points.2023 ~ total_goals.for.2022 , data = df.predict.2023)
summary(fit_goals.for)
# Linear regression to predict next year's points from goals against
fit_goals.against <- lm(points.2023 ~ total_goals.against.2022 , data = df.predict.2023)
summary(fit_goals.against)

fit_goals.merged <- lm(points.2023 ~ total_xg.for.2022 + total_xg.against.2022 + total_goals.for.2022 + total_goals.against.2022, data = df.predict.2023)
summary(fit_goals.merged)

# Rank models
model_summaries <- list(
  fit_xg = summary(fit_xg)$adj.r.squared,
  fit_xg.for = summary(fit_xg.for)$adj.r.squared,
  fit_xg.against = summary(fit_xg.against)$adj.r.squared,
  fit_goals = summary(fit_goals)$adj.r.squared,
  fit_goals.for = summary(fit_goals.for)$adj.r.squared,
  fit_goals.against = summary(fit_goals.against)$adj.r.squared,
  fit_goals.merged = summary(fit_goals.merged)$adj.r.squared
)

model_summaries_df <- data.frame(
  Model = names(model_summaries),
  Adj.R.Squared = unlist(model_summaries)
)
# Plot model summaries
# This graphic shows the results of several linear regression models
#   predicting next year's points from previous year's statistics. 
#    The models are ranked by their adjusted R-squared, with higher values
#    indicating better fit. The bars show the actual values of the
#    adjusted R-squared for each model. The numbers on top of the bars
#    are the actual values of the adjusted R-squared. The key takeaways
#    are:
#      - The model with both xG for and against has the highest
#        adjusted R-squared, indicating that it is the best fit.
#     - The model with only xG for has the second highest adjusted
#        R-squared, indicating that xG for is a better predictor than
#        goals for.
#      - The model with only goals against has the lowest adjusted
#        R-squared, indicating that goals against is a poor predictor
#        of next year's points.
ggplot(model_summaries_df, aes(x = reorder(Model, Adj.R.Squared), y = Adj.R.Squared)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Model", y = "Adj.R.Squared") +
  geom_text(aes(x = reorder(Model, Adj.R.Squared), y = Adj.R.Squared + 0.02, label = Adj.R.Squared), size = 3, check_overlap = TRUE)


# Predict next year's points
df.predict <- df.summary.short %>%
  mutate(predicted_points_xg = predict(fit_xg, .),
         predicted_points_goals = predict(fit_goals, .))

# Plot actual points against predicted points
ggplot(df.predict, aes(x = points.2023, y = predicted_points_xg, color = "xg")) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(y = predicted_points_goals, color = "goals")) +
  labs(x = "Actual Points", y = "Predicted Points", color = "Statistic") +
  theme(legend.position = "bottom")

install.packages("mgcv")
library(mgcv)
fit_gam <- gam(points.2023 ~ s(total_xg.for.2022) + s(total_xg.against.2022), 
               data = df.predict.2023)
summary(fit_gam)
install.packages("randomForest")
library(randomForest)
fit_rf <- randomForest(points.2023 ~ total_xg.for.2022 + total_xg.against.2022, 
                       data = df.predict.2023)
summary(fit_rf)

install.packages("splines")
library(splines)
fit_spline <- lm(points.2023 ~ bs(total_xg.for.2022, df = 3) + 
                 bs(total_xg.against.2022, df = 3), 
                 data = df.predict.2023)
summary(fit_spline)

fit_log <- lm(log(points.2023) ~ log(total_xg.for.2022) + log(total_xg.against.2022), 
              data = df.predict.2023)
summary(fit_log)