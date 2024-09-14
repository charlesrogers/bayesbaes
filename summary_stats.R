# This function takes a dataframe and a year and creates two data frames for home teams and away teams, with the
# following columns:
# - matchday_points: the number of points each team scored in each matchday (3 for a win, 1 for a draw, 0 for a loss)
# - home/away_goals.scored: the average number of goals scored by each team in their home/away games
# - home/away_goals.conceded: the average number of goals conceded by each team in their home/away games
# - home/away_xg.produced: the average number of expected goals scored by each team in their home/away games
# - home/away_xg.conceded: the average number of expected goals conceded by each team in their home/away games

# The data frame is then grouped by team name, and the summary
# statistics are calculated

# The full_join function is then used to join the two data frames on
# the team names, and the final data frame is created
create_summary_stats <- function(data, year) {
  df.home <- data %>%
    select(Home, Away, HomeGoals, AwayGoals, Home_xG, Away_xG) %>%
    mutate(matchday_points.home = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals == AwayGoals, 1, 0))) %>%
    group_by(Home) %>%
    summarise(
      home_points = sum(matchday_points.home),
      home_goals.scored = mean(HomeGoals, na.rm = TRUE),
      home_goals.conceded = mean(AwayGoals, na.rm = TRUE),
      home_xg.for = sum(Home_xG,na.rm = TRUE),
      home_xg.against = sum(Away_xG,na.rm = TRUE),
      ave_home_xg.for = mean(Home_xG,na.rm = TRUE),
      ave_home_xg.against = mean(Away_xG,na.rm = TRUE)
    ) 

  df.away <- data %>%
    select(Home, Away, HomeGoals, AwayGoals, Home_xG, Away_xG) %>%
    mutate(matchday_points.away = ifelse(AwayGoals > HomeGoals, 3, ifelse(HomeGoals == AwayGoals, 1, 0))) %>%
    group_by(Away) %>%
    summarise(
      away_points = sum(matchday_points.away),
      away_goals.scored = mean(AwayGoals, na.rm = TRUE),
      away_goals.conceded = mean(HomeGoals, na.rm = TRUE),
      away_xg.for = sum(Away_xG,na.rm = TRUE),
      away_xg.against = sum(Away_xG,na.rm = TRUE),
      ave_away_xg.for = mean(Away_xG,na.rm = TRUE),
      ave_away_xg.against = mean(Home_xG,na.rm = TRUE)
    )

  df.final <- df.home %>%
    full_join(df.away, by = c("Home" = "Away")) %>%
    mutate(total_goals.for = home_goals.scored + away_goals.scored,
           total_goals.against = home_goals.conceded + away_goals.conceded,
      average_goals.scored = (home_goals.scored + away_goals.scored) / 2,
           average_goals.conceded = (home_goals.conceded + away_goals.conceded) / 2,
           average_xg.for = (home_xg.for + away_xg.for) / 2,
           average_xg.against = (home_xg.against + away_xg.against) / 2,
          points = home_points + away_points,
          average_points = (home_points + away_points) / 38,
          total_xg.for = home_xg.for + away_xg.for,
          total_xg.against = home_xg.against + away_xg.against,
          year = as.integer(year),
          rank = rank(desc(points), ties.method = "min"),
          relegated = ifelse(rank %in% 18:20, 1, 0)
    ) %>%
    rename(team = Home) 
    
  return(df.final)
}

df.serieA_summary.2019 <- create_summary_stats(serieA_2019, 2019)
df.serieA_summary.2020 <- create_summary_stats(serieA_2020, 2020)
df.serieA_summary.2021 <- create_summary_stats(serieA_2021, 2021)
df.serieA_summary.2022 <- create_summary_stats(serieA_2022, 2022)
df.serieA_summary.2023 <- create_summary_stats(serieA_2023, 2023)

create_serieA_summary <- function(data_list) {
  df.full <- do.call(rbind, data_list)
  df.full %>% 
    pivot_longer(cols = -c(team, year), names_to = "statistic", values_to = "value") %>% 
    pivot_wider(names_from = c(statistic, year), values_from = value, names_sep = ".")
}

df.summary.short <- create_serieA_summary(list(df.serieA_summary.2019, df.serieA_summary.2020, df.serieA_summary.2021, df.serieA_summary.2022, df.serieA_summary.2023)) %>%
  select(team,starts_with("points"),starts_with("total_goals"), starts_with("total_xg"))


# Create data frame for linear regression
df.predict <- df.summary.short %>%
  mutate(year = as.numeric(year)) %>%
  filter(year < 2023) %>%
  select(team, year, starts_with("total_xg"), starts_with("total_goals")) %>%
  pivot_longer(cols = -c(team, year), names_to = "statistic", values_to = "value") %>%
  mutate(year = year + 1)

# Linear regression to predict next year's points from xg
fit_xg <- lm(points.2023 ~ total_xg.for.2022 + total_xg.against.2022, data = df.summary.short)
summary(fit_xg)

# Linear regression to predict next year's points from goals scored
fit_goals <- lm(points.2023 ~ total_goals.for.2022 + total_goals.against.2022, data = df.summary.short)
summary(fit_goals)

fit_goals.merged <- lm(points.2023 ~ total_xg.for.2022 + total_xg.against.2022+total_goals.for.2022 + total_goals.against.2022, data = df.summary.short)
summary(fit_goals.merged)


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

