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
# create_summary_stats <- function(data, year) {
#   df.home <- data %>%
#     select(Home, Away, HomeGoals, AwayGoals, Home_xG, Away_xG) %>%
#     mutate(matchday_points.home = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals == AwayGoals, 1, 0))) %>%
#     group_by(Home) %>%
#     summarise(
#       home_points = sum(matchday_points.home),
#       home_goals.scored = mean(HomeGoals, na.rm = TRUE),
#       home_goals.conceded = mean(AwayGoals, na.rm = TRUE),
#       home_xg.for = sum(Home_xG,na.rm = TRUE),
#       home_xg.against = sum(Away_xG,na.rm = TRUE),
#       ave_home_xg.for = mean(Home_xG,na.rm = TRUE),
#       ave_home_xg.against = mean(Away_xG,na.rm = TRUE)
#     ) 

#   df.away <- data %>%
#     select(Home, Away, HomeGoals, AwayGoals, Home_xG, Away_xG) %>%
#     mutate(matchday_points.away = ifelse(AwayGoals > HomeGoals, 3, ifelse(HomeGoals == AwayGoals, 1, 0))) %>%
#     group_by(Away) %>%
#     summarise(
#       away_points = sum(matchday_points.away),
#       away_goals.scored = mean(AwayGoals, na.rm = TRUE),
#       away_goals.conceded = mean(HomeGoals, na.rm = TRUE),
#       away_xg.for = sum(Away_xG,na.rm = TRUE),
#       away_xg.against = sum(Away_xG,na.rm = TRUE),
#       ave_away_xg.for = mean(Away_xG,na.rm = TRUE),
#       ave_away_xg.against = mean(Home_xG,na.rm = TRUE)
#     )

#   df.final <- df.home %>%
#     full_join(df.away, by = c("Home" = "Away")) %>%
#     mutate(total_goals.for = home_goals.scored + away_goals.scored,
#            total_goals.against = home_goals.conceded + away_goals.conceded,
#       average_goals.scored = (home_goals.scored + away_goals.scored) / 2,
#            average_goals.conceded = (home_goals.conceded + away_goals.conceded) / 2,
#            average_xg.for = (home_xg.for + away_xg.for) / 2,
#            average_xg.against = (home_xg.against + away_xg.against) / 2,
#           points = home_points + away_points,
#           average_points = (home_points + away_points) / 38,
#           total_xg.for = home_xg.for + away_xg.for,
#           total_xg.against = home_xg.against + away_xg.against,
#           year = as.integer(year),
#           rank = rank(desc(points), ties.method = "min"),
#           relegated = ifelse(rank %in% 18:20, 1, 0)
#     ) %>%
#     rename(team = Home) 
    
#   return(df.final)
# }



# df.serieA_summary.2019 <- create_summary_stats(serieA_2019, 2019)
# df.serieA_summary.2020 <- create_summary_stats(serieA_2020, 2020)
# df.serieA_summary.2021 <- create_summary_stats(serieA_2021, 2021)
# df.serieA_summary.2022 <- create_summary_stats(serieA_2022, 2022)
# df.serieA_summary.2023 <- create_summary_stats(serieA_2023, 2023)

df.seasons.games <- df.seasons %>% 
  select(Season_End_Year, Wk, Home, Away, HomeGoals, AwayGoals) %>%
  rename(Year=Season_End_Year, Week=Wk) %>%
  pivot_longer(
    cols = c(Home, Away),
    names_to = "location",
    values_to = "team"
  ) %>% 
    mutate(
      team.year=paste(team,Year,sep = "."),
      location = if_else(location == "Home", "home", "away"),
      goals_for = if_else(location == "home", HomeGoals, AwayGoals),
      goals_against = if_else(location == "home", AwayGoals, HomeGoals),
      goal_differential = goals_for - goals_against,
      outcome=if_else(goal_differential > 0, "win", if_else(goal_differential < 0 , "loss", "tie"))
    ) %>% 
  select(-c(HomeGoals,AwayGoals))

# Display the first 10 rows of the reshaped data
reshaped_df %>% head(n = 10)
# Create Season Dataframes
df.season.2019 <- df.seasons %>%
  filter(Season_End_Year %in% c("2019"))

df.season.2020 <- df.seasons %>%
  filter(Season_End_Year %in% c("2020"))

df.season.2021 <- df.seasons %>%
  filter(Season_End_Year %in% c("2021"))


df.season.2022 <- df.seasons %>%
  filter(Season_End_Year %in% c("2022"))

df.season.2023 <- df.seasons %>%
  filter(Season_End_Year %in% c("2023"))

# Full dataset
rbind()

df.seasons %>%
  group_by(Home,Season_End_Year) %>%
  mutate(matchday_points.home = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals == AwayGoals, 1, 0))) %>%
  summarise(wins = sum(matchday_points.home==3)) %>%
  View()

create_summary_stats <- function(data,Year) {

  df.home <- data %>%
    select(Home, Away, HomeGoals, AwayGoals, Home_xG, Away_xG) %>%
    mutate(matchday_points.home = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals == AwayGoals, 1, 0))) %>%
    group_by(Home) %>%
    summarise(
      home_points = sum(matchday_points.home),
      home_goals.scored.total = sum(HomeGoals, na.rm = TRUE),
      home_goals.conceded.total = sum(AwayGoals, na.rm = TRUE),
      home_goals.scored = mean(HomeGoals, na.rm = TRUE),
      home_goals.conceded = mean(AwayGoals, na.rm = TRUE),
      home_xg.for = sum(Home_xG,na.rm = TRUE),
      home_xg.against = sum(Away_xG,na.rm = TRUE),
      ave_home_xg.for = mean(Home_xG,na.rm = TRUE),
      ave_home_xg.against = mean(Away_xG,na.rm = TRUE),
      home_wins = sum(matchday_points.home==3),
      home_draws = sum(matchday_points.home==1),
      home_losses = sum(matchday_points.home==0)
    ) 

  df.away <- data %>%
    select(Home, Away, HomeGoals, AwayGoals, Home_xG, Away_xG) %>%
    mutate(matchday_points.away = ifelse(AwayGoals > HomeGoals, 3, ifelse(HomeGoals == AwayGoals, 1, 0))) %>%
    group_by(Away) %>%
    summarise(
      away_points = sum(matchday_points.away),
      away_goals.scored.total = sum(AwayGoals, na.rm = TRUE),
      away_goals.conceded.total = sum(HomeGoals, na.rm = TRUE),
      away_goals.scored = mean(AwayGoals, na.rm = TRUE),
      away_goals.conceded = mean(HomeGoals, na.rm = TRUE),
      away_xg.for = sum(Away_xG,na.rm = TRUE),
      away_xg.against = sum(Away_xG,na.rm = TRUE),
      ave_away_xg.for = mean(Away_xG,na.rm = TRUE),
      ave_away_xg.against = mean(Home_xG,na.rm = TRUE),
      away_wins = sum(matchday_points.away==3),
      away_draws = sum(matchday_points.away==1),
      away_losses = sum(matchday_points.away==0)
    )

  df.final <- df.home %>%
    full_join(df.away, by = c("Home" = "Away")) %>%
    mutate(
      total.goals.scored = home_goals.scored.total + away_goals.scored.total,
      total.goals.conceded = home_goals.conceded.total + away_goals.conceded.total,
      total.goal_differential = total.goals.scored - total.goals.conceded,
      goals.for.average = home_goals.scored + away_goals.scored,
           goals.against.average = home_goals.conceded + away_goals.conceded,
           average_goals.scored = (home_goals.scored + away_goals.scored) / 2,
           average_goals.conceded = (home_goals.conceded + away_goals.conceded) / 2,
           average_xg.for = (home_xg.for + away_xg.for) / 2,
           average_xg.against = (home_xg.against + away_xg.against) / 2,
           points = home_points + away_points,
           points.pct=points/(38*3),
           average_points = (home_points + away_points) / 38,
           total_xg.for = home_xg.for + away_xg.for,
           total_xg.against = home_xg.against + away_xg.against,
           Year = as.integer(Year),
           rank = rank(desc(points), ties.method = "min"),
           relegated = ifelse(rank %in% 18:20, 1, 0),
           wins = home_wins + away_wins,
           draws = home_draws + away_draws,
           losses = home_losses + away_losses
    ) %>%
   rename(team = Home) 
    
  return(df.final)
}

df.season.2019.summary <- create_summary_stats(df.season.2019,"2019")
df.season.2020.summary <- create_summary_stats(df.season.2020,"2020")
df.season.2021.summary <- create_summary_stats(df.season.2021,"2021")
df.season.2022.summary <- create_summary_stats(df.season.2022,"2022")
df.season.2023.summary <- create_summary_stats(df.season.2023,"2023")

create_serieA_summary <- function(data_list) {
  df.full <- do.call(rbind, data_list)
  df.full %>% 
    pivot_longer(cols = -c(team, Year), names_to = "statistic", values_to = "value") %>% 
    pivot_wider(names_from = c(statistic, Year), values_from = value, names_sep = ".")
}

df.isa.summary.19_23 <- create_serieA_summary(list(df.season.2019.summary, df.season.2020.summary, df.season.2021.summary, df.season.2022.summary,df.season.2023.summary)) %>%
  select(team,starts_with("points"),starts_with("total_goals"), starts_with("total_xg"),starts_with("wins"),starts_with("draws"),starts_with("losses"),everything())

create_serieA_summary <- function(data_list) {
  df.full <- do.call(rbind, data_list)
  df.full %>% 
    pivot_longer(cols = -c(team, Year), names_to = "statistic", values_to = "value") %>% 
    pivot_wider(names_from = c(statistic, Year), values_from = value, names_sep = ".")
}


