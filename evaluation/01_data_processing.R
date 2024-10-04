# 01_data_processing.R

# Load required libraries
library(tidyverse)

# Function to read and preprocess historical game data
read_historical_data <- function(file_path) {
  # Read the CSV file
  data <- read_csv(file_path)
  
  # Perform basic checks on the data
  if (nrow(data) == 0) {
    stop("The historical data file is empty.")
  }
  
  # Check if required columns are present
  required_columns <- c("date", "home_team", "away_team", "home_score", "away_score")
  if (!all(required_columns %in% colnames(data))) {
    stop("The historical data file is missing required columns.")
  }
  
  # Convert date column to Date type
  data <- data %>%
    mutate(date = as.Date(date))
  
  # Calculate win/loss for each game
  data <- data %>%
    mutate(
      home_win = ifelse(home_score > away_score, 1, 0),
      away_win = ifelse(away_score > home_score, 1, 0)
    )
  
  # Calculate team performance metrics
  team_performance <- data %>%
    gather(key = "team_type", value = "team", home_team, away_team) %>%
    group_by(team) %>%
    summarise(
      games_played = n(),
      wins = sum(ifelse(team_type == "home_team", home_win, away_win)),
      losses = sum(ifelse(team_type == "home_team", away_win, home_win)),
      win_rate = wins / games_played
    )
  
  # Join team performance back to the original data
  data <- data %>%
    left_join(team_performance, by = c("home_team" = "team")) %>%
    rename(home_win_rate = win_rate) %>%
    left_join(team_performance, by = c("away_team" = "team")) %>%
    rename(away_win_rate = win_rate)
  
  return(data)
}

# Function to input new game details
input_new_game <- function() {
  # Prompt user for input
  cat("Enter details for the new game:\n")
  home_team <- readline("Home Team: ")
  away_team <- readline("Away Team: ")
  game_date <- readline("Game Date (YYYY-MM-DD): ")
  
  # Create a tibble with the input data
  new_game <- tibble(
    date = as.Date(game_date),
    home_team = home_team,
    away_team = away_team
  )
  
  return(new_game)
}

# Example usage:
# historical_data <- read_historical_data("data/historical_games.csv")
# new_game <- input_new_game()