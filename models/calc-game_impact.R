library(tidyverse)

calculate_game_importance <- function(df, champions_spots, europa_spots, relegation_spots, total_games = 38) {
  df <- df %>% 
    mutate(Wk = as.numeric(Wk))

  team_stats <- df %>%
    arrange(Season_End_Year, Wk) %>%
    group_by(Season_End_Year) %>%
    mutate(
      games_left = total_games - Wk,
      total_teams = n_distinct(Home)
    ) %>%
    pivot_longer(cols = c(Home, Away), names_to = "home_away", values_to = "Team") %>%
    mutate(
      points = case_when(
        home_away == "Home" & HomeGoals > AwayGoals ~ 3,
        home_away == "Away" & AwayGoals > HomeGoals ~ 3,
        HomeGoals == AwayGoals ~ 1,
        TRUE ~ 0
      ),
      game_gd = if_else(home_away == "Home", HomeGoals - AwayGoals, AwayGoals - HomeGoals)
    ) %>%
    group_by(Season_End_Year, Team) %>%
    arrange(Season_End_Year, Team, Wk) %>%
    mutate(
      total_points = cumsum(points),
      total_gd = cumsum(game_gd),
      points_at_start = lag(total_points, default = 0),
      gd_at_start = lag(total_gd, default = 0)
    ) %>%
    ungroup()

  weekly_rankings <- team_stats %>%
    group_by(Season_End_Year, Wk) %>%
    mutate(
      max_points = points_at_start + (games_left * 3),
      min_points = points_at_start
    ) %>%
    arrange(Season_End_Year, Wk, desc(points_at_start), desc(gd_at_start), desc(max_points)) %>%
    mutate(
      rank = row_number(),
      leader_points = max(points_at_start),
      second_max_points = sort(max_points, decreasing = TRUE)[2],
      min_champions_points = nth(max_points, champions_spots + 1),
      min_europa_points = nth(max_points, champions_spots + europa_spots + 1),
      max_relegation_points = sort(min_points, decreasing = TRUE)[n() - relegation_spots + 1],
      can_win_league = max_points >= leader_points,
      can_reach_champions = max_points >= min_champions_points,
      can_reach_europa = max_points >= min_europa_points,
      can_be_relegated = min_points <= max_relegation_points,
      
      # New columns for locking positions
      locked_league_win = points_at_start > second_max_points,
      locked_champions = rank <= champions_spots & min_points > nth(max_points, champions_spots + 1),
      locked_europa = rank <= (champions_spots + europa_spots) & min_points > nth(max_points, champions_spots + europa_spots + 1),
      locked_relegation = rank > (n() - relegation_spots) & max_points < nth(sort(min_points, decreasing = TRUE), n() - relegation_spots)
    ) %>%
    group_by(Season_End_Year, Team) %>%
    mutate(
      locked_league_win = cummax(as.integer(locked_league_win)),
      locked_champions = cummax(as.integer(locked_champions)),
      locked_europa = cummax(as.integer(locked_europa)),
      locked_relegation = cummax(as.integer(locked_relegation))
    ) %>%
    ungroup()

  df_with_rankings <- df %>%
    left_join(weekly_rankings %>% select(Season_End_Year, Wk, Team, points_at_start, max_points, 
                                         can_win_league, can_reach_champions, can_reach_europa, can_be_relegated,
                                         locked_league_win, locked_champions, locked_europa, locked_relegation),
              by = c("Season_End_Year", "Wk", "Home" = "Team")) %>%
    rename_with(~paste0("home_", .), .cols = c(points_at_start, max_points, 
                                               can_win_league, can_reach_champions, can_reach_europa, can_be_relegated,
                                               locked_league_win, locked_champions, locked_europa, locked_relegation)) %>%
    left_join(weekly_rankings %>% select(Season_End_Year, Wk, Team, points_at_start, max_points, 
                                         can_win_league, can_reach_champions, can_reach_europa, can_be_relegated,
                                         locked_league_win, locked_champions, locked_europa, locked_relegation),
              by = c("Season_End_Year", "Wk", "Away" = "Team")) %>%
    rename_with(~paste0("away_", .), .cols = c(points_at_start, max_points, 
                                               can_win_league, can_reach_champions, can_reach_europa, can_be_relegated,
                                               locked_league_win, locked_champions, locked_europa, locked_relegation))

  df_with_importance <- df_with_rankings %>%
    mutate(
      league_conseq = case_when(
        (home_can_win_league & home_locked_league_win == 0) | (away_can_win_league & away_locked_league_win == 0) |
        (home_can_reach_champions & home_locked_champions == 0) | (away_can_reach_champions & away_locked_champions == 0) |
        (home_can_reach_europa & home_locked_europa == 0) | (away_can_reach_europa & away_locked_europa == 0) |
        (home_can_be_relegated & home_locked_relegation == 0) | (away_can_be_relegated & away_locked_relegation == 0) ~ 1,
        TRUE ~ 0
      ),
      league_conseq.home = case_when(
        (home_can_win_league & home_locked_league_win == 0) | (home_can_reach_champions & home_locked_champions == 0) |
        (home_can_reach_europa & home_locked_europa == 0) | (home_can_be_relegated & home_locked_relegation == 0) ~ 1,
        TRUE ~ 0
      ),
      league_conseq.away = case_when(
        (away_can_win_league & away_locked_league_win == 0) | (away_can_reach_champions & away_locked_champions == 0) |
        (away_can_reach_europa & away_locked_europa == 0) | (away_can_be_relegated & away_locked_relegation == 0) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(Season_End_Year, Wk, Home, Away, league_conseq, Competition_Name, everything())

  return(df_with_importance)
}

# Example usage:
df_with_importance <- calculate_game_importance(df.season.2019, champions_spots = 4, europa_spots = 2, relegation_spots = 3) %>%
  select(Wk,Home,Away,starts_with("league_conseq"),starts_with("home_can"),starts_with("away_can"),everything())
write.csv(df_with_importance, "df.importance.csv", row.names = FALSE)