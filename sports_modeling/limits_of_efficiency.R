var.19 <- df.season.2019.summary %>%
  group_by(team) %>%
  mutate(team,
    win_pct = wins / 38,
    draw_pct = draws / 38,
    loss_pct = losses / 38,
    team_variance = (win_pct * (1 - win_pct) + 
      draw_pct * (1 - draw_pct) + 
      loss_pct * (1 - loss_pct)) / 38
) %>%
  select(team, win_pct, draw_pct, loss_pct,team_variance)

league_stats <- var.19 %>%
  summarise(
    var_observed = var(win_pct),
    var_randomness = mean(win_pct * (1 - win_pct) + 
                          draw_pct * (1 - draw_pct) + 
                          loss_pct * (1 - loss_pct)) / 38,
    var_skill = var_observed - var_randomness,
    skill_percentage = (var_skill / var_observed) * 100
  )

var.19 %>%
  select(team, win_pct, draw_pct, loss_pct) %>%
  View()