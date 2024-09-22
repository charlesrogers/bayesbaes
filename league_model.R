aggregated_goals.2019 <- dbGetQuery(isa.db, "SELECT * FROM season_games WHERE  Season_End_Year = 2019")


aggregated_goals.2019 <- aggregated_goals.2019 %>% select(-...1, -X)
aggregated_goals.2019 <- aggregated_goals.2019 %>% mutate(date = ymd(date))

teams <- unique(c(aggregated_goals.2019$Home, aggregated_goals.2019$Away))
team_ids <- setNames(seq_along(teams), teams)

aggregated_goals.2019 <- aggregated_goals.2019 %>% mutate(
  home_team_id = team_ids[Home],
  away_team_id = team_ids[Away]
)

print(aggregated_goals.2019)

# Calculate aggregated goals for and goals against by team
aggregated_team_stats.2019 <- aggregated_goals.2019 %>%
  group_by(Home) %>%
  summarise(
    goals_for = sum(HomeGoals),
    goals_against = sum(AwayGoals)
  ) %>%
  rename(team = Home) %>%
  bind_rows(
    aggregated_goals.2019 %>%
      group_by(Away) %>%
      summarise(
        goals_for = sum(AwayGoals),
        goals_against = sum(HomeGoals)
      ) %>%
      rename(team = Away)
  ) %>%
  group_by(team) %>%
  summarise(
    goals_for = sum(goals_for),
    goals_against = sum(goals_against)
  ) %>%
  mutate(
    diff = goals_for - goals_against,
    diff_g = diff / 38
  )
# Function to calculate win, tie, and loss probabilities using Skellam distribution
calculate_probabilities <- function(goals_for, goals_against) {
  lambda1 <- goals_for / 38 # average goals scored per game
  lambda2 <- goals_against / 38 # average goals conceded per game

  win_prob <- 1 - pskellam(0, lambda1, lambda2) # P(X > 0)
  tie_prob <- dskellam(0, lambda1, lambda2) # P(X = 0)
  loss_prob <- pskellam(-1, lambda1, lambda2) # P(X < 0)

  return(c(win_prob, tie_prob, loss_prob))
}

aggregated_team_stats.2019 <- aggregated_team_stats.2019 %>%
  rowwise() %>%
  mutate(probabilities = list(calculate_probabilities(goals_for, goals_against))) %>%
  mutate(
    win_prob = probabilities[1],
    tie_prob = probabilities[2],
    loss_prob = probabilities[3]
  ) %>%
  select(-probabilities)

league_avg_goals_for <- mean(aggregated_team_stats.2019$goals_for) / 38
league_avg_goals_against <- mean(aggregated_team_stats.2019$goals_against) / 38


# Add expected goals for and against
aggregated_team_stats.2019 <- aggregated_team_stats.2019 %>%
  mutate(
    exp_goals_for = league_avg_goals_for + diff_g / 2,
    exp_goals_ag = league_avg_goals_against - diff_g / 2
  )

# Print aggregated team goal stats
print(aggregated_team_stats.2019)

#### 3. Plot initial team MOV rankings ####
ggplot(aggregated_team_stats.2019, aes(x = reorder(team, diff_g), y = diff_g, fill = diff_g)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c(option = "G", direction = -1) +
  coord_flip() +
  labs(x = "Team", y = "Goal Difference Per Game", fill = "Goal Difference") +
  theme_minimal() +
  theme(legend.position = "none")

#### 4. Base model ####
# Prepare data for Stan
stan_data <- list(
  N = nrow(aggregated_goals.2019),
  T = length(unique(c(aggregated_goals.2019$home_team_id, aggregated_goals.2019$away_team_id))),
  home_team = aggregated_goals.2019$home_team_id,
  away_team = aggregated_goals.2019$away_team_id,
  home_goals = aggregated_goals.2019$HomeGoals,
  away_goals = aggregated_goals.2019$AwayGoals
)


# Specify Stan model
stan_model_code <- "
data {
    int<lower=1> N;             // Number of games
    int<lower=1> T;             // Number of teams
    int home_team[N];           // Home team index
    int away_team[N];           // Away team index
    int home_goals[N];          // Goals home
    int away_goals[N];          // Goals away
}
parameters {
    real home_advantage;        // Home advantage
    real goal_mean;             // Goal mean
    real<lower=0> sigma_att;    // OFF sigma
    real<lower=0> sigma_def;    // DEF sigma
    vector[T] att_raw;          // Raw OFF
    vector[T] def_raw;          // Raw DEF
}
transformed parameters {
    vector[T] att;              // centered OFF
    vector[T] def;              // centered DEF

    // Center OFF & DEF to have mean zero
    att = att_raw - mean(att_raw);
    def = def_raw - mean(def_raw);
}
model {
    // Priors for global parameters
    home_advantage ~ normal(0, 1);  // Home advantage prior
    goal_mean ~ normal(0, 1);       // Goal mean prior

    // Priors for standard deviations
    sigma_att ~ normal(0, 1);       // Sigma OFF prior
    sigma_def ~ normal(0, 1);       // Sigma DEF prior

    // Priors for team abilities
    att_raw ~ normal(0, sigma_att); // Raw OFF prior
    def_raw ~ normal(0, sigma_def); // Raw DEF prior

    // Likelihood
    home_goals ~ poisson_log(home_advantage + att[home_team] + def[away_team] + goal_mean);
    away_goals ~ poisson_log(att[away_team] - def[home_team] + goal_mean);
}
"

# Fit Stan model with mcmc
fit <- stan(
  model_code = stan_model_code,
  data = stan_data,
  iter = 4000,
  warmup = 1000,
  chains = 2,
  cores = 6,
  seed = 1,
  init = "random",
  control = list(max_treedepth = 10)
)

# Print Stan fit
print(fit)

# Print selected parameter trace plots
traceplot(fit, pars = c("home_advantage", "goal_mean"))

#### 5. Plot estimated posterior team strength ####
# Extract parameters from the fitted model
posterior <- rstan::extract(fit)

# Calculate aggregate team strengths on neutral ice
agg_strength <- posterior$att - posterior$def


# Melt the posterior samples into long-format data frame
agg_strength_df <- melt(agg_strength)
colnames(agg_strength_df) <- c("Iteration", "Team", "Strength")

# Get the team names from the team_ids
team_names <- names(team_ids)

# Order team names alphabetically
ordered_team_names <- sort(team_names)

# Create a mapping from original team indices to alphabetical order
team_mapping <- match(team_names, ordered_team_names)


# Update the Team column in agg_strength_df to use the new alphabetical order
agg_strength_df$Team <- factor(team_mapping[agg_strength_df$Team],
  levels = 1:length(ordered_team_names),
  labels = ordered_team_names
)
# Plot the distributions for all teams
ggplot(agg_strength_df, aes(x = Strength, fill = Team)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~Team, scales = "free", labeller = labeller(Team = label_value)) +
  xlab("Aggregate Strength") +
  ylab("Density") +
  scale_fill_viridis_d(option = "C", direction = 1) +
  theme_minimal() +
  theme(legend.position = "none")

# Plots for 4 selected teams
# Extract posterior samples
posterior <- rstan::extract(fit)

# Calculate aggregate team strengths on neutral ice
agg_strength <- posterior$att - posterior$def

# Melt the posterior samples into long-format data frame
agg_strength_df <- melt(agg_strength)
colnames(agg_strength_df) <- c("Iteration", "Team", "Strength")

# Get the team names from the 'team_ids'
team_names <- names(team_ids)

# Select specific teams
selected_teams <- c("Frosinone", "Napoli", "Juventus", "Inter")

# Ensure selected teams exist in the team_ids
selected_team_ids <- team_ids[selected_teams]

# Filter the data frame for selected teams
agg_strength_df <- subset(agg_strength_df, Team %in% selected_team_ids)

# Map team IDs to names directly
agg_strength_df$Team <- factor(agg_strength_df$Team, levels = selected_team_ids, labels = selected_teams)

# Order team names alphabetically for the plot labels
agg_strength_df <- agg_strength_df[order(agg_strength_df$Team), ]

# Plot the distributions for the 4 selected teams
ggplot(agg_strength_df, aes(x = Strength, fill = Team)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~Team, scales = "free", labeller = labeller(Team = label_value)) +
  xlab("Aggregate Strength") +
  ylab("Density") +
  scale_fill_viridis_d(option = "C", direction = 1) +
  theme_minimal() +
  theme(legend.position = "none")



#### 6. Model upgrades: even strength + special teams ####
# Prepare data for Stan
stan_data <- list(
  N = nrow(aggregated_goals.2019),
  T = length(unique(c(aggregated_goals.2019$home_team_id, aggregated_goals.2019$away_team_id))),
  home_team = aggregated_goals.2019$home_team_id,
  away_team = aggregated_goals.2019$away_team_id,
  home_even_goals = aggregated_goals.2019$HomeGoals,
  away_even_goals = aggregated_goals.2019$AwayGoals
)

# Specify Stan model
stan_model_code <- "
data {
  int<lower=1> N;                 // Number of games
  int<lower=1> T;                 // Number of teams
  int home_team[N];               // Home team index
  int away_team[N];               // Away team index
  int home_even_goals[N];         // ES goals home
  int away_even_goals[N];         // ES goals away
}
parameters {
  real home_advantage;            // Home advantage
  real goal_mean;                 // Goal mean
  real<lower=0> sigma_att_even;   // Sigma ES OFF
  real<lower=0> sigma_def_even;   // Sigma ES DEF
  vector[T] att_raw_even;         // Raw ES OFF
  vector[T] def_raw_even;         // Raw ES DEF
}
transformed parameters {
  vector[T] att_even;             // centered ES OFF
  vector[T] def_even;             // centered ES DEF

  // Center OFF & DEF to have mean zero
  att_even = att_raw_even - mean(att_raw_even);
  def_even = def_raw_even - mean(def_raw_even);
}
model {
  // Priors for global parameters
  home_advantage ~ normal(0, 0.1); // Home advantage prior
  goal_mean ~ normal(0, 1);

  // Priors for standard deviations
  sigma_att_even ~ normal(0, 1);  // Sigma ES OFF prior
  sigma_def_even ~ normal(0, 1);  // Sigma ES DEF prior

  // Priors for team abilities
  att_raw_even ~ normal(0, sigma_att_even);   // Raw ES OFF prior
  def_raw_even ~ normal(0, sigma_def_even);   // Raw ES DEF prior

// Likelihood

  // ES
  home_even_goals ~ poisson_log(home_advantage + att_even[home_team] + def_even[away_team] + goal_mean);
  away_even_goals ~ poisson_log(att_even[away_team] + def_even[home_team] + goal_mean);
}

"

# Fit Stan model with mcmc
fit <- stan(
  model_code = stan_model_code,
  data = stan_data,
  iter = 10000,
  warmup = 1000,
  chains = 2,
  cores = 6,
  seed = 1,
  init = "random",
  control = list(max_treedepth = 12))


  # Print summary of the fit
print(fit)

# Print selected parameter trace plots
traceplot(fit, pars = c("home_advantage", "goal_mean"))

#### 7. Plot estimated posterior team strength ####
# Extract parameters from the fitted model
posterior <- rstan::extract(fit)


# Calculate aggregate team strengths on neutral ice for even strength and special teams
agg_strength_even <- posterior$att_even - posterior$def_even


# Melt the posterior samples into long-format data frames
agg_strength_even_df <- melt(agg_strength_even)
colnames(agg_strength_even_df) <- c("Iteration", "Team", "Strength")
agg_strength_even_df$Type <- "Even Strength"



# Get the team names from the 'team_ids'
team_names <- names(team_ids)  # Assuming 'team_ids' contains team names as names

# Order team names alphabetically
ordered_team_names <- sort(team_names)

# Create a mapping from original team indices to alphabetical order
team_mapping <- match(team_names, ordered_team_names)

# Update the Team column in agg_strength_df to use the new alphabetical order
agg_strength_df$Team <- factor(team_mapping[agg_strength_df$Team], levels = 1:length(ordered_team_names), labels = ordered_team_names)

agg_strength_df <- agg_strength_df %>%
  mutate(Type="Even Strength")

# Plots for all teams
ggplot(agg_strength_df, aes(x = Strength, fill = Type)) +
    geom_density(alpha = 0.5, position = "identity") +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    facet_wrap(~ Team, scales = "free", labeller = labeller(Team = label_value)) +
    xlab("Aggregate Strength") +
    ylab("Density") +
    scale_fill_viridis_d(option = "E", direction = 1) +
    theme_minimal() +
    theme(legend.position = "right")



#### 12. Simulating future games ####
# Extract parameters from the fitted model
posterior <- rstan::extract(fit)

# Create parameters list containing full posterior samples
params <- list(
    att_even = posterior$att_even,
    def_even = posterior$def_even,
    home_advantage = posterior$home_advantage,
    goal_mean = posterior$goal_mean)

# Team names and ids
teams <- unique(c(aggregated_goals$home_team, aggregated_goals$away_team))
team_ids <- setNames(1:length(teams), teams)

# Function to simulate goals scored by a team
simulate_goals <- function(home, att, def, home_advantage, goal_mean) {
    log_lambda <- att + def + ifelse(home == 1, home_advantage, 0) + goal_mean
    lambda <- exp(log_lambda)
    goals <- rpois(length(lambda), lambda = lambda)
    return(goals)
}

# Function to simulate home team goals
simulate_home_goals <- function(home_team_id, away_team_id, params, n_simulations = 10000) {
    idx <- sample(1:length(params$home_advantage), n_simulations, replace = TRUE)
    home_advantage <- params$home_advantage[idx]
    goal_mean <- params$goal_mean[idx]

    home_goals_even <- simulate_goals(1, params$att_even[idx, home_team_id], params$def_even[idx, away_team_id], home_advantage, goal_mean)

    return(data.frame(home_goals_even))
}

# Function to simulate away team goals
simulate_away_goals <- function(home_team_id, away_team_id, params, n_simulations = 10000) {
    idx <- sample(1:length(params$home_advantage), n_simulations, replace = TRUE)
    goal_mean <- params$goal_mean[idx]

    away_goals_even <- simulate_goals(0, params$att_even[idx, away_team_id], params$def_even[idx, home_team_id], 0, goal_mean)
    return(data.frame(away_goals_even))
}

# Function to simulate a single game matchup
simulate_matchup <- function(home_team, away_team, params, n_simulations = 10000) {
    home_team_id <- team_ids[home_team]
    away_team_id <- team_ids[away_team]

    home_goals <- simulate_home_goals(home_team_id, away_team_id, params, n_simulations)
    away_goals <- simulate_away_goals(home_team_id, away_team_id, params, n_simulations)

    # Sum of even and special team goals for regulation
    home_regulation_goals <- home_goals$home_goals_even
    away_regulation_goals <- away_goals$away_goals_even

    results <- data.frame(
        home_full_time_goals = home_regulation_goals,
        away_full_time_goals = away_regulation_goals,
        home_regulation_goals = home_regulation_goals,
        away_regulation_goals = away_regulation_goals
    )

    return(results)
}

# Function to calculate win probabilities and other metrics
calculate_metrics <- function(results, n_simulations = 10000) {
    # Full game metrics
    home_win_prob <- mean(results$home_full_time_goals > results$away_full_time_goals)
    away_win_prob <- mean(results$away_full_time_goals > results$home_full_time_goals)
    home_goals <- mean(results$home_full_time_goals)
    away_goals <- mean(results$away_full_time_goals)
    total_goals <- mean(results$home_full_time_goals + results$away_full_time_goals)
    home_win_margin <- mean(results$home_full_time_goals - results$away_full_time_goals)
    home_puckline_fav <- mean((results$home_full_time_goals - results$away_full_time_goals) > 1.5)
    away_puckline_dog <- mean((results$away_full_time_goals - results$home_full_time_goals) > -1.5)
    away_puckline_fav <- mean((results$away_full_time_goals - results$home_full_time_goals) > 1.5)
    home_puckline_dog <- mean((results$home_full_time_goals - results$away_full_time_goals) > -1.5)

    # Create a dataframe with metrics and results
    metrics <- data.frame(
        Metric = c("home_win_prob", "away_win_prob", "home_goals", "away_goals", "total_goals", "home_win_margin",
                   "home_puckline_fav", "away_puckline_dog", "home_puckline_dog", "away_puckline_fav"),
        Result = c(home_win_prob, away_win_prob, home_goals, away_goals, total_goals, home_win_margin,
                   home_puckline_fav, away_puckline_dog, home_puckline_dog, away_puckline_fav)
    )

    return(metrics)
}

# Example team IDs
home_team <- "Juventus"
away_team <- "Inter"

# Simulate matchups
simulated_games <- simulate_matchup(home_team, away_team, params, n_simulations = 10000)

# Calculate metrics
metrics <- calculate_metrics(simulated_games, n_simulations = 10000)

# Print metrics
print(metrics)


#### 13. Plot simulation results ####
goal_data <- data.frame(
  goals = c(simulated_games$home_full_time_goals, simulated_games$away_full_time_goals),
  team = rep(c("Juventus", "Inter"), each = 10000))

# Plot the histogram overlay using ggplot2 with custom colors
ggplot(goal_data, aes(x = goals, fill = team, color = team)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("Juventus" = "orange", "Inter" = "red")) +
  scale_color_manual(values = c("Juventus" = "orange", "Inter" = "red")) +
  xlab("Goals Scored") +
  ylab("Frequency") +
  theme_minimal() +
  theme(legend.title = element_blank())
