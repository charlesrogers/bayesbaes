### Bayesian Sports Models in R: Chapter 15 - NHL Log-Linear Poisson Model | Andrew Mack | @Gingfacekillah

# Load libraries | Install required packages prior to loading!
library(ggplot2)        # ggplot: plotting functions
library(ggridges)       # density ridges plot add-on
library(viridis)        # viridis color palette for plots
library(bayesplot)      # Plot mcmc results
library(rstan)          # R interface for Stan programming language
library(tidyverse)      # data wrangling functions
library(lubridate)      # time & date functions
library(parallel)       # support for parallel programming
library(skellam)        # skellam distribution functions
library(reshape2)       # reshape grouped data from wide to long format


#### 1. Load the data ####
aggregated_goals <- read_csv("015_chapter_15/data/nhl2023_goals.csv")

#### 2. Data wrangling and initial team observations ####
aggregated_goals <- aggregated_goals %>% select(-...1, -X)
aggregated_goals <- aggregated_goals %>% mutate(date = ymd(date))

# Create a unique ID for each team
teams <- unique(c(aggregated_goals$home_team, aggregated_goals$away_team))
team_ids <- setNames(seq_along(teams), teams)

# Map team names to their corresponding IDs
aggregated_goals <- aggregated_goals %>% mutate(
    home_team_id = team_ids[home_team],
    away_team_id = team_ids[away_team])

# Print data frame
print(aggregated_goals)

# Calculate aggregated goals for and goals against by team
aggregated_team_stats <- aggregated_goals %>%
    group_by(home_team) %>%
    summarise(goals_for = sum(home_even + home_powerplay + home_shorthanded + home_OT + home_SO),
              goals_against = sum(away_even + away_powerplay + away_shorthanded + away_OT+ away_SO)) %>%
    rename(team = home_team) %>%
    bind_rows(
        aggregated_goals %>%
            group_by(away_team) %>%
            summarise(goals_for = sum(away_even + away_powerplay + away_shorthanded + away_OT + away_SO),
                      goals_against = sum(home_even + home_powerplay + home_shorthanded + home_OT + home_SO)) %>%
            rename(team = away_team)) %>%
    group_by(team) %>%
    summarise(goals_for = sum(goals_for),
              goals_against = sum(goals_against)) %>%
    mutate(diff = goals_for - goals_against,
           diff_g = diff/82)

# Function to calculate win, tie, and loss probabilities using Skellam distribution
calculate_probabilities <- function(goals_for, goals_against) {
    lambda1 <- goals_for / 82  # average goals scored per game
    lambda2 <- goals_against / 82  # average goals conceded per game

    win_prob <- 1 - pskellam(0, lambda1, lambda2)  # P(X > 0)
    tie_prob <- dskellam(0, lambda1, lambda2)  # P(X = 0)
    loss_prob <- pskellam(-1, lambda1, lambda2)  # P(X < 0)

    return(c(win_prob, tie_prob, loss_prob))
}

# Add columns for win, tie, and loss probabilities
aggregated_team_stats <- aggregated_team_stats %>%
    rowwise() %>%
    mutate(probabilities = list(calculate_probabilities(goals_for, goals_against))) %>%
    mutate(win_prob = probabilities[1],
           tie_prob = probabilities[2],
           loss_prob = probabilities[3]) %>%
    select(-probabilities)

# Calculate league average goals for and goals against per game
league_avg_goals_for <- mean(aggregated_team_stats$goals_for) / 82
league_avg_goals_against <- mean(aggregated_team_stats$goals_against) / 82

# Add expected goals for and against
aggregated_team_stats <- aggregated_team_stats %>%
    mutate(exp_goals_for = league_avg_goals_for + diff_g / 2,
           exp_goals_ag = league_avg_goals_against - diff_g / 2)

# Print aggregated team goal stats
print(aggregated_team_stats)


#### 3. Plot initial team MOV rankings ####
ggplot(aggregated_team_stats, aes(x = reorder(team, diff_g), y = diff_g, fill = diff_g)) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_c(option = "G", direction = -1) +
    coord_flip() +
    labs(x = "Team", y = "Goal Difference Per Game", fill = "Goal Difference") +
    theme_minimal() +
    theme(legend.position = "none")

#### 4. Base model ####
# Prepare data for Stan
stan_data <- list(
    N = nrow(aggregated_goals),
    T = length(unique(c(aggregated_goals$home_team_id, aggregated_goals$away_team_id))),
    home_team = aggregated_goals$home_team_id,
    away_team = aggregated_goals$away_team_id,
    home_goals = aggregated_goals$home_even + aggregated_goals$home_powerplay + aggregated_goals$home_shorthanded,
    away_goals = aggregated_goals$away_even + aggregated_goals$away_powerplay + aggregated_goals$away_shorthanded)

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
    control = list(max_treedepth = 10))

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
agg_strength_df$Team <- factor(team_mapping[agg_strength_df$Team], levels = 1:length(ordered_team_names),
                               labels = ordered_team_names)
# Plot the distributions for all teams
ggplot(agg_strength_df, aes(x = Strength, fill = Team)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    facet_wrap(~ Team, scales = "free", labeller = labeller(Team = label_value)) +
    xlab("Aggregate Strength") + ylab("Density") +
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
selected_teams <- c("Dallas Stars", "Florida Panthers", "San Jose Sharks", "Chicago Blackhawks")

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
    facet_wrap(~ Team, scales = "free", labeller = labeller(Team = label_value)) +
    xlab("Aggregate Strength") + ylab("Density") +
    scale_fill_viridis_d(option = "C", direction = 1) +
    theme_minimal() +
    theme(legend.position = "none")


#### 6. Model upgrades: even strength + special teams ####
# Prepare data for Stan
stan_data <- list(
    N = nrow(aggregated_goals),
    T = length(unique(c(aggregated_goals$home_team_id, aggregated_goals$away_team_id))),
    home_team = aggregated_goals$home_team_id,
    away_team = aggregated_goals$away_team_id,
    home_even_goals = aggregated_goals$home_even,
    away_even_goals = aggregated_goals$away_even,
    home_special_goals = aggregated_goals$home_powerplay + aggregated_goals$home_shorthanded,
    away_special_goals = aggregated_goals$away_powerplay + aggregated_goals$away_shorthanded)

# Specify Stan model
stan_model_code <- "
data {
    int<lower=1> N;                 // Number of games
    int<lower=1> T;                 // Number of teams
    int home_team[N];               // Home team index
    int away_team[N];               // Away team index
    int home_even_goals[N];         // ES goals home
    int away_even_goals[N];         // ES goals away
    int home_special_goals[N];      // ST goals home
    int away_special_goals[N];      // ST goals away
}
parameters {
    real home_advantage;            // Home advantage
    real goal_mean;                 // Goal mean
    real<lower=0> sigma_att_even;   // Sigma ES OFF
    real<lower=0> sigma_def_even;   // Sigma ES DEF
    real<lower=0> sigma_special;    // Sigma ST
    vector[T] att_raw_even;         // Raw ES OFF
    vector[T] def_raw_even;         // Raw ES DEF
    vector[T] att_raw_special;      // Raw ST OFF
    vector[T] def_raw_special;      // Raw ST DEF
}
transformed parameters {
    vector[T] att_even;             // centered ES OFF
    vector[T] def_even;             // centered ES DEF
    vector[T] att_special;          // centered ST OFF
    vector[T] def_special;          // centered ST DEF

    // Center OFF & DEF to have mean zero
    att_even = att_raw_even - mean(att_raw_even);
    def_even = def_raw_even - mean(def_raw_even);
    att_special = att_raw_special - mean(att_raw_special);
    def_special = def_raw_special - mean(def_raw_special);
}
model {
    // Priors for global parameters
    home_advantage ~ normal(0, 0.1); // Home advantage prior
    goal_mean ~ normal(0, 1);

    // Priors for standard deviations
    sigma_att_even ~ normal(0, 1);  // Sigma ES OFF prior
    sigma_def_even ~ normal(0, 1);  // Sigma ES DEF prior
    sigma_special ~ normal(0, 1);   // Sigma ST prior

    // Priors for team abilities
    att_raw_even ~ normal(0, sigma_att_even);   // Raw ES OFF prior
    def_raw_even ~ normal(0, sigma_def_even);   // Raw ES DEF prior
    att_raw_special ~ normal(0, sigma_special); // Raw ST OFF prior
    def_raw_special ~ normal(0, sigma_special); // Raw ST DEF prior

// Likelihood

    // ES
    home_even_goals ~ poisson_log(home_advantage + att_even[home_team] + def_even[away_team] + goal_mean);
    away_even_goals ~ poisson_log(att_even[away_team] + def_even[home_team] + goal_mean);

    // ST
    home_special_goals ~ poisson_log(att_special[home_team] + def_special[away_team]);
    away_special_goals ~ poisson_log(att_special[away_team] + def_special[home_team]);
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
traceplot(fit, pars = c("home_advantage", "goal_mean", "sigma_special"))


#### 7. Plot estimated posterior team strength ####
# Extract parameters from the fitted model
posterior <- rstan::extract(fit)

# Calculate aggregate team strengths on neutral ice for even strength and special teams
agg_strength_even <- posterior$att_even - posterior$def_even
agg_strength_special <- posterior$att_special - posterior$def_special

# Melt the posterior samples into long-format data frames
agg_strength_even_df <- melt(agg_strength_even)
colnames(agg_strength_even_df) <- c("Iteration", "Team", "Strength")
agg_strength_even_df$Type <- "Even Strength"

agg_strength_special_df <- melt(agg_strength_special)
colnames(agg_strength_special_df) <- c("Iteration", "Team", "Strength")
agg_strength_special_df$Type <- "Special Teams"

# Combine the data frames
agg_strength_df <- rbind(agg_strength_even_df, agg_strength_special_df)

# Get the team names from the 'team_ids'
team_names <- names(team_ids)  # Assuming 'team_ids' contains team names as names

# Order team names alphabetically
ordered_team_names <- sort(team_names)

# Create a mapping from original team indices to alphabetical order
team_mapping <- match(team_names, ordered_team_names)

# Update the Team column in agg_strength_df to use the new alphabetical order
agg_strength_df$Team <- factor(team_mapping[agg_strength_df$Team], levels = 1:length(ordered_team_names), labels = ordered_team_names)

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


#### 8. Model upgrades: even strength + special teams + overtime ####
# Prepare data for Stan
stan_data <- list(
    N = nrow(aggregated_goals),
    T = length(unique(c(aggregated_goals$home_team_id, aggregated_goals$away_team_id))),
    home_team = aggregated_goals$home_team_id,
    away_team = aggregated_goals$away_team_id,
    home_even_goals = aggregated_goals$home_even,
    away_even_goals = aggregated_goals$away_even,
    home_special_goals = aggregated_goals$home_powerplay + aggregated_goals$home_shorthanded,
    away_special_goals = aggregated_goals$away_powerplay + aggregated_goals$away_shorthanded,
    home_OT_goals = ifelse(!is.na(aggregated_goals$home_OT), aggregated_goals$home_OT, 0),
    away_OT_goals = ifelse(!is.na(aggregated_goals$away_OT), aggregated_goals$away_OT, 0),
    OT_indicator = ifelse((aggregated_goals$home_OT + aggregated_goals$away_OT) > 0, 1, 0))

# Specify Stan model
stan_model_code <- "
data {
    int<lower=1> N;                 // Number of games
    int<lower=1> T;                 // Number of teams
    int home_team[N];               // Home team index
    int away_team[N];               // Away team index
    int home_even_goals[N];         // ES goals home
    int away_even_goals[N];         // ES goals away
    int home_special_goals[N];      // ST goals home
    int away_special_goals[N];      // ST goals away
    int home_OT_goals[N];           // OT goals home
    int away_OT_goals[N];           // OT goals away
    int OT_indicator[N];            // Indicator for OT
}
parameters {
    real home_advantage;            // Home advantage
    real goal_mean;                 // Goal mean
    real<lower=0> sigma_att_even;   // Sigma ES OFF
    real<lower=0> sigma_def_even;   // Sigma ES DEF
    real<lower=0> sigma_special;    // Sigma ST
    real<lower=0> sigma_OT;         // Sigma OT
    vector[T] att_raw_even;         // Raw ES OFF
    vector[T] def_raw_even;         // Raw ES DEF
    vector[T] att_raw_special;      // Raw ST OFF
    vector[T] def_raw_special;      // Raw ST DEF
    vector[T] att_raw_OT;           // Raw OT OFF
    vector[T] def_raw_OT;           // Raw OT DEF
}
transformed parameters {
    vector[T] att_even;             // Centered ES OFF
    vector[T] def_even;             // Centered ES DEF
    vector[T] att_special;          // Centered ST OFF
    vector[T] def_special;          // Centered ST DEF
    vector[T] att_OT;               // Centered OT OFF
    vector[T] def_OT;               // Centered OT DEF

    // Center OFF & DEF to have mean zero
    att_even = att_raw_even - mean(att_raw_even);
    def_even = def_raw_even - mean(def_raw_even);
    att_special = att_raw_special - mean(att_raw_special);
    def_special = def_raw_special - mean(def_raw_special);
    att_OT = att_raw_OT - mean(att_raw_OT);
    def_OT = def_raw_OT - mean(def_raw_OT);
}
model {
    // Priors for global parameters
    home_advantage ~ normal(0, 0.1); // Home advantage prior
    goal_mean ~ normal(0, 1);

    // Priors for standard deviations
    sigma_att_even ~ normal(0, 1);  // Sigma ES OFF prior
    sigma_def_even ~ normal(0, 1);  // Sigma ES DEF prior
    sigma_special ~ normal(0, 1);   // Sigma ST prior
    sigma_OT ~ normal(0, 1);        // Sigma OT prior

    // Priors for team abilities
    att_raw_even ~ normal(0, 1);    // Raw ES OFF prior
    def_raw_even ~ normal(0, 1);    // Raw ES DEF prior
    att_raw_special ~ normal(0, 1); // Raw ST OFF prior
    def_raw_special ~ normal(0, 1); // Raw ST DEF prior
    att_raw_OT ~ normal(0, 1);      // Raw OT OFF prior
    def_raw_OT ~ normal(0, 1);      // Raw OT DEF prior

    // Likelihood
    // ES
    home_even_goals ~ poisson_log(home_advantage + att_even[home_team] + def_even[away_team] + goal_mean);
    away_even_goals ~ poisson_log(att_even[away_team] + def_even[home_team] + goal_mean);

    // ST
    home_special_goals ~ poisson_log(att_special[home_team] + def_special[away_team]);
    away_special_goals ~ poisson_log(att_special[away_team] + def_special[home_team]);

    // OT
    for (i in 1:N) {
        if (OT_indicator[i] == 1) {
            home_OT_goals[i] ~ poisson_log(att_OT[home_team[i]] + def_OT[away_team[i]]);
            away_OT_goals[i] ~ poisson_log(att_OT[away_team[i]] + def_OT[home_team[i]]);
        }
    }
}
"

# Fit Stan model with mcmc
fit <- stan(
    model_code = stan_model_code,
    data = stan_data,
    iter = 10000,
    warmup = 2000,
    chains = 2,
    cores = 6,
    seed = 1,
    init = "random",
    control = list(adapt_delta = 0.85, max_treedepth = 12))

# Print summary of the fit
print(fit)

# Print selected parameter trace plots
traceplot(fit, pars = c("sigma_OT", "sigma_special"))


#### 9. Plot estimated posterior team strength ####
# Extract parameters from the fitted model
posterior <- rstan::extract(fit)

# Calculate aggregate team strengths on neutral ice for even strength, special teams, and overtime
agg_strength_even <- posterior$att_even - posterior$def_even
agg_strength_special <- posterior$att_special - posterior$def_special
agg_strength_OT <- posterior$att_OT - posterior$def_OT

# Melt the posterior samples into long-format data frames
agg_strength_even_df <- melt(agg_strength_even)
colnames(agg_strength_even_df) <- c("Iteration", "Team", "Strength")
agg_strength_even_df$Type <- "Even Strength"

agg_strength_special_df <- melt(agg_strength_special)
colnames(agg_strength_special_df) <- c("Iteration", "Team", "Strength")
agg_strength_special_df$Type <- "Special Teams"

agg_strength_OT_df <- melt(agg_strength_OT)
colnames(agg_strength_OT_df) <- c("Iteration", "Team", "Strength")
agg_strength_OT_df$Type <- "Overtime"

# Combine the data frames
agg_strength_df <- rbind(agg_strength_even_df, agg_strength_special_df, agg_strength_OT_df)

# Get the team names from the 'team_ids'
team_names <- names(team_ids)  # Assuming 'team_ids' contains team names as names

# Order team names alphabetically
ordered_team_names <- sort(team_names)

# Create a mapping from original team indices to alphabetical order
team_mapping <- match(team_names, ordered_team_names)

# Update the Team column in agg_strength_df to use the new alphabetical order
agg_strength_df$Team <- factor(team_mapping[agg_strength_df$Team], levels = 1:length(ordered_team_names), labels = ordered_team_names)

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

# Plots for 4 selected teams
# Extract posterior samples
selected_teams <- c("Dallas Stars", "Florida Panthers", "San Jose Sharks", "Chicago Blackhawks")

# Get the team IDs for selected teams
selected_team_ids <- team_ids[selected_teams]

# Extract posterior samples
posterior <- rstan::extract(fit)

# Calculate aggregate team strengths on neutral ice for even strength, special teams, and overtime
agg_strength_even <- posterior$att_even - posterior$def_even
agg_strength_special <- posterior$att_special - posterior$def_special
agg_strength_OT <- posterior$att_OT - posterior$def_OT

# Melt the posterior samples into long-format data frames
agg_strength_even_df <- melt(agg_strength_even)
colnames(agg_strength_even_df) <- c("Iteration", "Team", "Strength")
agg_strength_even_df$Type <- "Even Strength"

agg_strength_special_df <- melt(agg_strength_special)
colnames(agg_strength_special_df) <- c("Iteration", "Team", "Strength")
agg_strength_special_df$Type <- "Special Teams"

agg_strength_OT_df <- melt(agg_strength_OT)
colnames(agg_strength_OT_df) <- c("Iteration", "Team", "Strength")
agg_strength_OT_df$Type <- "Overtime"

# Combine the data frames
agg_strength_df <- rbind(agg_strength_even_df, agg_strength_special_df, agg_strength_OT_df)

# Filter for selected teams
agg_strength_df <- subset(agg_strength_df, Team %in% selected_team_ids)

# Map the team IDs to the selected team names
agg_strength_df$Team <- factor(agg_strength_df$Team, levels = selected_team_ids, labels = selected_teams)

# Plot the 4 teams
ggplot(agg_strength_df, aes(x = Strength, fill = Type)) +
    geom_density(alpha = 0.5, position = "identity") +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    facet_wrap(~ Team, scales = "free", labeller = labeller(Team = label_value)) +

    xlab("Aggregate Strength") +
    ylab("Density") +
    scale_fill_viridis_d(option = "E", direction = 1) +
    theme_minimal() +
    theme(legend.position = "right")


#### 10. Model upgrades: even strength + special teams + overtime + shootouts ####
# Prepare data for Stan
stan_data <- list(
    N = nrow(aggregated_goals),
    T = length(unique(c(aggregated_goals$home_team_id, aggregated_goals$away_team_id))),
    home_team = aggregated_goals$home_team_id,
    away_team = aggregated_goals$away_team_id,
    home_even_goals = aggregated_goals$home_even,
    away_even_goals = aggregated_goals$away_even,
    home_special_goals = aggregated_goals$home_powerplay + aggregated_goals$home_shorthanded,
    away_special_goals = aggregated_goals$away_powerplay + aggregated_goals$away_shorthanded,
    home_OT_goals = aggregated_goals$home_OT,
    away_OT_goals = aggregated_goals$away_OT,
    OT_indicator = ifelse((aggregated_goals$home_OT + aggregated_goals$away_OT) > 0, 1, 0),   # Indicator for OT
    home_SO_goals = aggregated_goals$home_SO,
    away_SO_goals = aggregated_goals$away_SO,
    SO_indicator = ifelse((aggregated_goals$home_SO + aggregated_goals$away_SO) == 1, 1, 0))  # Indicator for SO

# Specify Stan model
stan_model_code <- "
data {
    int<lower=1> N;                 // Number of games
    int<lower=1> T;                 // Number of teams
    int home_team[N];               // Home team index
    int away_team[N];               // Away team index
    int home_even_goals[N];         // ES goals by home team
    int away_even_goals[N];         // ES goals by away team
    int home_special_goals[N];      // ST goals by home team
    int away_special_goals[N];      // ST goals by away team
    int home_OT_goals[N];           // OT goals by home team
    int away_OT_goals[N];           // OT goals by away team
    int OT_indicator[N];            // OT indicator
    int home_SO_goals[N];           // SO goals by home team
    int away_SO_goals[N];           // SO goals by away team
    int SO_indicator[N];            // SO indicator
}
parameters {
    real<upper=0.35> home_advantage; // Home advantage
    real<lower=0> sigma_att_even;    // Sigma ES OFF
    real<lower=0> sigma_def_even;    // Sigma ES DEF
    real<lower=0> sigma_special;     // Sigma ST
    real<lower=0> sigma_OT;          // Sigma OT
    real<lower=0> sigma_SO;          // Sigma SO
    real goal_mean;                  // Goal mean

    vector[T] att_raw_even;          // Raw ES OFF
    vector[T] def_raw_even;          // Raw ES DEF
    vector[T] att_raw_special;       // Raw ST OFF
    vector[T] def_raw_special;       // Raw ST DEF
    vector[T] att_raw_OT;            // Raw OT OFF
    vector[T] def_raw_OT;            // Raw OT DEF
    vector[T] att_raw_SO;            // Raw SO OFF
    vector[T] def_raw_SO;            // Raw SO DEF
}
transformed parameters {
    vector[T] att_even;              // Centered ES OFF
    vector[T] def_even;              // Centered ES DEF
    vector[T] att_special;           // Centered ST OFF
    vector[T] def_special;           // Centered ST DEF
    vector[T] att_OT;                // Centered OT OFF
    vector[T] def_OT;                // Centered OT DEF
    vector[T] att_SO;                // Centered SO OFF
    vector[T] def_SO;                // Centered SO DEF

    // Center OFF & DEF to have mean zero
    att_even = att_raw_even - mean(att_raw_even);
    def_even = def_raw_even - mean(def_raw_even);
    att_special = att_raw_special - mean(att_raw_special);
    def_special = def_raw_special - mean(def_raw_special);
    att_OT = att_raw_OT - mean(att_raw_OT);
    def_OT = def_raw_OT - mean(def_raw_OT);
    att_SO = att_raw_SO - mean(att_raw_SO);
    def_SO = def_raw_SO - mean(def_raw_SO);
}
model {
    // Priors for global parameters
    home_advantage ~ normal(0, 1); // Home advantage prior
    goal_mean ~ normal(0,1);       // Goal mean prior

    // Priors for standard deviations
    sigma_att_even ~ normal(0, 1); // Sigma ES OFF prior
    sigma_def_even ~ normal(0, 1); // Sigma ES DEF prior
    sigma_special ~ normal(0, 1);  // Sigma ST prior
    sigma_OT ~ normal(0, 1);       // Sigma OT prior
    sigma_SO ~ normal(0, 1);       // Sigma SO prior

    // Priors for team abilities
    att_raw_even ~ normal(0, 1);   // Raw ES OFF prior
    def_raw_even ~ normal(0, 1);   // Raw ES DEF prior
    att_raw_special ~ normal(0, 1);// Raw ST OFF prior
    def_raw_special ~ normal(0, 1);// Raw ST DEF prior
    att_raw_OT ~ normal(0, 1);     // Raw OT OFF prior
    def_raw_OT ~ normal(0, 1);     // Raw OT DEF prior
    att_raw_SO ~ normal(0, 1);     // Raw SO OFF prior
    def_raw_SO ~ normal(0, 1);     // Raw SO DEF prior

    // Likelihood for scoring models

    // ES
    home_even_goals ~ poisson_log(home_advantage + att_even[home_team] + def_even[away_team] + goal_mean);
    away_even_goals ~ poisson_log(att_even[away_team] + def_even[home_team] + goal_mean);

    // ST
    home_special_goals ~ poisson_log(att_special[home_team] + def_special[away_team]);
    away_special_goals ~ poisson_log(att_special[away_team] + def_special[home_team]);

    // Only consider OT goals if the game went to OT
    for (i in 1:N) {
        if (OT_indicator[i] == 1) {
            home_OT_goals[i] ~ poisson_log(att_OT[home_team[i]] + def_OT[away_team[i]]);
            away_OT_goals[i] ~ poisson_log(att_OT[away_team[i]] + def_OT[home_team[i]]);
        }
    }

    // Only consider SO goals if the game went to SO
    for (i in 1:N) {
        if (SO_indicator[i] == 1) {
            home_SO_goals[i] ~ binomial_logit(1, att_SO[home_team[i]] + def_SO[away_team[i]]);
            away_SO_goals[i] ~ binomial_logit(1, att_SO[away_team[i]] + def_SO[home_team[i]]);
        }
    }
}
"

# Fit Stan model with mcmc
fit <- stan(
    model_code = stan_model_code,
    data = stan_data,
    iter = 10000,
    warmup = 2000,
    chains = 2,
    cores = 6,
    seed = 1,
    init = "random",
    control = list(adapt_delta = 0.85, max_treedepth = 12))

# Print summary of the fit
print(fit)

# Print selected parameter trace plots
traceplot(fit, pars = c("sigma_OT", "sigma_special", "sigma_SO"))


#### 11. Plot estimated posterior team strength ####
# Extract parameters from the fitted model
posterior <- rstan::extract(fit)

# Calculate aggregate team strengths on neutral ice for even strength, special teams, overtime, and shootouts
agg_strength_even <- posterior$att_even - posterior$def_even
agg_strength_special <- posterior$att_special - posterior$def_special
agg_strength_OT <- posterior$att_OT - posterior$def_OT
agg_strength_SO <- posterior$att_SO - posterior$def_SO

# Melt the posterior samples into long-format data frames
agg_strength_even_df <- melt(agg_strength_even)
colnames(agg_strength_even_df) <- c("Iteration", "Team", "Strength")
agg_strength_even_df$Type <- "Even Strength"

agg_strength_special_df <- melt(agg_strength_special)
colnames(agg_strength_special_df) <- c("Iteration", "Team", "Strength")
agg_strength_special_df$Type <- "Special Teams"

agg_strength_OT_df <- melt(agg_strength_OT)
colnames(agg_strength_OT_df) <- c("Iteration", "Team", "Strength")
agg_strength_OT_df$Type <- "Overtime"

agg_strength_SO_df <- melt(agg_strength_SO)
colnames(agg_strength_SO_df) <- c("Iteration", "Team", "Strength")
agg_strength_SO_df$Type <- "Shootouts"

# Combine the data frames
agg_strength_df <- rbind(agg_strength_even_df,
                         agg_strength_special_df,
                         agg_strength_OT_df,
                         agg_strength_SO_df)

# Get the team names from the 'team_ids'
team_names <- names(team_ids)  # Assuming 'team_ids' contains team names as names

# Order team names alphabetically
ordered_team_names <- sort(team_names)

# Create a mapping from original team indices to alphabetical order
team_mapping <- match(team_names, ordered_team_names)

# Update the Team column in agg_strength_df to use the new alphabetical order
agg_strength_df$Team <- factor(team_mapping[agg_strength_df$Team], levels = 1:length(ordered_team_names),
                               labels = ordered_team_names)

# Plots for all teams
ggplot(agg_strength_df, aes(x = Strength, fill = Type)) +
    geom_density(alpha = 0.5, position = "identity") +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    facet_wrap(~ Team, scales = "free", labeller = labeller(Team = label_value)) +
    ggtitle("Distribution of Aggregate Team Strengths for Each Parameter") +
    xlab("Aggregate Strength") +
    ylab("Density") +
    scale_fill_viridis_d(option = "E", direction = 1) +
    theme_minimal() +
    theme(legend.position = "right")

# Plots for 4 selected teams
# Extract posterior samples
posterior <- rstan::extract(fit)

# Select specific teams
selected_teams <- c("Dallas Stars", "Florida Panthers", "Chicago Blackhawks", "San Jose Sharks")

# Calculate aggregate team strengths on neutral ice for even strength, special teams, overtime, and shootouts
agg_strength_even <- posterior$att_even - posterior$def_even
agg_strength_special <- posterior$att_special - posterior$def_special
agg_strength_OT <- posterior$att_OT - posterior$def_OT
agg_strength_SO <- posterior$att_SO - posterior$def_SO

# Melt the posterior samples into long-format data frames
agg_strength_even_df <- melt(agg_strength_even)
colnames(agg_strength_even_df) <- c("Iteration", "Team", "Strength")
agg_strength_even_df$Type <- "Even Strength"

agg_strength_special_df <- melt(agg_strength_special)
colnames(agg_strength_special_df) <- c("Iteration", "Team", "Strength")
agg_strength_special_df$Type <- "Special Teams"

agg_strength_OT_df <- melt(agg_strength_OT)
colnames(agg_strength_OT_df) <- c("Iteration", "Team", "Strength")
agg_strength_OT_df$Type <- "Overtime"

agg_strength_SO_df <- melt(agg_strength_SO)
colnames(agg_strength_SO_df) <- c("Iteration", "Team", "Strength")
agg_strength_SO_df$Type <- "Shootouts"

# Combine the data frames
agg_strength_df <- rbind(agg_strength_even_df, agg_strength_special_df, agg_strength_OT_df, agg_strength_SO_df)

# Get the team names from the 'team_ids'
team_names <- names(team_ids)  # Assuming 'team_ids' contains team names as names

# Order team names alphabetically
ordered_team_names <- sort(team_names)

# Create a mapping from original team indices to alphabetical order
team_mapping <- match(team_names, ordered_team_names)

# Update the Team column in agg_strength_df to use the new alphabetical order
agg_strength_df$Team <- factor(team_mapping[agg_strength_df$Team], levels = 1:length(ordered_team_names), labels = ordered_team_names)

# Filter for selected teams
agg_strength_df_selected <- subset(agg_strength_df, Team %in% selected_teams)

# Update the Team column in agg_strength_df_selected to use the new alphabetical order
agg_strength_df_selected$Team <- factor(agg_strength_df_selected$Team, levels = selected_teams, labels = selected_teams)

# Plots the 4 teams
ggplot(agg_strength_df_selected, aes(x = Strength, fill = Type)) +
    geom_density(alpha = 0.5, position = "identity") +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    facet_wrap(~ Team, scales = "free", ncol = 2) +
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
    att_special = posterior$att_special,
    def_special = posterior$def_special,
    att_OT = posterior$att_OT,
    def_OT = posterior$def_OT,
    att_SO = posterior$att_SO,
    def_SO = posterior$def_SO,
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
    home_goals_special <- simulate_goals(1, params$att_special[idx, home_team_id], params$def_special[idx, away_team_id], 0, 0)

    return(data.frame(home_goals_even, home_goals_special))
}

# Function to simulate away team goals
simulate_away_goals <- function(home_team_id, away_team_id, params, n_simulations = 10000) {
    idx <- sample(1:length(params$home_advantage), n_simulations, replace = TRUE)
    goal_mean <- params$goal_mean[idx]

    away_goals_even <- simulate_goals(0, params$att_even[idx, away_team_id], params$def_even[idx, home_team_id], 0, goal_mean)
    away_goals_special <- simulate_goals(0, params$att_special[idx, away_team_id], params$def_special[idx, home_team_id], 0, 0)

    return(data.frame(away_goals_even, away_goals_special))
}

# Function to simulate a single game matchup
simulate_matchup <- function(home_team, away_team, params, n_simulations = 10000) {
    home_team_id <- team_ids[home_team]
    away_team_id <- team_ids[away_team]

    home_goals <- simulate_home_goals(home_team_id, away_team_id, params, n_simulations)
    away_goals <- simulate_away_goals(home_team_id, away_team_id, params, n_simulations)

    # Sum of even and special team goals for regulation
    home_regulation_goals <- home_goals$home_goals_even + home_goals$home_goals_special
    away_regulation_goals <- away_goals$away_goals_even + away_goals$away_goals_special

    # Identify ties and simulate OT
    ties <- home_regulation_goals == away_regulation_goals
    idx <- sample(1:length(params$home_advantage), sum(ties), replace = TRUE)
    ot_goals_home <- simulate_goals(1, params$att_OT[idx, home_team_id], params$def_OT[idx, away_team_id], 0, 0)
    ot_goals_away <- simulate_goals(0, params$att_OT[idx, away_team_id], params$def_OT[idx, home_team_id], 0, 0)
    ot_results <- ifelse(ot_goals_home > ot_goals_away, "home", "away")
    ot_ties <- ot_goals_home == ot_goals_away

    # Identify remaining ties and simulate shootouts using Bradley-Terry model
    shootout_idx <- sample(1:length(params$home_advantage), sum(ot_ties), replace = TRUE)
    shootout_home_probs <- exp(params$att_SO[shootout_idx, home_team_id] - params$def_SO[shootout_idx, away_team_id])
    shootout_away_probs <- exp(params$att_SO[shootout_idx, away_team_id] - params$def_SO[shootout_idx, home_team_id])
    shootout_results <- ifelse(runif(sum(ot_ties)) < shootout_home_probs / (shootout_home_probs + shootout_away_probs), "home", "away")

    results <- data.frame(
        home_full_time_goals = home_regulation_goals + ifelse(ties, ifelse(ot_ties, ifelse(shootout_results == "home", 1, 0), ifelse(ot_results == "home", 1, 0)), 0),
        away_full_time_goals = away_regulation_goals + ifelse(ties, ifelse(ot_ties, ifelse(shootout_results == "away", 1, 0), ifelse(ot_results == "away", 1, 0)), 0),
        home_regulation_goals = home_regulation_goals,
        away_regulation_goals = away_regulation_goals,
        home_power_play_goals = home_goals$home_goals_special,
        away_power_play_goals = away_goals$away_goals_special,
        home_ot_goals = ifelse(ties, ifelse(ot_results == "home", 1, 0), 0),
        away_ot_goals = ifelse(ties, ifelse(ot_results == "away", 1, 0), 0),
        home_shootout_goals = ifelse(ties, ifelse(ot_ties, ifelse(shootout_results == "home", 1, 0), 0), 0),
        away_shootout_goals = ifelse(ties, ifelse(ot_ties, ifelse(shootout_results == "away", 1, 0), 0), 0)
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
home_team <- "Edmonton Oilers"
away_team <- "Chicago Blackhawks"

# Simulate matchups
simulated_games <- simulate_matchup(home_team, away_team, params, n_simulations = 10000)

# Calculate metrics
metrics <- calculate_metrics(simulated_games, n_simulations = 10000)

# Print metrics
print(metrics)


#### 13. Plot simulation results ####
goal_data <- data.frame(
    goals = c(simulated_games$home_full_time_goals, simulated_games$away_full_time_goals),
    team = rep(c("Edmonton Oilers", "Chicago Blackhawks"), each = 10000))

# Plot the histogram overlay using ggplot2 with custom colors
ggplot(goal_data, aes(x = goals, fill = team, color = team)) +
    geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
    scale_fill_manual(values = c("Edmonton Oilers" = "orange", "Chicago Blackhawks" = "red")) +
    scale_color_manual(values = c("Edmonton Oilers" = "orange", "Chicago Blackhawks" = "red")) +
    xlab("Goals Scored") +
    ylab("Frequency") +
    theme_minimal() +
    theme(legend.title = element_blank())
