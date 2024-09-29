library(tidyverse)
# https://rpubs.com/gingfacekillah/btm

# Load the dataset containing Serie A match results
data <- df.season.2019 

# Prepare the data for Bradley-Terry model analysis
bt.games <- data %>%
  mutate(
    h_mov = HomeGoals - AwayGoals,
    h_result = case_when(
      h_mov > 0 ~ 1,  # Home win
      h_mov < 0 ~ 0,  # Home loss
      h_mov == 0 ~ 0.5  # Draw
    )
  ) %>%
  select(
    date = Date,
    home = Home,
    away = Away,
    h_score = HomeGoals,
    a_score = AwayGoals,
    h_mov,
    h_result
  )

# Define the logistic function for calculating home team win probability
bt.home_forecast <- function(home_rating,
                             visitor_rating,
                             home_adv) {
  1 / (1 + exp(-(home_adv + home_rating - visitor_rating))) # Logistic Function
}

# Get unique team names and create initial ratings
teams <- as.vector(sort(unique(bt.games$home)))
bt.ratings <- rep(1.000, length(teams) + 1) # Starting Rating is 1.000 for all teams, plus one for home advantage
names(bt.ratings) <- c(teams, "homeadv") # 'homeadv' represents the home advantage

# Define the log-likelihood function
ll <- function(bt.ratings) {
  bt.games %>%
    mutate(forecast = bt.home_forecast(
      bt.ratings[home],
      bt.ratings[away],
      bt.ratings[length(bt.ratings)]
    )) %>%
    mutate(result.f = case_when(
      h_result == 1 ~ forecast,
      h_result == 0 ~ 1 - forecast,
      h_result == 0.5 ~ 0.5  # Probability of draw is 0.5
    )) %>%
    summarise(ll.final = sum(log(result.f))) %>%
    pull(ll.final)
}

# Optimize the log-likelihood function to find best ratings
bt.optim <- optim(bt.ratings, ll,
  method = "BFGS", # Broyden–Fletcher–Goldfarb–Shanno algorithm for non-linear optimization
  control = list(fnscale = -1)
) # fnscale = -1 is crucial for maximization instead of minimization
bt.optim.ratings <- bt.optim$par # Extract the optimized ratings
bt.optim.ratings # Display the optimized ratings

# Prepare ratings for plotting
plot.ratings <- stack(bt.optim.ratings) %>% # Convert named vector to dataframe
  select(
    team = ind,
    rating = values
  )

plot.ratings$team <- as.character(plot.ratings$team)

# Create a plot of team ratings
plot.ratings %>%
  arrange(rating) %>% # Sort teams by their rating
  mutate(team = factor(team, levels = team)) %>%
  ggplot(aes(x = rating, y = team)) +
  geom_point(aes(size = 2, colour = rating)) +
  labs(
    y = "Team",
    x = "Logistic Strength Rating"
  )

# Function to forecast future games
xbt.forecast <- function(home, away) {
  1 / (1 + exp(-(bt.optim.ratings["homeadv"] + bt.optim.ratings[home] - bt.optim.ratings[away])))
}

# Example forecast for Juventus vs Juventus
xbt <- xbt.forecast("Napoli", "Juventus")
xbt <- xbt.forecast("Juventus", "Juventus")
names(xbt) <- NULL # Remove names from the returned probability for cleaner output
round(xbt,4) # Round output probability to 4 decimal points
# Home advantage: 0.6463 - .5 = 0.1463... why doesn't this equal "homeadv 0.6026524" from the calculated ratings?