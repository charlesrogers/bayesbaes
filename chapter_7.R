library(tidyverse)
alpha <-10
beta <-8  
#Number of simulations
n_sim <-10000  
#Simulate theta from the posterior Beta distribution  
theta_sim <-rbeta(n_sim, alpha, beta) 
#We know that there are 7 games left in the season, so we’ll use the binomial distribution to simulate the number of wins given our freshly simulated theta values from the beta distribution. We’ll then add in the 6 games  that the team has already won to create a posterior of simulated season wins.  Notice that the output of our first simulation theta_sim is now the input  into our remaining_wins_sim.  #Simulate #of wins in the remaining 7 games 
remaining_wins_sim <-rbinom(n_sim, 7, theta_sim)  
#Total wins, adding the already won 6 games  
total_wins_sim <-6 + remaining_wins_sim  
#Create a data frame for ggplot  
total_wins_df <-data.frame(total_wins = total_wins_sim) 
ggplot(total_wins_df, aes(x=total_wins))+
  geom_histogram(binwidth = 1,fill="red", alpha = .7)

## exact wins
prob_7_wins <- mean(total_wins_sim==7)
prob_12_wins <- mean(total_wins_sim==12)

## win prob
prob_over_70 <- mean(total_wins_sim/17>.7)
prob_under_50 <- mean(total_wins_sim/17<.5)

## over/under
prob_more_than_9.5 <- mean(total_wins_sim>9.5)
prob_less_than_9.5<- 1-prob_more_than_9.5

## Median win rate

median_winrate <- round(median(total_wins_sim/17),4)



# GAMMA
shape <- 30
rate <- 9
lambda_sim <- rgamma(n_sim,shape, rate)
## PLOT
future_goals_sim <- rpois(n_sim,lambda_sim)
future_goals_df <- data_frame(future_goals=future_goals_sim)
ggplot(future_goals_df, aes(x=future_goals))+
  geom_histogram(binwidth = 1,fill="red", alpha = .7)

## Exact Goals
prob_4_goals <- mean(future_goals_sim==4)
prob_10_goals <- mean(future_goals_sim==10)

## Over/Under
prob_more_5.5 <- mean(future_goals_sim >5.5)
prob_less_5.5 <- 1-prob_more_5.5

## Maximum A Posterior
MAP_estimate <- function(x){
  d <- density(x)
  d$x[which.max(d$y)]}
lamda_map <- MAP_estimate(lambda_sim)
