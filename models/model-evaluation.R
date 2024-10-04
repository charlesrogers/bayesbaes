library(brms)
library(loo)            # LOO-PSIS Bayesian model analysis

# Perform LOO-PSIS for each model
loo_ft_poisson <- loo(ft_poisson_model)
loo_ft_nbinom <- loo(ft_nbinom_model)
fg_poisson_loo <- loo(fg_poisson_model)
fg_nbinom_loo <- loo(fg_nbinom_model)
threes_poisson_loo <- loo(threes_poisson_model)
threes_nbinom_loo <- loo(threes_nbinom_model)

# Compare models using LOO-PSIS
compare_ft <- loo_compare(loo_ft_poisson, loo_ft_nbinom)
compare_fg <- loo_compare(fg_poisson_loo, fg_nbinom_loo)
compare_threes <- loo_compare(threes_poisson_loo, threes_nbinom_loo)

# Print comparisons
print(compare_ft)
print(compare_fg)
print(compare_threes)

#The model with the higher Expected Log Predictive  Density (ELPD) is 
# considered to have better predictive accuracy. The  elpd_diff column 
# shows the difference in ELPD between the models,  while the 
# se_diff column provides the standard error of the difference.  

# By comparing the ELPD values, we can determine which model fits 
# the data better. A larger positive value in elpd_diff indicates a better
# fit,  while a negative value suggests a worse fit compared to the 
# reference model. 