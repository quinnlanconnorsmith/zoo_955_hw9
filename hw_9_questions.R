
# Calculate abundance at a specific timestep using a logistic
# growth curve.
calc_current_abundance <- function(N_previous, r, K) {
  N_previous + r*N_previous*(1 - N_previous/K)
}

# Generate an abundance time series table using a logistic growth curve
calc_logistic_growth_curve <- function(N_initial, time, r, K) {
  N <- rep(NA, length(time)) # Vector of NAs to fill
  N[1] <- N_initial
  for (i in time[2:length(time)]) {
    N[i] <- calc_current_abundance(N[i-1], r, K)
  } 
  return(N)
}

# Generate a time series of abundance for a population growing according to the 
# discrete time logistic growth model.

r <- 0.2
K <- 100
time = seq(from=1, to=50)

# Generate an abundance time series using a growth rate (r) of
# 0.2 (or 20%), a carrying capacity (K) of 100, and starting at
# 5% of the carrying capacity (N_initial = 0.05*100 = 5).
abundance_vals <- calc_logistic_growth_curve(N_initial = 5, time, r, K)
abundance_ts <- data.frame(time, N = abundance_vals)

##### Q1 #####

# Plot the data.

plot(abundance_ts$time, abundance_ts$N)

# Generate a model predicted time series of abundance for a population growing 
# according to the discrete time logistic growth model
# Pretend you don’t know the true parameters (r and K) and adjust them until 
# your model fits the data pretty well (visually)


#Q2. Plot the data
#•Create a function that returns the negative log likelihood of the 
#model parameters (r, K) given the data (Nt)
#•Use optim() or a grid search and the function you just created to 
#estimate the parameters of your model – remember to add the 
#variance as a third parameter to be estimated (or use its analytical MLE: SSE/n)

#•Q3. How well can you estimate the model parameters?

#•Add observation error ( Nt + rnorm(mean=0, sd=?) ) to your data
#•Use optim() or a grid search and the function you just created to 
#estimate the parameters of your model based on this new data – 
#remember to add the variance as a third parameter to be estimated (or use its analytical MLE: SSE/n)

#•Q4. How well can you estimate the model parameters now?  
#Is there any evidence of correlation in your parameter estimates (e.g., a ridge in the likelihood surface)?

#•Start your model with observation error at 5% of K
#•Use optim() or a grid search and the function you just created to 
#estimate the parameters of your model based on this new data – 
#remember to add the variance as a third parameter to be estimated (or use its analytical MLE: SSE/n)

#•Q5. How well can you estimate the model parameters now?

#•Q6. Repeating this analysis starting at 90% of K, how well can you estimate the model parameters?