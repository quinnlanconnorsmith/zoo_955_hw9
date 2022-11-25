
library(dplyr)

# Calculate abundance at a specific timestep using a logistic growth curve
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

##### Q1 #####

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

# Plot the data.

plot(abundance_ts$time, abundance_ts$N)

##### Q2 #####

# Generate a model predicted time series of abundance for a population growing 
# according to the discrete time logistic growth model
# Pretend you don’t know the true parameters (r and K) and adjust them until 
# your model fits the data pretty well (visually)

# The data already fit a curve because of how we generated it, so this
# seems like a strange step. Maybe we did something wrong above?

# Found mathematical equation at 
# https://math.libretexts.org/Bookshelves/Calculus/Book%3A_Calculus_(OpenStax)/08%3A_Introduction_to_Differential_Equations/8.04%3A_The_Logistic_Equation
logistic_growth_eqn  <- function(P0, t, K, r) {
  p_exp_rt <- P0*exp(r*t)
  (K*p_exp_rt) / ( K - P0 + p_exp_rt)
}

# Try K = 50 and r = 0.6
time_curve_guess <- seq(1,50,by=0.01)

# Plot the data points
plot(abundance_ts$time, abundance_ts$N)

# Plot a curve with K=50, r=0.6
lines(x = time_curve_guess, col = 'red', lwd = 2,
      y = logistic_growth_eqn(t = time_curve_guess, P0 = 5, K = 50, r = 0.6))

# That capped out to early, increase K
lines(x = time_curve_guess, col = 'blue', lwd = 2,
      y = logistic_growth_eqn(t = time_curve_guess, P0 = 5, K = 150, r = 0.6))

# Now you can't see the top, decrease K and decrease r so the curve is less sharp
lines(x = time_curve_guess, col = 'darkgreen', lwd = 2,
      y = logistic_growth_eqn(t = time_curve_guess, P0 = 5, K = 100, r = 0.4))

# Still reaches max way to soon, decrease r
lines(x = time_curve_guess, col = 'purple', lwd = 2,
      y = logistic_growth_eqn(t = time_curve_guess, P0 = 5, K = 100, r = 0.1))

# That last one was too low, increase r
lines(x = time_curve_guess, col = 'green', lwd = 2,
      y = logistic_growth_eqn(t = time_curve_guess, P0 = 5, K = 100, r = 0.2))

# Now slightly too high, actually, go in between 0.1 and 0.2
lines(x = time_curve_guess, col = 'cornflowerblue', lwd = 2,
      y = logistic_growth_eqn(t = time_curve_guess, P0 = 5, K = 100, r = 0.175))

# Interestingly, r = 0.175 visually appears to follow the curve more closely
# (though, it underestimates the population size compared to r = 0.2).

##### Q3 #####

# Create a function that returns the negative log likelihood of the 
# model parameters (r, K) given the data (Nt)

calc_negll <- function(data, params) {
  r <- params[1]
  K <- params[2]
  sigma <- params[3]
  N <- data$N
  time <- data$time
  Nfit <- calc_logistic_growth_curve(N[1], time, r, K)
  -sum(dnorm(x=N, mean=Nfit, sd=sigma, log=T))
}

# Use optim() or a grid search and the function you just created to 
# estimate the parameters of your model – remember to add the 
# variance as a third parameter to be estimated (or use its analytical MLE: SSE/n)

# Below is the grid search option

# Set up parameter options
negll_grid_params <- expand.grid(
  r = seq(0,1,by=0.1), 
  K = seq(10,100,by=10),
  sigma = seq(1,20,by=1))

# Calculate -LogLikelihood for each combo
negll_grid_out <- apply(negll_grid_params, 1, calc_negll, data=abundance_ts) 

# Extract the parameters that result in the smallest negative log likelihood
negll_grid_mins <- negll_grid_params[which.min(negll_grid_out),] 
negll_grid_mins

# Here is the `optim()` option (I like this less ...)
optim(par = c(0.1, 100, 1), calc_negll, data = abundance_ts)

# How well can you estimate the model parameters?
# We can estimate them exactly because we generated the data given a
# specific r and K ... do we need different input data here since these 
# data were generated using the logistic growth equation with the specific
# K & r provided? 

##### Q4 #####

# Add observation error ( Nt + rnorm(mean=0, sd=?) ) to your data

set.seed(30)
abundance_ts_with_error <- abundance_ts %>% 
  mutate(N = abs(N + rnorm(nrow(abundance_ts), mean=0, sd=10)))

# Adjust sd above until this looks more like a real ecological dataset
plot(abundance_ts_with_error$time, abundance_ts_with_error$N) 

# Use optim() or a grid search and the function you just created to 
# estimate the parameters of your model based on this new data – 
# remember to add the variance as a third parameter to be estimated 
# (or use its analytical MLE: SSE/n)

negll_grid_out_error <- apply(negll_grid_params, 1, calc_negll, data=abundance_ts_with_error) 
negll_grid_mins_error <- negll_grid_params[which.min(negll_grid_out_error),] 
negll_grid_mins_error

# How well can you estimate the model parameters now?
# Considering it was an exact match before, not nearly as well. The
# K is the same, but the growth rate is lower. Using the `optim()` approach,
# we get the same growth rate but the K is slightly lower at 98
optim(par = c(0.1, 100, 1), calc_negll, data = abundance_ts_with_error)

# Is there any evidence of correlation in your parameter 
# estimates (e.g., a ridge in the likelihood surface)?
library(ggplot2)
bind_cols(negll_grid_params, negll = negll_grid_out_error) %>% 
  filter(sigma == negll_grid_mins_error$sigma) %>% 
  ggplot(aes(x = r, y = K, z = negll)) + 
  geom_contour_filled() + 
  ggtitle('Parameter correlation when using 5% of the carrying capacity') + 
  ylab('Values for K') + xlab('Values for r')

# No correlation because there is a peak and not a ridge in the plot.

##### Q5 #####

# Start your model with observation error at 5% of K

# Use optim() or a grid search and the function you just created to 
# estimate the parameters of your model based on this new data – 
# remember to add the variance as a third parameter to be estimated 
# (or use its analytical MLE: SSE/n)

# How well can you estimate the model parameters now?

# This is what we already did in Q1 - Q4 because we set the initial population
# size (N_intial = 5) at 5% of the carrying capacity (K = 100). So instead,
# we are showing this with 50%.

# Generate an abundance time series using a growth rate (r) of
# 0.2 (or 20%), a carrying capacity (K) of 100, and starting at
# 50% of the carrying capacity (N_initial = 0.50*100 = 50).
abundance_vals_q5 <- calc_logistic_growth_curve(N_initial = 50, time, r, K)
set.seed(501)
abundance_ts_q5 <- data.frame(time, N = abundance_vals_q5) %>% 
  # But add in observation error
  mutate(N = abs(N + rnorm(nrow(abundance_ts), mean=0, sd=10)))

# Plot the data with and without the noise (without the noise should just
# be the actual curve)
plot(abundance_ts_q5$time, abundance_ts_q5$N)
lines(abundance_ts_q5$time, abundance_vals_q5)

# Calculate -LogLikelihood for each combo
negll_grid_out_q5 <- apply(negll_grid_params, 1, calc_negll, data=abundance_ts_q5) 

# Extract the parameters that result in the smallest negative log likelihood
negll_grid_mins_q5 <- negll_grid_params[which.min(negll_grid_out_q5),] 
negll_grid_mins_q5

# This nailed it exactly - r = 0.2 and K = 100. There is a tiny bit of 
# correlation between r and K for values of r from ~0.2 to ~0.4. See this plot:
bind_cols(negll_grid_params, negll = negll_grid_out_q5) %>% 
  filter(sigma == negll_grid_mins_q5$sigma) %>% 
  ggplot(aes(x = r, y = K, z = negll)) + 
  geom_contour_filled() + 
  ggtitle('Parameter correlation when using 50% of the carrying capacity') + 
  ylab('Values for K') + xlab('Values for r')

##### Q6 #####

# Repeating this analysis starting at 90% of K, how well can you 
# estimate the model parameters?

# Generate an abundance time series using a growth rate (r) of
# 0.2 (or 20%), a carrying capacity (K) of 100, and starting at
# 90% of the carrying capacity (N_initial = 0.90*100 = 90).
abundance_vals_q6 <- calc_logistic_growth_curve(N_initial = 90, time, r, K)
set.seed(28)
abundance_ts_q6 <- data.frame(time, N = abundance_vals_q6) %>% 
  # But add in observation error
  mutate(N = abs(N + rnorm(nrow(abundance_ts), mean=0, sd=10)))

# Plot the data with and without the noise (without the noise should just
# be the actual curve)
plot(abundance_ts_q6$time, abundance_ts_q6$N)
lines(abundance_ts_q6$time, abundance_vals_q6)

# Calculate -LogLikelihood for each combo
negll_grid_out_q6 <- apply(negll_grid_params, 1, calc_negll, data=abundance_ts_q6) 

# Extract the parameters that result in the smallest negative log likelihood
negll_grid_mins_q6 <- negll_grid_params[which.min(negll_grid_out_q6),] 
negll_grid_mins_q6

# It is closer than expected, actually. The K lines up but the growth rate
# is too low (r = 0.1). Though, there seems to be a bit of correlation
# between r and K since there isn't as much data to confirm the pattern.
# See this plot:
bind_cols(negll_grid_params, negll = negll_grid_out_q6) %>% 
  filter(sigma == negll_grid_mins_q6$sigma) %>% 
  ggplot(aes(x = r, y = K, z = negll)) + 
  geom_contour_filled() + 
  ggtitle('Parameter correlation when using 90% of the carrying capacity') + 
  ylab('Values for K') + xlab('Values for r')
