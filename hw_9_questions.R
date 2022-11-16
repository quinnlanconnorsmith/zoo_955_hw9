
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

# TODO: no idea what to do here
abundance.glm <- glm(N ~ time, data=abundance_ts, family="binomial")
time_tiny <- seq(1,50,by=0.01)
lines(time_tiny, predict(abundance.glm, data.frame(time=time_tiny), type="response"))

# Plot the data

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
# TODO: Probably need different input data here
negll_grid_out <- apply(negll_grid_params, 1, calc_negll, data=abundance_ts) 

# Extract the parameters that result in the smallest negative log likelihood
negll_grid_mins <- negll_grid_params[which.min(negll_grid_out),] 
negll_grid_mins

# Here is the `optim()` option (I like this less ...)
optim(par = c(0.1, 100, 1), calc_negll, data = abundance_ts)

# How well can you estimate the model parameters?
# TODO: ANSWER THIS

##### Q4 #####

# Add observation error ( Nt + rnorm(mean=0, sd=?) ) to your data

set.seed(30)
abundance_ts_with_error <- abundance_ts %>% 
  mutate(N = N + rnorm(nrow(abundance_ts), mean=0, sd=10))

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

# TODO: Answer this

# Is there any evidence of correlation in your parameter 
# estimates (e.g., a ridge in the likelihood surface)?
library(ggplot2)
bind_cols(negll_grid_params, negll = negll_grid_out_error) %>% 
  filter(sigma == negll_grid_mins_error$sigma) %>% 
  ggplot(aes(x = r, y = K, z = negll)) + 
  geom_contour_filled() + 
  ggtitle(sprintf('Using sigma=%s', round(negll_grid_mins_error$sigma, digits = 2))) + 
  ylab('Values for K') + xlab('Values for r')

# TODO: Answer this / get the plot to work

##### Q5 #####

# Start your model with observation error at 5% of K

# Use optim() or a grid search and the function you just created to 
# estimate the parameters of your model based on this new data – 
# remember to add the variance as a third parameter to be estimated 
# (or use its analytical MLE: SSE/n)

# How well can you estimate the model parameters now?

##### Q6 #####

# Repeating this analysis starting at 90% of K, how well can you 
# estimate the model parameters?

