#QS attempt 1 at hw9
#Failure 
#Generate a time series of abundance for a population growing according to the discrete time logistic growth model

#<-1.15 #growth of 15% 
#K <- 100 #carrying capactiy 
#time = seq(from=1, to=50) #50 time steps 
#N=array(dim=c(1,length(time))); N[1]=5 #array to store goodie, initial pop of 5

#for (i in time[2:length(time)]){ 
#  N[i] = N[i-1]*r
#  }

#for (i in time[2:length(time)]){ 
#  N[i] = N[i-1]*r{
#    if (N<K)
#  }
#}

#N
#Why are these all 1's?
#matplot(t(N))

#Round2 

r<-.15 #growth of 15% 
K <- 100 #carrying capacity 
time = seq(from=1, to=50) #50 time steps 


# The equation 
eq <- function(N_p, r, K) {
  N_p + r*N_p*(1 - N_p/K)
}

# Generate time series 
pop <- function(N_i, time, r, K) {
  N <- rep(NA, length(time)) 
  N[1] <- N_i
  for (i in time[2:length(time)]) {
    N[i] <- eq(N[i-1], r, K)
  } 
  return(N)
}


# Begin grid search: i <- 1; j <- 1
#for(i in 1:length(b0_grid)){
#  b0_curr <- b0_grid[i]
#  for(j in 1:length(b1_grid)){
#    b1_curr <- b1_grid[j]
#    y_pred <- b0_curr + b1_curr*x
#    rss <- rss_1(y, y_pred)
#    rss_matrix[i,j] <- rss
#  }
#}

#Q1. Plot the data
#• Generate a model predicted time series of abundance for a population growing according to the discrete time logistic growth model
#• Pretend you don’t know the true parameters (r and K) and adjust them until your model fits the data pretty well (visually)

vals <- pop(N_i = 5, time, r, K)
df <- data.frame(time, N = vals)

# Plot the data.

plot(df$time, df$N)

#Q2.Create a function that returns the negative log likelihood of the 
#model parameters (r, K) given the data (Nt)
#•Use optim() or a grid search and the function you just created to 
#estimate the parameters of your model – remember to add the 
#variance as a third parameter to be estimated (or use its analytical MLE: SSE/n)


y=5 + r*5*(1 - 5/K)
#Standard deviation = 34.69
sigma <- sd(df$N)
mean <- mean(df$N)

nll_1 <- function(r,K, sigma){
  -sum(dnorm(x=y, mean=mean, sd=sigma, log=T))
}


fun_1 <- function(par){
  r <- par[1]
  K <- par[2]
  sigma <- par[3]
  mean <- mean
  nll <- nll_1(r,K, sigma)
}

#optim() stuff 
fit <- optim(par=c(1,2,3), fn=fun_1)
optim_coef <- fit$par

optim_coef

#[1] 70.208542  1.616995 31.496970
#Well that seems very off 
#[1] -0.1541297  2.3647926  5.7116327
#[1] -22.417269   3.032915  52.368151

#•Q3. How well can you estimate the model parameters?
#Not very well it would seem 


#•Add observation error ( Nt + rnorm(mean=0, sd=?) ) to your data
#•Use optim() or a grid search and the function you just created to 
#estimate the parameters of your model based on this new data – 
#remember to add the variance as a third parameter to be estimated (or use its analytical MLE: SSE/n)

y2=5 + r*5*(1 - 5/K)+rnorm(length(time), mean=mean, sd=sigma)

nll_2 <- function(r,K, sigma){
  -sum(dnorm(x=y2, mean=mean, sd=sigma, log=T))
}


fun_2 <- function(par){
  r <- par[1]
  K <- par[2]
  sigma <- par[3]
  mean <- mean
  nll <- nll_2(r,K, sigma)
}

#optim() stuff 
fit <- optim(par=c(1,2,3), fn=fun_2)
optim_coef <- fit$par

optim_coef
#[1] -538.70340   21.98341 1134.80586
#Hmm this also seem very off 


#•Q4. How well can you estimate the model parameters now?  
#Is there any evidence of correlation in your parameter estimates (e.g., a ridge in the likelihood surface)?

#We have been running the model with Ni starting at 5 

#•Start your model with observation error at 5% of K
#•Use optim() or a grid search and the function you just created to 
#estimate the parameters of your model based on this new data – 
#remember to add the variance as a third parameter to be estimated (or use its analytical MLE: SSE/n)

#•Q5. How well can you estimate the model parameters now?

#•Q6. Repeating this analysis starting at 90% of K, how well can you estimate the model parameters?

vals_2 <- pop(N_i = 90, time, r, K)
df_2 <- data.frame(time, N = vals_2)

sigma_2 <- sd(df_2$N)
mean_2 <- mean(df_2$N)

eq <- function(N_p, r, K) {
  N_p + r*N_p*(1 - N_p/K)
}

# Generate time series 
pop <- function(N_i, time, r, K) {
  N <- rep(NA, length(time)) 
  N[1] <- N_i
  for (i in time[2:length(time)]) {
    N[i] <- eq(N[i-1], r, K)
  } 
  return(N)
}

plot(df_2$time, df_2$N)

y3=90 + r*90*(1 - 90/K)+rnorm(length(time), mean=mean_2, sd=sigma_2)

nll_3 <- function(r,K, sigma_2){
  -sum(dnorm(x=y2, mean=mean_2, sd=sigma_2, log=T))
}


fun_3 <- function(par){
  r <- par[1]
  K <- par[2]
  sigma <- par[3]
  mean <- mean
  nll <- nll_3(r,K, sigma_2)
}

#optim() stuff 
fit <- optim(par=c(1,2,3), fn=fun_3)
optim_coef <- fit$par

optim_coef

#Well this doesn't work quite as intended either 