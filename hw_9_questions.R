#Generate a time series of abundance for a population growing according to the discrete time logistic growth model.• r <- 0.2
#• K <- 100
#• time = seq(from=1, to=50)
#• N=array(dim=c(1,length(time))); N[1]=5
#• for (i in time[2:length(time)]{• N[i] = ...
#• } 

#Q1. Plot the data
#• Generate a model predicted time series of abundance for a population growing according to the discrete time logistic growth model
#• Pretend you don’t know the true parameters (r and K) and adjust them until your model fits the data pretty well (visually)

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