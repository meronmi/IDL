# basic (pseudo?!) R code to perform Metopolis Hastings sampling of posterior distribution of paramters
	
# you have a function called MODEL, which gives model output, and which MUST DEPEND on parameters that are supplied
# so, for eaxmple: model_result <- MODEL(parameters)

# you have OBS, which are the observed 'real' values of whatever the model simulates
# these are used in the calculation of liklihood
# here, we define a function to calculate this liklihood

calc_lik <- function(simulated, observed) {
# this is defined assuming a NORMAL DISTRIBUTION on the parameters
# this may not be correct, but there is a good argumnet for using it if you don't know what else it SHOULD be!

	n_obs <- length(observed)
	# assuming we don't need to check that simulated and observed are same length etc.

	if(any(is.na(simulated))) lik <- 0
	# easiest thing to do, becuase sometimes the model doesn't work with 'random' parameter sets!
	# in the MH framework, this parameter set is then ALWAYS rejected (as the liklihood ratio = 0)

	else{	dev <- (observed - simulated)
			var_dev <- var(dev)
			lik <- ((2 * pi * var_dev)^(-n_obs/2)) * exp((-1/(2 * var_dev)) * sum(dev^2))
			}
		
	return(lik)
	}

# so: liklihood <- calc_lik(model_result, OBS) 


# how long a MH MCMC chain do you want?
n_iter <- 50000
# how many parameters are being calibrated
n_param <- 6

# make a matrix to hold the sequence of 'sampled' parameter values
param_hist <- matrix(NA, ncol=n_param, nrow=n_iter+1)
# make a vector to hold the sequence of liklihood values associated with those parameter values
lik_hist <- vector(mode="numeric", length=n_iter+1)

# initialise with some (random?) first values of parameter vector
param_hist[1,] <- PARAM_0 # it doesn't matter what these are, but you need to specify them

model_result <- MODEL(PARAM_0)
lik_hist[1] <- calc_lik(model_result, OBS)

# now start the MH MCMC chain...
for (i in 2:(n_iter + 1)) {

	# define the new paraemeter set to be tested
	param_cand <- rnorm(n_param, param_hist[i-1,], param_jump_distr)
	# param_cand here, signifies candidate parameter set
	# param_jump_distr is a vector (length=number of parameters) with the sd of the distributions from which new values are drawn
	# new value = old value + rnorm(mean=0, sd=param_jump_distr)

	# I sometimes use extra code here to limit min and max possible values of parameters, but it may not be necessary:
	param_cand <- ifelse(param_cand < param_min, param_min, param_cand)
	param_cand <- ifelse(param_cand > param_max, param_max, param_cand)
	# obviously, you need to have defined param_min and param_max
	
	model_result <- MODEL(param_cand)
	lik <- calc_lik(model_result, OBS)

	accept <- runif(1)

	if((lik/lik_hist[i-1]) >= accept) {	param_hist[i,] <- param_cand
										lik_hist[i] <- lik
										}
	else {	param_hist[i,] <- param_hist[i-1,]
			lik_hist[i] <- lik_hist[i-1]
			}
	}

# when this loop has finished, you have a matrix holding the chain of sampled AND ACCEPTED parameter values
# whose distributions converge (in theory) to the posterior distributions of conditional parameter values

# discard the first X rows of this matrix as 'burn in', use the rest to estimate the parameter distrubutions!
