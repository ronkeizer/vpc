## VPC example for Theophylline model
setwd("R/")
source ("vpc.R")

## observation data, only change column headers to match what sim function expects
obs <- Theoph
colnames(obs) <- c("id", "wt", "dose", "time", "dv")
obs$sex <- round(runif(unique(obs$id))) # create a dummy covariate to show stratification

## Perform simulation
sim <- sim_data(obs, # the design of the dataset
                model = function(x, par) { # the model
                  pk_oral_1cmt (t = x$time, dose=x$dose * x$wt, ka = par$ka, ke = par$ke, cl = par$cl * x$wt, ruv = list(additive = 0.1))
                }, 
                theta = c(2.774, 0.0718, .0361),                 # parameter values
                omega_mat = c(0.08854, 
                              0.02421, 0.02241,
                              0.008069, 0.008639, 0.02862),      # specified as lower triangle by default; every theta has iiv, set to 0 if no iiv. 
                par_names = c("ka", "ke", "cl"),                 # link the parameters in the model to the thetas/omegas
                n = n_sim)

## Create the plot and return the plot data
vpc_dat <- vpc(sim, obs, 
               bins = c(0, 2, 4, 6, 8, 10, 25), 
               strat = "sex",
               plot = TRUE,
               ylab = "Concentration", xlab = "Time (hrs)")
