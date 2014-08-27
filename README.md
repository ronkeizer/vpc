vpc
===

Create visual predictive checks, a commonly used diagnostic plot in pharmacometrics, in R, without the use of NONMEM, PsN, and Xpose. This approach is more flexible, more easily customizable, and faster than the NONMEM+PsN+Xpose approach.

To be supplied to function:

-  observed data
-  function to simulate data (i.e. a structural model with parameter estimates / distributions), **OR** a simulation dataset, e.g. simulated from NONMEM or other simulation software

## Installation

    library("devtools")
    install_github("ronkeizer/vpc")
    library(vpc)
    ?vpc
    
## Example

    library(vpc)

    ## We're gonna use the famous theophylline dataset
    obs <- Theoph
    colnames(obs) <- c("id", "wt", "dose", "time", "dv")
    obs <- obs %>%
            group_by(id) %>%  
            mutate(sex = round(runif(1))) # generate a "sex" covariate
    
    ## In this example, we'll simulate new data in R
    ## But this can also be done using a simple $SIM block in NONMEM
    sim <- sim_data(obs, # the design of the dataset
                    model = function(x) { # the model
                      pk_oral_1cmt (t = x$time, dose=x$dose * x$wt, ka = x$ka, ke = x$ke, cl = x$cl * x$wt, ruv = list(additive = 0.1))
                    }, 
                    theta = c(2.774, 0.0718, .0361),                 # parameter values
                    omega_mat = c(0.08854,                           # specified as lower triangle by default; 
                                  0.02421, 0.02241,                  # note: assumed that every theta has iiv, set to 0 if no iiv. 
                                  0.008069, 0.008639, 0.02862),      
                    par_names = c("ka", "ke", "cl"),                 # link the parameters in the model to the thetas/omegas
                    n = 500)
    
    ## First plot with auto binning:    
    vpc_dat <- vpc(sim, obs, stratify = c("sex"), 
                   n_bins = 8                                   # aim for 8 or less bins in the autobin procedure
                   plot.dv = FALSE, facet = "wrap",
                   ylab = "Concentration", xlab = "Time (hrs)")

    ## More elaborate use of options
    vpc_dat <- vpc(sim, obs,                                    # supply simulation and observation dataframes
                   obs.dv = "dv",                               # these column names are the default,                           
                   obs.idv = "time",                            #   update these if different.
                   sim.dv = "sdv",
                   sim.idv = "time",
                   bins = c(0, 2, 4, 6, 8, 10, 25),             # specify bin separators manually
                   stratify = c("sex"),                         # multiple stratifications possible, just supply as vector
                   plot.dv = TRUE,                              # plot observations?
                   facet = "wrap",                              # wrap stratifications, or as "row" or "column"
                   ylab = "Concentration", xlab = "Time (hrs)", title="Visual predictive check")

    

