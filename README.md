vpc
===

Create visual predictive checks, a commonly used diagnostic plot in pharmacometrics in R 

## Rationale

The VPC is a widely used diagnostic tool in pharmacometrics is the VPC, most commonly created using [PsN](http://psn.sourceforge.net) and [Xpose](http://xpose.sourceforge.net), using NONMEM as the simulation engine. The aim of the current library is to provide an improved tool that is:

- a single-step process for creating a VPC (previously: simulation done in NONMEM, data processing by PsN, plotting by Xpose/lattice). This e.g. allows changing of vpc parameters such as binning / stratification upon creation of the plot, not in a separate pre-processing step. 
- more flexible regarding input (use simulated data from R, NONMEM data, or any other tool)
- more easily customizable, e.g. request any prediction / confidence interval or binning strategy upon plotting.
- more easily extensible: the output is a ggplot object which can be easily themed and extended
- easier to use in the case of survival / repeated time-to-event data (the PsN/Xpose approach is a bit hairy for this type of data)
- faster

## Functionality available

- VPC for continuous data
- VPC for censored data (or binary data in general)
- VPC for (single) time-to-event data simulated from NONNMEM
- stratification (single & multiple)
- prediction-correction
- auto-binning (search for x-variable density-nadirs)
- general-purpose function to simulate data from a mixed-effects structural model, from a fixed parameter vector and between-subject variability covariance matrix.

## Planned

- repeated tte from NONMEM
- simulate tte in R
- auto-binning by k-means clustering (from Lavielle et al. JPP 2011)
- Kaplan-Meier Mean Covariate plots [KMMC](http://page-meeting.org/pdf_assets/4280-2012-06%20PAGE%20KMMC.pdf)

## Installation

    library("devtools")
    install_github("ronkeizer/vpc")
    library(vpc)
    ?vpc
    
## Examples

Load the library and get the observation data. We'll be using the Theohpylline dataset, with randomly assigned 'sex' covariate. 

    library(dplyr)
    library(vpc)

    ## Load the theophylline PK dataset
    obs <- Theoph
    colnames(obs) <- c("id", "wt", "dose", "time", "dv")
    obs <- obs %>%
            group_by(id) %>%  
            mutate(sex = round(runif(1))) # generate a "sex" covariate
    
In this example, we'll simulate new data in R. But a simulation dataset from NONMEM or any other simulation software can be imported

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

    
VPC with auto binning:    

    vpc_dat <- vpc(sim, obs, stratify = c("sex"), 
                   n_bins = 8)                                   # aim for 8 or less bins in the autobin procedure

Similar VPC, but more explicit use of options:

    vpc_dat <- vpc(sim, obs,                                    # supply simulation and observation dataframes
                   obs.dv = "dv",                               # these column names are the default,                           
                   obs.idv = "time",                            #   update these if different.
                   sim.dv = "sdv",
                   sim.idv = "time",
                   bins = c(0, 2, 4, 6, 8, 10, 25),             # specify bin separators manually
                   stratify = c("sex"),                         # multiple stratifications possible, just supply as vector
                   pi = c(0.05, 0.95),                          # prediction interval simulated data to show
                   ci = c(0.05, 0.95),                          # confidence intervals to show
                   pred_corr = FALSE,                           # perform prediction-correction?
                   plot.dv = TRUE,                              # plot observations?
                   facet = "wrap",                              # wrap stratifications, or as "row" or "column"
                   ylab = "Concentration", 
                   xlab = "Time (hrs)", 
                   title="VPC Theophylline model")

The example below artificially induces an LLOQ of 5 for the above model / dataset, and generates a VPC for the probability of censoring.

     vpc_loq <- vpc_cens(sim, obs, lloq = 5)


### Time-to-event data

Assuming an estimation model and simulation model have been run in NONMEM, the simulation model using a 'dense grid'.

    obs <- tbl_df(read.table.nm("nm/sdtab51"))  
    sim <- tbl_df(read.table.nm("nm/simtab51"))
    
    ## create the VPC, stratified by dose
    vpc_t <- vpc_tte(sim, obs, 
                     n_bins = 15,
                     stratify = "dose",
                     facet = "wrap",
                     nonmem = TRUE,  # use NONMEM common data labels
                     smooth = TRUE)
