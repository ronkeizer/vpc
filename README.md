vpc
===

Create visual predictive checks, a commonly used diagnostic plot in pharmacometrics in R 

## Rationale

The VPC is a widely used diagnostic tool in pharmacometrics (see e.g. [here](http://page-meeting.org/default.asp?abstract=1434)), most commonly created using [PsN](http://psn.sourceforge.net) and [Xpose](http://xpose.sourceforge.net), using NONMEM as the simulation engine. The aim of the current library is to provide an improved tool that is:

- a single-step process for creating a VPC. 
  - allows changing of vpc parameters such as binning / stratification upon creation of the plot, not in a separate pre-processing step.
  - easier debugging than PsN+Xpose, all data parsing and plotting in one R function
- more flexible regarding input (use simulated data from R, NONMEM, Monolix, Stan, or any other simulation tool)
- easier to customize, e.g. request any prediction / confidence interval or binning strategy upon plotting.
- easier to extend: the output is a ggplot object which can be themed and extended
- more flexible in the case of survival / repeated time-to-event data
- faster

## Functionality available

- VPC for continuous data
- VPC for censored continuous data (e.g. below LOQ)
- VPC for time-to-event data (single and repeated)
- stratification (single & multiple)
- prediction-correction
- auto-binning (search for x-variable density-nadirs)
- Kaplan-Meier Mean Covariate plots [KMMC](http://page-meeting.org/pdf_assets/4280-2012-06%20PAGE%20KMMC.pdf)
- general-purpose function to simulate data from a mixed-effects structural model, a fixed parameter vector and between-subject variability covariance matrix.

## Planned

- update manual, provide more examples
- function to simulate tte and rtte in R
- auto-binning by k-means clustering (from Lavielle et al. JPP 2011)

## Installation

    library("devtools")
    install_github("ronkeizer/vpc")
    library(vpc)
    ?vpc
    
## Examples

Load the library and get the observation data. The examples here use the Theohpylline dataset, with randomly assigned 'sex' covariate. 

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
                   obs_dv = "dv",                               # these column names are the default,                           
                   obs_idv = "time",                            #   update these if different.
                   sim_dv = "sdv",
                   sim_idv = "time",
                   bins = c(0, 2, 4, 6, 8, 10, 25),             # specify bin separators manually
                   stratify = c("sex"),                         # multiple stratifications possible, just supply as vector
                   pi = c(0.05, 0.95),                          # prediction interval simulated data to show
                   ci = c(0.05, 0.95),                          # confidence intervals to show
                   pred_corr = FALSE,                           # perform prediction-correction?
                   plot_dv = TRUE,                              # plot observations?
                   facet = "wrap",                              # wrap stratifications, or as "row" or "column"
                   ylab = "Concentration", 
                   xlab = "Time (hrs)", 
                   title="VPC Theophylline model")

The example below artificially induces an LLOQ of 5 for the above model / dataset, and generates a VPC for the probability of censoring.

     vpc_loq <- vpc_cens(sim, obs, lloq = 5)


### Time-to-event data

As for the VPC for continuous data, the VPC for TTE data requires simulated data. In general, there are two distinct approach to simulate survival data:

- *Hazard integration*: Integrate the hazard over time, and at *any possible* observation timepoint randomly draw a binary value based on the probability of observing the event. The disadvantage of this method is that it is slow due to the numerical solving of the ODEs. Also, a dataset with a dense design grid has to be used for simulation, i.e. one that has observation times at every possible timepoint that an event can occur for all individuals. 

- *Direct sampling*: Sample event times directly from the distribution used to model the data (e.g. Weibull, exponential, Gompertz). Advantages of this approach is that it is much faster, and it does not require a dense grid. The disadvantage with this approach is however that the hazard is assumed constant over time, so models with time-dependent hazards cannot easily be simulated with this approach. This approach is straightforward in R but cannot easily be implemented in NONMEM. Example will follow soon.

### Example VPC for RTTE data

    library(vpc)
    data(rtte_obs_nm) 
    data(rtte_sim_nm) 

    # treat RTTE as TTE, no stratification
    vpc_tte(rtte_sim_nm, rtte_obs_nm, 
            rtte = FALSE, bin_method="obs",
            sim_dv = "dv", obs_idv = "t", sim_idv = "t", n_sim = 100)

    # stratified for covariate and study arm
    vpc_tte(rtte_sim_nm, rtte_obs_nm, 
            stratify = c("sex","drug"), 
            rtte = FALSE, bin_method = "spread", n_bins=16,
            sim_dv = "dv", obs_idv = "t", sim_idv = "t", n_sim = 100)

    # stratified per event number (we'll only look at first 3 events) and stratify per arm
    vpc_tte(rtte_sim_nm, rtte_obs_nm,
            rtte = TRUE, occasions = c(1:3),
            stratify = c("drug"), bin_method="obs", 
            sim_dv = "dv", obs_idv = "t", sim_idv = "t", n_sim = 100)

