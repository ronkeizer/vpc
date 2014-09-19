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

## Planned functionality

- function to simulate tte and rtte in R
- auto-binning by k-means clustering (from Lavielle et al. JPP 2011)

## Installation

    library("devtools")
    install_github("ronkeizer/vpc")
    library(vpc)
    ?vpc
   
## How to use

- There are three main functions:
  - `vpc`: VPC for continuous data
  - `vpc_cens`: VPC for censored continuous data (e.g. data < LOQ)
  - `vpc_tte`: VPC for (repeated) time-to-event data
- The main arguments to these three function are the `sim` and `obs` arguments, which specify a simulation dataset and an observation dataset. All other arguments can be used to customize data parsing and visual appearance of the VPC such as stratification and binning.
- All three functions will return an object with parsed data and the ggplot2 object. By default, the VPC will also be plotted.

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
    
In the first example, we'll perform the simulation in R. 

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

However, if we would do the simulation in NONMEM, we could simple use the commands below to create the VPC:

    obs <- read_table_nm("sdtab1")   # an output table with at least ID, TIME, DV
    sim <- read_table_nm("simtab1")  # a simulation file with at least ID, TIME, DV

The `read_table_nm()` function is a fast way to read in output from NONMEM's $TABLE record. 

Next, the VPC can then simply be created using:

    vpc (sim = sim, obs = obs)

Stratification for DOSE and SEX:

    vpc (sim = sim, obs = obs, strat = c("dose", "sex"))

Pred-correction, and plotting of data:

    vpc (sim = sim, obs = obs, pred_correction = TRUE, plot_dv = TRUE)

With more explicit use of options, and saving the object:

    vpc_dat <- vpc(sim = sim, 
                   obs = obs,                                   # supply simulation and observation dataframes
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

__Note: If you imported the data from NONMEM, the VPC function will automatically detect column names from NONMEM, such as ID, TIME, DV. If not, you might have to change the variable names for the dependent and independent variable, as well as the individual.__

The example below artificially induces an LLOQ of 5 for the above model / dataset, and generates a VPC for the probability of censoring.

     vpc_loq <- vpc_cens(sim, obs, lloq = 5)


## Time-to-event data

As for the VPC for continuous data, the VPC for TTE data requires simulated data. In general, there are two distinct approach to simulate survival data:

- *Hazard integration*: Integrate the hazard over time, and at *any possible* observation timepoint randomly draw a binary value based on the probability of observing the event. The disadvantage of this method is that it is slow due to the numerical solving of the ODEs. Also, a dataset with a dense design grid has to be used for simulation, i.e. one that has observation times at every possible timepoint that an event can occur for all individuals. 

- *Direct sampling*: Sample event times directly from the distribution used to model the data (e.g. Weibull, exponential, Gompertz). Advantages of this approach is that it is much faster, and it does not require a dense grid. The disadvantage with this approach is however that the hazard is assumed constant over time, so models with time-dependent hazards cannot easily be simulated with this approach. This approach is straightforward in R but cannot easily be implemented in NONMEM. Example will follow soon.

### Example RTTE data

    library(vpc)
    data(rtte_obs_nm) 
    data(rtte_sim_nm) 

    # treat RTTE as TTE, no stratification
    vpc_tte(sim = rtte_sim_nm, 
            obs = rtte_obs_nm, 
            rtte = FALSE, 
            sim_dv = "dv", obs_idv = "t", sim_idv = "t")

    # stratified for covariate and study arm
    vpc_tte(sim = rtte_sim_nm, 
            obs = rtte_obs_nm, 
            stratify = c("sex","drug"), 
            rtte = FALSE, 
            sim_dv = "dv", obs_idv = "t", sim_idv = "t")

    # stratified per event number (we'll only look at first 3 events) and stratify per arm
    vpc_tte(sim = rtte_sim_nm, 
            obs = rtte_obs_nm,
            rtte = TRUE, events = c(1:3),
            stratify = c("drug"),
            sim_dv = "dv", obs_idv = "t", sim_idv = "t")

