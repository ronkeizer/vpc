vpc
===

Create visual predictive checks, a commonly used diagnostic plot in pharmacometrics in R 

## Rationale

The VPC is a widely used diagnostic tool in pharmacometrics (see e.g. [here](http://page-meeting.org/default.asp?abstract=1434)), most commonly created using [PsN](http://psn.sourceforge.net) and [Xpose](http://xpose.sourceforge.net), using NONMEM as the simulation engine. The aim of the current library is to provide an improved tool that is:

- a single-step process for creating a VPC in R (not using Xpose or PsN). 
  - e.g. allows changing of vpc parameters such as binning / stratification upon creation of the plot, not in a separate pre-processing step.
  - easier debugging than PsN+Xpose, all data parsing and plotting in one R function
- more flexible regarding input (use simulated data from R, NONMEM, Monolix, Stan, or any other simulation tool)
- easier to customize, e.g. request any prediction / confidence interval or binning strategy upon plotting.
- easier to extend: the output is a ggplot object which can be themed and extended
- faster


## Functionality available

Plots:
- VPC for continuous data
- VPC for censored continuous data (e.g. below LOQ)
- VPC for categorical data
- VPC for time-to-event data (single and repeated)

Options:
- stratification (single & multiple)
- prediction-correction
- binning methods (Jenk's classification, K-means clustering, by time, by number of data points, by density, etc).
- automatated (using Sturges' rule) or manual input of number of bins
- Kaplan-Meier Mean Covariate plots [KMMC](http://page-meeting.org/pdf_assets/4280-2012-06%20PAGE%20KMMC.pdf)
- plot without simulated data (`sim=NULL`), or without observed data(`obs=NULL`).
- general-purpose function (`sim_data`) to simulate data from a mixed-effects structural model, a fixed parameter vector and between-subject variability covariance matrix.

## Planned functionality

- npde plots with prediction intervals, see e.g. [here](http://page-meeting.org/pdf_assets/9164-venise12_posternpde.pdf)
- function to simulate tte and rtte in R

## Installation

    library("devtools")
    install_github("ronkeizer/vpc")
    library(vpc)
    ?vpc
   
## How to use

See [vignette](http://ronkeizer.github.io/vpc/)
