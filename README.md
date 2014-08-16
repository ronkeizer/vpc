vpc
===

Create visual predictive checks, a commonly used diagnostic plot in pharmacometrics, in R. So without the use of NONMEM, PsN, and Xpose. This approach is more flexible, more easily customizable, and faster than the NONMEM+PsN+Xpose approach.

To be supplied:

-  observed data
-  binning strategy
-  structural model with parameter estimates / distributions (Alternatively, you can also supply a simulation data file, e.g. from NONMEM).

The module will:

-  simulate data based on the model and parameter distributions, either from point estimates or from uncertainty / posterior distributions
-  stratify if requested
-  compile aggregated data (binned)
-  create VPC plot using ggplot2, or return a dataset that can be used for plotting

## Installation

    library("devtools")
    install_github("vpc", "ronkeizer")
