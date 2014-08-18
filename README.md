vpc
===

Create visual predictive checks, a commonly used diagnostic plot in pharmacometrics, in R, without the use of NONMEM, PsN, and Xpose. This approach is more flexible, more easily customizable, and faster than the NONMEM+PsN+Xpose approach.

To be supplied to function:

-  observed data
-  binning strategy
-  function to simulate data (i.e. a structural model with parameter estimates / distributions), **OR**
-  a simulation data file, e.g. from NONMEM or any other simulation software

## Installation and example

    library("devtools")
    install_github("ronkeizer/vpc")
    library(vpc)
    ?vpc

