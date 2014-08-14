vpc
===

An implementation of visual predictive checks (as commonly used in pharmacometrics) in R that does not require the use of NONMEM, PsN, or Xpose. The reason for developing this R module is to be able to more easily create VPCs in other modeling platforms such as R::nlme, Stan, etc.

To be supplied by user:
- structural model (written in R)
- parameter distributions for fixed and random effect parameters, and random effects
- observed data
- binning strategy

The module will:
- simulate data based on the model and parameter distributions, either from point estimates or from uncertainty / posterior distributions
- stratify if necessary
- compile aggregated data (binned)
- create VPC plot using ggplot2
- 
