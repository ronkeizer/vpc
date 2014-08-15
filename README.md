vpc
===

Create visual predictive checks in R, a commonly used diagnostic plot in pharmacometrics. Create it completely within the R environment, so without the use of NONMEM, PsN, and Xpose. This approach is more flexible (allows also to use models written in Monolix, Stan, etc.), more easily customizable, and faster than the PsN+Xpose approach, although it requires the user to rewrite the model in R.

To be supplied by user:
-  structural model (written in R)
-  parameter distributions for fixed and random effect parameters, and random effects
-  observed data
-  binning strategy

The module will:
-  simulate data based on the model and parameter distributions, either from point estimates or from uncertainty / posterior distributions
-  stratify if necessary
-  compile aggregated data (binned)
-  create VPC plot using ggplot2
