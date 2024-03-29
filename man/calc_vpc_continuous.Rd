% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_vpc_continuous.R
\name{calc_vpc_continuous}
\alias{calc_vpc_continuous}
\title{Calculate aggregate statistics for simulated and observed VPC data}
\usage{
calc_vpc_continuous(sim, obs, loq, pi, ci, stratify, bins, bin_mid, verbose)
}
\arguments{
\item{sim}{this is usually a data.frame with observed data, containing the independent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}.  However it can also be an object like a nlmixr or xpose object}

\item{obs}{a data.frame with observed data, containing the independent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}}

\item{loq}{The list output from \code{define_loq()}}

\item{pi}{simulated prediction interval to plot. Default is c(0.05, 0.95),}

\item{ci}{confidence interval to plot. Default is (0.05, 0.95)}

\item{stratify}{character vector of stratification variables.}

\item{bins}{either "density", "time", or "data", "none", or one of the
approaches available in classInterval() such as "jenks" (default) or
"pretty", or a numeric vector specifying the bin separators.}

\item{bin_mid}{either "mean" for the mean of all timepoints (default) or "middle" to use the average of the bin boundaries.}

\item{verbose}{show debugging information (TRUE or FALSE)}
}
\value{
A list with "vpc_dat" and "aggr_obs"
}
\description{
Calculate aggregate statistics for simulated and observed VPC data
}
