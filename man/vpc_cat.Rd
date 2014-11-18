% Generated by roxygen2 (4.0.2): do not edit by hand
\name{vpc_cat}
\alias{vpc_cat}
\title{VPC function for categorical}
\usage{
vpc_cat(sim = NULL, obs = NULL, bins = "density", n_bins = 8,
  type = "bloq", obs_dv = NULL, sim_dv = NULL, obs_idv = NULL,
  sim_idv = NULL, obs_id = NULL, sim_id = NULL, nonmem = "auto",
  ci = c(0.05, 0.95), uloq = NULL, lloq = NULL, plot = FALSE,
  xlab = NULL, plot_sim_med = FALSE, ylab = NULL, title = NULL,
  smooth = TRUE, theme = "default", custom_theme = NULL, facet = "wrap")
}
\arguments{
\item{sim}{a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}}

\item{obs}{a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}}

\item{bins}{either "density", "time", or "data", or a numeric vector specifying the bin separators.}

\item{n_bins}{number of bins}

\item{type}{either "lloq" (default) or "uloq".}

\item{obs_dv}{variable in data.frame for observed dependent value. "dv" by default}

\item{sim_dv}{variable in data.frame for simulated dependent value. "sdv" by default}

\item{obs_idv}{variable in data.frame for observed independent value. "time" by default}

\item{sim_idv}{variable in data.frame for simulated independent value. "time" by default}

\item{obs_id}{variable in data.frame for observed individual. "id" by default}

\item{sim_id}{variable in data.frame for simulated individual. "id" by default}

\item{nonmem}{should variable names standard to NONMEM be used (i.e. ID, TIME, DV, PRED).  Default is "auto" for autodetect}

\item{ci}{confidence interval to plot. Default is (0.05, 0.95)}

\item{uloq}{Number or NULL indicating upper limit of quantification. Default is NULL.}

\item{lloq}{Number or NULL indicating lower limit of quantification. Default is NULL.}

\item{plot}{Boolean indacting whether to plot the ggplot2 object after creation. Default is FALSE.}

\item{xlab}{ylab as numeric vector of size 2}

\item{ylab}{ylab as numeric vector of size 2}

\item{title}{title}

\item{smooth}{"smooth" the VPC (connect bin midpoints) or show bins as rectangular boxes. Default is TRUE.}

\item{theme}{which theme to load from the themes object}

\item{custom_theme}{specify a custom ggplot2 theme}

\item{facet}{either "wrap", "columns", or "rows"}
}
\value{
a list containing calculated VPC information, and a ggplot2 object
}
\description{
Creates a VPC plot from observed and simulation data
sim,
}
\seealso{
\link{vpc}
}
