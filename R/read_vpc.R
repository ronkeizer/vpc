#' Read in VPC data
#' 
#' @inheritParams define_data_columns
#' @param sim this is usually a data.frame with observed data, containing the independent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}.  However it can also be an object like a nlmixr or xpose object
#' @param obs a data.frame with observed data, containing the independent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param psn_folder instead of specifying "sim" and "obs", specify a PsN-generated VPC-folder
#' @param software name of software platform using (e.g. nonmem, phoenix)
#' @param verbose show debugging information (TRUE or FALSE)
#' @return A list with names of "sim", "obs", "software", and "cols"
read_vpc <- function(sim, obs, psn_folder,
                     software,
                     sim_cols, obs_cols,
                     verbose=FALSE) {
  if(!is.null(psn_folder)) {
    if (is.null(obs)) {
      msg("Reading oberved data...", verbose=verbose)
      obs <- read_table_nm(paste0(psn_folder, "/m1/", dir(paste0(psn_folder, "/m1"), pattern="original.npctab")[1]))
    }
    if (is.null(sim)) {
      msg("Reading simulated data...", verbose=verbose)
      sim <- read_table_nm(paste0(psn_folder, "/m1/", dir(paste0(psn_folder, "/m1"), pattern="simulation.1.npctab")[1]))
    }
    software <- "nonmem"
  }
  if(is.null(obs) & is.null(sim)) {
    stop("At least a simulation or an observation dataset are required to create a plot!")
  }
  if (!is.null(obs)) {
    software <- guess_software(software, obs)
  } else {
    software <- guess_software(software, sim)
  }
  if (!is.null(obs)) {
    obs <- filter_dv(obs, verbose)
    class(obs) <- c(software, class(obs))
  }
  if (!is.null(sim)) {
    sim <- filter_dv(sim, verbose)
    class(sim) <- c(software, class(sim))
  }

  ## software specific parsing, if necessary
  if (software == "PKPDsim") {
    if (!is.null(obs)) {
      if("obs" %in% obs$comp) {
        obs <- obs %>% dplyr::filter(comp == "obs")
      }
      obs <- data.frame(obs)
    }
    if (!is.null(sim)) {
      if("obs" %in% sim$comp) {
        sim <- sim %>% dplyr::filter(comp == "obs")
      }
      sim <- data.frame(sim)
    }
  }
  ## define column names
  cols <-
    define_data_columns(
      sim=sim, obs=obs,
      sim_cols=sim_cols, obs_cols=obs_cols,
      software_type=software
    )

  list(sim=sim, obs=obs, software=software, cols=cols)
}
