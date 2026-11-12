#' Read in VPC data
#'
#' @inheritParams define_data_columns
#' @param sim this is usually a data.frame with observed data, containing the independent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}.  However it can also be an object like a nlmixr or xpose object
#' @param obs a data.frame with observed data, containing the independent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param psn_folder instead of specifying "sim" and "obs", specify a PsN-generated VPC-folder
#' @param sim_ref simulations of a reference dataset, for a reference-corrected
#'   VPC (rcVPC). Same format, columns and record order as \code{sim}, but
#'   simulated from a dataset in which selected independent variables (dose,
#'   covariates, time, ...) were set to reference values. Providing
#'   \code{sim_ref} enables reference-correction (Ibrahim et al., AAPS J 2025,
#'   \doi{10.1208/s12248-025-01065-2}): observed and simulated DV are normalized
#'   to the population prediction (\code{pred} column) and variability of the
#'   reference dataset, per record and before binning. Records are matched by
#'   position, so \code{obs} and every replicate in \code{sim} and
#'   \code{sim_ref} must contain the same records in the same order. The lower
#'   bound is taken from \code{pred_corr_lower_bnd}. The correction is
#'   undefined for DV at or below the lower bound (e.g. DV <= 0 with the
#'   default bound of 0); such records are excluded with a warning, also when
#'   \code{log_y = TRUE} (they are not set to \code{log_y_min}). A correction without the
#'   variability component (Eq. 1 only) can be done manually by multiplying DV
#'   in \code{obs} and \code{sim} by \code{PRED_REF / PRED} before calling
#'   \code{vpc()}.
#' @param software name of software platform using (e.g. nonmem, phoenix)
#' @param verbose show debugging information (TRUE or FALSE)
#' @return A list with names of "sim", "obs", "sim_ref", "software", and "cols"
read_vpc <- function(sim, obs, psn_folder,
                     software,
                     sim_cols, obs_cols,
                     sim_ref = NULL,
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
    class(obs) <- c(software, class(obs))
    obs <- filter_dv(obs, verbose)
  }
  if (!is.null(sim)) {
    class(sim) <- c(software, class(sim))
    sim <- filter_dv(sim, verbose)
  }
  if (!is.null(sim_ref)) {
    class(sim_ref) <- c(software, class(sim_ref))
    sim_ref <- filter_dv(sim_ref, verbose)
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
    if (!is.null(sim_ref)) {
      if("obs" %in% sim_ref$comp) {
        sim_ref <- sim_ref %>% dplyr::filter(comp == "obs")
      }
      sim_ref <- data.frame(sim_ref)
    }
  }
  ## define column names
  cols <-
    define_data_columns(
      sim=sim, obs=obs,
      sim_cols=sim_cols, obs_cols=obs_cols,
      software_type=software
    )

  list(sim=sim, obs=obs, sim_ref=sim_ref, software=software, cols=cols)
}
