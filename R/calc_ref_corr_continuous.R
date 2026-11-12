#' Perform reference-correction (rcVPC)
#'
#' Reference-correct the observed and simulated dependent variable, following
#' Ibrahim, Jonsson & Bergstrand, AAPS J (2025) 27:86
#' (\doi{10.1208/s12248-025-01065-2}). Each record is normalized to the
#' population prediction of a user-defined reference dataset, and its
#' variability is scaled to that of simulations of the reference dataset. The
#' correction is applied per record, before binning.
#'
#' Records are matched by position: \code{obs}, each replicate in \code{sim},
#' and each replicate in \code{sim_ref} must contain the same records in the
#' same order.
#'
#' @inheritParams calc_pred_corr_continuous
#' @param sim_ref formatted simulations of the reference dataset (data.frame
#'   with standardized \code{id} and \code{dv} columns plus the pred column).
#' @param lower_bnd lower bound of the population prediction (\code{lb}).
#' @param log_dv is the dependent variable on log scale?
#' @return A list with "sim" and "obs", with \code{dv} reference-corrected.
calc_ref_corr_continuous <- function(sim, obs, sim_ref, lower_bnd = 0, log_dv = FALSE, cols, verbose = FALSE) {
  if(is.null(sim)) {
    stop("Reference-correction requires simulated data (`sim`).")
  }
  if(!cols$sim$pred %in% names(sim_ref)) {
    stop("Reference-correction: pred-variable (", cols$sim$pred, ") not found in reference simulation (`sim_ref`).")
  }
  if(!is.null(obs)) {
    obs <- ensure_obs_pred(sim = sim, obs = obs, cols = cols, verbose = verbose)
  } else if (!cols$sim$pred %in% names(sim)) {
    stop("Reference-correction: pred-variable (", cols$sim$pred, ") not found in simulated dataset.")
  }

  sim_idx <- add_sim_index_number(sim, id = "id", sim_label = cols$sim$sim)
  ref_idx <- add_sim_index_number(sim_ref, id = "id", sim_label = cols$sim$sim)
  sim_mat <- records_to_matrix(sim, sim_idx, "sim")
  ref_mat <- records_to_matrix(sim_ref, ref_idx, "sim_ref")
  n_rec <- nrow(sim_mat$dv)
  if(nrow(ref_mat$dv) != n_rec) {
    stop("Reference-correction: number of records per replicate differs between `sim` (", n_rec,
         ") and `sim_ref` (", nrow(ref_mat$dv), ").")
  }
  if(!all(as.character(ref_mat$id) == as.character(sim_mat$id))) {
    stop("Reference-correction: `id` order of records differs between `sim` and `sim_ref`.")
  }
  if(!is.null(obs)) {
    if(nrow(obs) != n_rec) {
      stop("Reference-correction: number of records in `obs` (", nrow(obs),
           ") differs from records per replicate in `sim` (", n_rec, ").")
    }
    if(!all(as.character(obs$id) == as.character(sim_mat$id))) {
      stop("Reference-correction: `id` order of records differs between `obs` and `sim`.")
    }
  }

  pred_ref <- as.num(sim_ref[[cols$sim$pred]][ref_idx == ref_idx[1]])
  sim_pred <- matrix(as.num(sim[[cols$sim$pred]]), nrow = n_rec)
  msg("Performing reference-correction...", verbose = verbose)
  res <- ref_correction_core(
    sim_dv = sim_mat$dv,
    sim_pred = sim_pred,
    ref_dv = ref_mat$dv,
    pred_ref = pred_ref,
    obs_dv = if(!is.null(obs)) as.num(obs$dv) else NULL,
    obs_pred = if(!is.null(obs)) as.num(obs[[cols$obs$pred]]) else NULL,
    lower_bnd = lower_bnd,
    log_dv = log_dv
  )
  sim$dv[sim_mat$order] <- as.vector(res$sim)
  if(!is.null(obs)) {
    obs$dv <- res$obs
  }
  if(any(is.nan(c(res$obs, res$sim)))) {
    warning("Reference-correction produced NaN values (e.g. DV or PRED at or below `pred_corr_lower_bnd`), these are excluded from the VPC.")
  }
  list(sim = sim, obs = obs)
}

#' Reshape simulated records into a records x replicates matrix
#'
#' @param data simulated data with standardized \code{id} and \code{dv}
#' @param sim_idx simulation index for each row
#' @param what label used in error messages
#' @return list with \code{dv} (matrix), \code{id} (ids of first replicate),
#'   and \code{order} (row indices of \code{data} in matrix order)
records_to_matrix <- function(data, sim_idx, what) {
  n_per_sim <- table(sim_idx)
  if(length(unique(n_per_sim)) != 1) {
    stop("Reference-correction: replicates in `", what, "` contain different numbers of records.")
  }
  if(length(n_per_sim) < 2) {
    stop("Reference-correction: `", what, "` must contain at least 2 replicates.")
  }
  ord <- order(match(sim_idx, unique(sim_idx)), seq_along(sim_idx))
  n_rec <- as.numeric(n_per_sim[1])
  dv <- matrix(as.num(data$dv[ord]), nrow = n_rec)
  ids <- matrix(data$id[ord], nrow = n_rec)
  if(!all(as.character(ids) == as.character(ids[, 1]))) {
    stop("Reference-correction: `id` order of records differs between replicates in `", what, "`.")
  }
  list(dv = dv, id = ids[, 1], order = ord)
}

#' Core reference-correction function
#'
#' Implements Eq. 1-4 of Ibrahim et al. (2025), consistent with the PsN
#' implementation (as included in Pharmpy).
#'
#' @param sim_dv matrix (records x replicates) of simulated DV
#' @param sim_pred matrix (records x replicates) of PRED for the observed design
#' @param ref_dv matrix (records x reference replicates) of DV simulated from
#'   the reference dataset
#' @param pred_ref vector (records) of PRED for the reference dataset
#' @param obs_dv vector (records) of observed DV, or NULL
#' @param obs_pred vector (records) of PRED for the observed records, or NULL
#' @param lower_bnd lower bound (scalar or vector per record)
#' @param log_dv is DV on log scale?
#' @return list with \code{obs} (vector or NULL) and \code{sim} (matrix)
ref_correction_core <- function(sim_dv, sim_pred, ref_dv, pred_ref,
                                obs_dv = NULL, obs_pred = NULL,
                                lower_bnd = 0, log_dv = FALSE) {
  lb <- lower_bnd
  correct <- function(dv, pred) {
    if(log_dv) {
      dv + pred_ref - pred
    } else {
      factor <- (pred_ref - lb) / (pred - lb)
      factor[(pred - lb) == 0] <- 1
      lb + (dv - lb) * factor
    }
  }
  row_sd <- function(m) apply(m, 1, stats::sd, na.rm = TRUE)
  rc_sim <- correct(sim_dv, sim_pred)
  if(log_dv) {
    sd_ref <- row_sd(ref_dv)
    sd_rc  <- row_sd(rc_sim)
  } else {
    sd_ref <- suppressWarnings(row_sd(log(ref_dv - lb)))
    sd_rc  <- suppressWarnings(row_sd(log(rc_sim - lb)))
  }
  var_factor <- sd_ref / sd_rc
  var_factor[!is.na(sd_rc) & sd_rc == 0] <- 1
  var_correct <- function(rc) {
    if(log_dv) {
      pred_ref + (rc - pred_ref) * var_factor
    } else {
      log_pred_ref <- log(pred_ref - lb)
      suppressWarnings(lb + exp(log_pred_ref + (log(rc - lb) - log_pred_ref) * var_factor))
    }
  }
  list(
    obs = if(!is.null(obs_dv)) var_correct(correct(obs_dv, obs_pred)) else NULL,
    sim = var_correct(rc_sim)
  )
}

#' Make sure observed data has a pred column
#'
#' If the pred-column is not in the observed data, take it from the first
#' replicate of the simulated data.
#'
#' @inheritParams calc_pred_corr_continuous
#' @return \code{obs}, with pred column
ensure_obs_pred <- function(sim, obs, cols, verbose = FALSE) {
  if(!cols$obs$pred %in% names(obs)) {
    msg("Warning: specified pred-variable not found in observation dataset, trying to get from simulated dataset...", verbose)
    if (!cols$sim$pred %in% names(sim)) {
      stop(
        "Specified pred-variable for observed data (", cols$obs$pred,
        ") not found in simulated dataset, not able to perform correction!"
      )
    }
    obs <- obs %>% dplyr::ungroup()
    obs[[cols$obs$pred]] <- unlist(sim[seq_len(nrow(obs)), cols$sim$pred])
    msg("OK", verbose)
  } else if (!cols$sim$pred %in% names(sim)) {
    stop(
      "Specified pred-variable (", cols$sim$pred,
      ") not found in simulated dataset, not able to perform correction!"
    )
  }
  obs
}
