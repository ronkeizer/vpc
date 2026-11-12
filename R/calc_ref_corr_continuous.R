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
#' same order. Alignment is checked on \code{id} and \code{idv} for
#' \code{obs} and \code{sim} replicates, and on \code{id} only for
#' \code{sim_ref}, since the independent variable (e.g. time) may be
#' deliberately changed in the reference dataset.
#'
#' @inheritParams calc_pred_corr_continuous
#' @param sim_ref formatted simulations of the reference dataset (data.frame
#'   with standardized \code{id} and \code{dv} columns plus the pred column).
#' @param lower_bnd lower bound of the population prediction (\code{lb}).
#' @param log_dv is the dependent variable on log scale?
#' @param lloq,uloq limits of quantification. Simulated values outside these
#'   limits are set to \code{NA} after correction, so that the variability
#'   correction is based on all simulated data. Observed data is expected to
#'   be censored already.
#' @return A list with "sim" and "obs", with \code{dv} reference-corrected.
calc_ref_corr_continuous <- function(sim, obs, sim_ref, lower_bnd = 0, log_dv = FALSE, cols,
                                     lloq = NULL, uloq = NULL, verbose = FALSE) {
  if(is.null(sim)) {
    stop("Reference-correction requires simulated data (`sim`).")
  }
  if(!cols$sim$pred %in% names(sim_ref)) {
    stop("Reference-correction: pred-variable (", cols$sim$pred, ") not found in reference simulation (`sim_ref`).")
  }
  ## compute the replicate index once and store it, so it is reused downstream
  ## (and remains correct if rows are dropped below)
  sim_idx <- add_sim_index_number(sim, id = "id", sim_label = cols$sim$sim)
  sim[[cols$sim$sim]] <- sim_idx
  if(!is.null(obs)) {
    obs <- ensure_obs_pred(sim = sim, obs = obs, cols = cols, require_sim_pred = FALSE, verbose = verbose)
  } else if (!cols$sim$pred %in% names(sim)) {
    stop("Reference-correction: pred-variable (", cols$sim$pred, ") not found in simulated dataset.")
  }
  ref_idx <- add_sim_index_number(sim_ref, id = "id", sim_label = cols$sim$sim)
  sim_mat <- records_to_matrix(sim, sim_idx, "sim")
  ref_mat <- records_to_matrix(sim_ref, ref_idx, "sim_ref")
  sim_pred <- if(cols$sim$pred %in% names(sim)) {
    matrix(as.num(sim[[cols$sim$pred]][sim_mat$order]), nrow = nrow(sim_mat$dv))
  } else {
    NULL
  }
  ref_pred <- matrix(as.num(sim_ref[[cols$sim$pred]][ref_mat$order]), nrow = nrow(ref_mat$dv))
  pred_ref <- ref_pred[, 1]
  check_pred_constant(ref_pred, pred_ref, "`sim_ref` replicates")
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
    if(!all(as.character(obs$id) == as.character(sim_mat$id)) ||
       !isTRUE(all.equal(as.num(obs$idv), sim_mat$idv, check.attributes = FALSE))) {
      stop("Reference-correction: order of records (`id`, `idv`) differs between `obs` and `sim`.")
    }
  }
  ## PRED is a population prediction, so one PRED vector is used for all
  ## replicates (as in PsN/Pharmpy): from obs if available, else first replicate
  pred <- if(!is.null(obs)) as.num(obs[[cols$obs$pred]]) else sim_pred[, 1]
  if(!is.null(sim_pred)) {
    check_pred_constant(sim_pred, pred, if(!is.null(obs)) "`obs` and `sim` replicates" else "`sim` replicates")
  }
  sim_pred <- matrix(pred, nrow = n_rec, ncol = ncol(sim_mat$dv))
  msg("Performing reference-correction...", verbose = verbose)
  res <- ref_correction_core(
    sim_dv = sim_mat$dv,
    sim_pred = sim_pred,
    ref_dv = ref_mat$dv,
    pred_ref = pred_ref,
    ## censored observations (-Inf/Inf) are not corrected
    obs_dv = if(!is.null(obs)) ifelse(is.finite(as.num(obs$dv)), as.num(obs$dv), NA_real_) else NULL,
    obs_pred = if(!is.null(obs)) pred else NULL,
    lower_bnd = lower_bnd,
    log_dv = log_dv
  )
  sim_censored <- is_censored(sim$dv, lloq = lloq, uloq = uloq)
  sim_censored_dv <- censor_dv(sim$dv, lloq = lloq, uloq = uloq)[sim_censored]
  sim$dv[sim_mat$order] <- as.vector(res$sim)
  sim$dv[sim_censored] <- sim_censored_dv
  if(!is.null(obs)) {
    obs_dv_in <- as.num(obs$dv)
    obs$dv <- res$obs
    obs_censored <- is.infinite(obs_dv_in)
    obs$dv[obs_censored] <- obs_dv_in[obs_censored]
  }
  ## records with a non-missing, non-censored DV for which the correction
  ## failed are dropped (missing DV is kept here, and removed later)
  sim_bad <- rep(FALSE, nrow(sim))
  sim_bad[sim_mat$order] <- !is.na(as.vector(sim_mat$dv)) & !is.finite(as.vector(res$sim))
  sim_bad[sim_censored] <- FALSE
  obs_bad <- if(!is.null(obs)) is.finite(obs_dv_in) & !is.finite(res$obs) else logical(0)
  n_bad <- sum(sim_bad) + sum(obs_bad)
  if(n_bad > 0) {
    warning(
      "Reference-correction produced ", n_bad, " missing or non-finite values ",
      "(e.g. DV or PRED at or below `pred_corr_lower_bnd`, or fewer than 2 ",
      "non-missing simulated values for a record). These are excluded from the VPC."
    )
    sim <- sim[!sim_bad, , drop = FALSE]
    if(!is.null(obs)) obs <- obs[!obs_bad, , drop = FALSE]
  }
  list(sim = sim, obs = obs)
}

#' Reshape simulated records into a records x replicates matrix
#'
#' @param data simulated data with standardized \code{id} and \code{dv}
#' @param sim_idx simulation index for each row
#' @param what label used in error messages
#' @return list with \code{dv} (matrix), \code{id} and \code{idv} (of first
#'   replicate), and \code{order} (row indices of \code{data} in matrix order)
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
  n_rep <- length(n_per_sim)
  ids <- data$id[ord]
  idv <- as.num(data$idv[ord])
  first <- seq_len(n_rec)
  if(!all(ids == rep(ids[first], n_rep)) || !isTRUE(all.equal(idv, rep(idv[first], n_rep)))) {
    stop("Reference-correction: order of records (`id`, `idv`) differs between replicates in `", what, "`.")
  }
  list(dv = dv, id = ids[first], idv = idv[first], order = ord)
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
  row_sd <- function(m) {
    n <- rowSums(!is.na(m))
    mu <- rowSums(m, na.rm = TRUE) / n
    v <- rowSums((m - mu)^2, na.rm = TRUE) / (n - 1)
    v[n < 2] <- NA
    sqrt(v)
  }
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
#' replicate of the simulated data. If records in that replicate do not match
#' the observed data (\code{id}, \code{idv}), a warning is given and the first
#' \code{nrow(obs)} rows of the simulated data are used (previous behaviour).
#'
#' @inheritParams calc_pred_corr_continuous
#' @param require_sim_pred stop if the pred-column is missing from the
#'   simulated data, even if the observed data has it?
#' @return \code{obs}, with pred column
ensure_obs_pred <- function(sim, obs, cols, require_sim_pred = TRUE, verbose = FALSE) {
  if(!cols$obs$pred %in% names(obs)) {
    msg("Warning: specified pred-variable not found in observation dataset, trying to get from simulated dataset...", verbose)
    if (!cols$sim$pred %in% names(sim)) {
      stop(
        "Specified pred-variable for observed data (", cols$obs$pred,
        ") not found in simulated dataset, not able to perform correction!"
      )
    }
    sim_idx <- add_sim_index_number(sim, id = "id", sim_label = cols$sim$sim)
    first_rep <- sim[sim_idx == sim_idx[1], , drop = FALSE]
    aligned <- nrow(first_rep) == nrow(obs) &&
      all(as.character(first_rep$id) == as.character(obs$id)) &&
      isTRUE(all.equal(as.num(first_rep$idv), as.num(obs$idv), check.attributes = FALSE))
    if(!aligned) {
      warning(
        "Pred-variable for observed data taken from simulated data, but records in the first ",
        "replicate (`id`, `idv`) do not match the observed data. Using the first ", nrow(obs),
        " rows of the simulated data; PRED may be misaligned. Consider adding the ",
        "pred-variable to the observed data."
      )
      first_rep <- sim[seq_len(nrow(obs)), , drop = FALSE]
    }
    obs <- obs %>% dplyr::ungroup()
    obs[[cols$obs$pred]] <- unlist(first_rep[[cols$sim$pred]])
    msg("OK", verbose)
  } else if (require_sim_pred && !cols$sim$pred %in% names(sim)) {
    stop(
      "Specified pred-variable (", cols$sim$pred,
      ") not found in simulated dataset, not able to perform correction!"
    )
  }
  obs
}

#' Flag censored values
#'
#' @param dv dependent variable
#' @inheritParams define_loq
#' @return logical vector, TRUE when \code{dv} is below \code{lloq} or above
#'   \code{uloq}
is_censored <- function(dv, lloq = NULL, uloq = NULL) {
  cens <- rep(FALSE, length(dv))
  if(!is.null(lloq)) cens <- cens | (!is.na(dv) & dv < lloq)
  if(!is.null(uloq)) cens <- cens | (!is.na(dv) & dv > uloq)
  cens
}

#' Warn when PRED differs between replicates
#'
#' @param pred_mat matrix (records x replicates) of PRED
#' @param pred vector (records) of PRED that will be used
#' @param what label for warning message
#' @param tolerance maximum relative difference per value
#' @return NULL, invisibly
check_pred_constant <- function(pred_mat, pred, what, tolerance = 1e-4) {
  rel_diff <- abs(pred_mat - pred) / pmax(abs(pred), .Machine$double.eps)
  n_diff <- sum(rel_diff > tolerance, na.rm = TRUE)
  if(n_diff > 0) {
    warning(
      "Reference-correction: PRED differs between ", what, " for ", n_diff, " values. ",
      "PRED should be identical for each record; check record alignment. ",
      "Using PRED from ", if(grepl("obs", what)) "`obs`" else "first replicate", "."
    )
  }
  invisible(NULL)
}
