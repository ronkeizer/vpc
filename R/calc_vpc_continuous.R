#' Calculate aggregate statistics for simulated and observed VPC data
#'
#' @inheritParams read_vpc
#' @inheritParams define_bins
#' @param loq The list output from \code{define_loq()}
#' @param bin_mid either "mean" for the mean of all timepoints (default) or "middle" to use the average of the bin boundaries.
#' @param pi simulated prediction interval to plot. Default is c(0.05, 0.95),
#' @param ci confidence interval to plot. Default is (0.05, 0.95)
#' @param stratify character vector of stratification variables.
#' @param verbose show debugging information (TRUE or FALSE)
#' @param pred_corr Was prediction-correction applied to \code{sim} and
#'   \code{obs}? When \code{TRUE} and a limit of quantification is used,
#'   censored values are expected to be \code{-Inf} (\code{lloq}) or
#'   \code{Inf} (\code{uloq}), with censoring determined on the uncorrected
#'   scale, and quantiles for both observed and simulated data are computed
#'   treating those values as censored. Prediction-corrected
#'   values are not compared to the limit again, since the limit applies to the
#'   uncorrected scale. A quantile that falls in the censored part of the
#'   distribution is returned as \code{NA}.
#' @return A list with "vpc_dat" and "aggr_obs"
#' @importFrom rlang .data
calc_vpc_continuous <- function(sim, obs, loq, pi, ci, stratify, bins, bin_mid, verbose,
                                pred_corr = FALSE) {
  cens_type <- loq$cens_type
  cens_limit <- loq$cens_limit
  pc_cens <- isTRUE(pred_corr) && cens_type %in% c("left", "right")
  if (pc_cens) {
    # With prediction-correction, censoring was determined on the uncorrected
    # DV (censored values are -Inf/Inf). Corrected values must not be compared
    # to the (uncorrected) limit again, so use an infinite limit: only the
    # infinite values are then treated as censored.
    cens_limit <- if (cens_type == "left") -Inf else Inf
  }
  if (pc_cens) {
    # per-replicate quantiles keep censored quantiles as -Inf/Inf, so that
    # they are counted as censored when aggregated over replicates
    sim_quantile_rep <- function(x, probs) {
      quantile_cens_inf(x = x, probs = probs, limit = cens_limit, cens = cens_type)
    }
    sim_quantile <- function(x, probs) {
      quantile_cens(x = x, probs = probs, limit = cens_limit, cens = cens_type)
    }
  } else {
    sim_quantile_rep <- sim_quantile <- function(x, probs) {
      quantile(x, probs, na.rm = TRUE)
    }
  }
  if(!is.null(sim)) {
    msg("Calculating statistics for simulated data...", verbose=verbose)
    aggr_sim <-
      sim %>%
      dplyr::group_by(strat, sim, bin) %>%
      dplyr::summarise(
        q5 = sim_quantile_rep(dv, pi[1]),
        q50 = sim_quantile_rep(dv, 0.5),
        q95 = sim_quantile_rep(dv, pi[2]),
        mean_idv = mean(idv)
      )

    vpc_dat <-
      aggr_sim %>%
      dplyr::group_by(strat, bin) %>%
      dplyr::summarise(
        q5.low = sim_quantile(q5, ci[1]),
        q5.med = sim_quantile(q5, 0.5),
        q5.up = sim_quantile(q5, ci[2]),
        q50.low = sim_quantile(q50, ci[1]),
        q50.med = sim_quantile(q50, 0.5),
        q50.up = sim_quantile(q50, ci[2]),
        q95.low = sim_quantile(q95, ci[1]),
        q95.med = sim_quantile(q95, 0.5),
        q95.up = sim_quantile(q95, ci[2]),
        bin_mid = mean(.data$mean_idv, na.rm = TRUE)
      )

    bounds <- bin_boundaries(bins, vpc_dat$bin, vpc_dat$strat)
    vpc_dat$bin_min <- bounds$bin_min
    vpc_dat$bin_max <- bounds$bin_max
    if(bin_mid == "middle") {
      vpc_dat$bin_mid <- (vpc_dat$bin_min + vpc_dat$bin_max)/2
    }
  } else {
    vpc_dat <- NULL
  }
  if(!is.null(obs)) {
    msg("Calculating statistics for observed data...", verbose=verbose)
    aggr_obs <-
      obs %>%
      dplyr::group_by(strat,bin) %>%
      dplyr::summarise(
        obs5 = quantile_cens(x=dv, probs=pi[1], limit = cens_limit, cens = cens_type),
        obs50 = quantile_cens(x=dv, probs=0.5, limit = cens_limit, cens = cens_type),
        obs95 = quantile_cens(x=dv, probs=pi[2], limit = cens_limit, cens = cens_type),
        bin_mid = mean(idv)
      )
    bounds <- bin_boundaries(bins, aggr_obs$bin, aggr_obs$strat)
    aggr_obs$bin_min <- bounds$bin_min
    aggr_obs$bin_max <- bounds$bin_max
    if(bin_mid == "middle") {
      aggr_obs$bin_mid <- (aggr_obs$bin_min + aggr_obs$bin_max)/2
    }
  } else {
    aggr_obs <- NULL
  }
  if(!is.null(stratify)) {
    if(length(stratify) == 2) {
      vpc_dat <- add_strat_columns(vpc_dat, stratify, obs, sim)
      aggr_obs <- add_strat_columns(aggr_obs, stratify, obs, sim)
    }
  }

  list(
    vpc_dat=vpc_dat,
    aggr_obs=aggr_obs
  )
}
