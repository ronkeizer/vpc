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
#'   censored values are expected to be \code{NA} (censoring determined on the
#'   uncorrected scale), and quantiles for both observed and simulated data are
#'   computed treating those \code{NA} values as censored (below all other
#'   values for \code{lloq}, above for \code{uloq}). Prediction-corrected
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
    # DV (censored values are NA). Corrected values must not be compared to the
    # (uncorrected) limit again, so use an infinite limit: only the NA values
    # are then treated as censored.
    cens_limit <- if (cens_type == "left") -Inf else Inf
  }
  if (pc_cens) {
    sim_quantile <- function(x, probs) {
      quantile_cens(x = x, probs = probs, limit = cens_limit, cens = cens_type)
    }
  } else {
    sim_quantile <- function(x, probs) {
      quantile(x, probs, na.rm = TRUE)
    }
  }
  if(!is.null(sim)) {
    msg("Calculating statistics for simulated data...", verbose=verbose)
    aggr_sim <-
      sim %>%
      dplyr::group_by(strat, sim, bin) %>%
      dplyr::summarise(
        q5 = sim_quantile(dv, pi[1]),
        q50 = sim_quantile(dv, 0.5),
        q95 = sim_quantile(dv, pi[2]),
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

    vpc_dat$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
    vpc_dat$bin_max <- rep(bins[2:length(bins)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
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
    aggr_obs$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(aggr_obs$strat)) )[aggr_obs$bin]
    aggr_obs$bin_max <- rep(bins[2:length(bins)], length(unique(aggr_obs$strat)) )[aggr_obs$bin]
    if(bin_mid == "middle") {
      aggr_obs$bin_mid <- (aggr_obs$bin_min + aggr_obs$bin_max)/2
    }
  } else {
    aggr_obs <- NULL
  }
  if(!is.null(stratify)) {
    if(length(stratify) == 2) {
      vpc_dat$strat1 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)-1]
      vpc_dat$strat2 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)]
      aggr_obs$strat1 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)-1]
      aggr_obs$strat2 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)]
    }
  }

  list(
    vpc_dat=vpc_dat,
    aggr_obs=aggr_obs
  )
}
