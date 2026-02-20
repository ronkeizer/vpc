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
#' @return A list with "vpc_dat" and "aggr_obs"
#' @importFrom rlang .data
calc_vpc_continuous <- function(sim, obs, loq, pi, ci, stratify, bins, bin_mid, verbose) {
  if(!is.null(sim)) {
    msg("Calculating statistics for simulated data...", verbose=verbose)
    aggr_sim <-
      sim %>%
      dplyr::group_by(strat, sim, bin) %>%
      dplyr::summarise(
        q5 = quantile(dv, pi[1], na.rm = TRUE),
        q50 = quantile(dv, 0.5, na.rm = TRUE),
        q95 = quantile(dv, pi[2], na.rm = TRUE),
        mean_idv = mean(idv)
      )

    # TODO: Review 2021-04: Shouldn't this aggregation of sim use
    # quantile_cens()?
    vpc_dat <-
      aggr_sim %>%
      dplyr::group_by(strat, bin) %>%
      dplyr::summarise(
        q5.low = quantile(q5, ci[1]),
        q5.med = quantile(q5, 0.5),
        q5.up = quantile(q5, ci[2]),
        q50.low = quantile(q50, ci[1]),
        q50.med = quantile(q50, 0.5),
        q50.up = quantile(q50, ci[2]),
        q95.low = quantile(q95, ci[1]),
        q95.med = quantile(q95, 0.5),
        q95.up = quantile(q95, ci[2]),
        bin_mid = mean(.data$mean_idv)
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
        obs5 = quantile_cens(x=dv, probs=pi[1], limit = loq$cens_limit, cens = loq$cens_type),
        obs50 = quantile_cens(x=dv, probs=0.5, limit = loq$cens_limit, cens = loq$cens_type),
        obs95 = quantile_cens(x=dv, probs=pi[2], limit = loq$cens_limit, cens = loq$cens_type),
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
