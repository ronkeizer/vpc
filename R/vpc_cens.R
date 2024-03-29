#' VPC function for left- or right-censored data (e.g. BLOQ data)
#'
#' Creates a VPC plot from observed and simulation data for censored data. Function can handle both left- (below lower limit of quantification) and right-censored (above upper limit of quantification) data.
#'
#' @inheritParams format_vpc_input_data
#' @inheritParams read_vpc
#' @inheritParams plot_vpc
#' @inheritParams as_vpcdb
#' @inheritParams define_bins
#' @param bin_mid either "mean" for the mean of all timepoints (default) or "middle" to use the average of the bin boundaries.
#' @param stratify_color variable to stratify and color lines for observed data. Only 1 stratification variables can be supplied.
#' @param ci confidence interval to plot. Default is (0.05, 0.95)
#' @param vpcdb boolean whether to return the underlying vpcdb rather than the plot
#' @param verbose show debugging information (TRUE or FALSE)
#' @return a list containing calculated VPC information (when vpcdb=TRUE), or a ggplot2 object (default)
#' @export
#' @seealso \link{sim_data}, \link{vpc}, \link{vpc_tte}, \link{vpc_cat}
#' @examples
#'
#' ## See vpc.ronkeizer.com for more documentation and examples
#' library(vpc)
#'
#' vpc_cens(sim = simple_data$sim, obs = simple_data$obs, lloq = 30)
#' vpc_cens(sim = simple_data$sim, obs = simple_data$obs, uloq = 120)
#'
vpc_cens <- function(sim = NULL,
                     obs = NULL,
                     psn_folder = NULL,
                     bins = "jenks",
                     n_bins = 8,
                     bin_mid = "mean",
                     obs_cols = NULL,
                     sim_cols = NULL,
                     software = "auto",
                     show = NULL,
                     stratify = NULL,
                     stratify_color = NULL,
                     ci = c(0.05, 0.95),
                     uloq = NULL,
                     lloq = NULL,
                     xlab = "Time",
                     ylab = "Probability of <LOQ",
                     title = NULL,
                     smooth = TRUE,
                     vpc_theme = NULL,
                     facet = "wrap",
                     labeller = NULL,
                     vpcdb = FALSE,
                     verbose = FALSE) {
  vpc_data <-
    read_vpc(
      sim=sim, obs=obs, psn_folder=psn_folder,
      software=software,
      sim_cols=sim_cols, obs_cols=obs_cols
    )
  obs <- vpc_data$obs
  software_type <- vpc_data$software
  cols <- vpc_data$cols

  loq_data <-
    define_loq(
      lloq=lloq, uloq=uloq,
      pred_corr=FALSE, pred_corr_lower_bnd=NULL,
      require_loq=TRUE
    )
  lloq <- loq_data$lloq
  uloq <- loq_data$uloq
  pred_corr <- loq_data$pred_corr
  pred_corr_lower_bnd <- loq_data$pred_corr_lower_bnd
  cens_type <- loq_data$cens_type
  cens_limit <- loq_data$cens_limit

  ## checking whether stratification columns are available
  msg("Stratifying data...", verbose=verbose)
  check_stratification_columns_available(data=obs, stratify=stratify, type="observation")
  check_stratification_columns_available(data=vpc_data$sim, stratify=stratify, type="simulation")
  check_stratification_columns_available(data=obs, stratify=stratify_color, type="observation")
  check_stratification_columns_available(data=vpc_data$sim, stratify=stratify_color, type="simulation")

  ## parse data into specific format
  if(!is.null(obs)) {
    obs <-
      format_vpc_input_data(
        dat=obs,
        cols=cols$obs,
        lloq=lloq, uloq=uloq,
        stratify=stratify,
        log_y=FALSE, log_y_min=0,
        what="observed",
        verbose=verbose
      )
  }
  if(!is.null(vpc_data$sim)) {
    vpc_data$sim <-
      format_vpc_input_data(
        dat=vpc_data$sim,
        cols=cols$sim,
        lloq=NULL, uloq=NULL,
        stratify=stratify,
        log_y=FALSE, log_y_min=0,
        what="simulated",
        verbose=verbose
      )
    # add sim index number
    vpc_data$sim$sim <- add_sim_index_number(vpc_data$sim)
  }

  stratify_original <- stratify
  if(!is.null(stratify_color)) {
    if (is.null(stratify)) {
      stratify <- stratify_color
    }
    if (length(stratify_color) > 1) {
      stop("Error: please specify only 1 stratification variable for color!")
    }
    if (!stratify_color %in% stratify) {
      stratify_original <- stratify
      stratify <- c(stratify, stratify_color)
    }
  }

  # Binning ####
  bins_data <- define_bins(obs=obs, sim=vpc_data$sim, bins=bins, n_bins=n_bins, verbose=verbose)
  bins <- bins_data$bins
  obs <- bins_data$obs
  sim <- bins_data$sim

  ## Parsing data to get the quantiles for the VPC
  if (!is.null(sim)) {
    tmp1 <- sim %>%
      dplyr::group_by(strat, sim, bin)
    vpc_dat <- tmp1 %>%
      dplyr::summarise(ploq = loq_frac(dv, limit = cens_limit, cens = cens_type),
                       mn_idv = mean(idv)) %>%
      dplyr::group_by(strat, bin) %>%
      dplyr::summarise(q50.low = quantile(ploq, ci[1]),
                       q50.med = quantile(ploq, 0.5),
                       q50.up = quantile(ploq, ci[2]),
                       bin_mid = mean(mn_idv)) %>%
      dplyr::ungroup()
    vpc_dat$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
    vpc_dat$bin_max <- rep(bins[2:length(bins)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
    if(bin_mid == "middle") {
      vpc_dat$bin_mid <- apply(dplyr::bind_cols(vpc_dat$bin_min, vpc_dat$bin_max), 1, mean)
    }
  } else {
    vpc_dat <- NULL
  }
  if(!is.null(obs)) {
    tmp <- obs %>%
      dplyr::group_by(strat,bin)
    aggr_obs <- tmp %>%
      dplyr::summarise(obs50 = loq_frac(dv, limit = cens_limit, cens = cens_type),
                       bin_mid = mean(idv)) %>%
      dplyr::ungroup()
    aggr_obs$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(aggr_obs$strat)) )[aggr_obs$bin]
    aggr_obs$bin_max <- rep(bins[2:length(bins)], length(unique(aggr_obs$strat)) )[aggr_obs$bin]
    if(bin_mid == "middle") {
      aggr_obs$bin_mid <- apply(dplyr::bind_cols(aggr_obs$bin_min, aggr_obs$bin_max), 1, mean)
    }
  } else {
    aggr_obs <- NULL
  }
  if (!is.null(stratify_original)) {
    if (length(stratify) == 2) {
      vpc_dat$strat1 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)-1]
      vpc_dat$strat2 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)]
      aggr_obs$strat1 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)-1]
      aggr_obs$strat2 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)]
    }
  }

  ## plotting starts here
  vpc_db <-
    as_vpcdb(
      sim = sim,
      vpc_dat = vpc_dat,
      stratify = stratify,
      stratify_original = stratify_original,
      stratify_color = stratify_color,
      aggr_obs = aggr_obs,
      obs = obs,
      bins = bins,
      facet = facet,
      labeller = labeller,
      type = "censored",
      xlab = ifelse(is.null(xlab), cols$obs$idv, xlab),
      ylab = ifelse(is.null(ylab), cols$obs$dv, ylab),
      show = show
    )
  if(vpcdb) {
    return(vpc_db)
  } else {
    msg("Plotting...", verbose=verbose)
    pl <- plot_vpc(db = vpc_db,
                   vpc_theme = vpc_theme,
                   smooth = smooth,
                   log_y = FALSE,
                   title = title)
    return(pl)
  }
}
