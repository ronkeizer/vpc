#' VPC function
#'
#' Creates a VPC plot from observed and simulation data
#' 
#' @inheritParams format_vpc_input_data
#' @inheritParams read_vpc
#' @inheritParams define_loq
#' @inheritParams define_bins
#' @inheritParams calc_vpc_continuous
#' @inheritParams as_vpcdb
#' @inheritParams plot_vpc
#' @param vpcdb Boolean whether to return the underlying vpcdb rather than the plot
#' @param verbose show debugging information (TRUE or FALSE)
#' @param ... Other arguments sent to other methods (like xpose or nlmixr);  Note these arguments are not used in the default vpc and are ignored by the default method.
#' @return a list containing calculated VPC information (when vpcdb=TRUE), or a ggplot2 object (default)
#' @export
#' @seealso \link{sim_data}, \link{vpc_cens}, \link{vpc_tte}, \link{vpc_cat}
#' @examples
#'
#' ## See vpc.ronkeizer.com for more documentation and examples
#' library(vpc)
#'
#' # Basic commands:
#' vpc(sim = simple_data$sim, obs = simple_data$obs)
#' vpc(sim = simple_data$sim, obs = simple_data$obs, lloq = 20)
#'
#'@export
vpc <- function(sim, ...){
    UseMethod("vpc")
}
#' @rdname vpc
#'@export
vpc.default <- function(sim, ...){
    call <- as.list(match.call(expand.dots=TRUE))[-1];
    do.call(utils::getFromNamespace("vpc_vpc","vpc"), call, envir = parent.frame(1))
}
#' @rdname vpc
#'@export
vpc_vpc <- function(sim = NULL,
                    obs = NULL,
                    psn_folder = NULL,
                    bins = "jenks",
                    n_bins = "auto",
                    bin_mid = "mean",
                    obs_cols = NULL,
                    sim_cols = NULL,
                    software = "auto",
                    show = NULL,
                    stratify = NULL,
                    pred_corr = FALSE,
                    pred_corr_lower_bnd = 0,
                    pi = c(0.05, 0.95),
                    ci = c(0.05, 0.95),
                    uloq = NULL,
                    lloq = NULL,
                    log_y = FALSE,
                    log_y_min = 1e-3,
                    xlab = NULL,
                    ylab = NULL,
                    title = NULL,
                    smooth = TRUE,
                    vpc_theme = NULL,
                    facet = "wrap",
                    scales = "fixed",
                    labeller = NULL,
                    vpcdb = FALSE,
                    verbose = FALSE, ...) {
  msg("Configuring and initializing...", verbose=verbose)
  vpc_data <-
    read_vpc(
      sim=sim, obs=obs, psn_folder=psn_folder,
      software=software,
      sim_cols=sim_cols, obs_cols=obs_cols
    )
  vpc_data$obs <- vpc_data$obs
  software_type <- vpc_data$software
  cols <- vpc_data$cols

  loq_data <-
    define_loq(
      lloq=lloq, uloq=uloq,
      pred_corr=pred_corr, pred_corr_lower_bnd=pred_corr_lower_bnd,
      require_loq=FALSE
    )
  lloq <- loq_data$lloq
  uloq <- loq_data$uloq
  pred_corr <- loq_data$pred_corr
  pred_corr_lower_bnd <- loq_data$pred_corr_lower_bnd
  cens_type <- loq_data$cens_type
  cens_limit <- loq_data$cens_limit

  ## checking whether stratification columns are available
  msg("Stratifying data...", verbose=verbose)
  check_stratification_columns_available(data=vpc_data$obs, stratify=stratify, type="observation")
  check_stratification_columns_available(data=vpc_data$sim, stratify=stratify, type="simulation")

  ## parse data into specific format
  if(!is.null(vpc_data$obs)) {
    msg("Parsing observed data...", verbose=verbose)
    vpc_data$obs <-
      format_vpc_input_data(
        dat=vpc_data$obs,
        cols=cols$obs,
        lloq=lloq, uloq=uloq,
        stratify=stratify,
        log_y=log_y, log_y_min=log_y_min,
        what="observed",
        verbose=verbose,
        pred_corr=pred_corr
      )
  }
  if(!is.null(vpc_data$sim)) {
    msg("Parsing simulated data...", verbose=verbose)
    if((!is.null(lloq) || !is.null(uloq)) && pred_corr) {
      warning("Prediction-correction cannot be used together with censored data (<LLOQ or >ULOQ). VPC plot will be shown for non-censored data only!")
      vpc_data$sim <-
        format_vpc_input_data(
          dat=vpc_data$sim,
          cols=cols$sim,
          lloq=lloq, uloq=uloq,
          stratify=stratify,
          log_y=log_y, log_y_min=log_y_min,
          what="simulated",
          verbose=verbose,
          pred_corr=pred_corr
        )
    } else {
      vpc_data$sim <-
        format_vpc_input_data(
          dat=vpc_data$sim,
          cols=cols$sim,
          lloq=NULL, uloq=NULL,
          stratify=stratify,
          log_y=log_y, log_y_min=log_y_min,
          what="simulated",
          verbose=verbose,
          pred_corr=pred_corr
        )
    }
  }
  if(pred_corr) {
    uloq <- NULL
    lloq <- NULL
  }

  # Binning ####
  bins_data <- define_bins(obs=vpc_data$obs, sim=vpc_data$sim, bins=bins, n_bins=n_bins, verbose=verbose)
  bins <- bins_data$bins
  labeled_bins <- bins_data$labeled
  obs <- bins_data$obs
  sim <- bins_data$sim

  # Calculations ####
  calc_data <-
    calc_vpc_continuous(
      sim=sim, obs=obs,
      pred_corr=pred_corr,
      loq=loq_data,
      pi=pi, ci=ci,
      cols=cols,
      stratify=stratify,
      bins=bins,
      bin_mid=bin_mid,
      verbose=verbose
    )
  # Wrapup ####
  # data combined and handed off to separate plotting function
  vpc_db <-
    as_vpcdb(
      sim = calc_data$sim,
      vpc_dat = calc_data$vpc_dat,
      smooth = smooth,
      stratify = stratify,
      aggr_obs = calc_data$aggr_obs,
      obs = calc_data$obs,
      bins = bins,
      facet = facet,
      scales = scales,       
      labeller = labeller,
      lloq = lloq,
      uloq = uloq,
      type = "continuous",
      xlab = ifelse(is.null(xlab), cols$obs$idv, xlab),
      ylab = ifelse(is.null(ylab), cols$obs$dv, ylab),
      show = show
    )
  if(vpcdb) {
    return(vpc_db)
  } else {
    msg("Plotting...", verbose=verbose)
    pl <- plot_vpc(vpc_db,
                   vpc_theme = vpc_theme,
                   smooth = smooth,
                   log_y = log_y,
                   title = title)
    return(pl)
  }
}

#' Calculate aggregate statistics for simulated and observed VPC data
#' 
#' @inheritParams read_vpc
#' @inheritParams define_loq
#' @inheritParams define_bins
#' @param loq The list output from \code{define_loq()}
#' @param bin_mid either "mean" for the mean of all timepoints (default) or "middle" to use the average of the bin boundaries.
#' @param pi simulated prediction interval to plot. Default is c(0.05, 0.95),
#' @param ci confidence interval to plot. Default is (0.05, 0.95)
#' @param cols A length 2, named list with one element named "obs" and the other
#'   named "sim", each containing a sub-list with elements for mapping columns
#'   names in the data to expected column names for use.
#' @param stratify character vector of stratification variables.
#' @param verbose show debugging information (TRUE or FALSE)
#' @return A list with "sim" and "obs" (with \code{pred_corr} performed, if
#'   requested) and "vpc_dat" and "aggr_obs".
calc_vpc_continuous <- function(sim, obs, pred_corr, loq, pi, ci, cols, stratify, bins, bin_mid, verbose) {
  if(pred_corr) {
    if(!is.null(obs) & !cols$obs$pred %in% names(obs)) {
      msg("Warning: Prediction-correction: specified pred-variable not found in observation dataset, trying to get from simulated dataset...", verbose)
      if (!cols$obs$pred %in% names(sim)) {
        stop(
          "Prediction-correction: specified pred-variable for observed data (", cols$obs$pred,
          ") not found in simulated dataset, not able to perform pred-correction!"
        )
      } else {
        obs <- obs %>% dplyr::ungroup()
        obs[[cols$obs$pred]] <- unlist(sim[1:length(obs$id), cols$sim$pred])
        msg ("OK", verbose)
      }
    } else if (!cols$sim$pred %in% names(sim)) {
      stop(
        "Prediction-correction: specified pred-variable (", cols$sim$pred,
        ") not found in simulated dataset, not able to perform pred-correction!"
      )
    }
  }
  if(!is.null(obs)) {
    if(pred_corr) {
      msg("Performing prediction-correction on observed data...", verbose=verbose)
      obs$pred <- obs[[cols$obs$pred]]
      obs <- obs %>% dplyr::group_by(strat, bin) %>% dplyr::mutate(pred_bin = median(as.num(pred)))
      obs[obs$pred != 0,]$dv <- pred_corr_lower_bnd + (obs[obs$pred != 0,]$dv - pred_corr_lower_bnd) * (obs[obs$pred != 0,]$pred_bin - pred_corr_lower_bnd) / (obs[obs$pred != 0,]$pred - pred_corr_lower_bnd)
    }
  }
  if(!is.null(sim)) {
    sim$sim <- add_sim_index_number(sim, id = "id", sim_label=cols$sim)
    if(pred_corr) {
      msg("Performing prediction-correction on simulated data...", verbose=verbose)
      sim$pred <- sim[[cols$sim$pred]]
      sim <- sim %>% dplyr::group_by(strat, bin) %>% dplyr::mutate(pred_bin = median(pred))
      sim[sim$pred != 0,]$dv <- pred_corr_lower_bnd + (sim[sim$pred != 0,]$dv - pred_corr_lower_bnd) * (sim[sim$pred != 0,]$pred_bin - pred_corr_lower_bnd) / (sim[sim$pred != 0,]$pred - pred_corr_lower_bnd)
    }
  }
  if(!is.null(sim)) {
    msg("Calculating statistics for simulated data...", verbose=verbose)
    
    aggr_sim <-
      sim %>% 
      dplyr::group_by(strat, sim, bin) %>% 
      dplyr::summarise(
        q5 = quantile(dv, pi[1]),
        q50 = quantile(dv, 0.5),
        q95 = quantile(dv, pi[2]),
        mean_idv = mean(idv)
      )
    
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
        bin_mid = mean(mean_idv)
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
    tmp1 <- obs %>% dplyr::group_by(strat,bin)
    if (!is.null(loq$cens_limit)) {
      aggr_obs <-
        tmp1 %>% 
        dplyr::summarise(
          obs5 = quantile_cens(x=dv, probs=pi[1], limit = loq$cens_limit, cens = loq$cens_type),
          obs50 = quantile_cens(x=dv, probs=0.5, limit = loq$cens_limit, cens = loq$cens_type),
          obs95 = quantile_cens(x=dv, probs=pi[2], limit = loq$cens_limit, cens = loq$cens_type),
          bin_mid = mean(idv)
        )
    } else {
      aggr_obs <-
        tmp1 %>% 
        dplyr::summarise(
          obs5 = quantile(x=dv, probs=pi[1]),
          obs50 = quantile(x=dv, probs=0.5),
          obs95 = quantile(x=dv, probs=pi[2]),
          bin_mid = mean(idv)
        )
    }
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
    sim=sim,
    obs=obs,
    vpc_dat=vpc_dat,
    aggr_obs=aggr_obs
  )
}
