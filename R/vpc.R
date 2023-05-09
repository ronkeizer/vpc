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
      warning("Be careful using pred-correction together with censored data (<LLOQ or >ULOQ). VPC plot will be shown for non-censored data only!")
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
  pred_corr_data <-
    calc_pred_corr_continuous(
      sim=sim, obs=obs,
      pred_corr=pred_corr,
      pred_corr_lower_bnd=pred_corr_lower_bnd,
      cols=cols,
      verbose=verbose
    )
  calc_data <-
    calc_vpc_continuous(
      sim=pred_corr_data$sim, obs=pred_corr_data$obs,
      loq=loq_data,
      pi=pi, ci=ci,
      stratify=stratify,
      bins=bins,
      bin_mid=bin_mid,
      verbose=verbose
    )
  # Wrapup ####
  # data combined and handed off to separate plotting function
  vpc_db <-
    as_vpcdb(
      sim = pred_corr_data$sim,
      vpc_dat = calc_data$vpc_dat,
      smooth = smooth,
      stratify = stratify,
      aggr_obs = calc_data$aggr_obs,
      obs = pred_corr_data$obs,
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
