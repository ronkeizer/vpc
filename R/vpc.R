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
#' @inheritParams define_pred_corr_method
#' @param pred_corr_method Form of the prediction-correction applied when
#'   \code{pred_corr = TRUE}: \code{"multiplicative"} (default; Bergstrand et
#'   al., AAPS J 2011, optionally using \code{pred_corr_lower_bnd}) or
#'   \code{"additive"}. The additive form is intended for
#'   log-transform-both-sides (LTBS) models, and computes \code{dv + (pred_bin -
#'   pred)}, where \code{pred_bin} is the median prediction in the bin. It
#'   assumes that \code{dv} and \code{pred} in both the observed and simulated
#'   data are \emph{already on the log scale}; no transformation is performed
#'   internally, so correction, quantiles and plot are all on the log scale.
#'   Consequently, \code{log_y} cannot be used together with it, and
#'   \code{pred_corr_lower_bnd} is ignored. Note that for many LTBS workflows,
#'   the default multiplicative correction on linear-scale data combined with
#'   \code{log_y = TRUE} is already adequate; use the additive form when the
#'   correction specifically needs to be evaluated on the log scale.
#' @section Prediction-correction with censored data: When \code{pred_corr =
#'   TRUE} is combined with \code{lloq} or \code{uloq}, the following steps are
#'   performed, identically for observed and simulated data:
#'   \enumerate{
#'     \item Values below \code{lloq} (or above \code{uloq}) are flagged as
#'     censored based on the \emph{uncorrected} DV.
#'     \item The median prediction per bin (\code{pred_bin}) is calculated
#'     from the predictions of \emph{all} records in the bin, including
#'     censored ones.
#'     \item Only non-censored values are prediction-corrected.
#'     \item Percentiles are calculated per bin (and per simulation) with the
#'     censored values retained and ranked below (\code{lloq}) or above
#'     (\code{uloq}) all non-censored values, i.e. they are not dropped. The
#'     corrected values are not compared to the limit of quantification again.
#'     Percentiles that fall within the censored part of the distribution are
#'     returned as \code{NA} and not shown.
#'   }
#'   Since prediction-corrected values are no longer on the scale of the limit
#'   of quantification, the LLOQ/ULOQ line is not drawn. Note that percentiles
#'   are calculated using \code{stats::quantile()} with its default method
#'   (\code{type = 7}), which may differ from other software (e.g. PsN).
#' @param vpcdb Boolean whether to return the underlying vpcdb rather than the plot
#' @param verbose show debugging information (TRUE or FALSE)
#' @param ... Other arguments sent to other methods (like xpose or nlmixr);  Note these arguments are not used in the default vpc and are ignored by the default method.
#' @return a list containing calculated VPC information (when vpcdb=TRUE), or a ggplot2 object (default)
#' @export
#' @seealso \link{vpc_cens}, \link{vpc_tte}, \link{vpc_cat}
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
                    pred_corr_method = c("multiplicative", "additive"),
                    ltbs = FALSE,
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
  pred_corr_method <-
    define_pred_corr_method(
      pred_corr_method=pred_corr_method, ltbs=ltbs,
      pred_corr=pred_corr, pred_corr_lower_bnd=pred_corr_lower_bnd,
      log_y=log_y
    )

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
      warning(
        "Prediction-correction is used together with censored data (<LLOQ or >ULOQ). ",
        "Censoring is determined on the uncorrected observed and simulated DV, only non-censored values are prediction-corrected, ",
        "and censored values are counted as censored (not dropped) when calculating percentiles. ",
        "Percentiles that fall in the censored range are not shown. See ?vpc for details."
      )
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
      pred_corr_method=pred_corr_method,
      cols=cols,
      verbose=verbose
    )
  calc_data <-
    calc_vpc_continuous(
      sim=pred_corr_data$sim, 
      obs=pred_corr_data$obs,
      loq=loq_data,
      pi=pi, ci=ci,
      stratify=stratify,
      bins=bins,
      bin_mid=bin_mid,
      verbose=verbose,
      pred_corr=pred_corr
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
