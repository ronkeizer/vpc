#' Perform prediction-correction
#'
#' @inheritParams read_vpc
#' @inheritParams define_loq
#' @param pred_corr_method Form of the prediction-correction, either
#'   \code{"multiplicative"} (default) or \code{"additive"}. See
#'   \code{\link{vpc}} for details.
#' @param cols A length 2, named list with one element named "obs" and the other
#'   named "sim", each containing a sub-list with elements for mapping columns
#'   names in the data to expected column names for use.
#' @return A list with "sim" and "obs" (with \code{pred_corr} performed, if
#'   requested)
calc_pred_corr_continuous <- function(sim, obs, pred_corr, pred_corr_lower_bnd, cols, verbose,
                                      pred_corr_method = "multiplicative") {
  if(pred_corr) {
    if(!is.null(obs)) {
      obs <- ensure_obs_pred(sim = sim, obs = obs, cols = cols, verbose = verbose)
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
      obs <- pred_correction_core(obs, cols$obs$pred, pred_corr_lower_bnd, pred_corr_method)
    }
  }
  if(!is.null(sim)) {
    sim$sim <- add_sim_index_number(sim, id = "id", sim_label=cols$sim$sim)
    if(pred_corr) {
      msg("Performing prediction-correction on simulated data...", verbose=verbose)
      sim <- pred_correction_core(sim, cols$sim$pred, pred_corr_lower_bnd, pred_corr_method)
    }
  }
  list(
    sim=sim,
    obs=obs
  )
}

#' Core prediction correction function
#' 
#' Perform pred-correction for observations that are not censored.
#' Censored observations are set to `-Inf` (below `lloq`) or `Inf` (above
#' `uloq`) in `format_vpc_input_data()` and are left as is.
#'
#' Two forms of correction are available:
#' \describe{
#'   \item{multiplicative}{(Bergstrand et al., AAPS J 2011) Rows with a
#'   prediction of zero are left uncorrected.
#'   \deqn{pcDV = lb + (DV - lb) \frac{PRED_{bin} - lb}{PRED - lb}}{pcDV = lb + (DV - lb) * (PRED_bin - lb) / (PRED - lb)}
#'   where \eqn{lb} is \code{pred_corr_lower_bnd}.}
#'   \item{additive}{For log-transform-both-sides (LTBS) models, where
#'   \code{dv} and \code{pred} are already on the log scale.
#'   \deqn{pcDV = DV + (PRED_{bin} - PRED)}{pcDV = DV + (PRED_bin - PRED)}
#'   \code{pred_corr_lower_bnd} is not used.}
#' }
#' In both cases \eqn{PRED_{bin}}{PRED_bin} is the median of \code{pred}
#' within the stratum and bin.
#' 
#' @param data dataset, either `sim` or `obs` data.frame
#' @param pred_col cols$obs$pred
#' @inheritParams calc_pred_corr_continuous
#' 
#' @returns data.frame
pred_correction_core <- function(data, pred_col, pred_corr_lower_bnd = 0,
                                 pred_corr_method = "multiplicative") {
  pred_corr_method <- match.arg(pred_corr_method, c("multiplicative", "additive"))
  data$pred <- data[[pred_col]]
  data <- data %>% 
    dplyr::group_by(strat, bin) %>% 
    dplyr::mutate(pred_bin = median(as.num(pred)))
  if (pred_corr_method == "additive") {
    row_idx <- !is.na(data$pred) & is.finite(data$dv)
    data[row_idx,]$dv <- data[row_idx,]$dv +
      (data[row_idx,]$pred_bin - data[row_idx,]$pred)
  } else {
    row_idx <- data$pred != 0 & is.finite(data$dv)
    data[row_idx,]$dv <- pred_corr_lower_bnd + 
      (data[row_idx,]$dv - pred_corr_lower_bnd) * 
      (data[row_idx,]$pred_bin - pred_corr_lower_bnd) / 
      (data[row_idx,]$pred - pred_corr_lower_bnd)
  }
  data  
}

#' Determine and check the prediction-correction method
#'
#' Resolves the \code{ltbs} convenience alias into \code{pred_corr_method}
#' and checks for incompatible argument combinations.
#'
#' @inheritParams calc_pred_corr_continuous
#' @inheritParams define_loq
#' @inheritParams format_vpc_input_data
#' @param ltbs Convenience alias: \code{TRUE} is equivalent to
#'   \code{pred_corr_method = "additive"}.
#' @return The resolved prediction-correction method, either
#'   \code{"multiplicative"} or \code{"additive"}.
define_pred_corr_method <- function(pred_corr_method = c("multiplicative", "additive"),
                                    ltbs = FALSE,
                                    pred_corr = FALSE,
                                    pred_corr_lower_bnd = 0,
                                    log_y = FALSE) {
  method_given <- length(pred_corr_method) == 1
  pred_corr_method <- match.arg(pred_corr_method)
  if (!is.logical(ltbs) || length(ltbs) != 1 || is.na(ltbs)) {
    stop("`ltbs` must be TRUE or FALSE.")
  }
  if (ltbs) {
    if (method_given && pred_corr_method != "additive") {
      stop("`ltbs = TRUE` implies `pred_corr_method = \"additive\"`, but `pred_corr_method = \"", pred_corr_method, "\"` was specified.")
    }
    pred_corr_method <- "additive"
  }
  if (pred_corr && pred_corr_method == "additive") {
    if (!is.null(pred_corr_lower_bnd) && any(pred_corr_lower_bnd != 0)) {
      warning("`pred_corr_lower_bnd` is not used with the additive (LTBS) prediction-correction and will be ignored.")
    }
    if (isTRUE(log_y)) {
      stop("The additive (LTBS) prediction-correction expects `dv` and `pred` to already be on the log scale, so `log_y = TRUE` should not be used together with it.")
    }
  }
  pred_corr_method
}
