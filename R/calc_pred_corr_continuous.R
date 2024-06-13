#' Perform prediction-correction
#'
#' @inheritParams read_vpc
#' @inheritParams define_loq
#' @param cols A length 2, named list with one element named "obs" and the other
#'   named "sim", each containing a sub-list with elements for mapping columns
#'   names in the data to expected column names for use.
#' @return A list with "sim" and "obs" (with \code{pred_corr} performed, if
#'   requested)
calc_pred_corr_continuous <- function(sim, obs, pred_corr, pred_corr_lower_bnd, cols, verbose) {
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
      obs <- pred_correction_core(obs, cols$obs$pred, pred_corr_lower_bnd)
    }
  }
  if(!is.null(sim)) {
    sim$sim <- add_sim_index_number(sim, id = "id", sim_label=cols$sim$sim)
    if(pred_corr) {
      msg("Performing prediction-correction on simulated data...", verbose=verbose)
      sim <- pred_correction_core(sim, cols$sim$pred, pred_corr_lower_bnd)
    }
  }
  list(
    sim=sim,
    obs=obs
  )
}

#' Core prediction correction function
#' 
#' Perform pred-correction for predictions that were non-zero and
#' were not missing observation. The latter can happen e.g. when 
#' censored data is set to NA in `format_vpc_input_data()`.
#' 
#' @param data dataset, either `sim` or `obs` data.frame
#' @param pred_col cols$obs$pred
#' @inheritParams calc_pred_corr_continuous
#' 
#' @returns data.frame
pred_correction_core <- function(data, pred_col, pred_corr_lower_bnd) {
  data$pred <- data[[pred_col]]
  data <- data %>% 
    dplyr::group_by(strat, bin) %>% 
    dplyr::mutate(pred_bin = median(as.num(pred)))
  row_idx <- data$pred != 0 & !is.na(data$dv)
  data[row_idx,]$dv <- pred_corr_lower_bnd + 
    (data[row_idx,]$dv - pred_corr_lower_bnd) * 
    (data[row_idx,]$pred_bin - pred_corr_lower_bnd) / 
    (data[row_idx,]$pred - pred_corr_lower_bnd)
  data  
}
