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
      obs$pred <- obs[[cols$obs$pred]]
      obs <- obs %>% dplyr::group_by(strat, bin) %>% dplyr::mutate(pred_bin = median(as.num(pred)))
      obs[obs$pred != 0,]$dv <- pred_corr_lower_bnd + (obs[obs$pred != 0,]$dv - pred_corr_lower_bnd) * (obs[obs$pred != 0,]$pred_bin - pred_corr_lower_bnd) / (obs[obs$pred != 0,]$pred - pred_corr_lower_bnd)
    }
  }
  if(!is.null(sim)) {
    sim$sim <- add_sim_index_number(sim, id = "id", sim_label=cols$sim$sim)
    if(pred_corr) {
      msg("Performing prediction-correction on simulated data...", verbose=verbose)
      sim$pred <- sim[[cols$sim$pred]]
      sim <- sim %>% dplyr::group_by(strat, bin) %>% dplyr::mutate(pred_bin = median(pred))
      sim[sim$pred != 0,]$dv <- pred_corr_lower_bnd + (sim[sim$pred != 0,]$dv - pred_corr_lower_bnd) * (sim[sim$pred != 0,]$pred_bin - pred_corr_lower_bnd) / (sim[sim$pred != 0,]$pred - pred_corr_lower_bnd)
    }
  }
  list(
    sim=sim,
    obs=obs
  )
}
