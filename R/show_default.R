#' Defaults for show argument
#' 
#' @export
show_default <- list (
  obs_dv = FALSE,
  obs_ci = TRUE,
  obs_median = TRUE,
  sim_median = FALSE,
  sim_median_ci = TRUE,
  pi = FALSE,
  pi_ci = TRUE,
  pi_as_area = FALSE,
  bin_sep = TRUE,
  sim_km = FALSE,
  obs_cens = TRUE
)

#' Defaults for show argument for TTE VPC
#' 
#' @export
show_default_tte <- list (
  obs_dv = TRUE,
  obs_ci = FALSE,
  obs_median = TRUE,
  sim_median = TRUE,
  sim_median_ci = TRUE,
  pi = FALSE,
  pi_ci = TRUE,
  pi_as_area = FALSE,
  bin_sep = TRUE,
  sim_km = FALSE,
  obs_cens = TRUE
)
