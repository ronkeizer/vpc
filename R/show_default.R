#' Defaults for show argument with various endpoint types
#' 
#' @export
show_default <-
  list(
    continuous=
      list (
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
      ),
    categorical=
      list(
        obs_dv = FALSE,
        obs_ci = FALSE,
        obs_median = TRUE,
        sim_median = TRUE,
        sim_median_ci = TRUE,
        pi = FALSE,
        pi_ci = FALSE,
        pi_as_area = FALSE,
        bin_sep = TRUE,
        sim_km = FALSE,
        obs_cens = TRUE
      ),
    # TODO: Review 2021-04: "censored" values are now defaults, but possible to
    # change rather than forced
    censored=
      list(
        obs_dv = FALSE,
        obs_ci = FALSE,
        obs_median = TRUE,
        sim_median = FALSE,
        sim_median_ci = TRUE,
        pi = FALSE,
        pi_ci = FALSE,
        pi_as_area = FALSE,
        bin_sep = TRUE,
        sim_km = FALSE,
        obs_cens = TRUE
      ),
    "time-to-event"=
      list (
        obs_dv = FALSE,
        obs_ci = FALSE,
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
  )
