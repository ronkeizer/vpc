#' Define data column defaults for various softwares
#' 
#' @param sim simulated data
#' @param obs observed data
#' @param sim_cols list for mapping simulation data columns, e.g. `list(dv = "DV", id = "ID", idv = "TIME", pred="PRED")`
#' @param obs_cols list for mapping observation data columns, e.g. `list(dv = "DV", id = "ID", idv = "TIME", pred="PRED")`
#' @param software_type software type, one of `nonmem`, `phoenix`, `PKPDsim`
define_data_columns <- function(sim, obs, sim_cols, obs_cols, software_type) {
  software_types <- c("nonmem", "phoenix", "PKPDsim")
  if(software_type %in% software_types) {
    if (software_type == "nonmem") {
      obs_cols_default <- list(dv = "DV", id = "ID", idv = "TIME", pred = "PRED")
      sim_cols_default <- list(dv = "DV", id = "ID", idv = "TIME", pred = "PRED")
    }
    if (software_type == "phoenix") {
      obs_cols_default <- list(dv = "COBS", id = "ID", idv = "TIME", pred = "PRED")
      sim_cols_default <- list(dv = "COBS", id = "ID", idv = "TIME", pred = "PRED")
    }
    if (software_type == "PKPDsim") {
      obs_cols_default <- list(dv = "y", id = "id", idv = "t")
      sim_cols_default <- list(dv = "y", id = "id", idv = "t", pred="pred")
    }
  } else {
    obs_cols_default <- list(dv = "dv", id = "id", idv = "time", pred = "pred")
    sim_cols_default <- list(dv = "dv", id = "id", idv = "time", pred = "pred")
  }
  obs_cols <- replace_list_elements(obs_cols_default, obs_cols)
  sim_cols <- replace_list_elements(sim_cols_default, sim_cols)
  return(list(sim = sim_cols, obs = obs_cols))
}
