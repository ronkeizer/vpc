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
      obs_cols_default <- list(dv = "y", id = "id", idv = "time")
      sim_cols_default <- list(dv = "y", id = "id", idv = "time")
    }    
  } else {
    obs_cols_default <- list(dv = "dv", id = "id", idv = "time", pred = "pred")
    sim_cols_default <- list(dv = "dv", id = "id", idv = "time", pred = "pred")    
  }
  obs_cols <- replace_list_elements(obs_cols_default, obs_cols)
  sim_cols <- replace_list_elements(sim_cols_default, sim_cols)
  return(list(sim = sim_cols, obs = obs_cols))
}