define_data_columns <- function(sim, obs, sim_cols, obs_cols, software_type) {
  software_types <- c("nonmem", "phoenix") 
  if(software_type %in% software_types) {
    if (software_type == "nonmem") {
      obs_cols_default <- list(dv = "DV", id = "ID", idv = "TIME", pred = "PRED")
      sim_cols_default <- list(dv = "DV", id = "ID", idv = "TIME", pred = "PRED")
      if(!is.null(obs)) {
        old_class <- class(obs)
        class(obs) <- c("nonmem", old_class)
      }
      if(!is.null(sim)) {
        old_class <- class(sim)
        class(sim) <- c("nonmem", old_class)
      }
    } 
    if (software_type == "phoenix") {
      obs_cols_default <- list(dv = "COBS", id = "ID", idv = "TIME", pred = "PRED")
      sim_cols_default <- list(dv = "COBS", id = "ID", idv = "TIME", pred = "PRED")
    }    
  } else {
    obs_cols_default <- list(dv = "dv", id = "id", idv = "time", pred = "pred")
    sim_cols_default <- list(dv = "dv", id = "id", idv = "time", pred = "pred")    
  }
  obs_cols <- replace_list_elements(obs_cols_default, obs_cols)
  sim_cols <- replace_list_elements(sim_cols_default, sim_cols)
  return(list(sim = sim_cols, obs = obs_cols))
}