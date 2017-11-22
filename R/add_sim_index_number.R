#' Add sim index number
#' 
#' @description Add simulation index number to simulation when not present
#' @param sim a data.frame containing the simulation data
#' @param id character specifying the column name in the data.frame
#' @param sim_label label to indicate simulation index (if available)
add_sim_index_number <- function (sim, id = "id", sim_label = "sim") { # for multiple simulations in a single dataframe, add an index number for every simulation
  if(!is.null(sim_label) && sim_label %in% colnames(sim)) { # Keep simulation index column if already present
    return(sim[[sim_label]])
  }
  sim[[id]] <- as.num(sim[[id]])
  sim_id <- cumsum(unlist(sapply(rle(sim[[id]])$lengths, FUN = function(w) seq(length(w), w, 1))) < 2)
  sim$sim <- 1
  for(i in unique(sim[[id]])){
    sim$sim[sim[[id]]==i] <- cumsum(stats::ave(1:length(sim_id[sim[[id]]==i]), 
                                        sim_id[sim[[id]]==i], FUN = function(h) 1:length(h))<2)
  }
  return(sim$sim)
}
