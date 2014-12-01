#' Customize VPC theme
#' 
#' @param obs_color Color for observationss points
#' @param obs_size Size for observation points
#' @param obs_median_color Color for median observation line
#' @param obs_median_linetype Linetype for median observation line
#' @param obs_median_size Size for median observation line
#' @param obs_ci_color Color for observation CI lines
#' @param obs_ci_linetype Linetype for observation CI lines 
#' @param obs_ci_size Size for observations CI lines
#' @param sim_pi_fill Fill color for simulated prediction interval areas
#' @param sim_pi_alpha Transparency for simulated prediction interval areas
#' @param sim_pi_color Color for simulated prediction interval lines
#' @param sim_pi_linetype Linetype for simulated prediction interval lines
#' @param sim_pi_size Size for simulated prediction interval lines
#' @param sim_median_fill Fill color for simulated median areas
#' @param sim_median_alpha Transparency for simulated median areas
#' @param sim_median_color Color for simulated median lines
#' @param sim_median_linetype Linetype for simulated median lines
#' @param sim_median_size Size for simulated median lines
#' @param bin_separators_color Color for bin separator lines, NA for don't plot
#' @param bin_separators_location Where to plot bin separators ("t" for top, "b" for bottom)
#' @export
create_vpc_theme <- function (update = NULL) {
  tmp <- structure(list(  
    obs_color = "#000000",
    obs_size = 1,
                   
    obs_median_color = "#000000",
    obs_median_linetype = "solid",
    obs_median_size = 1.5,  
                   
    obs_ci_color = "#000000",
    obs_ci_linetype = "dashed",
    obs_ci_size = .5,    
                   
    sim_pi_fill = "#3388cc",
    sim_pi_alpha = 0.15,  
    sim_pi_color = "#000000",
    sim_pi_linetype = 'dotted',
    sim_pi_size = 1,  
                   
    sim_median_fill = "#3388cc",
    sim_median_alpha = 0.3,  
    sim_median_color = "#000000",
    sim_median_linetype = "dashed",
    sim_median_size = 1,  
                   
    bin_separators_color = "#000000"                     
  ), class = "vpc_theme")
  if(!is.null(update)) {
    for(i in seq(names(update))) {
      tmp[[names(update)[i]]] <- update[[names(update)[i]]]
    }
  }
  tmp
}
