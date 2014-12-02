#' Customize VPC theme
#' 
#' @param obs_color color for observationss points
#' @param obs_size size for observation points
#' @param obs_median_color color for median observation line
#' @param obs_median_linetype linetype for median observation line
#' @param obs_median_size size for median observation line
#' @param obs_ci_color color for observation CI lines
#' @param obs_ci_linetype linetype for observation CI lines 
#' @param obs_ci_size size for observations CI lines
#' @param sim_pi_fill fill color for simulated prediction interval areas
#' @param sim_pi_alpha transparency for simulated prediction interval areas
#' @param sim_pi_color color for simulated prediction interval lines
#' @param sim_pi_linetype linetype for simulated prediction interval lines
#' @param sim_pi_size size for simulated prediction interval lines
#' @param sim_median_fill fill color for simulated median areas
#' @param sim_median_alpha transparency for simulated median areas
#' @param sim_median_color color for simulated median lines
#' @param sim_median_linetype linetype for simulated median lines
#' @param sim_median_size size for simulated median lines
#' @param bin_separators_color color for bin separator lines, NA for don't plot
#' @param bin_separators_location where to plot bin separators ("t" for top, "b" for bottom)
#' @export
#' @return A list with vpc theme specifiers
#' @export create_vpc_theme
#' @details
#' This function creates a theme that customizes how the VPC looks, i.e. colors, fills, transparencies, linetypes an sizes, etc.

create_vpc_theme <- function (update = NULL) {
  tmp <- structure(list(  
    obs_color = "#000000",
    obs_size = 1,
                   
    obs_median_color = "#000000",
    obs_median_linetype = "solid",
    obs_median_size = 1,  
                   
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
