#' Customize VPC theme
#' 
#' @param update a list containing the theme settings
#' @details
#' This function creates a theme that customizes how the VPC looks, i.e. colors, fills, transparencies, linetypes an sizes, etc. The following arguments can be specified in the input list:
#' \itemize{
#'  \item{obs_color}: {color for observationss points}
#'  \item{obs_size}: {size for observation points}
#'  \item{obs_median_color}: {color for median observation line}
#'  \item{obs_median_linetype}: {linetype for median observation line}
#'  \item{obs_median_size}: {size for median observation line}
#'  \item{obs_ci_color}: {color for observation CI lines}
#'  \item{obs_ci_linetype}: {linetype for observation CI lines}
#'  \item{obs_ci_size}: {size for observations CI lines}
#'  \item{sim_pi_fill}: {fill color for simulated prediction interval areas}
#'  \item{sim_pi_alpha}: {transparency for simulated prediction interval areas}
#'  \item{sim_pi_color}: {color for simulated prediction interval lines}
#'  \item{sim_pi_linetype}: {linetype for simulated prediction interval lines}
#'  \item{sim_pi_size}: {size for simulated prediction interval lines}
#'  \item{sim_median_fill}: {fill color for simulated median area}
#'  \item{sim_median_alpha}: {transparency for simulated median area}
#'  \item{sim_median_color}: {color for simulated median line}
#'  \item{sim_median_linetype}: {linetype for simulated median line}
#'  \item{sim_median_size}: {size for simulated median line}
#'  \item{bin_separators_color}: {color for bin separator lines, NA for don't plot}
#'  \item{bin_separators_location}: {where to plot bin separators ("t" for top, "b" for bottom)}
#' }
#' @return A list with vpc theme specifiers
#' @export create_vpc_theme

create_vpc_theme <- function (update = list()) {
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
  if(!is.null(update) & length(names(update)) > 0) {
    for(i in seq(names(update))) {
      tmp[[names(update)[i]]] <- update[[names(update)[i]]]
    }
  }
  tmp
}
