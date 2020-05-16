#' Create a customized VPC theme
#' 
#' @param update list containing the plot elements to be updated. Run `new_vpc_theme()` with no arguments to show an overview of available plot elements.
#' 
#' @details
#' This function creates a theme that customizes how the VPC looks, i.e. colors, fills, transparencies, linetypes an sizes, etc. The following arguments can be specified in the input list:
#' \itemize{
#'  \item{obs_color}: {color for observations points}
#'  \item{obs_size}: {size for observation points}
#'  \item{obs_median_color}: {color for median observation line}
#'  \item{obs_median_linetype}: {linetype for median observation line}
#'  \item{obs_median_size}: {size for median observation line}
#'  \item{obs_ci_fill}: {color for observation CI fill}
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
#'  \item{loq_color}: {color of line showing limit of quantification}
#' }
#' @return A list with vpc theme specifiers
#' @export
#' @examples 
#' theme1 <- new_vpc_theme(update = list(
#'   obs_color = "red",
#'   obs_ci_color = "#aa0000",
#'   obs_alpha = .3,
#'   sim_pi_fill = "#cc8833",
#'   sim_pi_size = 2
#' ))
#' vpc(simple_data$sim, simple_data$obs, vpc_theme = theme1)
#' 
new_vpc_theme <- function (update = NULL) {
  tmp <- structure(list(  
    obs_color = "#000000",
    obs_size = 1,
                   
    obs_median_color = "#000000",
    obs_median_linetype = "solid",
    obs_median_size = 1,
    obs_alpha = .7,
    obs_shape = 1,
                   
    obs_ci_color = "#000000",
    obs_ci_linetype = "dashed",
    obs_ci_fill = grDevices::rgb(0.5,0.5,0.5,0.2), ## only for TTE
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
                   
    bin_separators_color = "#000000",
    loq_color = "#990000"
  ), class = "vpc_theme")
  n <- names(tmp)
  if(is.null(update)) {
#    stop(paste0("Please specify a list with plot elements to update. Available elements: \n  - ", paste(n, collapse="\n  - ")))
    return(tmp)
  }
  if(!is.null(update) & length(names(update)) > 0) {
    for(i in seq(names(update))) {
      if(names(update)[i] %in% n) {
        tmp[[names(update)[i]]] <- update[[names(update)[i]]]
      } else {
        warning(paste0("`", names(update)[i],"` is not recognized as a plot element, ignoring."))
      }
    }
  }
  tmp
}
