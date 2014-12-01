vpc_theme <- function (update = NULL) {
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

# bar <- vpc_theme(bin)
# class(bar) <- "vpc_theme"
# 
# update_theme <- function ( 
#   
#   obs_color = "#000000",
#   obs_size = 1,
#   
#   obs_median_color = "#000000",
#   obs_median_linetype = "solid",
#   obs_median_size = 1.5,  
#   
#   obs_ci_color = "#000000",
#   obs_ci_linetype = "dashed",
#   obs_ci_size = .5,    
#   
#   sim_pi_fill = "#3388cc",
#   sim_pi_alpha = 0.15,  
#   sim_pi_color = "#000000",
#   sim_pi_linetype = 'dotted',
#   sim_pi_size = 1,  
# 
#   sim_median_fill = "#3388cc",
#   sim_median_alpha = 0.3,  
#   sim_median_color = "#000000",
#   sim_median_linetype = "dashed",
#   sim_median_size = 1,  
#   
#   bin_separators_color = "#000000"  
# ) {
#   tmp <- list (
#     obs_color = obs_color,
#     obs_size = obs_size,
#     
#     obs_median_color = obs_median_color ,
#     obs_median_linetype = obs_median_linetype ,
#     obs_median_size = obs_median_size,  
#     obs_ci_color = obs_ci_color ,
#     obs_ci_linetype = obs_ci_linetype ,
#     obs_ci_size = obs_ci_size,  
#     
#     sim_pi_fill = sim_pi_fill,
#     sim_pi_alpha = sim_pi_alpha,    
#     sim_pi_color = sim_pi_color,
#     sim_pi_linetype = sim_pi_linetype,
#     sim_pi_size = sim_pi_size,  
#     
#     sim_median_fill = sim_median_fill,
#     sim_median_alpha = sim_median_alpha,  
#     sim_median_color = sim_median_color,
#     sim_median_linetype = sim_median_linetype,
#     sim_median_size = sim_median_size,  
#     
#     bin_separators_color = bin_separators_color  
#   )      
#   class(tmp) <- "vpc_theme"
#   return(tmp)
# }
