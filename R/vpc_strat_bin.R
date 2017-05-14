#' Create VPC plot with different binning settings for each stratification group
#'
#' \code{vpc_strat_bin} provides a workaround to create a VPC plot with
#' different bin settings for each stratification group of data.
#'
#' To control the graph settings with ggplot2 syntax, you can set 
#' \code{return_fig = FALSE}, edit each subplot in \code{plot_data$glist} with ggplot2,
#' and apply \code{\link{combine_vps_strat}} function to create combined plot.
#'
#' @export
#' @param sim see \code{\link{vpc}} function
#' @param obs see \code{\link{vpc}} function
#' @param bins see \code{\link{vpc}} function
#' @param bin_list a list to assign \code{bins} for each stratification group. 
#' Each element is applied starting from horizontal direction then vertical
#' direction
#' @param n_bins see \code{\link{vpc}} function
#' @param n_bins_list a vector to assign \code{n_bins} for each stratification group.
#' Each element is applied starting from horizontal direction then vertical
#' direction
#' @param stratify see \code{\link{vpc}} function. The name of the column cannot be 
#' "strat" or "strat_for_title"
#' @param strat_fig_dir set the direction of figure arrangement for two 
#' stratification variables. If strat_fig_dir == 1 (default), first variable 
#' will be spread in the horizontal direction
#' @param facet_names see \code{\link{vpc}} function
#' @param verbose show debugging information (TRUE or FALSE)
#' @param xylab boolean indicting whether to show axis labels for individual subplots
#' @param return_fig boolean whether to return a combined figure (TRUE, default)
#'  or a list object of individual plots (FALSE)
#' @param ... other inputs passed on to \code{\link{vpc}} function
#' @return a combined figure (return_fig==TRUE, default) or a list object of individual plots (return_fig==FALSE)
#' @examples
#' vpc_strat_bin(sim = simple_data$sim,
#'               obs = simple_data$obs,
#'               stratify = "ISM", 
#'               n_bins_list = c(4,8))
#'
#'
vpc_strat_bin <- function(sim = NULL,
                          obs = NULL,
                          bins     = "jenks",
                          bin_list = NULL, 
                          n_bins   = "auto",
                          n_bins_list = NULL, 
                          stratify = NULL,
                          strat_fig_dir = 1,
                          facet_names = TRUE,
                          verbose = FALSE,
                          xylab = FALSE,
                          return_fig = TRUE,
                          ...){
  
  
  ## checking whether stratification columns are available
  if(!is.null(stratify)) {
    if(verbose) {
      message("Stratifying oberved data...")
    }
    if(!is.null(obs)) {
      check_stratification_columns_available(obs, stratify, "observation")
    }
    if(!is.null(sim)) {
      check_stratification_columns_available(sim, stratify, "simulation")
    }
  }
  
  ## Assign new column combining two stratification variables
  obs <- add_stratification(obs,stratify) 
  sim <- add_stratification(sim,stratify) 
  
  if(facet_names == FALSE) {
      for(j in seq(stratify)) {
        obs$strat <- as.factor(gsub(paste0(stratify[j],"="), "", obs$strat))
        sim$strat <- as.factor(gsub(paste0(stratify[j],"="), "", sim$strat))
      }
  }
  
  obs <- mutate(obs,strat_for_title = strat)
  sim <- mutate(sim,strat_for_title = strat)
  
  ## Organize stratification properties
  n_strat1 <- select_(obs,stratify[1]) %>% unique() %>% nrow()
  
  ### Change order of stratify elements depending on figure organization
  strat_vec_matrix <- 
    obs$strat %>% unique() %>% sort() %>% matrix(nrow = n_strat1, byrow = TRUE)
  if(strat_fig_dir==2) strat_vec_matrix <- t(strat_vec_matrix)
  strat_vec <- as.vector(strat_vec_matrix)
  
  ### Specify size of subplot matrix for plot_grid
  plot_data <- list(nrow=NULL,ncol=NULL)
  if (strat_fig_dir == 1){
    plot_data$ncol <- n_strat1
  }else{
    plot_data$nrow <- n_strat1
  }
  
  
  ## Make sure #stratification categories and #n_bins_list or #bin_list are the same
  if(!is.null(bin_list) & length(bin_list)<length(strat_vec)){
    stop("#(Elements in bin_list) must be more than #(Stratification categories)")
  }
  if(!is.null(n_bins_list) & length(n_bins_list)<length(strat_vec)){
    stop("#(Elements in n_bins_list) must be more than #(Stratification categories)")
  }
  
  
  ## Create a list of figures
  glist <- list()
  
  for (k in seq_along(strat_vec)){
    ### Assign bins and n_bins from their lists
    if(!is.null(bin_list)){
      bins <- bin_list[[k]] 
    }
    if(!is.null(n_bins_list)){
      n_bins <- n_bins_list[[k]] 
    }
    
    
    if(nrow(filter(obs, strat==strat_vec[[k]]))==0){
      #### Assign NA if no data corresponds to specified stratification variable
      glist[[k]] <- NA
    }else{
      #### Otherwise create a plot
      glist[[k]] <-
        vpc(sim = filter(sim, strat==strat_vec[[k]]),
            obs = filter(obs, strat==strat_vec[[k]]),
            bins= bins,
            n_bins= n_bins,
            stratify = "strat_for_title",
            stratify_color = NULL, 
            facet_names = FALSE,
            ...)
      
      if(xylab==FALSE){
        glist[[k]] <- 
          glist[[k]] +
          xlab("") + ylab("")
      }
      
    }
  }
  
  plot_data$glist <- glist
  
  if(return_fig) {
    return(combine_vps_strat(plot_data))
  } else {
    return(plot_data)
  }
  
  
}



#' Combine a list of figures from vpc_strat_bin function
#'
#' \code{combine_vps_strat} combines a list of figures from 
#' \code{\link{vpc_strat_bin}} function
#' 
#' You can control the subplot appearance by applying ggplot functions to each
#' subplot items (\code{plot_data$glist[[1]]}, \code{plot_data$glist[[2]]}, ...)
#'
#' @export
#' @param plot_data a list of figures and figure settings generated with 
#' \code{\link{vpc_strat_bin}} function
#' @return a combined figure
#' @examples
#' plot_data <- 
#'   vpc_strat_bin(sim = simple_data$sim,
#'                 obs = simple_data$obs,
#'                 stratify = "ISM", 
#'                 n_bins_list = c(4,8),
#'                 return_fig = F)
#'                 
#' for (k in 1:2) plot_data$glist[[k]] <- plot_data$glist[[k]] + scale_y_log10()
#' combine_vps_strat(plot_data)
#'
#'
combine_vps_strat <- function(plot_data){
  plot <- 
    cowplot::plot_grid(plotlist = plot_data$glist, 
                       nrow = plot_data$nrow,
                       ncol = plot_data$ncol,
                       align= "hv")

  return(plot)
}
