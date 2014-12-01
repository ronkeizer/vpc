#' VPC function for categorical
#' 
#' Creates a VPC plot from observed and simulation data
#' sim, 
#' @param sim a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param obs a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param bins either "density", "time", or "data", or a numeric vector specifying the bin separators.  
#' @param type either "lloq" (default) or "uloq".
#' @param n_bins number of bins
#' @param obs_dv variable in data.frame for observed dependent value. "dv" by default
#' @param sim_dv variable in data.frame for simulated dependent value. "sdv" by default
#' @param obs_idv variable in data.frame for observed independent value. "time" by default
#' @param sim_idv variable in data.frame for simulated independent value. "time" by default
#' @param obs_id variable in data.frame for observed individual. "id" by default
#' @param sim_id variable in data.frame for simulated individual. "id" by default
#' @param nonmem should variable names standard to NONMEM be used (i.e. ID, TIME, DV, PRED).  Default is "auto" for autodetect
#' @param ci confidence interval to plot. Default is (0.05, 0.95)
#' @param uloq Number or NULL indicating upper limit of quantification. Default is NULL.  
#' @param lloq Number or NULL indicating lower limit of quantification. Default is NULL.  
#' @param plot Boolean indacting whether to plot the ggplot2 object after creation. Default is FALSE.
#' @param plot_sim_med Plot the simulated median? Default is FALSE
#' @param xlab ylab as numeric vector of size 2
#' @param ylab ylab as numeric vector of size 2
#' @param title title
#' @param smooth "smooth" the VPC (connect bin midpoints) or show bins as rectangular boxes. Default is TRUE.
#' @param vpc_theme theme to be used in VPC. Expects list of class vpc_theme created with function vpc_theme()
#' @param ggplot_theme specify a custom ggplot2 theme
#' @param facet either "wrap", "columns", or "rows" 
#' @return a list containing calculated VPC information, and a ggplot2 object
#' @export
#' @seealso \link{vpc}
vpc_cat  <- function(sim = NULL, 
                     obs = NULL, 
                     bins = "jenks",
                     n_bins = "auto",
                     type = "bloq",
                     obs_dv = NULL,
                     sim_dv =  NULL,
                     obs_idv = NULL,
                     sim_idv = NULL,
                     obs_id = NULL,
                     sim_id = NULL,
                     nonmem = "auto",
                     ci = c(0.05, 0.95),
                     uloq = NULL, 
                     lloq = NULL, 
                     plot = FALSE,
                     xlab = NULL, 
                     plot_sim_med = FALSE,
                     ylab = NULL,
                     title = NULL,
                     smooth = TRUE,
                     vpc_theme = NULL,
                     ggplot_theme = NULL,
                     facet = "wrap") {
  if (nonmem == "auto") {
    if(sum(c("ID","TIME") %in% colnames(obs)) == 2) { # most likely, data is from NONMEM
      nonmem <- TRUE
    } else {
      nonmem <- FALSE
    }        
  } else {
    if(class(nonmem) != "logical") {
      nonmem <- FALSE
    }
  } 
  if (nonmem) {
    if (is.null(obs_dv)) { obs_dv <- "DV" }
    if (is.null(obs_idv)) { obs_idv <- "TIME" }
    if (is.null(obs_id)) { obs_id <- "ID" }
    if (is.null(sim_dv)) { sim_dv <- "DV" }
    if (is.null(sim_idv)) { sim_idv <- "TIME" }
    if (is.null(sim_id)) { sim_id <- "ID" }    
    if(!is.null(obs)) {
      if("MDV" %in% colnames(obs)) {
        obs <- obs[obs$MDV == 0,]
      }
      if("EVID" %in% colnames(obs)) {
        obs <- obs[obs$EVID == 0,]
      }      
    }
    if(!is.null(sim)) {  
      if("MDV" %in% colnames(sim)) {
        sim <- sim[sim$MDV == 0,]
      }
      if("EVID" %in% colnames(obs)) {
        sim <- sim[sim$EVID == 0,]
      }
    }
  } else {
    if(is.null(obs_dv)) { obs_dv = "dv" }
    if(is.null(sim_dv)) { sim_dv = "dv" }
    if(is.null(obs_idv)) { obs_idv = "time" }
    if(is.null(sim_idv)) { sim_idv = "time" }
    if(is.null(obs_id)) { obs_id = "id" }
    if(is.null(sim_id)) { sim_id = "id" }
  }
  if(is.null(obs) & is.null(sim)) {
    stop("At least a simulation or an observation dataset are required to create a plot!")
  }
  if (class(bins) != "numeric") {
    if(!is.null(obs)) {
      bins <- auto_bin(obs, bins, n_bins, x=obs_idv)      
    } else { # get from sim
      bins <- auto_bin(sim, bins, n_bins, x=obs_idv)            
    }
    if (is.null(bins)) {
      stop("Binning unsuccessful, try increasing the number of bins.")
    }
  }
  log_y <- FALSE # dummy, required for format_vpc_input_data function
  log_y_min <- 0
  stratify <- NULL
  if (!is.null(obs)) {  
    obs <- format_vpc_input_data(obs, obs_dv, obs_idv, obs_id, lloq, uloq, stratify, bins, log_y, log_y_min, "observed")
  }
  if (!is.null(sim)) {  
    sim <- format_vpc_input_data(sim, sim_dv, sim_idv, sim_id, lloq, uloq, stratify, bins, log_y, log_y_min, "simulated")
    sim$sim <- add_sim_index_number(sim, id = "id")    
  }
  fact_perc <- function(x, fact) { sum(x == fact) / length(x) } # below lloq, default     
  obs$dv <- as.factor(obs$dv) 
  lev <- levels(obs$dv)  
  if (!is.null(sim)) {
    tmp1 <- sim %>% group_by(sim, bin)
    for (i in seq(lev)) {
      if (i == 1) {
        aggr_sim <- tmp1 %>% dplyr::summarize(fact_perc(dv, lev[i]))
      } else {
        aggr_sim <- cbind(aggr_sim, tmp1 %>% dplyr::summarize(fact_perc(dv, lev[i])) )           
      }
    } 
    aggr_sim <- cbind(aggr_sim, tmp1 %>% dplyr::summarize(mean(idv)))
    aggr_sim <- data.frame(aggr_sim)
    aggr_sim <- aggr_sim[,-grep("(bin.|sim.)", colnames(aggr_sim))]
    colnames(aggr_sim) <- c("sim", "bin", paste0("fact_", lev), "mn_idv") 
    tmp3 <- reshape2::melt(aggr_sim, id=c("sim", "bin", "mn_idv"))
    tmp3$strat <- rep(lev, each = length(aggr_sim[,1]))
    tmp4 <- tmp3 %>% group_by(strat, bin)    
    vpc_dat <- data.frame(cbind(tmp4 %>% dplyr::summarize(quantile(value, ci[1])),
                                tmp4 %>% dplyr::summarize(quantile(value, 0.5)),
                                tmp4 %>% dplyr::summarize(quantile(value, ci[2])),
                                tmp4 %>% dplyr::summarize(mean(mn_idv))
                                ))
    vpc_dat <- vpc_dat[,-grep("(bin.|strat.)", colnames(vpc_dat))]
    colnames(vpc_dat) <- c("strat", "bin", "prob_low", "prob_med", "prob_up", "bin_mid")  
    vpc_dat$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
    vpc_dat$bin_max <- rep(bins[2:length(bins)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
#    vpc_dat$bin_mid <- (vpc_dat$bin_min + vpc_dat$bin_max) / 2    
  } else {
    vpc_dat <- NULL
  }
  if(!is.null(obs)) {
    tmp <- obs %>% group_by(bin)
    for (i in seq(lev)) {
      if (i == 1) {
        aggr_obs <- tmp %>% dplyr::summarize(fact_perc(dv, lev[i]))
      } else {
        aggr_obs <- cbind(aggr_obs, tmp %>% dplyr::summarize(fact_perc(dv, lev[i])) )           
      }
    }     
    tmp1 <- data.frame(cbind(aggr_obs, data.frame(tmp %>% dplyr::summarize(mean(idv)))))
    tmp1 <- tmp1[,-grep("(bin.|strat.|sim.)", colnames(tmp1))]
    colnames(tmp1) <- c("bin", paste0("fact_", lev), "bin_mid")    
    tmp2 <- reshape2::melt(tmp1, id=c("bin", "bin_mid"))
    tmp2$strat <- rep(lev, each=length(aggr_obs[,1]))
    tmp2$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(tmp2$strat)) )[tmp2$bin]
    tmp2$bin_max <- rep(bins[2:length(bins)], length(unique(tmp2$strat)) )[tmp2$bin]  
    aggr_obs <- tmp2
    colnames(aggr_obs)[4] <- "prob"
  } else {
    aggr_obs <- NULL
  }
#   if (!is.null(stratify_original)) {
#     if (length(stratify) == 2) {
#       vpc_dat$strat1 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)-1]
#       vpc_dat$strat2 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)]
#       aggr_obs$strat1 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)-1]
#       aggr_obs$strat2 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)]   
#     }
#   }
  if(is.null(vpc_theme) || (class(vpc_theme) != "vpc_theme")) {
    vpc_theme <- create_vpc_theme()
  }
  if (!is.null(sim)) {
    pl <- ggplot(vpc_dat, aes(x=bin_mid, y=dv)) 
    if(plot_sim_med) {
      geom_line(aes(y=prob_med), linetype='dashed')       
    }
    if (smooth) {
      pl <- pl + 
        geom_ribbon(aes(x=bin_mid, y=prob_low, ymin=prob_low, ymax=prob_up), fill=vpc_theme$sim_median_fill, alpha=vpc_theme$sim_median_alpha) 
    } else {
      pl <- pl + 
        geom_rect(aes(xmin=bin_min, xmax=bin_max, x=bin_mid, y=prob_low, ymin=prob_low, ymax=prob_up), fill=vpc_theme$sim_median_fill, alpha=vpc_theme$sim_median_alpha) 
    }
  } else {
    if (!is.null(stratify_color)) {
      if (length(stratify) == 2) {
        pl <- ggplot(aggr_obs, aes(y=dv, colour=as.factor(strat2)))         
      } else {
        pl <- ggplot(aggr_obs, aes(y=dv, colour=as.factor(strat)))           
      }
      pl <- pl + scale_colour_discrete(name="")
    } else {
      pl <- ggplot(aggr_obs, aes(y=dv))        
    }    
  }
  if (!is.null(obs)) {
    pl <- pl +
      geom_line(data=aggr_obs, aes(x=bin_mid, y=prob), linetype='solid') 
  }
  bdat <- data.frame(cbind(x=bins, y=NA))
  pl <- pl + 
    geom_rug(data=bdat, sides = "t", aes(x = x, y=y), colour="#333333")
#   if (!is.null(stratify)) {
#     if (length(stratify_original) == 1) {
#       if (!is.null(stratify_color)) {
#         if (facet == "wrap") {
#           pl <- pl + facet_wrap(~ strat1)      
#         } else {
#           if(length(grep("row", facet))>0) {
#             pl <- pl + facet_grid(strat1 ~ .)                
#           } else {
#             pl <- pl + facet_grid(. ~ strat1)                
#           }
#         } 
#       } else { 
#         if (facet == "wrap") {
#           pl <- pl + facet_wrap(~ strat)      
#         } else {
#           if(length(grep("row", facet))>0) {
#             pl <- pl + facet_grid(strat ~ .)                
#           } else {
#             pl <- pl + facet_grid(. ~ strat)                
#           }
#         }         
#       } 
#     } else { # 2 grid-stratification 
#       if ("strat1" %in% c(colnames(vpc_dat), colnames(aggr_obs))) {
#         if(length(grep("row", facet))>0) {
#           pl <- pl + facet_grid(strat1 ~ strat2)                
#         } else {
#           pl <- pl + facet_grid(strat2 ~ strat1)                
#         }        
#       } else { # only color stratification
#         if ("strat" %in% c(colnames(vpc_dat), colnames(aggr_obs))) {
#           # color stratification only
#         } else {          
#           stop ("Stratification unsuccesful.")          
#         }
#       }
#     }
#   }
  if (facet == "wrap") {  
    pl <- pl + facet_wrap(~ strat)                
  } else {
    if(length(grep("row", facet))>0) {
      pl <- pl + facet_grid(strat ~ .)                
    } else { 
      pl <- pl + facet_grid(. ~ strat)                
    }    
  }
  if (!is.null(title)) {
    pl <- pl + ggtitle(title)  
  }
  if (!is.null(ggplot_theme)) {  
    pl <- pl + ggplot_theme()    
  } else {
    pl <- pl + theme_plain() 
  }
  if(!is.null(xlab)) {
    pl <- pl + xlab(xlab)
  } else {
    pl <- pl + xlab(obs_idv)
  }
  if(!is.null(ylab)) {
    pl <- pl + ylab(ylab)
  } else {
    pl <- pl + ylab("Probability")
  }
  pl <- pl + ylim(c(0,1))
  if (plot) {
    print(pl)    
  }
  return(pl)
}