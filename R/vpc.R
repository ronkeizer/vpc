#' VPC function
#' 
#' Creates a VPC plot from observed and simulation data
#' @param sim a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param obs a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param bins either "auto" or a numeric vector specifying the bin separators.  
#' @param bins either "density", "time", or "data", or a numeric vector specifying the bin separators.  
#' @param n_bins when using the "auto" binning method, what number of bins to aim for
#' @param obs_dv variable in data.frame for observed dependent value. "dv" by default
#' @param sim_dv variable in data.frame for simulated dependent value. "sdv" by default
#' @param obs_idv variable in data.frame for observed independent value. "time" by default
#' @param sim_idv variable in data.frame for simulated independent value. "time" by default
#' @param obs_id variable in data.frame for observed individual. "id" by default
#' @param sim_id variable in data.frame for simulated individual. "id" by default
#' @param obs_pred variable in data.frame for population predicted value. "pred" by default
#' @param sim_pred variable in data.frame for population predicted value. "pred" by default
#' @param nonmem should variable names standard to NONMEM be used (i.e. ID, TIME, DV, PRED). Default is "auto" for autodetect
#' @param plot_obs_dv should observations be plotted?
#' @param plot_obs_ci default is TRUE
#' @param plot_pi_ci default is TRUE
#' @param plot_obs_median default is TRUE
#' @param plot_sim_median default is TRUE
#' @param plot_sim_median_ci default is TRUE
#' @param stratify character vector of stratification variables. Only 1 or 2 stratification variables can be supplied.
#' @param stratify_color variable to stratify and color lines for observed data. Only 1 stratification variables can be supplied.
#' @param pred_corr perform prediction-correction? 
#' @param pred_corr_lower_bnd lower bound for the prediction-correction
#' @param pi simulated prediction interval to plot. Default is c(0.05, 0.95), 
#' @param ci confidence interval to plot. Default is (0.05, 0.95)
#' @param uloq Number or NULL indicating upper limit of quantification. Default is NULL.  
#' @param lloq Number or NULL indicating lower limit of quantification. Default is NULL.  
#' @param plot Boolean indacting whether to plot the ggplot2 object after creation. Default is FALSE.
#' @param log_y Boolean indacting whether y-axis should be shown as logarithmic. Default is FALSE.
#' @param log_y_min minimal value when using log_y argument. Default is 1e-3.
#' @param xlab ylab as numeric vector of size 2
#' @param ylab ylab as numeric vector of size 2
#' @param title title
#' @param smooth "smooth" the VPC (connect bin midpoints) or show bins as rectangular boxes. Default is TRUE.
#' @param theme which theme to load from the themes object
#' @param custom_theme specify a custom ggplot2 theme
#' @param facet either "wrap", "columns", or "rows" 
#' @return a list containing calculated VPC information, and a ggplot2 object
#' @export
#' @seealso \link{sim_data}, \link{vpc_cens}, \link{vpc_tte}
#' @examples
#' obs <- Theoph
#' colnames(obs) <- c("id", "wt", "dose", "time", "dv")
#' obs <- obs %>%   # create a dummy covariate to show stratification
#'  group_by(id) %>%  
#'  mutate(sex = round(runif(1)))
#' 
#' sim <- sim_data(obs, # the design of the dataset
#'                 model = function(x) { # the model
#'                   pk_oral_1cmt (t = x$time, dose=x$dose * x$wt, ka = x$ka, 
#'                                 ke = x$ke, cl = x$cl * x$wt, 
#'                                 ruv = list(additive = 0.1))
#'                 }, 
#'                 theta = c(2.774, 0.0718, .0361),             # parameter values
#'                 omega_mat = c(0.08854,                       # specified as lower triangle 
#'                               0.02421, 0.02241,              # note: assumed every th has iiv,
#'                               0.008069, 0.008639, 0.02862),  #  set to 0 if no iiv. 
#'                 par_names = c("ka", "ke", "cl"),             # link the parameters in the model  
#'                 n = 500)                                     #   to the thetas/omegas
#' 
#' vpc_dat <- vpc(sim, obs, stratify = c("sex"))
vpc <- function(sim = NULL, 
                obs = NULL, 
                bins = "jenks",
                n_bins = "auto",
                obs_dv = NULL,
                sim_dv =  NULL,
                obs_idv = NULL,
                sim_idv = NULL,
                obs_id = NULL,
                sim_id = NULL,
                obs_pred = NULL,
                sim_pred = NULL,
                nonmem = "auto",
                plot = FALSE,
                plot_obs_dv = FALSE,
                plot_obs_ci = TRUE,
                plot_pi_ci = TRUE,
                plot_obs_median = TRUE,
                plot_sim_median = TRUE,
                plot_sim_median_ci = TRUE,
                stratify = NULL,
                stratify_color = NULL,
                legend_pos = NULL,
                pred_corr = FALSE,
                pred_corr_lower_bnd = 0,
                pi = c(0.05, 0.95), 
                ci = c(0.05, 0.95),
                uloq = NULL, 
                lloq = NULL, 
                log_y = FALSE,
                log_y_min = 1e-3,
                xlab = NULL, 
                ylab = NULL,
                title = NULL,
                smooth = TRUE,
                theme = "default",
                custom_theme = NULL,
                facet = "wrap") {
  if (nonmem == "auto") {
    if(sum(c("ID", "TIME") %in% colnames(obs)) == 2) { # most likely, data is from NONMEM
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
    if (is.null(obs_pred)) { obs_pred <- "PRED" }
    if (is.null(sim_dv)) { sim_dv <- "DV" }
    if (is.null(sim_idv)) { sim_idv <- "TIME" }
    if (is.null(sim_id)) { sim_id <- "ID" }
    if (is.null(sim_pred)) { sim_pred <- "PRED" }
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
    if(is.null(obs_pred)) { obs_pred = "pred" }
    if(is.null(sim_pred)) { sim_pred = "pred" }      
  }
  if(is.null(obs) & is.null(sim)) {
    stop("At least a simulation or an observation dataset are required to create a plot!")
  }
  stratify_original <- stratify
  if(!is.null(stratify_color)) {
    if (is.null(stratify)) {
      stratify <- stratify_color
    }
    if (length(stratify_color) > 1) {
      stop("Error: please specify only 1 stratification variable for color!")      
    }
    if (!stratify_color %in% stratify) {
      stratify_original <- stratify
      stratify <- c(stratify, stratify_color)
    }
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
  if (pred_corr) {
    if (!is.null(obs) & !obs_pred %in% names(obs)) {
      warning("Warning: Prediction-correction: specified pred-variable not found in observation dataset, trying to get from simulated dataset...")      
      if (!sim_pred %in% names(sim)) {
        stop("Error: Prediction-correction: specified pred-variable not found in simulated dataset, not able to perform pred-correction!")
      } else {
        obs[[obs_pred]] <- sim[1:length(obs[,1]), sim_pred]
        warning ("OK")
      }
    } else {
      if (!sim_pred %in% names(sim)) {
        stop("Warning: Prediction-correction: specified pred-variable not found in simulated dataset, not able to perform pred-correction!")
      }      
    }
    if(!is.null(obs)) {
      obs$pred <- obs[[obs_pred]]      
    }
    if(!is.null(sim)) {
      sim$pred <- sim[[sim_pred]]      
    }
  }
  if (!is.null(obs)) {  
    obs <- format_vpc_input_data(obs, obs_dv, obs_idv, obs_id, lloq, uloq, stratify, bins, log_y, log_y_min, "observed")
    if (pred_corr) {
      obs <- obs %>% group_by(strat, bin) %>% mutate(pred_bin = mean(pred))
      obs[obs$pred != 0,]$dv <- pred_corr_lower_bnd + (obs[obs$pred != 0,]$dv - pred_corr_lower_bnd) * (obs[obs$pred != 0,]$pred_bin - pred_corr_lower_bnd) / (obs[obs$pred != 0,]$pred - pred_corr_lower_bnd)
    }
  }
  if (!is.null(sim)) {  
    sim <- format_vpc_input_data(sim, sim_dv, sim_idv, sim_id, lloq, uloq, stratify, bins, log_y, log_y_min, "simulated")
    sim$sim <- add_sim_index_number(sim, id = "id")    
    if (pred_corr) {
      sim <- sim %>% group_by(strat, bin) %>% mutate(pred_bin = mean(pred))
      sim[sim$pred != 0,]$dv <- pred_corr_lower_bnd + (sim[sim$pred != 0,]$dv - pred_corr_lower_bnd) * (sim[sim$pred != 0,]$pred_bin - pred_corr_lower_bnd) / (sim[sim$pred != 0,]$pred - pred_corr_lower_bnd)
    }
  }
  if (!is.null(sim)) {
    tmp1 <- sim %>% group_by(strat, sim, bin)
    aggr_sim <- data.frame(cbind(tmp1 %>% dplyr::summarize(quantile(dv, pi[1])),
                                 tmp1 %>% dplyr::summarize(quantile(dv, 0.5 )),
                                 tmp1 %>% dplyr::summarize(quantile(dv, pi[2])),
                                 tmp1 %>% dplyr::summarize(mean(idv))))
    aggr_sim <- aggr_sim[,-grep("(bin.|strat.|sim.)", colnames(aggr_sim))]  
    colnames(aggr_sim)[grep("quantile", colnames(aggr_sim))] <- c("q5", "q50", "q95")
    colnames(aggr_sim)[length(aggr_sim[1,])] <- "mn_idv"
    tmp <- aggr_sim %>% group_by(strat, bin)
    vpc_dat <- data.frame(cbind(tmp %>% dplyr::summarize(quantile(q5, ci[1])),
                                tmp %>% dplyr::summarize(quantile(q5, 0.5)),
                                tmp %>% dplyr::summarize(quantile(q5, ci[2])),
                                tmp %>% dplyr::summarize(quantile(q50, ci[1])),
                                tmp %>% dplyr::summarize(quantile(q50, 0.5)),
                                tmp %>% dplyr::summarize(quantile(q50, ci[2])),
                                tmp %>% dplyr::summarize(quantile(q95, ci[1])),
                                tmp %>% dplyr::summarize(quantile(q95, 0.5)),
                                tmp %>% dplyr::summarize(quantile(q95, ci[2])),
                                tmp %>% dplyr::summarize(mean(mn_idv))))
    vpc_dat <- vpc_dat[,-grep("(bin.|strat.)", colnames(vpc_dat))]
    colnames(vpc_dat) <- c("strat", "bin", 
                           "q5.low","q5.med","q5.up", 
                           "q50.low","q50.med","q50.up",
                           "q95.low","q95.med","q95.up",
                           "bin_mid")
    vpc_dat$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
    vpc_dat$bin_max <- rep(bins[2:length(bins)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
#    vpc_dat$bin_mid <- ((vpc_dat$bin_min + vpc_dat$bin_max) / 2)[vpc_dat$bin]
  } else {
    vpc_dat <- NULL
  }
  if(!is.null(obs)) {
    tmp1 <- obs %>% group_by(strat,bin)
    aggr_obs <- data.frame(cbind(tmp1 %>% summarise(quantile(dv, 0.05)),
                                 tmp1 %>% summarise(quantile(dv, 0.5 )),
                                 tmp1 %>% summarise(quantile(dv, 0.95)),
                                 tmp1 %>% summarise(mean(idv))))
    aggr_obs <- aggr_obs[,-grep("(bin.|strat.|sim.)", colnames(aggr_obs))]
    colnames(aggr_obs) <- c("strat", "bin", "obs5","obs50","obs95", "bin_mid")
    aggr_obs$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(aggr_obs$strat)) )[aggr_obs$bin]
    aggr_obs$bin_max <- rep(bins[2:length(bins)], length(unique(aggr_obs$strat)) )[aggr_obs$bin]
#    aggr_obs$bin_mid <- ((aggr_obs$bin_min + aggr_obs$bin_max)/2) [aggr_obs$bin]    
  } else {
    aggr_obs <- NULL
  }
  if(is.null(xlab)) {
    xlab <- obs_idv
  }
  if(is.null(ylab)) {
    ylab <- obs_dv
  }
  if (!is.null(stratify_original)) {
    if (length(stratify) == 2) {
      vpc_dat$strat1 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)-1]
      vpc_dat$strat2 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)]
      aggr_obs$strat1 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)-1]
      aggr_obs$strat2 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)]   
    }
  }  
  if (!is.null(sim)) {
    pl <- ggplot(vpc_dat, aes(x=bin_mid)) 
    if(plot_sim_median) {
      pl <- pl + geom_line(aes(y=q50.med), linetype='dashed')           
    }
    if(plot_sim_median_ci) {
      if (smooth) {
        pl <- pl +
          geom_ribbon(aes(x=bin_mid, y=q50.low, ymin=q50.low, ymax=q50.up), alpha=themes[[theme]]$med_area_alpha, fill = themes[[theme]]$med_area) 
      } else {
        pl <- pl +
          geom_rect(aes(xmin=bin_min, xmax=bin_max, y=q50.low, ymin=q50.low, ymax=q50.up), alpha=themes[[theme]]$med_area_alpha, fill = themes[[theme]]$med_area) 
      }       
    }
    if (plot_pi_ci) {
      if (smooth) {
        pl <- pl + 
          geom_ribbon(aes(x=bin_mid, y=q5.low, ymin=q5.low, ymax=q5.up), alpha=themes[[theme]]$pi_area_alpha, fill = themes[[theme]]$pi_area) +
          geom_ribbon(aes(x=bin_mid, y=q95.low, ymin=q95.low, ymax=q95.up), alpha=themes[[theme]]$pi_area_alpha, fill = themes[[theme]]$pi_area) 
      } else {
        pl <- pl + 
          geom_rect(aes(xmin=bin_min, xmax=bin_max, y=q5.low, ymin=q5.low, ymax=q5.up), alpha=themes[[theme]]$pi_area_alpha, fill = themes[[theme]]$pi_area) +
          geom_rect(aes(xmin=bin_min, xmax=bin_max, y=q95.low, ymin=q95.low, ymax=q95.up), alpha=themes[[theme]]$pi_area_alpha, fill = themes[[theme]]$pi_area)     
      }      
    }
  } else {
    if (!is.null(stratify_color)) {
      if (length(stratify) == 2) {
        pl <- ggplot(aggr_obs, aes(colour=as.factor(strat2)))         
      } else {
        pl <- ggplot(aggr_obs, aes(colour=as.factor(strat)))           
      }
      pl <- pl + scale_colour_discrete(name="")
    } else {
      pl <- ggplot(aggr_obs)  
    }
  }
  if(!is.null(obs)) {
    if (plot_obs_median) {
      pl <- pl +
        geom_line(data=aggr_obs, aes(x=bin_mid, y=obs50), linetype='solid')       
    }
    if(plot_obs_ci) {
      pl <- pl +
        geom_line(data=aggr_obs, aes(x=bin_mid, y=obs5), linetype='dotted') +
        geom_line(data=aggr_obs, aes(x=bin_mid, y=obs95), linetype='dotted') 
    }
    if (plot_obs_dv) {
      pl <- pl + geom_point(data=obs, aes(x=idv, y = dv))
    }    
  }
  bdat <- data.frame(cbind(x=bins, y=NA))
  pl <- pl + 
    geom_rug(data=bdat, sides = "t", aes(x = x, y=y), colour="#333333")
  pl <- pl + xlab(xlab) + ylab(ylab)
  if (log_y) {
    pl <- pl + scale_y_log10() 
  }
  if (!is.null(stratify)) {
    if (length(stratify_original) == 1) {
      if (!is.null(stratify_color)) {
        if (facet == "wrap") {
          pl <- pl + facet_wrap(~ strat1)      
        } else {
          if(length(grep("row", facet))>0) {
            pl <- pl + facet_grid(strat1 ~ .)                
          } else {
            pl <- pl + facet_grid(. ~ strat1)                
          }
        } 
      } else { 
        if (facet == "wrap") {
          pl <- pl + facet_wrap(~ strat)      
        } else {
          if(length(grep("row", facet))>0) {
            pl <- pl + facet_grid(strat ~ .)                
          } else {
            pl <- pl + facet_grid(. ~ strat)                
          }
        }         
      } 
    } else { # 2 grid-stratification 
      if ("strat1" %in% c(colnames(vpc_dat), colnames(aggr_obs))) {
        if(length(grep("row", facet))>0) {
          pl <- pl + facet_grid(strat1 ~ strat2)                
        } else {
          pl <- pl + facet_grid(strat2 ~ strat1)                
        }        
      } else { # only color stratification
        if ("strat" %in% c(colnames(vpc_dat), colnames(aggr_obs))) {
          # color stratification only
        } else {          
          stop ("Stratification unsuccesful.")          
        }
      }
    }
  }
  if (!is.null(title)) {
    pl <- pl + ggtitle(title)  
  }
  if (!is.null(custom_theme)) {  
    pl <- pl + custom_theme()    
  } else {
    if (!is.null(theme)) {
      pl <- pl + theme_plain()
    } 
  }
  if (plot) {
    print(pl)    
  }
  return(pl)
}
