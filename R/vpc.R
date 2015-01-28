#' VPC function
#' 
#' Creates a VPC plot from observed and simulation data
#' @param sim a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param obs a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param psn_folder instead of specyfing "sim" and "obs", specify a PsN-generated VPC-folder
#' @param bins either "auto" or a numeric vector specifying the bin separators.  
#' @param bins either "density", "time", or "data", or a numeric vector specifying the bin separators.  
#' @param n_bins when using the "auto" binning method, what number of bins to aim for
#' @param obs_cols observation dataset column names
#' @param sim_cols simulation dataset column names
#' @param obs_pred variable in data.frame for population predicted value. "pred" by default
#' @param sim_pred variable in data.frame for population predicted value. "pred" by default
#' @param show what to show in VPC (obs_ci, pi_as_area, pi_ci, obs_median, sim_median, sim_median_ci) 
#' @param software name of software platform using (eg nonmem, phoenix)
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
#' @param vpc_theme theme to be used in VPC. Expects list of class vpc_theme created with function vpc_theme()
#' @param ggplot_theme specify a custom ggplot2 theme
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
                psn_folder = NULL,
                bins = "jenks",
                n_bins = "auto",
                obs_cols = NULL,
                sim_cols = NULL,
                software = "auto",
                show = NULL,                
                legend_pos = NULL,
                plot = FALSE,
                stratify = NULL,
                stratify_color = NULL,
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
                vpc_theme = NULL,
                ggplot_theme = NULL,
                facet = "wrap") {
  if(is.null(obs) & is.null(sim)) {
    stop("At least a simulation or an observation dataset are required to create a plot!")
  }

   if(!is.null(psn_folder)) {
     obs <- read_table_nm(paste0(psn_folder, "/m1/", dir(paste0(psn_folder, "/m1"), pattern="original.npctab")[1]))
     sim <- read_table_nm(paste0(psn_folder, "/m1/", dir(paste0(psn_folder, "/m1"), pattern="simulation.1.npctab")[1]))
     software = "nonmem"
   }
   software_type <- guess_software(software, obs)
   # column names
   show_default <- list (
     obs_dv = FALSE,
     obs_ci = TRUE,
     obs_median = TRUE,
     sim_median = FALSE,
     sim_median_ci = TRUE,
     pi = FALSE,
     pi_ci = TRUE,
     pi_as_area = FALSE)
   show <- replace_list_elements(show_default, show)
   
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
   } else {
     # assumes phoenix dataset
     obs_cols_default <- list(dv = "COBS", id = "ID", idv = "TIME", pred = "PRED")
     sim_cols_default <- list(dv = "COBS", id = "ID", idv = "TIME", pred = "PRED")
   }
   
   obs_cols <- replace_list_elements(obs_cols_default, obs_cols)
   sim_cols <- replace_list_elements(sim_cols_default, sim_cols)
   
   if(!is.null(obs)) {
      obs <- filter_dv(obs)
   }
   if(!is.null(sim)) {  
     sim <- filter_dv(sim)
   }
 
  obs <- format_vpc_input_data(obs, obs_cols, lloq, uloq, stratify, bins, log_y, log_y_min, "observed")
  sim <- format_vpc_input_data(sim, sim_cols, lloq, uloq, stratify, bins, log_y, log_y_min, "simulated")
  
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
      bins <- auto_bin(obs, bins, n_bins)  
    } else { # get from sim
      bins <- auto_bin(sim, bins, n_bins)            
    }
    if (is.null(bins)) {
      stop("Binning unsuccessful, try increasing the number of bins.")
    }
  }
    obs <- bin_data(obs, bins, "idv") 
    sim <- bin_data(sim, bins, "idv")  
    
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
    if (pred_corr) {
      obs <- obs %>% group_by(strat, bin) %>% mutate(pred_bin = mean(pred))
      obs[obs$pred != 0,]$dv <- pred_corr_lower_bnd + (obs[obs$pred != 0,]$dv - pred_corr_lower_bnd) * (obs[obs$pred != 0,]$pred_bin - pred_corr_lower_bnd) / (obs[obs$pred != 0,]$pred - pred_corr_lower_bnd)
    }
  }  
  if (!is.null(sim)) {  
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
  if(is.null(vpc_theme) || (class(vpc_theme) != "vpc_theme")) {
    vpc_theme <- create_vpc_theme()
  }
  if(is.null(xlab)) {
    xlab <- obs_cols$idv
  }
  if(is.null(ylab)) {
    ylab <- obs_cols$dv
  }
  if (!is.null(stratify_original)) {
    if (length(stratify) == 2) {
      vpc_dat$strat1 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)-1]
      vpc_dat$strat2 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)]
      aggr_obs$strat1 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)-1]
      aggr_obs$strat2 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)]   
    }
  }  
  # plotting starts here
  vpc_db <- list(sim = sim,
             vpc_dat = vpc_dat,
             vpc_theme = vpc_theme,
             show = show,
             smooth = smooth,
             stratify = stratify,
             stratify_color = stratify_color,
             aggr_obs = aggr_obs,
             obs = obs,
             bins = bins,
             xlab = xlab,
             ylab = ylab,
             log_y = log_y,
             facet = facet,
             title = title,
             theme = theme,
             ggplot_theme = ggplot_theme,
             plot = plot)
  pl <- plot_vpc(vpc_db)
  return(pl)
}
