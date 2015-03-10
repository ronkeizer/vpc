#' VPC function for categorical
#' 
#' Creates a VPC plot from observed and simulation data
#' sim, 
#' @param sim a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param obs a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param bins either "density", "time", or "data", or a numeric vector specifying the bin separators.  
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
                     psn_folder = NULL,
                     bins = "jenks",
                     n_bins = "auto",
                     obs_cols = NULL,
                     sim_cols = NULL,
                     software = "auto",
                     show = NULL,                
                     legend_pos = NULL,
                     ci = c(0.05, 0.95),
                     uloq = NULL, 
                     lloq = NULL, 
                     xlab = NULL, 
                     plot_sim_med = FALSE,
                     ylab = NULL,
                     title = NULL,
                     smooth = TRUE,
                     vpc_theme = NULL,
                     ggplot_theme = NULL,
                     facet = "wrap",
                     plot = TRUE,
                     vpcdb = FALSE,
                     verbose = FALSE) {

  if(is.null(obs) & is.null(sim)) {
    stop("At least a simulation or an observation dataset are required to create a plot!")
  }
  if(!is.null(psn_folder)) {
    if(!is.null(obs)) {
      obs <- read_table_nm(paste0(psn_folder, "/m1/", dir(paste0(psn_folder, "/m1"), pattern="original.npctab")[1]))
    }
    if(!is.null(sim)) {
      sim <- read_table_nm(paste0(psn_folder, "/m1/", dir(paste0(psn_folder, "/m1"), pattern="simulation.1.npctab")[1]))
    }
    software = "nonmem"
  }
  if (!is.null(obs)) {
    software_type <- guess_software(software, obs)
  } else {
    software_type <- guess_software(software, sim)
  }

  ## define what to show in plot
  show <- replace_list_elements(show_default, show)
  
  ## define column names
  cols <- define_data_columns(sim, obs, sim_cols, obs_cols, software_type)
  if(!is.null(obs)) {
    old_class <- class(obs)
    class(obs) <- c(software_type, old_class)
  }
  if(!is.null(sim)) {
    old_class <- class(sim)
    class(sim) <- c(software_type, old_class)
  }
  
  ## parse data into specific format
  if(!is.null(obs)) {
    obs <- filter_dv(obs, verbose)
    obs <- format_vpc_input_data(obs, cols$obs, lloq, uloq, strat = NULL, bins, FALSE, 0, "observed", verbose)
  }
  if(!is.null(sim)) {  
    sim <- filter_dv(sim, verbose)
    sim <- format_vpc_input_data(sim, cols$sim, lloq, uloq, strat = NULL, bins, FALSE, 0, "simulated", verbose)
    sim$sim <- add_sim_index_number(sim, id = "id")    
  }
  
  if (class(bins) != "numeric") {
    if(!is.null(obs)) {
      bins <- auto_bin(obs, bins, n_bins)  
    } else { # get from sim
      bins <- auto_bin(sim, bins, n_bins)            
    }
    if (is.null(bins)) {
      msg("Automatic binning unsuccessful, try increasing the number of bins, or specify vector of bin separators manually.", verbose)
    }
  }
  bins <- unique(bins)
  if(!is.null(obs)) {
    obs <- bin_data(obs, bins, "idv") 
  }
  if(!is.null(sim)) {
    sim <- bin_data(sim, bins, "idv")  
  }
  
  ## parsing 
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
    colnames(vpc_dat) <- c("strat", "bin", "q50.low","q50.med","q50.up", "bin_mid")  
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
    colnames(aggr_obs)[4] <- "obs50"
  } else {
    aggr_obs <- NULL
  }
  if(is.null(vpc_theme) || (class(vpc_theme) != "vpc_theme")) {
    vpc_theme <- create_vpc_theme()
  }
  
  ## plotting starts here
  show$median_ci = FALSE
  show$obs_dv = FALSE
  show$obs_ci = FALSE
  show$sim_median = TRUE
  show$sim_median_ci = TRUE
  show$pi_as_area = FALSE
  show$pi_ci = FALSE
  show$pi = FALSE
  vpc_db <- list(sim = sim,
                 vpc_dat = vpc_dat,
                 vpc_theme = vpc_theme,
                 show = show,
                 smooth = smooth,
                 stratify = "strat",
                 stratify_original = "strat",
                 stratify_color = NULL,
                 aggr_obs = aggr_obs,
                 obs = obs,
                 bins = bins,
                 xlab = xlab,
                 ylab = ylab,
                 log_y = FALSE,
                 facet = facet,
                 title = title,
                 theme = theme,
                 ggplot_theme = ggplot_theme,
                 plot = plot)
  if(vpcdb) return(vpc_db)
  pl <- plot_vpc(vpc_db)
  pl <- pl + ylim(c(0,1))
  if (plot) {
    print(pl)    
  }
}