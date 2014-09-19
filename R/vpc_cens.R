#' VPC function for left- or right-censored data (e.g. BLOQ data)
#' 
#' Creates a VPC plot from observed and simulation data
#' sim, 
#' @param sim a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param obs a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param bins either "auto" or a numeric vector specifying the bin separators.  
#' @param type either "lloq" (default) or "uloq".
#' @param n_bins when using the "auto" binning method, what number of bins to aim for
#' @param auto_bin_type auto-binning type, default is "simple".
#' @param obs_dv variable in data.frame for observed dependent value. "dv" by default
#' @param sim_dv variable in data.frame for simulated dependent value. "sdv" by default
#' @param obs_idv variable in data.frame for observed independent value. "time" by default
#' @param sim_idv variable in data.frame for simulated independent value. "time" by default
#' @param obs_id variable in data.frame for observed individual. "id" by default
#' @param sim_id variable in data.frame for simulated individual. "id" by default
#' @param nonmem should variable names standard to NONMEM be used (i.e. ID, TIME, DV, PRED).  Default is "auto" for autodetect
#' @param stratify character vector of stratification variables. Only 1 or 2 stratification variables can be supplied.
#' @param ci confidence interval to plot. Default is (0.05, 0.95)
#' @param uloq Number or NULL indicating upper limit of quantification. Default is NULL.  
#' @param lloq Number or NULL indicating lower limit of quantification. Default is NULL.  
#' @param plot Boolean indacting whether to plot the ggplot2 object after creation. Default is TRUE.
#' @param xlab ylab as numeric vector of size 2
#' @param ylab ylab as numeric vector of size 2
#' @param title title
#' @param smooth "smooth" the VPC (connect bin midpoints) or show bins as rectangular boxes. Default is TRUE.
#' @param theme which theme to load from the themes object
#' @param custom_theme specify a custom ggplot2 theme
#' @param facet either "wrap", "columns", or "rows" 
#' @return a list containing calculated VPC information, and a ggplot2 object
#' @export
#' @seealso \link{vpc}
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
#' vpc_loq <- vpc_cens(sim, obs, lloq = 5)
vpc_cens <- function(sim, 
                     obs, 
                     bins = NULL, 
                     n_bins = 8,
                     type = "bloq",
                     auto_bin_type = "simple",
                     obs_dv = "dv",
                     sim_dv = "sdv",
                     obs_idv = "time",
                     sim_idv = "time",
                     obs_id = "id",
                     sim_id = "id",
                     nonmem = "auto",
                     stratify = NULL,
                     ci = c(0.05, 0.95),
                     uloq = NULL, 
                     lloq = NULL, 
                     plot = TRUE,
                     xlab = NULL, 
                     ylab = NULL,
                     title = NULL,
                     smooth = TRUE,
                     theme = "default",
                     custom_theme = NULL,
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
    obs_dv = "DV"
    obs_idv = "TIME"
    obs_id = "ID"
    obs_pred <- "PRED"
    sim_dv = "DV"
    sim_idv = "TIME"
    sim_id = "ID"
    sim_pred <- "PRED"
    if("MDV" %in% colnames(obs)) {
      obs <- obs[obs$MDV == 0,]
    }
    if("EVID" %in% colnames(obs)) {
      obs <- obs[obs$EVID == 0,]
    }
    if("MDV" %in% colnames(sim)) {
      sim <- sim[sim$MDV == 0,]
    }
    if("EVID" %in% colnames(obs)) {
      sim <- sim[sim$EVID == 0,]
    }
  }
  if (class(bins) != "numeric") {
    bins <- auto_bin(obs, auto_bin_type, n_bins, x=obs_idv)
  }  
  sim <- format_vpc_input_data(sim, sim_dv, sim_idv, sim_id, lloq, uloq, stratify, bins, FALSE, 0)
  obs <- format_vpc_input_data(obs, obs_dv, obs_idv, obs_id, lloq, uloq, stratify, bins, FALSE, 0)
  loq_perc <- function(x) { sum(x <= lloq) / length(x) } # below lloq, default   
  if (tolower(type) == "uloq") {
    loq_perc <- function(x) { sum(x >= uloq) / length(x) }
  }
  aggr_sim <- data.frame(cbind(sim %>% group_by(strat, sim, bin) %>% summarise(loq_perc(dv))))    
  colnames(aggr_sim)[grep("loq_perc", colnames(aggr_sim))] <- "ploq"
  tmp <- aggr_sim %>% group_by(strat, bin)    
  vpc_dat <- data.frame(cbind(tmp %>% summarise(quantile(ploq, ci[1])),
                              tmp %>% summarise(quantile(ploq, 0.5)),
                              tmp %>% summarise(quantile(ploq, ci[2])) ))
  vpc_dat <- vpc_dat[,-grep("(bin.|strat.)", colnames(vpc_dat))]
  colnames(vpc_dat) <- c("strat", "bin", "ploq_low", "ploq_med", "ploq_up")  
  vpc_dat$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(vpc_dat$strat)) )
  vpc_dat$bin_max <- rep(bins[2:length(bins)], length(unique(vpc_dat$strat)) )
  vpc_dat$bin_mid <- (vpc_dat$bin_min + vpc_dat$bin_max) / 2
  aggr_obs <- data.frame(cbind(obs %>% group_by(strat,bin) %>% summarise(loq_perc(dv)),
                               obs %>% group_by(strat,bin) %>% summarise(loq_perc(dv)),
                               obs %>% group_by(strat,bin) %>% summarise(loq_perc(dv)) ))
  aggr_obs <- aggr_obs[,-grep("(bin.|strat.|sim.)", colnames(aggr_obs))]
  colnames(aggr_obs) <- c("strat", "bin", "ploq_low","ploq_med","ploq_up")    
  aggr_obs$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(aggr_obs$strat)) )
  aggr_obs$bin_max <- rep(bins[2:length(bins)], length(unique(aggr_obs$strat)) )
  aggr_obs$bin_mid <- (aggr_obs$bin_min + aggr_obs$bin_max)/2 
  pl <- ggplot(vpc_dat, aes(x=bin_mid, y=dv)) + 
    geom_line(aes(y=ploq_med), linetype='dashed') + 
    geom_ribbon(aes(x=bin_mid, y=ploq_low, ymin=ploq_low, ymax=ploq_up), fill=themes[[theme]]$med_area, alpha=themes[[theme]]$med_area_alpha) +
    geom_line(data=aggr_obs, aes(x=bin_mid, y=ploq_med), linetype='solid') +
    geom_line(data=aggr_obs, aes(x=bin_mid, y=ploq_low), linetype='dotted') +
    geom_line(data=aggr_obs, aes(x=bin_mid, y=ploq_up), linetype='dotted')  
  bdat <- data.frame(cbind(x=bins, y=NA))
  pl <- pl + 
    geom_rug(data=bdat, sides = "t", aes(x = x, y=y), colour="#333333")
  if (!is.null(stratify)) {
    if(facet == "wrap") {
      pl <- pl + facet_wrap(~ strat)      
    } else {
      if(length(grep("row", facet))>0) {
        pl <- pl + facet_grid(strat ~ .)                
      } else {
        pl <- pl + facet_grid(. ~ strat)                
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
  if(!is.null(xlab)) {
    pl <- pl + xlab(xlab)
  } else {
    pl <- pl + xlab(obs_idv)
  }
  if(!is.null(ylab)) {
    pl <- pl + ylab(ylab)
  } else {
    pl <- pl + ylab(paste("Fraction", type))
  }
  if (plot) {
    print(pl)    
  }
  invisible(
    list(
      obs = obs, 
      sim = sim,
      bins = bins, 
      pl = pl
    )
  )
}