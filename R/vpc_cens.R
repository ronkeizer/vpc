#' VPC function for left- or right-censored data (e.g. BLOQ data)
#' 
#' Creates a VPC plot and/or plotting data from observed and simulation data
#' @param sim 
#' @param obs
#' @return Either the data for plotting a VPC or a ggplot2 object
#' @export
#' @examples
#' obs <- Theoph
#' colnames(obs) <- c("id", "wt", "dose", "time", "dv")
#' obs <- obs %>%   # create a dummy covariate to show stratification
#'  group_by(id) %>%  
#'  mutate(sex = round(runif(1)))
#' 
#' sim <- sim_data(obs, # the design of the dataset
#'                 model = function(x) { # the model
#'                   pk_oral_1cmt (t = x$time, dose=x$dose * x$wt, ka = x$ka, ke = x$ke, cl = x$cl * x$wt, ruv = list(additive = 0.1))
#'                 }, 
#'                 theta = c(2.774, 0.0718, .0361),                 # parameter values
#'                 omega_mat = c(0.08854,                           # specified as lower triangle by default; 
#'                               0.02421, 0.02241,                  # note: assumed that every theta has iiv, set to 0 if no iiv. 
#'                               0.008069, 0.008639, 0.02862),      
#'                 par_names = c("ka", "ke", "cl"),                 # link the parameters in the model to the thetas/omegas
#'                 n = 500)
#' 
#' vpc_loq <- vpc_cens(sim, obs, lloq = 5)
vpc_cens <- function(sim, 
                     obs, 
                     bins = NULL, 
                     n_bins = 8,
                     auto_bin_type = "simple",
                     obs.dv = "dv",
                     sim.dv =  "sdv",
                     obs.idv = "time",
                     sim.idv = "time",
                     obs.id = "id",
                     sim.id = "id",
                     nonmem = FALSE,
                     stratify = NULL,
                     pi = c(0.05, 0.95), 
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
                     type = "bloq",
                     facet = "wrap") {
  if (class(bins) != "numeric") {
    bins <- auto_bin(obs, auto_bin_type, n_bins, x=obs.idv)
  }  
  sim <- format_vpc_input_data(sim, sim.dv, sim.idv, sim.id, lloq, uloq, stratify, bins, FALSE, 0, nonmem)
  obs <- format_vpc_input_data(obs, obs.dv, obs.idv, obs.id, lloq, uloq, stratify, bins, FALSE, 0, nonmem)
  loq_perc <- function(x) { sum(x <= lloq) / length(x) } # below lloq, default   
  if (type == "uloq") {
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
    pl <- pl + xlab(obs.idv)
  }
  if(!is.null(ylab)) {
    pl <- pl + ylab(ylab)
  } else {
    pl <- pl + ylab(paste("Fraction", type))
  }
  if (plot) {
    print(pl)    
  }
  return(
    list(
      obs = obs, 
      sim = sim,
      bins = bins, 
      pl = pl
    )
  )
}