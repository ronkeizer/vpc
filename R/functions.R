#' VPC function
#' 
#' Creates a VPC plot and/or plotting data from observed and simulation data
#' @param sim 
#' @param obs
#' @return Either the data for plotting a VPC or a ggplot2 object
#' @export
#' @seealso \link{sim_data}
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
#' vpc_dat <- vpc(sim, obs, stratify = c("sex"))
vpc <- function(sim, obs, 
                bins = "auto",
                n_bins = 8,
                auto_bin_type = "simple",
                obs.dv = "dv",
                sim.dv =  "sdv",
                obs.idv = "time",
                sim.idv = "time",
                obs.id = "id",
                sim.id = "id",
                obs.pred = "pred",
                sim.pred = "pred",
                nonmem = FALSE,
                plot.dv = FALSE,
                stratify = NULL,
                pred_corr = FALSE,
                pred_corr_lower_bnd = 0,
                pi = c(0.05, 0.95), 
                ci = c(0.05, 0.95),
                uloq = NULL, 
                lloq = NULL, 
                plot = TRUE,
                log_y = FALSE,
                log_y_min = 1e-3,
                xlab = NULL, 
                ylab = NULL,
                title = NULL,
                smooth = TRUE,
                theme = "default",
                custom_theme = NULL,
                facet = "wrap") {
  if (class(bins) != "numeric") {
    bins <- auto_bin(obs, auto_bin_type, n_bins, x=obs.idv)
  }
  if (pred_corr) {
    if (nonmem) {
      obs.pred <- "PRED"
      sim.pred <- "PRED"
    }
    if (!obs.pred %in% names(obs)) {
      cat("Warning: Prediction-correction: specified pred-variable not found in observations, trying to get from simulated dataset...")      
      if (!sim.pred %in% names(sim)) {
        cat("Warning: Prediction-correction: specified pred-variable not found in simulated dataset, not able to perform pred-correction!")
        return()
      } else {
        obs[[obs.pred]] <- sim[1:length(obs[,1]), sim.pred]
        cat ("OK")
      }
    } else {
      if (!sim.pred %in% names(sim)) {
        cat("Warning: Prediction-correction: specified pred-variable not found in simulated dataset, not able to perform pred-correction!")
        return()
      }      
    }
    obs$pred <- obs[[obs.pred]]
    sim$pred <- sim[[sim.pred]]
  }
  sim <- format_vpc_input_data(sim, sim.dv, sim.idv, sim.id, lloq, uloq, stratify, bins, log_y, log_y_min, nonmem)
  obs <- format_vpc_input_data(obs, obs.dv, obs.idv, obs.id, lloq, uloq, stratify, bins, log_y, log_y_min, nonmem)
  if (pred_corr) {
    obs <- obs %>% group_by(strat, bin) %>% mutate(pred_bin = mean(pred))
    obs[obs$pred != 0,]$dv <- pred_corr_lower_bnd + (obs[obs$pred != 0,]$dv - pred_corr_lower_bnd) * (obs[obs$pred != 0,]$pred_bin - pred_corr_lower_bnd) / (obs[obs$pred != 0,]$pred - pred_corr_lower_bnd)
    sim <- sim %>% group_by(strat, sim, bin) %>% mutate(pred_bin = mean(pred))
    sim[sim$pred != 0,]$dv <- pred_corr_lower_bnd + (sim[sim$pred != 0,]$dv - pred_corr_lower_bnd) * (sim[sim$pred != 0,]$pred_bin - pred_corr_lower_bnd) / (sim[sim$pred != 0,]$pred - pred_corr_lower_bnd)
  }
  aggr_sim <- data.frame(cbind(sim %>% group_by(strat, sim, bin) %>% summarise(quantile(dv, pi[1])),
                               sim %>% group_by(strat, sim, bin) %>% summarise(quantile(dv, 0.5 )),
                               sim %>% group_by(strat, sim, bin) %>% summarise(quantile(dv, pi[2]))))
  aggr_sim <- aggr_sim[,-grep("(bin.|strat.|sim.)", colnames(aggr_sim))]  
  colnames(aggr_sim)[grep("quantile", colnames(aggr_sim))] <- c("q5", "q50", "q95")
  tmp <- aggr_sim %>% group_by(strat, bin)
  vpc_dat <- data.frame(cbind(tmp %>% summarise(quantile(q5, ci[1])),
                              tmp %>% summarise(quantile(q5, 0.5)),
                              tmp %>% summarise(quantile(q5, ci[2])),
                              tmp %>% summarise(quantile(q50, ci[1])),
                              tmp %>% summarise(quantile(q50, 0.5)),
                              tmp %>% summarise(quantile(q50, ci[2])),
                              tmp %>% summarise(quantile(q95, ci[1])),
                              tmp %>% summarise(quantile(q95, 0.5)),
                              tmp %>% summarise(quantile(q95, ci[2])) ))
  vpc_dat <- vpc_dat[,-grep("(bin.|strat.)", colnames(vpc_dat))]
  colnames(vpc_dat) <- c("strat", "bin", 
                         "q5.5","q5.50","q5.95", 
                         "q50.5","q50.50","q50.95",
                         "q95.5","q95.50","q95.95")
  vpc_dat$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(vpc_dat$strat)) )
  vpc_dat$bin_max <- rep(bins[2:length(bins)], length(unique(vpc_dat$strat)) )
  vpc_dat$bin_mid <- (vpc_dat$bin_min + vpc_dat$bin_max) / 2
  aggr_obs <- data.frame(cbind(obs %>% group_by(strat,bin) %>% summarise(quantile(dv, 0.05)),
                               obs %>% group_by(strat,bin) %>% summarise(quantile(dv, 0.5 )),
                               obs %>% group_by(strat,bin) %>% summarise(quantile(dv, 0.95)) ))
  aggr_obs <- aggr_obs[,-grep("(bin.|strat.|sim.)", colnames(aggr_obs))]
  colnames(aggr_obs) <- c("strat", "bin", "obs5","obs50","obs95")
  aggr_obs$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(aggr_obs$strat)) )
  aggr_obs$bin_max <- rep(bins[2:length(bins)], length(unique(aggr_obs$strat)) )
  aggr_obs$bin_mid <- (aggr_obs$bin_min + aggr_obs$bin_max)/2 
  if(is.null(xlab)) {
    xlab <- obs.idv
  }
  if(is.null(ylab)) {
    ylab <- obs.dv
  }
  pl <- ggplot(vpc_dat, aes(x=bin_mid, y=dv)) + 
    geom_line(aes(y=q50.50), linetype='dashed') 
  if (smooth) {
    pl <- pl + 
      geom_ribbon(aes(x=bin_mid, y=q50.5, ymin=q50.5, ymax=q50.95), alpha=themes[[theme]]$med_area_alpha, fill = themes[[theme]]$med_area) +
      geom_ribbon(aes(x=bin_mid, y=q5.5, ymin=q5.5, ymax=q5.95), alpha=themes[[theme]]$pi_area_alpha, fill = themes[[theme]]$pi_area) +
      geom_ribbon(aes(x=bin_mid, y=q95.5, ymin=q95.5, ymax=q95.95), alpha=themes[[theme]]$pi_area_alpha, fill = themes[[theme]]$pi_area) 
  } else {
    pl <- pl + 
      geom_rect(aes(xmin=bin_min, xmax=bin_max, y=q50.5, ymin=q50.5, ymax=q50.95), alpha=themes[[theme]]$med_area_alpha, fill = themes[[theme]]$med_area) +
      geom_rect(aes(xmin=bin_min, xmax=bin_max, y=q5.5, ymin=q5.5, ymax=q5.95), alpha=themes[[theme]]$pi_area_alpha, fill = themes[[theme]]$pi_area) +
      geom_rect(aes(xmin=bin_min, xmax=bin_max, y=q95.5, ymin=q95.5, ymax=q95.95), alpha=themes[[theme]]$pi_area_alpha, fill = themes[[theme]]$pi_area)     
  }
  pl <- pl +
    geom_line(data=aggr_obs, aes(x=bin_mid, y=obs50), linetype='solid') +
    geom_line(data=aggr_obs, aes(x=bin_mid, y=obs5), linetype='dotted') +
    geom_line(data=aggr_obs, aes(x=bin_mid, y=obs95), linetype='dotted') 
  if (plot.dv) {
    pl <- pl + geom_point(data=obs, aes(x=idv, y = dv))
  }
  bdat <- data.frame(cbind(x=bins, y=NA))
  pl <- pl + 
    geom_rug(data=bdat, sides = "t", aes(x = x, y=y), colour="#333333")
  pl <- pl + xlab(xlab) + ylab(ylab)
  if (log_y) {
    pl <- pl + scale_y_log10() 
  }
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
  if (plot) {
    print(pl)    
  }
  return(
    list(
      obs = tbl_df(obs), 
      sim = tbl_df(sim),
      bins = bins, 
      pl = pl
    )
  )
}

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

#' VPC function for survival-type data 
#' 
#' Creates a VPC plot and/or plotting data from observed and simulation data
#' @param sim 
#' @param obs
#' @return A VPC object 
#' @export
#' @examples
#'\dontrun{
#' ## read obs and sim data from NONMEM tables
#' obs <- tbl_df(read.table.nm("nm/sdtab51"))  
#' # sim <- tbl_df(read.table.nm("nm/simtab51"))
#' # saveRDS(sim, file="nm/simtab51.gz", compress = TRUE)
#' sim <- readRDS(file="nm/simtab51.gz")
#' 
#' ## create the VPC, stratified by dose
#' vpc1 <- vpc_tte(sim, obs, 
#'                 n_bins = 15,
#'                 stratify = "dose",
#'                 facet = "wrap",
#'                 nonmem = TRUE,  # use NONMEM common data labels
#'                 smooth = TRUE) + xlab("Test")
#'}
vpc_tte <- function(sim, obs, 
                    bins = "auto",
                    n_bins = 32,
                    auto_bin_type = "simple",
                    obs.dv = "dv",
                    sim.dv =  "sdv",
                    obs.idv = "time",
                    sim.idv = "time",
                    obs.id = "id",
                    sim.id = "id",
                    pi.med = FALSE, 
                    nonmem = FALSE,
                    stratify = NULL,
                    ci = c(0.05, 0.95),
                    plot = TRUE,
                    xlab = NULL, 
                    ylab = NULL,
                    title = NULL,
                    smooth = TRUE,
                    theme = "default",
                    custom_theme = NULL,
                    facet = "wrap") {
  if(nonmem) {
    colnames(obs) <- tolower(colnames(obs))
    colnames(sim) <- tolower(colnames(sim))
    sim.dv <- "dv"
  }

  obs <- add_stratification(obs, stratify)
  sim <- add_stratification(sim, stratify)
  
  # compute KM curve for observations
  obs_km <- compute_kaplan(obs, strat = "strat")
  
  # compute KM curve CI for simulations
  # first get the number of simulations (req'd if sim# is not specified)
  sim_id <- unique(sim$id)
  sim$id_shift <- c(sim$id[2:length(sim$id)], 0) 
  len <- (1:length(sim$id))[sim$id == tail(sim_id,1) & sim$id_shift == sim_id[1]][1]
  n_sims <- length(sim$id) / len
  if (n_sims != round(n_sims)) {
    cat ("Could not determine number of simulations performed, please specify manually!")
    return()
  }
  sim$sim <- rep(1:n_sims, each=length(sim[,1])/n_sims) 
  
  all <- c()
  for (i in 1:n_sims) {
    tmp <- sim %>%
      filter(sim == i) %>%
      group_by(id) %>% 
      mutate (event = max(dv))
    cens <- tmp %>% filter(event == 0) %>% filter(time == max(time))
    evnt <- tmp %>% filter(dv == 1) %>% filter(time == min(time))
    tmp <- compute_kaplan(rbind(cens, evnt), strat = "strat")
    all <- rbind(all, cbind(i, tmp))
  }
  bins <- seq(from = 0, max(all$time)*1.04, by = diff(range(all$time))/(n_bins), )
  all$bin <- cut(all$time, breaks = bins, labels = FALSE, right = TRUE)
  all$bin_min <- bins[all$bin] 
  all$bin_max <- bins[all$bin+1] 
  all$bin_mid <- (all$bin_min + all$bin_max)/2 
  
  sim_km <- all %>% 
    group_by (bin, strat) %>% 
    summarise (bin_mid = head(bin_mid,1), bin_min = head(bin_min,1), bin_max = head(bin_max,1), 
               qmin = quantile(surv, 0.05), qmax = quantile(surv, 0.95), qmed = median(surv),
               step = 0)
  if (smooth) {
    geom_line_custom <- geom_line
  } else {
    geom_line_custom <- geom_step
  }
  pl <- ggplot(sim_km, aes(x=bin_mid, y=qmed, group=i)) 
  if (smooth) {
    pl <- pl + geom_ribbon(aes(min = qmin, max=qmax, y=qmed), fill = themes[[theme]]$med_area, alpha=0.5)
  } else {
    pl <- pl + geom_rect(aes(xmin=bin_min, xmax=bin_max, ymin=qmin, ymax=qmax), alpha=themes[[theme]]$med_area_alpha, fill = themes[[theme]]$med_area)   
  }
  if (pi.med) {
    pl <- pl + geom_line_custom(linetype="dashed")
  }
  pl <- pl + geom_step(data = obs_km, aes(x=time, y=surv)) 
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
  } 
  if(!is.null(ylab)) {
    pl <- pl + ylab(ylab)
  } 
  if(plot) {
    print(pl)
  }
  return(
    list(
      obs = obs_km, 
      sim = sim_km,
      bins = bins, 
      pl = pl
    )
  )
}

compute_kaplan <- function(dat, strat = NULL) {
  if (!is.null(strat)) {
    strats <- unique(dat[[strat]])
    tmp <- c()
    for (i in seq(strats)) {
      km_fit <- survfit(Surv(time = time, dv != 0) ~ 1, data = dat[dat[[strat]] == strats[i],])
      tmp <- rbind(tmp, data.frame(time = km_fit$time, surv = km_fit$surv, strat = strats[i]))          
    }
    return(tmp)      
  } else {
    km_fit <- survfit(Surv(time = time, dv != 0) ~ 1, data = dat)
    data.frame(time = km_fit$time, surv = km_fit$surv)           
  }
}

add_step <- function(dat = ., vars) {
    dat$step <- 0
    tmp <- dat[-1,]
    tmp$step <- 1
    tmp[,vars] <- dat[-length(dat[,1]), vars]    
    newdat <- data.frame(rbind(dat, tmp))
    newdat %>% arrange(bin, -step)    
}

add_stratification <- function (dat, strat) {
  if(is.null(strat)) {
    dat$strat <- 1
  } else {
    dat$strat <- ""
    for(i in seq(strat)) {
      if(i > 1) { 
        dat$strat <- paste0(dat$strat, ", ")
      }
      dat$strat <- paste0(dat$strat, strat[i], "=", dat[,strat[i]])
    }
  }  
  dat$strat <- as.factor(dat$strat)
  return(dat)
}

format_vpc_input_data <- function(dat, dv, idv, id, lloq, uloq, strat, bins, log_y, log_y_min, nonmem) {
  if (nonmem) {
    dv = "DV"
    idv = "TIME"
    id = "ID"
    if("MDV" %in% colnames(dat)) {
      dat <- dat[dat$MDV == 0,]
    }
    if("EVID" %in% colnames(dat)) {
      dat <- dat[dat$EVID == 0,]
    }
  }
  if(id %in% colnames(dat)) {
    if ("id" %in% colnames(dat) &! id == "id") {
      colnames(dat)[match("id", colnames(dat))] <- "id.old"
    }
    colnames(dat)[match(id, colnames(dat))] <- "id"    
  }
  if(is.na(match("id", colnames(dat)))[1]) {
    cat ("No id column found in data, stopping!")
    stop()
  }  
  if(dv %in% colnames(dat)) {
    if ("dv" %in% colnames(dat) &! dv == "dv") {
      colnames(dat)[match("dv", colnames(dat))] <- "dv.old"
    }
    colnames(dat)[match(dv, colnames(dat))] <- "dv"    
  }
  if(is.na(match("dv", colnames(dat)))[1]) {
    cat ("No dv column found in data, stopping!")
    stop()
  }  
  if(idv %in% colnames(dat)) {
    if ("idv" %in% colnames(dat) &! idv == "idv") {
      colnames(dat)[match("idv", colnames(dat))] <- "idv.old"
    }
    colnames(dat)[match(idv, colnames(dat))] <- "idv"    
  }
  if(is.na(match("idv", colnames(dat)))[1]) {
    cat ("No idv column found in data, stopping!")
    stop()
  }  
  if (!is.null(uloq)) { dat$dv[dat$dv > uloq] <- uloq }
  if (!is.null(lloq)) { dat$dv[dat$dv < lloq] <- lloq }
  if (log_y) {
    dat$dv[dat$dv < log_y_min] <- log_y_min 
  }
  dat <- add_stratification(dat, strat)
  dat <- bin_data(dat, bins, "idv")  
  return(dat)
}

#' Simulate data based on a model and parameter distributions
#' 
#' @param design
#' @param model A function with the first argument the simulation design, i.e. a dataset with the columns ... The second argument to this function is a dataset with parameters for every individual. This can be supplied by the user, or generated by this sim_data if theta and omega_mat are supplied.
#' @param theta
#' @param omega_mat
#' @param par_names A vector describing 
#' @return A vector of simulated dependent variables (for us in the VPC plotting function)
#' @export
#' @family aggregate functions
#' @seealso \code{\link{vpc}}
#' @details
#' This function generates the simulated dependent values for use in the VPC plotting function.
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
#' vpc_dat <- vpc(sim, obs, stratify = c("sex"))
sim_data <- function (design = cbind(id = c(1,1,1), idv = c(0,1,2)), 
                      model = function(x) { return(x$alpha + x$beta) }, 
                      theta, 
                      omega_mat, 
                      par_names, par_values = NULL,
                      draw_iiv = "mvrnorm",
                      error = list(proportional = 0, additive = 0, exponential = 0),
                      n=100) {
  if (is.null(par_values)) {
    param <- draw_params_mvr( # draw parameter values. can also be just population values, or specified manually ()
      ids = unique(as.numeric(as.character(obs$id))), 
      n_sim = n, 
      theta, 
      omega_mat = triangle_to_full(omega_mat),
      par_names = par_names)      
  } else {
    param <- par_values
  }
  sim_des <- do.call("rbind", replicate(n, design, simplify = FALSE))
  sim_des$sim  <- rep(1:n, each=length(design[,1]))
  sim_des$join <- paste(sim_des$sim, sim_des$id, sep="_")
  param$join   <- paste(param$sim, param$id, sep="_")
  tmp <- tbl_df(merge(sim_des, param,
                      by.x="join", by.y="join"))
  tmp_pred <- cbind(design, matrix(rep(theta, each=length(design[,1])), ncol=length(theta)))
  colnames(tmp_pred)[length(tmp_pred)-length(par_names)+1:3] <- par_names
  tmp$sdv <- add_noise(model(tmp), ruv = error)  
  tmp$pred <- model(tmp_pred)
  colnames(tmp) <- gsub("\\.x", "", colnames(tmp))
  dplyr::arrange(tmp, sim, id, time)
}

sim_data_tte <- function (fit, t_cens = NULL, n = 100) {
  fit$coefficients <- as.list(fit$coefficients)
  dat <- data.frame(model.matrix(fit))
  for (i in seq(fit$coefficients)) { fit$coefficients[[i]] <- as.numeric (fit$coefficients[[i]]) }  
  fact <- as.matrix(attr(fit$terms, "factors"))
  parm <- t(fact) %*% as.numeric(fit$coefficients)
  tmp.single <- data.frame (
    par = exp(as.numeric(fit$coefficients[1]) + as.matrix(dat[,rownames(parm)]) %*% parm),
    dv = 1
  )
  tmp <- do.call("rbind", replicate(n, tmp.single, simplify = FALSE))
  tmp$sim  <- rep(1:n, each=length(tmp.single[,1]))
  if (!fit$dist %in% c("exponential", "weibull")) {
    cat (paste("Simulation of ", fit$dist, "distribution not yet implemented, sorry."))
    return()
  }
  if (fit$dist == "exponential") {
    tmp$t = rweibull(length(dat[,1]) * n, shape = 1, scale = tmp$par)
    # or using: tmp$t = rexp(length(design$id), 1/tmp$par) 
  } 
  if (fit$dist == "weibull") {
    # annoyinly, the survreg and rweibull mix up the shape/scale parameter names and also take the inverse!!!
    tmp$t = rweibull(length(dat[,1]) * n, shape = 1/fit$scale, scale = tmp$par)
  }
  if (sum(tmp$t > t_cens) > 0) {  
    tmp[tmp$t > t_cens,]$t <- t_cens
  }
  out <- c()
  for (i in 1:n) {
    km_fit <- compute_kaplan(tmp[tmp$sim == i,])
    idx_new <- idx + length(unique(tmp[tmp$sim == i,]$t))-1
    out <- rbind(out, cbind(i, km_fit$time, km_fit$surv))     
  }
  colnames(out) <- c("sim", "time", "dv")
  tbl_df(data.frame(out))
}

triangle_to_full <- function (vect) {
  for (i in 1:100) { # find the size of the matrix
    if (length(vect) == add_recurs(0,0,i)) {
      nr = i    
    }
  }
  k_given_i_j <- function(x , y ) ifelse( y<x, x*(x-1)/2 + y, y*(y-1)/2 + x )
  k_mat <- function(p) outer( 1:p, 1:p, k_given_i_j )
  return (matrix(vect[ k_mat( nr ) ] , nr = nr ))
}

add_recurs <- function(x, n, max) {
  x <- x + n
  n <- n + 1
  if (n <= max) {
    x <- add_recurs(x, n, max)
  }
  x
}

find_nadirs <- function (x, thresh = 0) {
  pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) > 0) + 2
  if (!missing(thresh)) {
    pks[x[pks - 1] - x[pks] > thresh]
  }
  else pks
}

auto_bin <- function (dat, type="simple", n_bins = 8, x="time") {
  if (type == "simple") {
    bws <- diff(range(dat[[x]])) * seq(from=0.01, to = 1, by=0.01)
    for (i in seq(bws)) {
      d <- density(dat[[x]], bw=bws[i])
      bins <- c(0, d$x[find_nadirs(d$y)], max(dat[[x]])*1.01)    
      if (length(bins) <= (n_bins-1)) {
        return(bins)
      }
    }      
  } else {
    return("Not implemented yet!")
  }
}

draw_params_mvr <- function(ids, n_sim, theta, omega_mat, par_names = NULL) {
  n_ids <- length(ids)
  if (!is.null(par_names)) {
    par <- data.frame(cbind(sim = rep(1:n_sim, each=n_ids), 
                            id = rep(ids, n_sim),
                            rep(theta, each=n_sim*n_ids) * exp (mvrnorm(n=n_sim*n_ids, c(0,0,0), omega_mat))))
    colnames(par) <- c("sim", "id", par_names)  
    return(par)    
  } else {
    cat("Parameter names have to be supplied!")
  }
}

#' Simulate PK data from a 1-compartment oral model
#' 
#' @param t Time after dose
#' @param tau Dosing interval
#' @param dose Dose
#' @param ka Absorption rate
#' @param ke Elimination rate
#' @param ruv Residual variability
#' @param par_names A vector describing 
#' @return A vector of predicted values, with or without added residual variability
#' @export
pk_oral_1cmt <- function (t, tau = 24, dose=120, ka = 1, ke = 1, cl = 10, ruv = NULL) {
  v = cl / ke
  tmp <- (dose/v) * (ka/(ka-ke)) * (exp(-ke*t) - exp(-ka*(t)))
  if(!is.null(ruv)) {
    tmp <- add_noise (tmp, ruv)
  }
  tmp
}

pk_iv_1cmt <- function (t, t_inf = 1, tau = 24, dose=120, CL = 0.345, Vc = 1.75, ruv = NULL) {
  k <- CL / Vc
  tmp <- c()
  tmp <- c(tmp, (dose / (CL * t_inf)) * (1-exp(-k*t[t < t_inf])) )
  tmp <- c(tmp, (dose / (CL * t_inf)) * (1-exp(-k*t_inf)) * exp(-k*(t[t >= t_inf] - t_inf)) )  
  if(!is.null(ruv)) {
    tmp <- add_noise (tmp, ruv)
  }
  tmp
}

#' @export
add_noise <- function(x, ruv = list(proportional = 0, additive = 0, exponential = 0)) {
  if (is.null(ruv$proportional)) { ruv$proportional <- 0 }
  if (is.null(ruv$additive)) { ruv$additive <- 0 }
  if (is.null(ruv$exponential)) { ruv$exponential <- 0 }
  x * (1 + rnorm(length(x), 0, ruv$proportional)) +  rnorm(length(x), 0, ruv$additive) * exp(rnorm(length(x), 0, ruv$exponential)) 
} 

#' @export
bin_data <- function(x, bins = c(0, 3, 5, 7), idv = "time") {
  x$bin <- cut(x[[idv]], bins, labels = FALSE, right=FALSE)
  return(x)
}

as.num <- function(x) { as.numeric(as.character(x)) }

themes <- list(
  "default" = list(
    pi_area = "#3388cc", pi_area_alpha = 0.2,  
    med_area = "#3388cc", med_area_alpha = 0.4  
  )
)

theme_plain <-  function () {
  theme(
    text = element_text(family="mono"),
    plot.title = element_text(family="sans", size = 16, vjust = 1.5),
    axis.title.x = element_text(family="sans",vjust=-0.25),
    axis.title.y = element_text(family="sans"),
    legend.background = element_rect(fill = "white"),
    legend.position = c(0.14, 0.80),
    panel.grid.major = element_line(colour = "#e5e5e5"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#efefef", colour = NA),
    strip.background = element_rect(fill = "#444444", colour = NA),
    strip.text = element_text(face="bold", colour = "white")
  )    
}

read.table.nm <- function(file, perl = TRUE) {
  if (perl) {
    cmd <- paste0("perl -e 'open (IN, \'<", file, "\'); my $i = 0; my $cols = 0; while (my $line = <IN>) { if ($line =~ m/[a-df-z]/i) { unless($line =~ m/^TABLE NO/ || $cols == 1) { print $line; $cols = 1; } } else { print $line } } ; close(IN);'")
    cmd <- "ls"
    tab <- read.table (pipe(cmd), header=T);    
  } else { # extremely slow....
    tab <- readLines (file)
    skip <- grep('/[a-z]/i', tab)[1] - 1
    del_rows <- c(grep("TABLE", tab)[-1] , grep ("TABLE", tab)[-1] + 1)
    tab <- tab[-del_rows]
    read.table(textConnection(tab), skip=1, header=T) 
  }    
}

