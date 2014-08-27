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
#' vpc_dat <- vpc(sim, obs, 
#'                bins = c(0, 2, 4, 6, 8, 10, 25), 
#'                stratify = "sex",
#'                ylab = "Concentration", xlab = "Time (hrs)", title="Visual predictive check")
vpc <- function(sim, obs, 
                bins = NULL, 
                n_bins = 8,
                obs.dv = "dv",
                sim.dv =  "sdv",
                obs.idv = "time",
                sim.idv = "time",
                plot.dv = TRUE,
                stratify = NULL,
                pi = c(0.05, 0.95), 
                ci = c(0.05, 0.95),
                uloq = NULL, 
                lloq = NULL, 
                plot = TRUE,
                log_y = FALSE,
                xlab = NULL, 
                ylab = NULL,
                title = NULL,
                smooth = TRUE,
                theme = "default",
                custom_theme = NULL,
                facet = "wrap",
                return_what = NULL) {
  sim <- format_vpc_input_data(sim, sim.dv, sim.idv, lloq, uloq, stratify, bins)
  obs <- format_vpc_input_data(obs, obs.dv, obs.idv, lloq, uloq, stratify, bins)
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
  pl <- pl + xlab(xlab) + ylab(ylab)
  if (log_y) {
    pl <- pl + scale_y_log10() 
  }
  if (!is.null(stratify)) {
    if(facet == "wrap") {
      pl <- pl + facet_wrap(~ strat)      
    } else {
      if(length(grep("horiz", facet))>0) {
        pl <- pl + facet_grid(. ~ strat)                
      } else {
        pl <- pl + facet_grid(strat ~ .)                
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
  if(!is.null(return_what)) {
    if(return_what == "data") {
      return(list(vpc_dat = vpc_dat, obs = aggr_obs))    
    } else {
      return(pl)
    }    
  }
}

#' VPC function for left- or right-censored data (e.g. BLOQ data)
#' 
#' Creates a VPC plot and/or plotting data from observed and simulation data
#' @param sim 
#' @param obs
#' @return Either the data for plotting a VPC or a ggplot2 object
#' @export
vpc_loq <- function(sim, 
                    obs, 
                    bins = NULL, 
                    n_bins = 8,
                    obs.dv = "dv",
                    sim.dv =  "sdv",
                    obs.idv = "time",
                    sim.idv = "time",
                    stratify = NULL,
                    pi = c(0.05, 0.95), 
                    ci = c(0.05, 0.95),
                    uloq = NULL, 
                    lloq = NULL, 
                    plot = TRUE,
                    log_y = FALSE,
                    xlab = NULL, 
                    ylab = NULL,
                    title = NULL,
                    smooth = TRUE,
                    theme = "default",
                    custom_theme = NULL,
                    return_what = NULL,
                    type = "bloq") {
  sim <- format_vpc_input_data(sim, sim.dv, sim.idv, lloq, uloq, stratify, bins)
  obs <- format_vpc_input_data(obs, obs.dv, obs.idv, lloq, uloq, stratify, bins)
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
  if (log_y) {
    pl <- pl + scale_y_log10() 
  }
  if (!is.null(stratify)) {
    pl <- pl + facet_wrap(~ strat)
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
  if(!is.null(return_what)) {
    if(return_what == "data") {
      return(list(vpc_dat = vpc_dat, obs = aggr_obs))    
    } else {
      return(pl)
    }    
  }
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
#' vpc_dat <- vpc(sim, obs, 
#'                bins = c(0, 2, 4, 6, 8, 10, 25), 
#'                stratify = "sex",
#'                ylab = "Concentration", xlab = "Time (hrs)", title="Visual predictive check")
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
      n_sim = 500, 
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
  tmp <- merge(sim_des, param,
               by.x="join", by.y="join")
  tmp$sdv <- add_noise(model(tmp), ruv = error)  
  colnames(tmp) <- gsub("\\.x", "", colnames(tmp))
  dplyr::arrange(tmp, sim, id, time)
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

themes <- list(
  "default" = list(
    pi_area = "#3388cc", pi_area_alpha = 0.2,  
    med_area = "#3388cc", med_area_alpha = 0.4  
  )
)

format_vpc_input_data <- function(dat, dv, idv, lloq, uloq, strat, bins) {
  if(length(match(dv, colnames(dat))) > 0) {
    colnames(dat)[match(dv, colnames(dat))] <- "dv"    
  }
  if(is.na(match("dv", colnames(dat)))[1]) {
    cat ("No dv column found in data, stopping!")
    stop()
  }  
  if(length(match(idv, colnames(dat))) > 0) {
    colnames(dat)[match(idv, colnames(dat))] <- "idv"    
  }
  if(is.na(match("idv", colnames(dat)))[1]) {
    cat ("No idv column found in data, stopping!")
    stop()
  }  
  if (!is.null(uloq)) { dat$dv[dat$dv > uloq] <- uloq }
  if (!is.null(lloq)) { dat$dv[dat$dv < lloq] <- lloq }
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
  dat <- bin_data(dat, bins, "idv")  
  return(dat)
}

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


