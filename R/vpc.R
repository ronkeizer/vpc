#########################################
## Functions for VPC (not using Xpose) ##
#########################################
require("dplyr")
require("ggplot2")
require("MASS")

sim_data <- function (design = cbind(id = c(1,1,1), idv = c(0,1,2)), 
                      model, theta, omega_mat, par_names,
                      n=100) {
  param = draw_params_mvr( # draw parameter values. can also be just population values
    n_ids = length(unique(obs$id)), 
    n_sim = 500, 
    theta, 
    omega_mat = triangle_to_full(omega_mat),
    par_names = par_names)  
  sim_des <- do.call("rbind", replicate(n, design, simplify = FALSE))
  sim_des$sdv <- model(sim_des, param)  
  sim_des$sim <- rep(1:n, each = length(design[,1]))
  sim_des
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

draw_params_mvr <- function(n_ids, n_sim, theta, omega_mat, par_names = NULL) {
  if (!is.null(par_names)) {
    par <- data.frame(
      cbind(
        rep(theta, each=n_sim*n_ids) * exp (mvrnorm(n=n_sim*n_ids, c(0,0,0), omega_mat)),
        ruv = 0.618))
    colnames(par) <- par_names  
    return(par)    
  } else {
    cat("Parameter names have to be supplied!")
  }
}

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

add_noise <- function(x, ruv = list(proportional = 0, additive = 0, exponential = 0)) {
  if (is.null(ruv$proportional)) { ruv$proportional <- 0 }
  if (is.null(ruv$additive)) { ruv$additive <- 0 }
  if (is.null(ruv$exponential)) { ruv$exponential <- 0 }
  x * (1 + rnorm(length(x), 0, ruv$proportional)) +  rnorm(length(x), 0, ruv$additive) * exp(rnorm(length(x), 0, ruv$exponential)) 
} 

bin_data <- function(x, bins = c(0, 3, 5, 7), idv = "time") {
  x$bin <- cut(x[[idv]], bins, labels = FALSE, right=FALSE)
  return(x)
}

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
    dat$strat <- dat[[strat]]
  }
  dat <- bin_data(dat, bins, "idv")  
  return(dat)
}

vpc <- function(sim, obs, 
                bins = NULL, 
                n_bins = 8,
                dv = "dv", 
                idv = "time",
                strat = NULL,
                pi = c(0.05, 0.95), 
                ci = c(0.05, 0.95),
                uloq = NULL, 
                lloq = NULL, 
                plot = TRUE,
                log_y = FALSE,
                xlab = NULL, 
                ylab = NULL,
                theme = "default",
                custom_theme = NULL) {
  sim <- format_vpc_input_data(sim, "sdv", idv, lloq, uloq, strat, bins)
  obs <- format_vpc_input_data(obs, dv, idv, lloq, uloq, strat, bins)
  if (is.null(strat)) { strat <- "strat" }
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
                              tmp %>% summarise(quantile(q95, ci[2]))))
  vpc_dat <- vpc_dat[,-grep("(bin.|strat.)", colnames(vpc_dat))]
  colnames(vpc_dat) <- c("strat", "bin", "q5.5","q5.50","q5.95", "q50.5","q50.50","q50.95","q95.5","q95.50","q95.95")
  aggr_obs <- data.frame(cbind(obs %>% group_by(strat,bin) %>% summarise(quantile(dv, 0.05)),
                               obs %>% group_by(strat,bin) %>% summarise(quantile(dv, 0.5 )),
                               obs %>% group_by(strat,bin) %>% summarise(quantile(dv, 0.95))))
  aggr_obs <- aggr_obs[,-grep("(bin.|strat.|sim.)", colnames(aggr_obs))]
  colnames(aggr_obs) <- c("strat", "bin", "obs5","obs50","obs95")
  if(is.null(xlab)) {
    xlab <- idv
  }
  if(is.null(ylab)) {
    ylab <- dv
  }
  themes <- list(
    "default" = list(
      pi_area = "#3388cc", pi_area_alpha = 0.2,  
      med_area = "#3388cc", med_area_alpha = 0.4  
    )
  )
  pl <- ggplot(sim, aes(x=bin, y=dv)) + 
    geom_line(aes(x=bin, y=q50.50), data=vpc_dat, linetype='dashed') + 
    geom_ribbon(data=vpc_dat, aes(x=bin, y=q50.5, ymin=q50.5, ymax=q50.95), alpha=themes[[theme]]$med_area_alpha, fill = themes[[theme]]$med_area) +
    geom_ribbon(data=vpc_dat, aes(x=bin, y=q5.5, ymin=q5.5, ymax=q5.95), alpha=themes[[theme]]$pi_area_alpha, fill = themes[[theme]]$pi_area) +
    geom_ribbon(data=vpc_dat, aes(x=bin, y=q95.5, ymin=q95.5, ymax=q95.95), alpha=themes[[theme]]$pi_area_alpha, fill = themes[[theme]]$pi_area) +
    geom_line(data=aggr_obs, aes(x=bin, y=obs50), linetype='solid') +
    geom_line(data=aggr_obs, aes(x=bin, y=obs5), linetype='dotted') +
    geom_line(data=aggr_obs, aes(x=bin, y=obs95), linetype='dotted') +
    xlab(xlab) + ylab(ylab)
  if (log_y) {
    pl <- pl + scale_y_log10() 
  }
  if (!is.null(strat)) {
    pl <- pl + facet_wrap(~ strat)
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
  return(list(vpc_dat = vpc_dat, obs = aggr_obs))
}

theme_plain <-  function () {
  theme(
    axis.text = element_text(size = 14),
    legend.background = element_rect(fill = "white"),
    legend.position = c(0.14, 0.80),
    panel.grid.major = element_line(colour = "#e5e5e5"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#efefef", colour = NA),
    strip.background = element_rect(fill = "#444444", colour = NA),
    strip.text = element_text(face="bold", colour = "white")
  )    
}

vpc_loq <- function(sim, obs, type = "lloq", 
                    bins, strat = NULL, dv = "dv", idv = "time",
                    pi = c(0.05, 0.95), ci = c(0.05, 0.95),
                    uloq = NULL, lloq = NULL) {
  sim <- format_vpc_input_data(sim, dv, idv, lloq, uloq, strat, bins)
  obs <- format_vpc_input_data(obs, dv, idv, lloq, uloq, strat, bins)
  loq_perc <- function(x) { sum(x <= lloq) / length(x) } # below lloq, default   
  if (type == "uloq") {
    loq_perc <- function(x) { sum(x >= uloq) / length(x) }
  }  
  aggr_sim <- data.frame(cbind(sim %>% group_by(strat, sim, bin) %>% summarise(loq_perc(dv))))    
  colnames(aggr_sim)[grep("loq_perc", colnames(aggr_sim))] <- "ploq"
  tmp <- aggr_sim %>% group_by(strat, bin)    
  vpc_dat <- data.frame(cbind(tmp %>% summarise(quantile(ploq, ci[1])),
                              tmp %>% summarise(quantile(ploq, 0.5)),
                              tmp %>% summarise(quantile(ploq, ci[2]))))
  vpc_dat <- vpc_dat[,-grep("(bin.|strat.)", colnames(vpc_dat))]
  colnames(vpc_dat) <- c("strat", "bin", "ploq_low", "ploq_med", "ploq_up")  
  aggr_obs <- data.frame(cbind(obs %>% group_by(strat,bin) %>% summarise(loq_perc(dv)),
                               obs %>% group_by(strat,bin) %>% summarise(loq_perc(dv)),
                               obs %>% group_by(strat,bin) %>% summarise(loq_perc(dv))))
  aggr_obs <- aggr_obs[,-grep("(bin.|strat.|sim.)", colnames(aggr_obs))]
  colnames(aggr_obs) <- c("strat", "bin", "ploq_low","ploq_med","ploq_up")    
  
  pl <- ggplot(sim, aes(x=bin, y=dv)) + 
    geom_line(aes(x=bin, y=ploq_med), data=vpc_dat, linetype='dashed') + 
    geom_ribbon(data=vpc_dat, aes(x=bin, y=ploq_low, ymin=ploq_low, ymax=ploq_up), alpha=0.2) +
    geom_line(data=aggr_obs, aes(x=bin, y=ploq_med), linetype='solid') +
    geom_line(data=aggr_obs, aes(x=bin, y=ploq_low), linetype='dotted') +
    geom_line(data=aggr_obs, aes(x=bin, y=ploq_up), linetype='dotted')
 
  if (!is.null(strat)) {
    pl <- pl + facet_grid(strat ~ .)
  }
  print (pl)
}

