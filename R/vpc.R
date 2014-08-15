#########################################
## Functions for VPC (not using Xpose) ##
#########################################

sim_data <- function (design = cbind(id = c(1,1,1), idv = c(0,1,2)), 
                      model, param, n=100) {
  sim_des <- do.call("rbind", replicate(n, design, simplify = FALSE))
  sim_des$sdv <- model(sim_des, param)  
  sim_des$sim <- rep(1:n, each = length(design[,1]))
  sim_des
}

bin_data <- function(x, bins = c(0, 3, 5, 7), idv = "time") {
  x$bin <- cut(x[[idv]], bins, labels = FALSE, right=FALSE)
  return(x)
}

vpc <- function(sim, obs, 
                bins = c(0,2,4,6,8,10), 
                dv = "dv", idv = "time",
                strat = NULL,
                pi = c(0.05, 0.95), ci = c(0.05, 0.95),
                uloq = NULL, lloq = NULL) {
  sim <- format_vpc_input_data(sim, dv, idv, lloq, uloq, strat, bins)
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
  pl <- ggplot(sim, aes(x=bin, y=dv)) + 
    geom_line(aes(x=bin, y=q50.50), data=vpc_dat, linetype='dashed') + 
    geom_ribbon(data=vpc_dat, aes(x=bin, y=q50.5, ymin=q50.5, ymax=q50.95), alpha=0.2) +
    geom_ribbon(data=vpc_dat, aes(x=bin, y=q5.5, ymin=q5.5, ymax=q5.95), alpha=0.2) +
    geom_ribbon(data=vpc_dat, aes(x=bin, y=q95.5, ymin=q95.5, ymax=q95.95), alpha=0.2) +
    geom_line(data=aggr_obs, aes(x=bin, y=obs50), linetype='solid') +
    geom_line(data=aggr_obs, aes(x=bin, y=obs5), linetype='dotted') +
    geom_line(data=aggr_obs, aes(x=bin, y=obs95), linetype='dotted')
  if (!is.null(strat)) {
    pl <- pl + facet_grid(strat ~ .)
  }
  print(pl)
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
  }
  dat <- bin_data(dat, bins, "idv")  
  return(dat)
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
