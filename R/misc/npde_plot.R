#' Create VPC-like NPDE plot (npde vs idv)
#' 
#' Creates NPDE plot from observed and simulation data
#' @param sim a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param obs a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param bins either "auto" or a numeric vector specifying the bin separators.  
#' @param n_bins when using the "auto" binning method, what number of bins to aim for
#' @param auto_bin_type auto-binning type, default is "simple".
#' @param obs_dv variable in data.frame for observed dependent value. "dv" by default
#' @param sim_dv variable in data.frame for simulated dependent value. "sdv" by default
#' @param obs_idv variable in data.frame for observed independent value. "time" by default
#' @param sim_idv variable in data.frame for simulated independent value. "time" by default
#' @param obs_id variable in data.frame for observed individual. "id" by default
#' @param sim_id variable in data.frame for simulated individual. "id" by default
#' @param obs_pred variable in data.frame for population predicted value. "pred" by default
#' @param sim_pred variable in data.frame for population predicted value. "pred" by default
#' @param nonmem should variable names standard to NONMEM be used (i.e. ID, TIME, DV, PRED). Default is "auto" for autodetect
#' @param plot_dv should observations be plotted?
#' @param stratify character vector of stratification variables. Only 1 or 2 stratification variables can be supplied.
#' @param pi simulated prediction interval to plot. Default is c(0.05, 0.95), 
#' @param ci confidence interval to plot. Default is (0.05, 0.95)
#' @param uloq Number or NULL indicating upper limit of quantification. Default is NULL.  
#' @param lloq Number or NULL indicating lower limit of quantification. Default is NULL.  
#' @param plot Boolean indacting whether to plot the ggplot2 object after creation. Default is TRUE.
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
npde_vs_time <- function (sim, 
                          obs,
                          nonmem = "auto",
                          obs_dv = "dv",
                          obs_idv = "time",
                          obs_id = "id",
                          sim_dv = "dv",
                          sim_idv = "time",
                          sim_id = "id",
                          n_bins = 8,
                          smooth = TRUE,
                          theme = "default",
                          plot = TRUE,
                          title = NULL,
                          custom_theme = NULL) {
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
  tmp_obs <- obs[, match(c(get("obs_id"), get("obs_idv"), get("obs_dv")), names(obs))]
  tmp_sim <- sim[, match(c(get("sim_id"), get("sim_idv"), get("sim_dv")), names(sim))]
  x <- autonpde(tmp_obs, tmp_sim, 1,2,3, boolsave=FALSE)
  # plot(x)
  pl_dat <- cbind(obs[, match(c(obs_id, obs_idv, obs_dv), names(obs))], x["results"]["res"])
  colnames(pl_dat)[1:3] <- c("id", "idv", "dv")
  #hist: ggplot(pl, ggplot2::aes(x=npde)) + geom_histogram() + theme_plain()
  # plot(x)
  sim$sim <- add_sim_index_number(sim, id=sim_id)    
  n_sim <- length(unique(sim$sim))

  # bin, compute obs perc, and sim quantiles 
  bins <- auto_bin(obs, "simple", n_bins, x=obs_idv)
  all_pi <- c()
  for (i in 1:(length(bins)-1)) {
    obs_tmp <- pl_dat[pl_dat$idv > bins[i] & pl_dat$idv < bins[i+1],]
    tmp <- compute.bands(length(obs_tmp[,1])) # crude. assuming for now that every id contributes 1 per bin
    bands <- data.frame(rbind(cbind(ymin = tmp$binf[1], ymax = tmp$bsup[1], fill="lower"),
                              cbind(ymin = tmp$binf[2], ymax = tmp$bsup[2], fill="median"),
                              cbind(ymin = tmp$binf[3], ymax = tmp$bsup[3], fill="upper")))
    bands$xmin <- bins[i]
    bands$xmax <- bins[i+1]
    bands$xmed <- (bands$xmin + bands$xmax)/2
    bands$ymin <- as.num(bands$ymin)
    bands$ymax <- as.num(bands$ymax)
    q <- quantile(obs_tmp$npde, c(0.025, 0.50, 0.975))
    bands$obs_min <- q[1]
    bands$obs_max <- q[3]
    bands$obs_med <- q[2]
    all_pi <- rbind(all_pi, bands)    
  }  

  pl <- ggplot(pl_dat) 
  if(smooth) {
    pl <- pl + geom_ribbon(data = all_pi, ggplot2::aes(x=xmed, ymin=ymin, ymax=ymax, fill=fill, group=fill), linetype='dashed', alpha=0.2) 
  } else {
    pl <- pl + geom_rect(data = all_pi, ggplot2::aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill), linetype='dashed', alpha=0.2) 
  }
  
  pl <- pl + geom_line(data = all_pi, ggplot2::aes(x=xmed, y=obs_med)) + 
    geom_line(data = all_pi, ggplot2::aes(x=xmed, y=obs_min), linetype='dashed') + 
    geom_line(data = all_pi, ggplot2::aes(x=xmed, y=obs_max), linetype='dashed') + 
    geom_line(aes(x=idv, y=npde, group=id), alpha = 0.1) + 
    geom_point(aes(x=idv, y=npde), alpha=0.3) + 
    geom_hline(yintercept=c(-1.96, 0, 1.96), alpha=0.3, linetype='dotted')    
  if (!is.null(custom_theme)) {  
    pl <- pl + custom_theme()    
  } else {
    if (!is.null(theme)) {
      pl <- pl + theme_plain()
    } 
  }
  bdat <- data.frame(cbind(x=bins, y=NA))
  pl <- pl + 
    geom_rug(data=bdat, sides = "t", ggplot2::aes(x = x, y=y), colour="#333333")
  if (!is.null(title)) {
    pl <- pl + ggtitle(title)  
  }
  if(plot) {
    print(pl)
  }
  invisible(pl)
}

## taken from npde library by Comets et al.
compute.bands.true<-function(sim.ypl,quant=c(0.025,0.5,0.975), distrib="norm", alpha=0.95) {
  # Compute a prediction interval around selected quantiles of the normal or uniform distribution, for different sizes of samples by using the data
  quant.pi<-c((1-alpha)/2,0.5,1-(1-alpha)/2)
  nsim<-dim(sim.ypl)[2]-2
  grp<-unique(sim.ypl[,1])
  binf<-bsup<-bmed<-matrix(nrow=length(grp),ncol=length(quant), dimnames=list(grp,quant))
  for(igrp in grp) {
    mat<-apply(sim.ypl[sim.ypl$grp==igrp,3:dim(sim.ypl)[2]],2,quantile,quant)
    xtab<-apply(mat,1,quantile,quant.pi)
    binf[grp==igrp,]<-xtab[1,]
    bmed[grp==igrp,]<-xtab[2,]
    bsup[grp==igrp,]<-xtab[3,]
  }
  return(list(binf=binf,bsup=bsup,bmed=bmed))
}

## taken from npde library by Comets et al.
compute.bands<-function(nsamp,nseuil=200,quant=c(0.025,0.5,0.975),distrib="norm", alpha=0.95) {
  # Compute a prediction interval around selected quantiles of the normal or uniform distribution, for different sizes of samples by randomly sampling from N(0,1) or U(0,1)
  ### nsamp: a vector giving the sizes of the samples
  ### size: alpha (defaults to a 95% PI)
  ### quantile: quant (defaults to 5th, 50th (median) and 95th percentiles)
  ### distribution: normal (default) or uniform
  # When the number of samples isamp is larger than 200, the PI is computed for n=200 and the size of the PI is then adjusted through sqrt(200/isamp)
  
  # Returns
  ### binf: lower bounds (as many columns as elements in quant) for the PI with level alpha
  ### bmed: median
  ### bsup: upper bounds
  #  msim<-10000
  msim<-1000 # number of replications used to compute the prediction interval
  idx1<-which(nsamp>=nseuil)
  quant.pi<-c((1-alpha)/2,0.5,1-(1-alpha)/2) 
  if(length(idx1)>0) {
    xsamp<-matrix(switch(distrib,norm=rnorm(msim*nseuil),unif=runif(msim*nseuil)), ncol=msim)
    mat<-apply(xsamp,2,quantile,quant)
    xseuil<-apply(mat,1,quantile,quant.pi)
    demi<-(xseuil[3,]-xseuil[1,])/2
  }
  binf<-bsup<-bmed<-matrix(nrow=length(nsamp),ncol=length(quant), dimnames=list(nsamp,quant))
  for(isamp in unique(nsamp)) {
    if(isamp<nseuil) {
      xsamp<-matrix(switch(distrib,norm=rnorm(msim*isamp),unif=runif(msim*isamp)), ncol=msim)
      mat<-apply(xsamp,2,quantile,quant)
      xtab<-apply(mat,1,quantile,quant.pi)
    } else {
      xtab<-matrix(nrow=3,ncol=length(quant))
      xtab[2,]<-switch(distrib,norm=qnorm(quant),unif=quant)
      xtab[1,]<-xtab[2,]-demi*sqrt(nseuil/isamp)
      xtab[3,]<-xtab[2,]+demi*sqrt(nseuil/isamp)
    }
    for(i in which(nsamp==isamp)) {
      binf[i,]<-xtab[1,]
      bmed[i,]<-xtab[2,]
      bsup[i,]<-xtab[3,]
    }
  }
  return(list(binf=binf,bsup=bsup,bmed=bmed))
}