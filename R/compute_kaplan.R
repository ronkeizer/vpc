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