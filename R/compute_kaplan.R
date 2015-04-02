compute_kaplan <- function(dat, strat = NULL, reverse_prob = FALSE) {
  if (!is.null(strat)) {
    strats <- unique(dat[[strat]])
    tmp <- c()
    for (i in seq(strats)) {
      km_fit <- survfit(Surv(time = time, dv != 0) ~ 1, data = dat[dat[[strat]] == strats[i],])
      if(reverse_prob) {
        tmp <- rbind(tmp, data.frame(time = km_fit$time, surv = 1-km_fit$surv, strat = strats[i]))            
      } else {
        tmp <- rbind(tmp, 
                     rbind(data.frame(time=0, surv=1, strat = strats[i]),
                           data.frame(time = km_fit$time, surv = km_fit$surv, strat = strats[i]))
        )
      }
    }
    return(tmp)      
  } else {
    km_fit <- survfit(Surv(time = time, dv != 0) ~ 1, data = dat)
    if(reverse_prob) {
      data.frame(time = km_fit$time, surv = 1-km_fit$surv)                       
    } else {
      data.frame(time = km_fit$time, surv = km_fit$surv)                 
    }
  }
}