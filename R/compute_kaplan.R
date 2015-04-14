compute_kaplan <- function(dat, strat = "strat", reverse_prob = FALSE, ci = NULL) {
    if(length(dat[[strat]]) == 0) {
      dat$strat <- 1
    }
    strats <- unique(dat[[strat]])
    tmp <- c()
    for (i in seq(strats)) {
      km_fit <- survfit(Surv(time = time, dv != 0) ~ 1, data = dat[dat[[strat]] == strats[i],])
      km_dat <- data.frame(time = km_fit$time, surv = km_fit$surv, strat = strats[i])
      if(!is.null(ci)) {        
        km_dat <- data.frame(cbind(km_dat, lower=km_fit$lower, upper=km_fit$upper)) 
      }
      if(reverse_prob) {
        km_dat$surv <- 1 - km_dat$surv
        if(!is.null(ci)) {        
          km_dat$upper <- 1 - km_dat$upper  
          km_dat$lower <- 1 - km_dat$lower
        }
      }
      first_rec <- data.frame(time=0, surv=1-as.numeric(reverse_prob), strat = strats[i])
      if(!is.null(ci)) {        
        first_rec <- data.frame(cbind(first_rec, lower=1-as.numeric(reverse_prob), upper=1-as.numeric(reverse_prob))) 
      }
      tmp <- rbind(tmp, rbind(first_rec, km_dat))                   
    }
    return(tmp)      
}