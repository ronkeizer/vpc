#' Compute Kaplan-Meier statistics
#' 
#' @param dat data.frame with events
#' @param strat vector of stratification variables
#' @param reverse_prob reverse the probability (i.e. return `1-probability`)?
#' @param ci confidence interval to calculate, numeric vector of length 2
compute_kaplan <- function(dat, strat = "strat", reverse_prob = FALSE, ci = NULL) {
    if(length(dat[[strat]]) == 0) {
      dat$strat <- 1
    }
    strats <- unique(dat[[strat]])
    tmp <- c()
    include_ci <- FALSE
    if(!is.null(ci)) {
      include_ci <- TRUE
      if(length(ci) == 2) { # when specified as c(0.05, 0.95)
        ci = diff(ci)        
      }
    } else {
      ci = 0.95
    }
    for (i in seq(strats)) {
      tmp1 <- dat[dat[[strat]] == strats[i],]
      if(length(grep( "rtte", strats[1])) > 0  & i > 1) {
        for (j in 1:i) {
          tmp_j <- dat[dat[[strat]] == strats[j] & dat$dv == 0,]     
          tmp1 <- rbind(tmp1, tmp_j)
        }
      }
      km_fit <- survival::survfit(survival::Surv(time = time, dv != 0) ~ 1, data = tmp1, conf.int = ci)
      km_dat <- data.frame(time = km_fit$time, surv = km_fit$surv, strat = strats[i])
      if(include_ci) {        
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
      if(include_ci) {        
        first_rec <- data.frame(cbind(first_rec, lower=1-as.numeric(reverse_prob), upper=1-as.numeric(reverse_prob))) 
      }
      tmp <- rbind(tmp, rbind(first_rec, km_dat))                   
    }
    tmp$qmed <- tmp$surv
    return(tmp)      
}