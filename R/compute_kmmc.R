compute_kmmc <- function(dat, strat = NULL, reverse_prob = FALSE, kmmc = "DOSE") {
  # mostly kept variable names intact to maintain similarity to compute_kaplan
  dat$kmmc <- dat[[kmmc]]
  if (!is.null(strat)) {
    strats <- unique(dat[[strat]])
    tmp <- c()
    for (i in seq(strats)) {
      summid <- dat %>% dplyr::group_by(id) %>% dplyr::mutate(covt = mean(kmmc))
      t <- unique(summid$time)
      km_fit <- data.frame(time = t[order(t)], surv=1)
      for (j in seq(km_fit$time)) {
        km_fit$surv[j] <- mean(summid[summid$time >= km_fit$time[j],]$covt)
      }
      if(reverse_prob) {
        tmp <- rbind(tmp, data.frame(time = km_fit$time, surv = 1-km_fit$surv, strat = strats[i]))
      } else {
        tmp <- rbind(tmp, data.frame(time = km_fit$time, surv = km_fit$surv, strat = strats[i]))
      }
    }
    return(tmp)
  } else {
    km_fit <- dat %>% dplyr::group_by(time) %>% dplyr::mutate(tmp = kmmc) %>% dplyr::summarise(surv=mean(tmp))
    if(reverse_prob) {
      data.frame(time = km_fit$time, surv = 1-km_fit$surv)
    } else {
      data.frame(time = km_fit$time, surv = km_fit$surv)
    }
  }
}
