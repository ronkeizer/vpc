#' Compute KMMC statistics
#' 
#' Kaplan-Meier Mean Covariate plots are a simulation-based diagnostic to study the influence of covariates and identify potential model misspecification.
#' 
#' @param dat data.frame with events
#' @param strat vector of stratification variables
#' @param reverse_prob reverse the probability (i.e. return `1-probability`)?
#' @param kmmc variable to create the KMMC plot for.
compute_kmmc <- function(dat, strat = NULL, reverse_prob = FALSE, kmmc = "DOSE") {
  # mostly kept variable names intact to maintain similarity to compute_kaplan
  dat$kmmc <- dat[[kmmc]]
  if (!is.null(strat)) {
    strats <- unique(dat[[strat]])
    tmp <- c()
    for (i in seq(strats)) {
      summid <- dat %>% dplyr::group_by_("id") %>% dplyr::mutate(covt = mean(kmmc))
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
