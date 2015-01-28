add_stratification <- function (dat, strat) {
  if(is.null(strat)) {
    dat$strat <- 1
  } else {
    if (strat %in% colnames(dat)) {
      dat$strat <- ""
      for(i in seq(strat)) {
        if(i > 1) { 
          dat$strat <- paste0(dat$strat, ", ")
        }
        dat$strat <- paste0(dat$strat, strat[i], "=", dat[,strat[i]])
      }      
    } else {
      dat$strat <- 1      
      warning("Specified stratification column name not found, not performing stratification.")
    }
  }  
  dat$strat <- as.factor(dat$strat)
  return(dat)
}
