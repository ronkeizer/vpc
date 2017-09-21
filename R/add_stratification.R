#' Adds stratification to data set
#' 
#' @param dat data.frame
#' @param strat vector of stratification variables
#' @param verbose verbosity (`TRUE` or `FALSE`)
add_stratification <- function (dat, strat, verbose = FALSE) {
  if(is.null(strat)) {
    dat$strat <- 1
  } else {
    if (all(strat %in% colnames(dat))) {
      dat$strat <- ""
      for(i in seq(strat)) {
        if(i > 1) { 
          dat$strat <- paste0(dat$strat, ", ")
        }
        dat$strat <- paste0(dat$strat, data.frame(dat)[,strat[i]])
      }      
    } else {
      dat$strat <- 1      
      msg("Specified stratification column name not found, not performing stratification.", verbose)
    }
  }  
  dat$strat <- as.factor(dat$strat)
  return(dat)
}
