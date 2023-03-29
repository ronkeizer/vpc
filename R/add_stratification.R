#' Adds stratification to data set
#'
#' @param dat An input data.frame or similar object
#' @param stratify character vector of stratification variables. Only 1 or 2 stratification variables can be supplied.
#' @param verbose verbosity (`TRUE` or `FALSE`)
add_stratification <- function (dat, stratify, verbose = FALSE) {
  if(is.null(stratify)) {
    dat$strat <- 1
  } else {
    if (all(stratify %in% colnames(dat))) {
      if(length(stratify) == 1) {
        dat$strat <- data.frame(dat)[,stratify]
      } else {
        dat$strat <- ""
        for(i in seq(stratify)) {
          if(i > 1) {
            dat$strat <- paste0(dat$strat, ", ")
          }
          dat$strat <- paste0(dat$strat, data.frame(dat)[,stratify[i]])
        }
      }
    } else {
      dat$strat <- 1
      msg("Specified stratification column name not found, not performing stratification.", verbose)
    }
  }
  if(class(dat$strat) != "factor") {
    dat$strat <- as.factor(dat$strat)
  }
  dat
}
