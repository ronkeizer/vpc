#' Check whether stratification columns are available
#'
#' @param data `data.frame` with observation or simulation data (or \code{NULL}
#'   to skip checking)
#' @param stratify vector of stratification columns (or \code{NULL} to skip
#'   checking)
#' @param type either `observation` or `simulation`
#' @return \code{TRUE} or raise an error about the missing columns
check_stratification_columns_available <- function(data, stratify, type = "observation") {
  if (!is.null(data) & !is.null(stratify)) {
    diffs <- setdiff(stratify, names(data))
    if(length(diffs) >=1) {
      stop(paste0("The following specified stratification columns were NOT found in ",type, " data: \n",
                  paste(diffs, collapse = ",")))
    }
  }
  TRUE
}
