#' Remove values that are not observed values from data
#'
#' @param x A data.frame or similar object
#' @param verbose show debugging information (TRUE or FALSE)
#' @param ... Passed to software-specific filtering function
#' @return \code{x} With non-observation rows removed
filter_dv <- function(x, verbose = FALSE, ...) {
  software_match <- intersect(class(x), names(filter_dv_software))
  if (length(software_match) > 1) {
    warning(
      "Multiple software packages matched for filtering values, not filtering.  Software matched: ",
      paste(software_match, collapse=", ")
    )
    x
  } else if (length(software_match) == 1) {
    filter_dv_software[[software_match]](x=x, verbose=verbose, ...)
  } else {
    warning(
      "No software packages matched for filtering values, not filtering.",
      "\n Object class: ", paste(class(x), collapse=", "),
      "\n Available filters: ", paste(names(filter_dv_software), collapse=", ")
    )
    x
  }
}

filter_dv_software <- list(
  "phoenix" = function(x, dv, verbose = FALSE, ...) {
    msg("Filtering rows with no DV values", verbose)
    x[!is.na(x[[dv]]),]
  },
  "nonmem" = function(x, verbose = FALSE, ...) {
    if ("EVID" %in% names(x)){
      msg("Filtering rows where EVID not 0", verbose)
      x <- x[x[["EVID"]] == 0,]
    }
    if("MDV" %in% names(x)) {
      msg("Filtering rows where MDV not 0", verbose)
      x <- x[x[["MDV"]] == 0,]
    }
    if(sum(c("EVID", "MDV") %in% names(x)) == 0) {
      msg("No MDV or EVID columns found to filter on", verbose)
    }
    return(x)
  }
)