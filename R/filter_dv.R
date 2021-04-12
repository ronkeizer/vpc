#' Remove values that are not observed values from data
#' 
#' @param dat A data.frame or similar object
#' @param verbose show debugging information (TRUE or FALSE)
#' @return dat With non-observation rows removed
filter_dv <- function(x, verbose = FALSE, ...) {
  software_match <- intersect(class(x), names(filter_dv_software))
  if (length(software_match) > 1) {
    # TODO: Review 2021-04: Matching the software is a bit stricter now
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
  # TODO: Review 2021-04: name was "phx" here and "phoenix" elsewhere.
  # Standardizing to "phoenix".
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


