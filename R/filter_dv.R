filter_dv <- function(x, ...) {
  available_methods <- c("phx", "nonmem")
  if(sum(available_methods %in% class(x)) > 0)  {
    UseMethod("filter_dv", x)    
  } else {
    x    
  }
}


filter_dv.phx <- function(x, dv, ...) {
  message("Filtering rows with no DV values")
  x[!is.na(x[[dv]]),]
}


filter_dv.nonmem <- function(x, ...) {
  if("MDV" %in% names(x)) {
    message("Filtering rows where MDV not 0")
    x <- x[x[["MDV"]] == 0,]
  } else if ("EVID" %in% names(x)){
    message("Filtering rows where EVID not 0")
    x <- x[x[["EVID"]] == 0,]
  } else {
    warning("No MDV or EVID columns found to filter on")
  }
  return(x)
}