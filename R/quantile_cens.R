#' Calculate quantiles respecting the censored data
#' 
#' @param x data
#' @param p quantile
#' @param limit censoring limit
#' @param cens censoring direction (left/right)
#' 
#' @export
quantile_cens <- function(x, p = 0.5, limit = 1, cens = "left") {
  if(cens %in% c("left", "lower", "bloq", "loq", "lloq")) {
    x[is.na(x)] <- -Inf
    x[x<limit] <- -Inf
  } else {
    x[is.na(x)] <- Inf
    x[x>limit] <- Inf
  }
  q <- quantile(x, p)
  ifelse(q %in% c(Inf, -Inf), NA, q)
}

#' Calculate fraction of observations below lloq / above uloq
#' 
#' @param x data
#' @param limit censoring limit
#' @param cens censoring direction (left/right)
#' @return The fraction of observations (\code{NA} is counted as below/above)
loq_frac <- function(x, limit = 1, cens = c("left", "right")) {
  cens <- match.arg(cens)
  if (cens == "left") {
    (sum(x < limit, na.rm=TRUE) + sum(is.na(x))) / length(x) 
  } else if (cens == "right") {
    (sum(x > limit, na.rm=TRUE) + sum(is.na(x))) / length(x) 
  } else {
    stop("Invalid value for cens: ", cens)
  }
}
