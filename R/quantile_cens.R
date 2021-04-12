#' Calculate quantiles respecting the censored data
#' 
#' @inheritParams stats::quantile
#' @param limit censoring limit
#' @param cens censoring direction ("left"/"right")
#' @return 
#' @export
quantile_cens <- function(x, probs = 0.5, limit = 1, cens = c("left", "right")) {
  cens <- match.arg(cens)
  if (cens == "left") {
    x[is.na(x)] <- -Inf
    x[x<limit] <- -Inf
  } else if (cens == "right") {
    x[is.na(x)] <- Inf
    x[x>limit] <- Inf
  } else {
    stop("Invalid value for cens: ", cens) # nocov
  }
  q <- quantile(x, probs=probs)
  # TODO: Revew on 2021-04: NA_real_ instead of NA is returned to ensure that
  # the return value is numeric
  ifelse(q %in% c(Inf, -Inf), NA_real_, q)
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
    stop("Invalid value for cens: ", cens) # nocov
  }
}
