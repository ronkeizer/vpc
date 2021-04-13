#' Calculate quantiles respecting the censored data
#' 
#' @inheritParams stats::quantile
#' @inheritParams loq_frac
#' @return The quantile of \code{x} treating \code{NA} values as censored
#' @export
quantile_cens <- function(x, probs = 0.5, limit = 1, cens = c("left", "right", "neither", "both")) {
  cens <- match.arg(cens)
  if (cens == "left") {
    x[is.na(x)] <- -Inf
    x[x<limit] <- -Inf
  } else if (cens == "right") {
    x[is.na(x)] <- Inf
    x[x>limit] <- Inf
  } else if (cens == "neither") {
    # TODO: Review on 2021-04: quantile_cens can now just work like quantile
    # (simplifying other code that checks if quantile_cens() or quantile()
    # should be used).  To consider: Should NA values be considered missing or
    # an error?

    # do nothing to x
  } else if (cens == "both") {
    stop("cens='both' is not yet supported") # nocov
  } else {
    stop("Invalid value for cens: ", cens) # nocov
  }
  q <- quantile(x, probs=probs)
  # TODO: Review on 2021-04: NA_real_ instead of NA is returned to ensure that
  # the return value is numeric
  ifelse(q %in% c(Inf, -Inf), NA_real_, q)
}

#' Calculate fraction of observations below lloq / above uloq
#' 
#' @param x A numeric vector
#' @param limit censoring limit (ignored if \code{cens="neither"})
#' @param cens censoring direction
#' @return The fraction of observations (\code{NA} is counted as below/above)
loq_frac <- function(x, limit = 1, cens = c("left", "right", "neither", "both")) {
  cens <- match.arg(cens)
  if (cens == "left") {
    if (length(limit) != 1) {
      stop("limit must be a scalar if cens='left'")
    }
    (sum(x < limit, na.rm=TRUE) + sum(is.na(x))) / length(x)
  } else if (cens == "right") {
    if (length(limit) != 1) {
      stop("limit must be a scalar if cens='right'")
    }
    (sum(x > limit, na.rm=TRUE) + sum(is.na(x))) / length(x)
  } else if (cens == "neither") {
    # Zero by definition if no censoring occurs
    0
  } else if (cens == "both") {
    if (length(limit) != 2) {
      stop("limit must have 2 elements if cens='both'")
    }
    (sum((x < min(limit)) | (x > max(limit)), na.rm=TRUE) + sum(is.na(x))) / length(x)
  } else {
    stop("Invalid value for cens: ", cens) # nocov
  }
}
