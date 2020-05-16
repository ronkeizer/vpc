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

#' Calculate percentiles below / above lloq / uloq
#' 
#' @param x data
#' @param limit censoring limit
#' @param cens censoring direction (left/right)
loq_perc <- function(x, limit = 1, cens = "left") {
  if(cens %in% c("left", "lower", "bloq", "loq", "lloq")) {
    (sum(x < limit, na.rm=TRUE) + sum(is.na(x))) / length(x) 
  } else {
    (sum(x > limit, na.rm=TRUE) + sum(is.na(x))) / length(x) 
  }
}
