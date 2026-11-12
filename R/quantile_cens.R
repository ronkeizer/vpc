#' Calculate quantiles respecting the censored data
#'
#' Censored values are represented as \code{-Inf} (left-censored, below the
#' lower limit of quantification) or \code{Inf} (right-censored, above the
#' upper limit of quantification). Values beyond \code{limit} are also treated
#' as censored. \code{NA} values are treated as missing (not censored) and are
#' ignored.
#'
#' @inheritParams stats::quantile
#' @inheritParams loq_frac
#' @return The quantile of \code{x}, or \code{NA_real_} if the quantile falls
#'   in the censored part of the distribution.
#' @export
quantile_cens <- function(x, probs = 0.5, limit = 1, cens = c("left", "right", "neither", "both")) {
  cens <- match.arg(cens)
  q <- quantile_cens_inf(x = x, probs = probs, limit = limit, cens = cens)
  ifelse(is.infinite(q), NA_real_, q)
}

#' Like \code{quantile_cens()}, but a quantile that
#'   falls in the censored part of the distribution is returned as \code{-Inf}
#'   or \code{Inf} (instead of \code{NA}), so that it can be aggregated further.
#' @noRd
quantile_cens_inf <- function(x, probs = 0.5, limit = 1, cens = c("left", "right", "neither", "both")) {
  cens <- match.arg(cens)
  if (cens == "left") {
    x[!is.na(x) & x < limit] <- -Inf
  } else if (cens == "right") {
    x[!is.na(x) & x > limit] <- Inf
  } else if (cens == "neither") {
    # do nothing to x
  } else if (cens == "both") {
    stop("cens='both' is not yet supported") # nocov
  } else {
    stop("Invalid value for cens: ", cens) # nocov
  }
  unname(quantile(x, probs = probs, na.rm = TRUE))
}

#' Calculate fraction of observations below lloq / above uloq
#'
#' Censored values are expected as \code{-Inf} (below lloq) or \code{Inf}
#' (above uloq), but values beyond \code{limit} are also counted. \code{NA}
#' values are treated as missing and excluded from both the numerator and the
#' denominator.
#'
#' @param x A numeric vector
#' @param limit censoring limit (ignored if \code{cens="neither"})
#' @param cens censoring direction
#' @return The fraction of non-missing observations below/above the limit
loq_frac <- function(x, limit = 1, cens = c("left", "right", "neither", "both")) {
  cens <- match.arg(cens)
  x <- x[!is.na(x)]
  if (cens == "left") {
    if (length(limit) != 1) {
      stop("limit must be a scalar if cens='left'")
    }
    sum(x < limit) / length(x)
  } else if (cens == "right") {
    if (length(limit) != 1) {
      stop("limit must be a scalar if cens='right'")
    }
    sum(x > limit) / length(x)
  } else if (cens == "neither") {
    # Zero by definition if no censoring occurs
    0
  } else if (cens == "both") {
    if (length(limit) != 2) {
      stop("limit must have 2 elements if cens='both'")
    }
    sum((x < min(limit)) | (x > max(limit))) / length(x)
  } else {
    stop("Invalid value for cens: ", cens) # nocov
  }
}

#' Censor values outside the limits of quantification
#'
#' @param dv dependent variable
#' @inheritParams define_loq
#' @return \code{dv} with values below \code{lloq} set to \code{-Inf} and values
#'   above \code{uloq} set to \code{Inf}. \code{NA} values are kept.
#' @noRd
censor_dv <- function(dv, lloq = NULL, uloq = NULL) {
  if (!is.null(lloq)) dv[!is.na(dv) & dv < lloq] <- -Inf
  if (!is.null(uloq)) dv[!is.na(dv) & dv > uloq] <- Inf
  dv
}

#' Set censored values (\code{-Inf}/\code{Inf}) to \code{NA}
#'
#' Used before returning data to the user (vpcdb) or plotting, so that
#' censored values are not drawn at the edge of the plot.
#'
#' @param dat data.frame with a \code{dv} column, or \code{NULL}
#' @return \code{dat} with infinite \code{dv} values set to \code{NA}
#' @noRd
cens_to_na <- function(dat) {
  if (!is.null(dat) && is.numeric(dat$dv)) {
    dat$dv[is.infinite(dat$dv)] <- NA
  }
  dat
}
