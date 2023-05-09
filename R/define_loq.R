#' Check and standardize limits of quantification
#'
#' @param uloq Number or NULL indicating upper limit of quantification. Default is NULL.
#' @param lloq Number or NULL indicating lower limit of quantification. Default is NULL.
#' @param pred_corr perform prediction-correction?
#' @param pred_corr_lower_bnd lower bound for the prediction-correction
#' @param require_loq Is at least one of \code{lloq} or \code{uloq} required?
#' @return A list with elements "lloq", "uloq", "cens_limit", "pred_corr",
#'   "pred_corr_lower_bnd", and "cens_type".  "cens_limit" indicates the range
#'   for censoring (\code{c(lloq, uloq)}, \code{NULL} if neither is given),
#'   "cens_type" indicates whether the data are "left" censored (low values are
#'   censored, only), "right" (high values are censored, only), "both" (low and
#'   high values are censored), or "neither" (no values are censored).
define_loq <- function(lloq = NULL, uloq = NULL,
                       pred_corr = FALSE, pred_corr_lower_bnd = 0,
                       require_loq) {
  ## Currently we can't handle both LLOQ and ULOQ
  if (!is.null(uloq) & !is.null(lloq)) {
    stop("Sorry, currently the vpc function cannot handle both upper and lower limit of quantification. Please specify either `lloq` or `uloq` but not both.")
  }
  if (require_loq & is.null(uloq) & is.null(lloq)) {
    stop("You have to specify either a lower limit of quantification (lloq=...) or an upper limit (uloq=...).")
  }
  cens_type <-
    if (is.null(lloq) & is.null(uloq)) {
      "neither"
    } else if (is.null(uloq)) {
      "left"
    } else if (is.null(lloq)) {
      "right"
    } else {
      "both" # nocov
    }
  list(
    lloq=lloq,
    uloq=uloq,
    cens_limit=c(lloq, uloq),
    pred_corr=pred_corr,
    pred_corr_lower_bnd=pred_corr_lower_bnd,
    cens_type=cens_type
  )
}
