#' Confirm that a column is in the data, and rename data to prepare that column for later use.
#' 
#' @param dat An input data.frame or similar object
#' @param cols A list with an element for colname giving the name for colname in
#'   \code{dat}.
#' @param colname The name of the column (character scalar)
#' @param coldesc The description of the column (character scalar)
#' @param what The description of the data (typically "observed" or "simulated")
#' @return If \code{colname} is already named \code{colname} in \code{dat},
#'   \code{dat} unchanged.  If not, check if \code{dat} has that column name
#'   already, and if so, name the existing \code{dat[[colname]]} to
#'   \code{dat[[paste0(colname, ".old")]]} and then rename
#'   \code{cols[[colname]]} to \code{colname}.
standardize_column <- function(dat, cols, colname, coldesc, what) {
  if (!(colname %in% names(cols))) {
    stop(
      "'colname' (", colname, ") must be in 'names(cols)'.  Available names: ",
      paste(names(cols), sep=", ")
    )
  }
  if (cols[[colname]] %in% colnames(dat)) {
    if (colname %in% colnames(dat) & !(cols[[colname]] == colname)) {
      colnames[match(colname, colnames(dat))] <- paste0(colname, ".old")
    }
    colnames(dat)[match(cols[[colname]], colnames(dat))] <- colname
  }
  if (is.na(match(colname, colnames(dat)))[1]) {
    stop(
      "No column for ", coldesc, " indicator found in ", what, " data, can't continue!",
      " Available columns: ",
      paste(colnames(dat), collapse = ", ")
    )
  }
  dat
}

#' Prepare VPC data for future calculations by standardizing column names and
#' modifying the input data based on the limits of quantification,
#' stratification, and logarithmic values.
#' 
#' @inheritParams standardize_column
#' @inheritParams define_loq
#' @param log_y Boolean indicting whether y-axis should be shown as logarithmic. Default is FALSE.
#' @param log_y_min minimal value when using log_y argument. Default is 1e-3.
#' @return \code{dat} modified based on other inputs.
format_vpc_input_data <- function(dat, cols, lloq, uloq, strat, log_y, log_y_min, what = "observed", verbose = FALSE, pred_corr = FALSE) {
  dat <- standardize_column(dat=dat, cols=cols, colname="id", coldesc="id indicator", what=what)
  dat <- standardize_column(dat=dat, cols=cols, colname="dv", coldesc="dependent variable", what=what)
  dat <- standardize_column(dat=dat, cols=cols, colname="idv", coldesc="indepentent variable", what=what)
  if (pred_corr) {
    if (!is.null(uloq)) { dat$dv[dat$dv > uloq] <- uloq }
    if (!is.null(lloq)) { dat$dv[dat$dv < lloq] <- lloq }
  } else {
    if (!is.null(uloq)) { dat$dv[dat$dv > uloq] <- NA }
    if (!is.null(lloq)) { dat$dv[dat$dv < lloq] <- NA }
  }
  if (log_y) {
    dat$dv[dat$dv < log_y_min] <- log_y_min
  }
  dat <- add_stratification(dat, strat)
  return(dat)
}
