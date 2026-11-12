#' Adds stratification to data set
#'
#' When two stratification variables are supplied, the combined `strat` column
#' is a factor whose levels are ordered by the levels of the first variable,
#' then by the levels of the second variable. Factor level order of the
#' original columns is respected; non-factor columns are sorted.
#'
#' @param dat An input data.frame or similar object
#' @param stratify character vector of stratification variables. Only 1 or 2 stratification variables can be supplied.
#' @param verbose verbosity (`TRUE` or `FALSE`)
add_stratification <- function (dat, stratify, verbose = FALSE) {
  if(is.null(stratify)) {
    dat$strat <- 1
  } else {
    if (all(stratify %in% colnames(dat))) {
      if(length(stratify) == 1) {
        dat$strat <- data.frame(dat)[,stratify]
      } else {
        strat_cols <- lapply(stratify, function(x) as.factor(dat[[x]]))
        strat_label <- do.call(paste, c(lapply(strat_cols, as.character), sep = ", "))
        # Order the combined levels by the first variable, then the second, etc.
        level_order <- do.call(order, lapply(strat_cols, as.integer))
        dat$strat <- factor(strat_label, levels = unique(strat_label[level_order]))
      }
    } else {
      dat$strat <- 1
      msg("Specified stratification column name not found, not performing stratification.", verbose)
    }
  }
  if(! inherits(dat$strat, "factor")) {
    dat$strat <- as.factor(dat$strat)
  }
  dat
}

#' Add separate columns for each of two stratification variables
#'
#' Adds `strat1` and `strat2` columns to `dat` based on its combined `strat`
#' column, by looking up the original stratification columns in the reference
#' data sets. This preserves the class and factor level order of the original
#' stratification columns.
#'
#' @param dat data.frame with a `strat` column (or `NULL`)
#' @param stratify character vector with two stratification variables
#' @param ... reference data sets (or `NULL`) containing both the `strat`
#'   column and the original stratification columns
#' @return `dat` with `strat1` and `strat2` columns added
#' @keywords internal
add_strat_columns <- function(dat, stratify, ...) {
  if (is.null(dat)) return(dat)
  refs <- Filter(function(x) !is.null(x) && all(c("strat", stratify) %in% names(x)), list(...))
  if (length(refs) == 0) {
    stop("No reference data available to determine stratification columns.")
  }
  lookup <- do.call(rbind, lapply(refs, function(x) {
    x <- as.data.frame(x)[, c("strat", stratify)]
    x$strat <- as.character(x$strat)
    unique(x)
  }))
  lookup <- lookup[!duplicated(lookup$strat), ]
  idx <- match(as.character(dat$strat), lookup$strat)
  for (i in seq_along(stratify)) {
    dat[[paste0("strat", i)]] <- lookup[[stratify[i]]][idx]
  }
  dat
}

#' Add a column for each stratification variable, named by the variable
#'
#' Like [add_strat_columns()], but names the columns after the stratification
#' variables instead of `strat1`, `strat2`, ...
#' @inheritParams add_strat_columns
#' @param stratify character vector of stratification variables
#' @keywords internal
#' @noRd
add_strat_value_columns <- function(dat, stratify, ...) {
  if (is.null(dat)) return(dat)
  dat <- add_strat_columns(dat, stratify, ...)
  for (i in seq_along(stratify)) {
    colnames(dat)[match(paste0("strat", i), colnames(dat))] <- stratify[i]
  }
  dat
}
