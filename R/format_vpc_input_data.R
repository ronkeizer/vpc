#' Confirm that a column is in the data, and rename data to prepare that column for later use.
#'
#' @param dat An input data.frame or similar object
#' @param cols A list with an element for colname giving the name for colname in
#'   \code{dat}.
#' @param colname The name of the column (character scalar)
#' @param coldesc The description of the column (character scalar)
#' @param what The description of the data (typically "observed" or "simulated")
#' @param default A default value (scalar or vector) to use if the column is not
#'   found.
#' @return If \code{colname} is already named \code{colname} in \code{dat},
#'   \code{dat} unchanged.  If not, check if \code{dat} has that column name
#'   already, and if so, name the existing \code{dat[[colname]]} to
#'   \code{dat[[paste0(colname, ".old")]]} and then rename
#'   \code{cols[[colname]]} to \code{colname}.
standardize_column <- function(dat, cols, colname, coldesc, what, default) {
  if (!(colname %in% names(cols))) {
    # Confirm that the cols list is correctly setup
    stop(
      "'colname' (", colname, ") must be in 'names(cols)'.  Available names: ",
      paste(names(cols), sep=", ")
    )
  }
  if (cols[[colname]] %in% colnames(dat)) {
    if (colname %in% colnames(dat) & !(cols[[colname]] == colname)) {
      # If there is a column with the expected name, but it is not actually the
      # column to use, name it with ".old" at the end.
      colnames(dat)[match(colname, colnames(dat))] <- paste0(colname, ".old")
    }
    colnames(dat)[match(cols[[colname]], colnames(dat))] <- colname
  }
  if (is.na(match(colname, colnames(dat)))[1]) {
    if (missing(default)) {
      stop(
        "No column for ", coldesc, " indicator found in ", what, " data, can't continue!",
        " Available columns: ",
        paste(colnames(dat), collapse = ", ")
      )
    } else {
      warning(
        "No column for ", coldesc,
        " indicator found in ", what, " data, using default value."
      )
      dat[[cols[[colname]]]] <- default
    }
  }
  dat
}

#' Prepare VPC data for future calculations by standardizing column names and
#' modifying the input data based on the limits of quantification,
#' stratification, and logarithmic values.
#'
#' @inheritParams standardize_column
#' @inheritParams define_loq
#' @inheritParams add_stratification
#' @param log_y Boolean indicting whether y-axis should be shown as logarithmic. Default is FALSE.
#' @param log_y_min minimal value when using log_y argument. Default is 1e-3.
#' @param verbose show debugging information (TRUE or FALSE)
#' @param drop_missing Remove records with a missing (\code{NA}) dependent
#'   variable (with a warning)? Missing values are never treated as censored.
#' @return \code{dat} modified based on other inputs. Values below \code{lloq}
#'   are set to \code{-Inf} and values above \code{uloq} to \code{Inf}
#'   (censored).
format_vpc_input_data <- function(dat, cols, lloq, uloq, stratify, log_y, log_y_min, what = "observed", verbose = FALSE, pred_corr = FALSE, drop_missing = TRUE) {
  dat <- standardize_column(dat=dat, cols=cols, colname="id", coldesc="id indicator", what=what)
  dat <- standardize_column(dat=dat, cols=cols, colname="dv", coldesc="dependent variable", what=what)
  dat <- standardize_column(dat=dat, cols=cols, colname="idv", coldesc="indepentent variable", what=what)
  if (drop_missing) {
    dat <- drop_missing_dv(dat, what = what)
  }
  dat$dv <- censor_dv(dat$dv, lloq = lloq, uloq = uloq)
  if (log_y) {
    dat$dv[is.finite(dat$dv) & dat$dv < log_y_min] <- log_y_min
  }
  dat <- add_stratification(dat=dat, stratify=stratify, verbose=verbose)
  return(dat)
}

#' Remove records with a missing dependent variable
#'
#' Missing (\code{NA}) values of the dependent variable are not treated as
#' censored. Censored values should be supplied as their actual (or any)
#' value below \code{lloq} / above \code{uloq}.
#'
#' @param dat data.frame with a standardized \code{dv} column
#' @param what The description of the data (typically "observed" or "simulated")
#' @return \code{dat} without records with a missing \code{dv}
#' @noRd
drop_missing_dv <- function(dat, what = "observed") {
  missing_dv <- is.na(dat$dv)
  if (any(missing_dv)) {
    warning(
      sum(missing_dv), " record(s) with a missing (NA) dependent variable removed from ", what, " data. ",
      "Missing values are not treated as censored: to count values as censored, supply a value below `lloq` (or above `uloq`)."
    )
    dat <- dat[!missing_dv, , drop = FALSE]
  }
  dat
}

#' @describeIn format_vpc_input_data Prepare VPC data for future calculations
#'   for time-to-event data
#' @param rtte repeated time-to-event data? Default is FALSE (treat as
#'   single-event TTE)
#' @param rtte_calc_diff recalculate time (T/F)? When simulating in NONMEM, you
#'   will probably need to set this to TRUE to recalculate the TIME to relative
#'   times between events (unless you output the time difference between events
#'   and specify that as independent variable to the vpc_tte() function.
#' @return A named list with "dat" modified, as required, and "stratify" with
#'   the stratification parameters
#' @importFrom rlang .data
format_vpc_input_data_tte <- function(dat, cols, stratify, rtte, rtte_calc_diff, what = "observed", verbose = FALSE) {
  dat <- standardize_column(dat=dat, cols=cols, colname="id", coldesc="id indicator", what=what, default=seq_len(nrow(dat)))
  dat <- standardize_column(dat=dat, cols=cols, colname="dv", coldesc="dependent variable", what=what)
  dat <- standardize_column(dat=dat, cols=cols, colname="idv", coldesc="indepentent variable", what=what)

  # Idiosyncrasies of time-to-event data
  if(max(dat$dv) > 1) { # guessing DV definition if not just 0/1
    if(max(dat$dv) == 2) { # common approach in NONMEM, 2 = censored
      dat$dv[dat$dv != 1] <- 0
      msg(
        paste(
          "Warning: vpc_tte() expected the", what,
          "dependent variable to contain only 0 (censored, or no event observed) or 1 (event observed).",
          "Setting all ", what, " != 1 to 0."
        ),
        verbose=verbose
      )
    } else {
      dat$dv[dat$dv != 1] <- 1 # some people use DV to indicate the event time.
      msg(
        paste(
          "Warning: vpc_tte() expected the ", what,
          "dependent variable to contain only 0 (censored, or no event observed) or 1 (event observed).",
          "Setting all ", what, " != 1 to 1."
        ),
        verbose=verbose
      )
    }
  } else if (max(dat$dv) == 1) {
    if (any(dat$dv > 0 & dat$dv < 1) > 0) {
      dat[dat$dv > 0  & dat$dv < 1,]$dv <- 0
    }
  }
  if("cens" %in% tolower(colnames(dat))) { # some people use a 'cens' column to indicate censoring
    msg(paste0("Detected column '",colnames(dat)[match("cens", tolower(colnames(dat)))],"' with censoring information in ", what, " data, assuming 1=censored event, 0=observed event. Please transpose data if assumption not correct."), TRUE)
    colnames(dat)[match("cens", tolower(colnames(dat)))] <- "cens"
    dat[dat$cens == 1,]$dv <- 0
  }
  dat$time <- as.num(dat$idv)
  if (what == "observed") {
    # set sim to simplify code below
    dat$sim <- 1
  } else if (what == "simulated") {
    # Data corrections that are specific to simulated data
    if ("nonmem" %in% class(dat)) { # necessary due to a bug in NONMEM simulation
      dat <- dat[!(dat$time == 0 & dat$dv == 1),]
    }
    # add sim index number
    dat$sim <- add_sim_index_number(dat, id = cols$id, sim_label = cols$sim)
    # set last_observation and repeat_obs per sim&id
    dat <-
      dat %>%
      dplyr::group_by(.data$sim, .data$id) %>%
      dplyr::mutate(
        last_obs = 1*(1:length(time) == length(time)),
        repeat_obs = 1*(cumsum(dv) > 1)
      )
  }

  if(rtte) {
    if(rtte_calc_diff) {
      dat <- relative_times(dat)
    }
    dat <- dat %>%
      dplyr::group_by(.data$sim, .data$id) %>%
      dplyr::arrange(.data$sim, .data$id, .data$time) %>%
      dplyr::mutate(rtte = 1:length(dv))
    stratify <- c(stratify, "rtte")
  } else {
    dat <-
      dat %>%
      dplyr::group_by(.data$sim, .data$id) %>%
      dplyr::mutate(
        last_obs = 1*(1:length(time) == length(time)),
        repeat_obs = 1*(cumsum(dv) > 1)
      ) %>%
      dplyr::filter(dv == 1 | last_obs == 1) %>%
      dplyr::filter(!duplicated(id))
    dat$rtte <- 1
  }

  if (what == "observed") {
    dat$sim <- NULL
  }

  dat <- add_stratification(dat=dat, stratify=stratify, verbose=verbose)
  list(
    dat=dat,
    stratify=stratify
  )
}
