#' Define bins for many types of data
#'
#' @inheritParams read_vpc
#' @param bins either "density", "time", or "data", "none", or one of the
#'   approaches available in classInterval() such as "jenks" (default) or
#'   "pretty", or a numeric vector specifying the bin separators. For \code{vpc()}
#'   and \code{vpc_cens()}, when binning
#'   by stratum (see \code{bin_by_strata}), \code{bins} can also be a list
#'   with one element (a binning method or numeric vector of bin separators)
#'   per stratum. The list should be named by stratum (the value of the
#'   stratification variable, or for two stratification variables the two
#'   values separated by a comma and a space, e.g. \code{"1, female"}, with
#'   missing values shown as \code{"<NA>"} and commas or backslashes within
#'   values escaped by a backslash), or
#'   be unnamed with one element per stratum in the order of the strata.
#' @param n_bins when using the "auto" binning method, what number of bins to
#'   aim for. When binning by stratum (\code{vpc()} and \code{vpc_cens()}),
#'   this can also be a list or vector with
#'   one value per stratum (named or ordered as for \code{bins}).
#' @param bin_by_strata Determine the bins separately for each stratum
#'   (\code{TRUE}), or use the same bins for all strata (\code{FALSE},
#'   default)? When \code{TRUE}, automatic binning methods are applied to the
#'   data of each stratum separately. Specifying \code{bins} as a list
#'   implies \code{bin_by_strata = TRUE}. Missing values of the
#'   stratification variable form their own stratum (with an \code{NA} name
#'   in the returned list of bins; for manual bins, use an \code{NA} name or,
#'   when no stratum has the literal value \code{"NA"}, the name
#'   \code{"NA"}).
#'   Binning by stratum is available for all automatic binning methods except
#'   \code{"percentiles"} (which labels the bins by their interval instead of
#'   plotting them on the numeric scale of the independent variable).
#' @param verbose show debugging information (TRUE or FALSE)
#' @return A list with named elements: "bins", the bin separator values (a
#'   named list with one numeric vector per stratum when binning by stratum);
#'   "labeled", are the bins labeled?; "obs", binned observed data; "sim",
#'   binned simulated data.  Additionally, "tmp_bins" is added for tte data.
define_bins <- function(obs, sim, bins, n_bins, bin_by_strata=FALSE, verbose=FALSE) {
  if (is.list(bins) || isTRUE(bin_by_strata)) {
    return(define_bins_by_strata(obs=obs, sim=sim, bins=bins, n_bins=n_bins, verbose=verbose))
  }
  labeled_bins <- bins[1] == "percentiles"
  if (!inherits(bins, "numeric")) {
    if (!is.null(obs)) {
      bins <- auto_bin(obs, bins, n_bins)
    } else { # get from sim
      bins <- auto_bin(sim, bins, n_bins)
    }
    if (is.null(bins)) {
      msg("Automatic binning unsuccessful, try increasing the number of bins, or specify vector of bin separators manually.", verbose)
    }
  }
  bins <- unique(bins)
  msg(paste0("Binning: ", paste(bins, collapse=' ')), verbose=verbose)
  if(!is.null(obs)) {
    obs <- bin_data(x=obs, bins=bins, idv="idv", labeled = labeled_bins)
  }
  if(!is.null(sim)) {
    sim <- bin_data(x=sim, bins=bins, idv="idv", labeled = labeled_bins)
  }
  list(
    bins=bins,
    labeled=labeled_bins,
    obs=obs,
    sim=sim
  )
}

#' @describeIn define_bins Define bins separately for each stratum
define_bins_by_strata <- function(obs, sim, bins, n_bins, verbose=FALSE) {
  ref <- if (!is.null(obs)) obs else sim
  if (is.null(ref)) {
    stop("No data available for binning.")
  }
  strata <- strata_levels(obs, sim)
  bin_list <- list()
  for (s in strata) {
    bins_s <- select_strat_element(bins, s, strata, "bins")
    n_bins_s <- select_strat_element(n_bins, s, strata, "n_bins")
    if (!is.numeric(bins_s) && "percentiles" %in% bins_s) {
      stop("`bins = \"percentiles\"` is not supported when binning by stratum.")
    }
    if (!is.numeric(bins_s)) {
      dat_s <- NULL
      if (!is.null(obs)) dat_s <- obs[strat_key(obs$strat) == s, , drop = FALSE]
      if ((is.null(dat_s) || nrow(dat_s) == 0) && !is.null(sim)) {
        dat_s <- sim[strat_key(sim$strat) == s, , drop = FALSE]
      }
      bins_s <- auto_bin(dat_s, bins_s, n_bins_s)
      if (is.null(bins_s)) {
        stop("Automatic binning unsuccessful for stratum `", strat_label(s), "`, try increasing the number of bins, or specify bin separators manually.")
      }
    }
    bin_list[[s]] <- unique(as.numeric(bins_s))
    msg(paste0("Binning (stratum ", strat_label(s), "): ", paste(bin_list[[s]], collapse=' ')), verbose=verbose)
  }
  obs <- bin_data_by_strata(obs, bin_list)
  sim <- bin_data_by_strata(sim, bin_list)
  # return the bins named by stratum value (NA for missing values)
  names(bin_list) <- strat_name(names(bin_list))
  list(
    bins=bin_list,
    labeled=FALSE,
    obs=obs,
    sim=sim
  )
}

#' Internal stratum key used for per-stratum binning
#'
#' Stratum values are prefixed with "v:", and missing values of the
#' stratification variable form their own stratum with key "na". The keys can
#' therefore never collide, also not with a stratum with the literal value
#' "NA".
#' @param strat stratum values (or names of a per-stratum list of bins, in
#'   which missing strata have an `NA` name)
#' @noRd
strat_key <- function(strat) {
  strat <- as.character(strat)
  ifelse(is.na(strat), "na", paste0("v:", strat))
}

#' Stratum value (NA for missing) from an internal stratum key
#' @noRd
strat_name <- function(key) {
  ifelse(key == "na", NA_character_, sub("^v:", "", key))
}

#' Label for a stratum key in messages
#' @noRd
strat_label <- function(key) {
  ifelse(key == "na", "<missing>", sub("^v:", "", key))
}

#' Keys of the strata across observed and simulated data
#' @noRd
strata_levels <- function(obs, sim) {
  get_levels <- function(x) {
    if (is.null(x)) return(character(0))
    lev <- if (is.factor(x$strat)) levels(droplevels(x$strat)) else as.character(unique(stats::na.omit(x$strat)))
    key <- strat_key(lev)
    if (anyNA(x$strat)) key <- c(key, "na")
    key
  }
  unique(c(get_levels(obs), get_levels(sim)))
}

#' Select the element of a per-stratum argument (bins or n_bins) for stratum
#' key `s`
#'
#' Elements are matched by name. The missing stratum is matched by an `NA`
#' name, or by the name "NA" when there is no stratum with the literal value
#' "NA".
#' @noRd
select_strat_element <- function(x, s, strata, name) {
  per_strat <- is.list(x) || (name == "n_bins" && length(x) > 1)
  if (!per_strat) return(x)
  nms <- names(x)
  if (!is.null(nms) && all(is.na(nms) | nms != "")) {
    keys <- strat_key(nms)
    if (!"v:NA" %in% strata) keys[keys == "v:NA"] <- "na"
    if (!s %in% keys) {
      stop("No `", name, "` specified for stratum `", strat_label(s), "`. Available strata: ",
           paste(strat_label(strata), collapse = "; "))
    }
    return(x[[match(s, keys)]])
  }
  if (length(x) != length(strata)) {
    stop("When `", name, "` is an unnamed list, it should have one element per stratum (",
         length(strata), "): ", paste(strat_label(strata), collapse = "; "))
  }
  x[[match(s, strata)]]
}

#' Bin data using separate bin separators per stratum, retaining row order
#'
#' @param bins list of bin separators, named by stratum key
#' @noRd
bin_data_by_strata <- function(x, bins) {
  if (is.null(x)) return(x)
  bin <- rep(NA_integer_, nrow(x))
  strat <- strat_key(x$strat)
  for (s in names(bins)) {
    idx <- which(strat == s)
    if (length(idx) > 0) {
      bin[idx] <- cut(x$idv[idx], bins[[s]], labels = FALSE, right = FALSE, include.lowest = TRUE)
    }
  }
  x$bin <- bin
  x
}

#' Lower and upper bin boundaries for a set of bins
#'
#' @param bins numeric vector of bin separators, or a named list of these (one
#'   per stratum, with an `NA` name for missing strata)
#' @param bin bin number (or factor)
#' @param strat stratum for each bin (only used when `bins` is a list)
#' @return a list with `bin_min` and `bin_max`
#' @noRd
bin_boundaries <- function(bins, bin, strat = NULL) {
  bin <- as.integer(bin)
  if (is.list(bins)) {
    strat <- strat_key(strat)
    keys <- strat_key(names(bins))
    bin_min <- bin_max <- rep(NA_real_, length(bin))
    for (s in intersect(unique(strat), keys)) {
      idx <- which(strat == s)
      b <- bins[[match(s, keys)]]
      bin_min[idx] <- b[bin[idx]]
      bin_max[idx] <- b[bin[idx] + 1]
    }
  } else {
    bin_min <- bins[bin]
    bin_max <- bins[bin + 1]
  }
  list(bin_min = bin_min, bin_max = bin_max)
}

#' @describeIn define_bins Define bins for time-to-event data
#' @inheritParams vpc_tte
define_bins_tte <- function(obs, sim, bins, n_bins, kmmc, verbose=FALSE) {
  if(!is.null(bins) && bins != FALSE) {
    warning("Binning is not recommended for `vpc_tte()`, plot might not show correctly!")
  }
  if(!is.null(kmmc) & (inherits(bins, "logical") && !bins)) {
    msg("Tip: with KMMC-type plots, binning of simulated data is recommended. See documentation for the 'bins' argument for more information.", verbose)
  }

  if (is.null(sim)) {
    tmp_bins <- unique(c(0, sort(unique(obs$time)), max(obs$time)))
  } else {
    tmp_bins <- unique(c(0, sort(unique(sim$time)), max(sim$time)))
    all_dat <- c()
    if(!(inherits(bins, "logical") && !bins)) {
      if(inherits(bins, "logical") && bins) {
        bins <- "time"
      }
      if(inherits(bins, "character")) {
        if (bins == "obs") {
          tmp_bins <- unique(c(0, sort(unique(obs$time)), max(obs$time)))
        } else {
          if (!(bins %in% c("time","data"))) {
            msg(paste0("Note: bining method ", bins," might be slow. Consider using method 'time', or specify 'bins' as numeric vector"), verbose)
          }
          tmp_bins <- unique(c(0, auto_bin(sim %>% dplyr::mutate(idv=time), type=bins, n_bins = n_bins-1), max(sim$time)))
        }
      }
      if(inherits(bins, "numeric")) {
        tmp_bins <- unique(c(0, bins, max(obs$time)))
      }
    }
  }
  list(
    bins=bins,
    labeled=FALSE,
    obs=obs,
    sim=sim,
    tmp_bins=tmp_bins
  )
}
