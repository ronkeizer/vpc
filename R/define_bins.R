#' Define bins for many types of data
#' 
#' @inheritParams read_vpc
#' @param bins either "density", "time", or "data", "none", or one of the
#'   approaches available in classInterval() such as "jenks" (default) or
#'   "pretty", or a numeric vector specifying the bin separators.
#' @param n_bins when using the "auto" binning method, what number of bins to
#'   aim for
#' @return A list with named elements: "bins", the bin separator values;
#'   "labeled", are the bins labeled?; "obs", binned observed data; "sim",
#'   binned simulated data.  Additionally, "tmp_bins" is added for tte data.
define_bins <- function(obs, sim, bins, n_bins, verbose=FALSE) {
  labeled_bins <- bins[1] == "percentiles"
  if (class(bins) != "numeric") {
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
  if(verbose) message(paste0("Binning: ", paste(bins, collapse=' ')))
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

#' @describeIn define_bins Define bins for time-to-event data
#' @inheritParams vpc_tte
define_bins_tte <- function(obs, sim, bins, n_bins, kmmc, verbose=FALSE) {
  if(!is.null(bins) && bins != FALSE) {
    message("Binning is not recommended for `vpc_tte()`, plot might not show correctly!")
  }
  if(!is.null(kmmc) & (class(bins) == "logical" && bins == FALSE)) {
    msg("Tip: with KMMC-type plots, binning of simulated data is recommended. See documentation for the 'bins' argument for more information.", verbose)
  }

  if (is.null(sim)) {
    tmp_bins <- unique(c(0, sort(unique(obs$time)), max(obs$time)))
  } else {
    tmp_bins <- unique(c(0, sort(unique(sim$time)), max(sim$time)))
    all_dat <- c()
    if(!(class(bins) == "logical" && bins == FALSE)) {
      if(class(bins) == "logical" && bins == TRUE) {
        bins <- "time"
      }
      if(class(bins) == "character") {
        if (bins == "obs") {
          tmp_bins <- unique(c(0, sort(unique(obs$time)), max(obs$time)))
        } else {
          if (!(bins %in% c("time","data"))) {
            msg(paste0("Note: bining method ", bins," might be slow. Consider using method 'time', or specify 'bins' as numeric vector"), verbose)
          }
          tmp_bins <- unique(c(0, auto_bin(sim %>% dplyr::mutate(idv=time), type=bins, n_bins = n_bins-1), max(sim$time)))
        }
      }
      if(class(bins) == "numeric") {
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