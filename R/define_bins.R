#' Define bins for many types of data
#' 
#' @inheritParams read_vpc
#' @param bins either "density", "time", or "data", "none", or one of the
#'   approaches available in classInterval() such as "jenks" (default) or
#'   "pretty", or a numeric vector specifying the bin separators.
#' @param n_bins when using the "auto" binning method, what number of bins to
#'   aim for
#' @return A list with two elements: "bins", the bin separator values; and
#'   "labeled" are the bins labeled?
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
  list(
    bins=bins,
    labeled=labeled_bins
  )
}
