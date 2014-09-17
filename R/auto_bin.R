#' Calculate appropriate bin separators for vpc
#' 
#' @param dat
#' @param type 
#' @param n_bins
#' @param x
#' @return A vector of bin separators
#' @export auto_bin
#' @seealso \code{\link{vpc}}
#' @details
#' This function calculates bin separators (e.g. for use in a vpc) based on nadirs in the density functions for the indenpendent variable

auto_bin <- function (dat, type="simple", n_bins = 8, x="time") {
  if (type == "simple") {
    bws <- diff(range(dat[[x]])) * seq(from=0.01, to = 1, by=0.01)
    for (i in seq(bws)) {
      d <- density(dat[[x]], bw=bws[i])
      bins <- c(0, d$x[find_nadirs(d$y)], max(dat[[x]])*1.01)    
      if (length(bins) <= (n_bins-1)) {
        return(bins)
      }
    }      
  } else {
    return("Not implemented yet!")
  }
}