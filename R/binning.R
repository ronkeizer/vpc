#' Calculate appropriate bin separators for vpc
#' 
#' @param dat data
#' @param type auto-binning type. Currently only "simple" available.
#' @param n_bins number of bins to aim for.
#' @param x name of independent variable in dataset
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

#' Bin data, e.g. for use in VPC
#' 
#' @param x data
#' @param bins numeric vector specifying bin separators
#' @param idv variable in the data specifies the independent variable (e.g. "time")
#' @export
bin_data <- function(x, bins = c(0, 3, 5, 7), idv = "time") {
  x$bin <- cut(x[[idv]], bins, labels = FALSE, right=FALSE)
  return(x)
}
