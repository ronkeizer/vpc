#' Lower to full triangle
#'
#' @description Convert the lower triangle of a covariance matrix to a full matrix object
#' @param vect the lower triangle of a covariance matrix
triangle_to_full <- function (vect) {
  nr <- NULL
  for (i in 1:100) { # find the size of the matrix
    if (length(vect) == add_recurs(0,0,i)) {
      nr <- i
    }
  }
  if (is.null(nr)) {
    stop("Length of `vect` (", length(vect), ") does not correspond to the lower triangle of a square matrix.")
  }
  k_given_i_j <- function(x , y ) ifelse( y<x, x*(x-1)/2 + y, y*(y-1)/2 + x )
  k_mat <- function(p) outer( 1:p, 1:p, k_given_i_j )
  return (matrix(vect[ k_mat( nr ) ] , nrow = nr ))
}
