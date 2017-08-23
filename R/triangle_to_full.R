#' Lower to full triangle
#' 
#' @description Convert the lower triangle of a covariance matrix to a full matrix object
#' @param vect the lower triangle of a covariance matrix
triangle_to_full <- function (vect) {
  for (i in 1:100) { # find the size of the matrix
    if (length(vect) == add_recurs(0,0,i)) {
      nr = i    
    }
  }
  add_recurs <- function(x, n, max) {
    x <- x + n
    n <- n + 1
    if (n <= max) {
      x <- add_recurs(x, n, max)
    }
    x
  }
  k_given_i_j <- function(x , y ) ifelse( y<x, x*(x-1)/2 + y, y*(y-1)/2 + x )
  k_mat <- function(p) outer( 1:p, 1:p, k_given_i_j )
  return (matrix(vect[ k_mat( nr ) ] , nrow = nr ))
}

