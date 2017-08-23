#' Add noise / residual error to data
#'
#' @param x data
#' @param ruv list describing the magnitude of errors. List arguments: "proportional", "additive", "exponential".
#' @export
#' @examples 
#' library(dplyr)
#' ipred <- c(10, 8, 6, 4, 2, 0) %>% add_noise(ruv = list(proportional = 0.1, additive = 0.2))
add_noise <- function(x, ruv = list(proportional = 0, additive = 0, exponential = 0)) {
  if (is.null(ruv$proportional)) { ruv$proportional <- 0 }
  if (is.null(ruv$additive)) { ruv$additive <- 0 }
  if (is.null(ruv$exponential)) { ruv$exponential <- 0 }
  x * (1 + rnorm(length(x), 0, ruv$proportional)) +  rnorm(length(x), 0, ruv$additive) * exp(rnorm(length(x), 0, ruv$exponential))
}
