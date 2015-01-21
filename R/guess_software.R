#' guess software package based on data
#' @param software string specifying type of software
#' @param x analysis data from software
#' @export
guess_software <- function(software, x) {
  return("nonmem")
}