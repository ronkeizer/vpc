`%>%` <- dplyr::`%>%`

is_equal <- function(test, ref, tol = 1e-3, relative=TRUE) {
  if(relative) {
    res <- abs((test - ref) / ref) < tol
  } else {
    res <- abs(test - ref) < tol
  }
  if(any(!res)) message(paste0("Test: ", test, " / Ref: ", ref, "\n"))
  return(all(res))
}
