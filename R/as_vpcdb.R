#' Create a vpcdb object, and standardize parameter checking
#' 
#' @param facet either "wrap", "columns", or "rows"
#' @inheritParams ggplot2::facet_grid
#' @param type The type of vpc (e.g. "continuous", "categorical", "censored", or
#'   "time-to-event")
#' @param labeller ggplot2 labeller function to be passed to underlying ggplot object
#' @param ... Extra parameters (not checked) added to the object
#' @return A vpcdb object which is simply a named list with some of the values
#'   checked for correctness
#' @export
as_vpcdb <- function(..., type=NULL, facet=NULL, scales=NULL, labeller=NULL) {
  if (is.null(type)) {
    stop("`type` must be specified")
  }
  if (!is.null(facet)) {
    if(! facet %in% c("wrap", "grid", "columns", "rows")) {
      stop("`facet` argument needs to be one of `wrap`, `columns`, or `rows`.")
    }
    if(facet == "grid") facet <- "rows"
  }
  if (!is.null(scales)) {
    if (! (scales %in% c("fixed", "free_x", "free_y", "free"))) {
      stop("`scales` argument needs to be one of `fixed`, `free_y`, `free_x` or `free`.")
    }
  }
  ret <-
    list(
      ...,
      type=type,
      labeller=labeller,
      facet=facet,
      scales=scales
    )
  specific_class <- gsub(paste("vpcdb", type), pattern="[^A-Za-z]+", replacement="_")
  class(ret) <- c(specific_class, "vpcdb", class(ret))
  ret
}
