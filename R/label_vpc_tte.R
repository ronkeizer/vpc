#' Custom ggplot2 labeller function.
#'
#' Slight rewrite of ggplot2::label_both, to make sure that labels
#' for events are ordered appropriately when doing facet_wrap.
#'
#' @param labels Data frame of labels. Usually contains only one element, but faceting over multiple factors entails multiple label variables.
#' @param multi_line Whether to display the labels of multiple factors on separate lines.
#' @param sep String separating variables and values.
#' 
#' @importFrom ggplot2 label_value
label_vpc_tte <- function (labels, multi_line = TRUE, sep = ": ") {
  value <- ggplot2::label_value(labels, multi_line = multi_line)
  variable <- ggplot2_label_variable(labels, multi_line = multi_line)
  if (multi_line) {
    out <- vector("list", length(value))
    for (i in seq_along(out)) {
      tmp <- as.numeric(value[[i]])
      value[[i]] <- as.character(tmp[order(tmp)])
      out[[i]] <- paste(variable[[i]], value[[i]], sep = sep)
    }
  }
  else {
    value <- do.call("paste", c(value, sep = ", "))
    variable <- do.call("paste", c(variable, sep = ", "))
    out <- Map(paste, variable, value, sep = sep)
    out <- list(unname(unlist(out)))
  }
  out
}

#' Function copied from ggplot2:::label_variable, with addition of `%||%`. 
#' Copied here since CRAN does not like use of `:::`.
#' 
#' @inheritParams label_vpc_tte
#' 
ggplot2_label_variable <- function (labels, multi_line = TRUE) {
  `%||%` <- function (a, b) {
    if (!is.null(a)) 
      a
    else b
  }
  if (multi_line) {
    row <- as.list(names(labels))
  }
  else {
    row <- list(paste(names(labels), collapse = ", "))
  }
  lapply(row, rep, nrow(labels) %||% length(labels[[1]]))
}