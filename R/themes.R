#' A nicer default theme for ggplot2
#'
#' @examples
#' vpc(simple_data$sim, simple_data$obs) + theme_plain()
#' 
#' @export
theme_plain <-  function () {
  ggplot2::theme(
    text = ggplot2::element_text(family="mono"),
    plot.title = ggplot2::element_text(family="sans", size = 16, vjust = 1.5),
    axis.title.x = ggplot2::element_text(family="sans",vjust=-0.25),
    axis.title.y = ggplot2::element_text(family="sans"),
    legend.background = ggplot2::element_rect(fill = "white"),
    #legend.position = c(0.14, 0.80),
    panel.grid.major = ggplot2::element_line(colour = "#e5e5e5"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "#efefef", colour = NA),
    strip.background = ggplot2::element_rect(fill = "#444444", colour = NA),
    strip.text = ggplot2::element_text(face="bold", colour = "white")
  )
}

#' Empty ggplot2 theme
#' 
#' @examples 
#' vpc(simple_data$sim, simple_data$obs) + theme_empty()
#' 
#' @export
theme_empty <- function () {
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black"))
}
