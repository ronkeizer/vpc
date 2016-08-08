#' Create new vpc theme
#' 
#' @param ... pass arguments to `new_vpc_theme`
#' @export
create_vpc_theme <- function(...) {
  message("Note: the `create_vpc_theme()` function has been renamed to `new_vpc_theme()`. Please update your scripts, `create_vpc_theme()` will be deprecated in future releases.")
  return(new_vpc_theme(...))
}