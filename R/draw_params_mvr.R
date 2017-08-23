#' Draw parameters from multivariate distribution
#' 
#' @param ids vector of id numbers
#' @param n_sim number of simulations
#' @param theta theta vector
#' @param omega_mat omega matrix
#' @param par_names parameter names vector
draw_params_mvr <- function(ids, n_sim, theta, omega_mat, par_names = NULL) {
  n_ids <- length(ids)
  if (!is.null(par_names)) {
    par <- data.frame(cbind(sim = rep(1:n_sim, each=n_ids),
                            id = rep(ids, n_sim),
                            rep(theta, each=n_sim*n_ids) * exp (MASS::mvrnorm(n=n_sim*n_ids, c(0,0,0), omega_mat))))
    colnames(par) <- c("sim", "id", par_names)
    return(par)
  } else {
    cat("Parameter names have to be supplied!")
  }
}
