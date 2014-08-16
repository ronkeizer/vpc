#' VPC package
#'
#' Create Visual Predictive Checks from within R, either from a specified model and parameter distributions, or from simulated data.
#'
#' @docType package
#' @name vpc
#' @author Ron Keizer \email{ronkeizer@@gmail.com}
#' @examples
#' ## Make a VPC for the classic Theophylline dataset and model
#' obs <- Theoph
#' colnames(obs) <- c("id", "wt", "dose", "time", "dv")
#' obs$sex <- round(runif(unique(obs$id))) # create a dummy covariate to show stratification
#' 
#' sim <- sim_data(obs,                                # the design of the dataset
#'                 model = function(x, par) {          # the model
#'                   pk_oral_1cmt (t = x$time, dose=x$dose * x$wt, ka = par$ka, ke = par$ke, cl = par$cl * x$wt, ruv = list(additive = 0.1))
#'                 }, 
#'                 theta = c(2.774, 0.0718, .0361),    # parameter values
#'                 omega_mat = c(0.08854,              # specified as lower triangle by default; 
#'                               0.02421, 0.02241,     # note: assumed that every theta has iiv, set to 0 if no iiv. 
#'                               0.008069, 0.008639, 0.02862),      
#'                 par_names = c("ka", "ke", "cl"),    # link the parameters in the model to the thetas/omegas
#'                 n = 500)
#' 
#' vpc_dat <- plot_vpc(sim, obs, 
#'                     bins = c(0, 2, 4, 6, 8, 10, 25), 
#'                     strat = "sex",
#'                     ylab = "Concentration", 
#'                     xlab = "Time (hrs)")
NULL
