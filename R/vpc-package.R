#' VPC package
#'
#' Create Visual Predictive Checks in R
#'
#' @docType package
#' @name vpc-package
#' @author Ron Keizer \email{ronkeizer@@gmail.com}
#' @importFrom stats density median model.matrix na.omit quantile rnorm rweibull step time
#' @importFrom utils head read.table tail

## to avoid warnings related to dplyr/ggplot usage:
globalVariables(c(".", "comp", "strat", "strat2", "bin", "pred", "dv", "idv", "q5", "q50",
                  "q95", "q5.med", "q5.low", "q5.up", "q50.med", "q50.low", "q50.up",
                  "q95.med", "q95.low", "q95.up", "obs.med", "obs.low", "obs.up",
                  "obs5", "obs50", "obs95", "last_obs", "id", 
                  "mn_idv", "value", "mn_idv", "ploq", "bin_mid", "bin_min",
                  "bin_max", "surv", "qmed", "strat_sim", "qmin", "qmax", "strat_color",
                  "y", "lower", "upper", "x", "classIntervals", "idx", "obs", "rtte",
                  "sim"))

NULL
