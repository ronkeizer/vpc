#' VPC package
#'
#' Create Visual Predictive Checks in R
#'
#' @docType package
#' @name vpc-package
#' @import MASS ggplot2 survival dplyr
#' @author Ron Keizer \email{ronkeizer@@gmail.com}
#' @importFrom stats density median model.matrix na.omit quantile rnorm rweibull step time
#' @importFrom utils head read.table tail
#' @importFrom magrittr %>%

## to avoid warnings related to dplyr/ggplot usage:
globalVariables(c(".", "comp", "strat", "bin", "pred", "dv", "idv", "q5", "q50", 
                  "q95", "mn_idv", "value", "mn_idv", "ploq", "bin_mid", "bin_min",
                  "bin_max", "surv", "qmed", "strat_sim", "qmin", "qmax", "strat_color",
                  "y", "lower", "upper", "x", "classIntervals", "idx", "obs", "rtte",
                  "sim"))

NULL
