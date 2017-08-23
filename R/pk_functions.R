#' Simulate PK data from a 1-compartment oral model
#' 
#' @param t Time after dose
#' @param tau Dosing interval
#' @param dose Dose
#' @param ka Absorption rate
#' @param ke Elimination rate
#' @param cl Clearance
#' @param ruv Residual variability
#' @return A vector of predicted values, with or without added residual variability
#' @examples 
#' dat1 <- vpc:::pk_oral_1cmt(t = c(0:72), tau = 24, dose = 120, 
#'                      ka = 1, ke = 1, cl = 10)
#' dat2 <- vpc:::pk_oral_1cmt(t = c(0:72), tau = 24, dose = 120, 
#'                      ka = 1, ke = 1, cl = 10, 
#'                      ruv = list(proportional = 0.1, additive = 0.1))
pk_oral_1cmt <- function (t, tau = 24, dose=120, ka = 1, ke = 1, cl = 10, ruv = NULL) {
  v = cl / ke
  tmp <- (dose/v) * (ka/(ka-ke)) * (exp(-ke*t) - exp(-ka*(t)))
  if(!is.null(ruv)) {
    tmp <- add_noise (tmp, ruv)
  }
  tmp
}

#' Simulate PK data from a 1-compartment iv model
#' 
#' @param t Time after dose
#' @param t_inf Infusion length
#' @param tau Dosing interval
#' @param dose Dose
#' @param CL Clearance
#' @param Vc Volume of distribution
#' @param ruv Residual variability
#' @return A vector of predicted values, with or without added residual variability
#' @examples
#' dat1 <- vpc:::pk_iv_1cmt(t = c(0:72), tau = 24, dose = 120, 
#'                    CL = 5, Vc = 50)
#' dat2 <- vpc:::pk_iv_1cmt(t = c(0:72), tau = 24, dose = 120, 
#'                    CL = 5, Vc = 50, 
#'                    ruv = list(proportional = 0.1, additive = 0.1))
pk_iv_1cmt <- function (t, t_inf = 1, tau = 24, dose=120, CL = 0.345, Vc = 1.75, ruv = NULL) {
  k <- CL / Vc
  tmp <- c()
  tmp <- c(tmp, (dose / (CL * t_inf)) * (1-exp(-k*t[t < t_inf])) )
  tmp <- c(tmp, (dose / (CL * t_inf)) * (1-exp(-k*t_inf)) * exp(-k*(t[t >= t_inf] - t_inf)) )  
  if(!is.null(ruv)) {
    tmp <- add_noise (tmp, ruv)
  }
  tmp
}