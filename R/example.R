## VPC example
design <- data.frame(cbind(id=rep(1:5, each=11), time=c(0:10), cov=runif(55)))
obs <- sim_data(design, model, par, n=1)

##
model <- function(x, par) {
  ids <- unique(x$id)
  y_hat <- 50*exp(-abs(par$a)*x$time) 
  y <- y_hat * exp(rnorm(length(y_hat), 0, .1)) 
}
par <- list(a = rnorm(length(design$id), 0, 0.3), 
            b = rnorm(length(design$id), 0, 0.5))
sim <- sim_data(design, model, par, n=100)
vpc(sim, obs, bins = c(0,2,4,6,8,10), dv="sdv", idv="time")
vpc_loq(sim, obs, bins = c(0,2,4,6,8,10), dv="sdv", idv="time", type="uloq", uloq=30)
