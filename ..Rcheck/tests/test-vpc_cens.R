library(vpc)
library(testit)
Sys.setenv("R_TESTS" = "")

obj <- vpc_cens(sim = vpc::simple_data$sim, obs = vpc::simple_data$obs, lloq = 30, vpcdb = TRUE)

obs50 <- c(0.56, 0.08, 0.00, 0.0, 0.02, 0.14, 0.42, 0.52)
sim50 <- c(0.5, 0.08, 0.00, 0.00, 0.02, 0.18, 0.46, 0.52)
sim5  <- c(0.36, 0.02, 0.00, 0.00, 0.0, 0.10, 0.40, 0.50)
sim95 <- c(0.6, 0.14, 0.01, 0.01, 0.06, 0.26, 0.50, 0.55)

assert("vpc_cense returned proper object", all(c("obs", "sim", "aggr_obs", "vpc_dat", "stratify", "bins") %in% names(obj)))

assert("vpc_cens parsed data correctly", 
  all(
    obs50 == obj$aggr_obs$obs50 &&
    sim50 == obj$vpc_dat$q50.med &&
    sim5 == obj$vpc_dat$q50.low &&
    sim95 == obj$vpc_dat$q50.up)
)
