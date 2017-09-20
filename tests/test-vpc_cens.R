library(vpc)
library(testit)
Sys.setenv("R_TESTS" = "")

## Test regular vpc in presence of lloq / uloq data
obj1 <- vpc(sim = vpc::simple_data$sim, obs = vpc::simple_data$obs, uloq = 120, vpcdb = TRUE)
obj2 <- vpc(sim = vpc::simple_data$sim, obs = vpc::simple_data$obs, lloq = 20, vpcdb = TRUE)
assert("6 values in upper percentile flagged as >uloq", sum(is.na(obj1$aggr_obs$obs95)) == 6)
assert("4 values in upper percentile flagged as <lloq", sum(is.na(obj2$aggr_obs$obs5)) == 4)
assert("1 values in upper percentile flagged as <lloq", sum(is.na(obj2$aggr_obs$obs50)) == 1)

## test vpc func
obj <- vpc_cens(sim = vpc::simple_data$sim, obs = vpc::simple_data$obs, lloq = 30, vpcdb = TRUE)
vpc_cens(sim = vpc::simple_data$sim, obs = vpc::simple_data$obs, lloq = 30)

obs50 <- c(0.56, 0.08, 0.00, 0.0, 0.02, 0.14, 0.42, 0.5, 0.54)
sim50 <- c(0.5, 0.08, 0.00, 0.00, 0.02, 0.18, 0.46, 0.5, 0.54)
sim5  <- c(0.36, 0.02, 0.00, 0.00, 0.0, 0.10, 0.40, 0.48, 0.5)
sim95 <- c(0.6, 0.14, 0.01, 0.01, 0.06, 0.26, 0.50, 0.52, 0.6)

assert("vpc_cense returned proper object", all(c("obs", "sim", "aggr_obs", "vpc_dat", "stratify", "bins") %in% names(obj)))

assert("vpc_cens parsed data correctly", 
  all(obs50 == obj$aggr_obs$obs50) &&
  all(sim50 == obj$vpc_dat$q50.med) &&
  all(sim5 == obj$vpc_dat$q50.low) &&
  all(sim95 == obj$vpc_dat$q50.up)
)
