library(vpc)
library(testit)
Sys.setenv("R_TESTS" = "")

tmp <- simple_data
obj <- vpc(sim = tmp$sim, obs = tmp$obs, vpcdb=TRUE)

assert("vpc returned proper object",
   all(c("obs", "sim", "aggr_obs", "vpc_dat", "stratify", "bins") %in% names(obj))
)

# print(round(sum(obj$vpc_dat$q50.med),2))
# print(round(sum(obj$vpc_dat$q50.low),2))
# print(round(sum(obj$vpc_dat$q5.med),2))
# print(round(sum(obj$vpc_dat$q95.med),2))
# print(round(sum(obj$vpc_dat$bin_mid),2))
# print(round(sum(obj$aggr_obs$obs50),2))
assert("vpc parsed data correctly", vpc:::is_equal(round(sum(obj$vpc_dat$q50.med),2), 636.98))
assert("vpc parsed data correctly", vpc:::is_equal(round(sum(obj$vpc_dat$q50.low),2), 556.89))
assert("vpc parsed data correctly", vpc:::is_equal(round(sum(obj$vpc_dat$q5.med),2), 297.14))
assert("vpc parsed data correctly", vpc:::is_equal(round(sum(obj$vpc_dat$q95.med),2), 1324.86))
assert("vpc parsed data correctly", vpc:::is_equal(round(sum(obj$vpc_dat$bin_mid),2), 40.75))
assert("vpc parsed data correctly", vpc:::is_equal(round(sum(obj$aggr_obs$obs50),2), 621.77))

assert("vpc plot succeeded", "ggplot" %in% class(plot_vpc(obj)))

## without obs:
obj2 <- vpc(sim = tmp$sim, vpcdb=TRUE)
assert("vpc plot succeeded", "ggplot" %in% class(plot_vpc(obj2)))
