library(vpc)
library(testit)
Sys.setenv("R_TESTS" = "")

tmp <- simple_data
cat <- cut(tmp$obs$DV, breaks = c(-1, 10, 40, 1000))
tmp$obs$DV <- match(cat, levels(cat))
cat2 <- cut(tmp$sim$DV, breaks = c(-1, 10, 40, 1000))
tmp$sim$DV <- match(cat2, levels(cat2))

obj <- vpc_cat(sim = tmp$sim, obs = tmp$obs, vpcdb = TRUE)

# plot_vpc(obj)
assert("vpc_cat returned proper object", all(c("obs", "sim", "aggr_obs", "vpc_dat", "stratify", "bins") %in% names(obj)))

assert("vpc_cat parsed data correctly", vpc:::is_equal(sum(obj$vpc_dat$q50.med), 11.02))
assert("vpc_cat parsed data correctly", vpc:::is_equal(sum(obj$vpc_dat$q50.low), 9.597))
assert("vpc_cat parsed data correctly", vpc:::is_equal(sum(obj$vpc_dat$q50.up), 12.383))
assert("vpc_cat parsed data correctly", vpc:::is_equal(sum(obj$vpc_dat$bin_mid), 122.25))
assert("vpc_cat parsed data correctly", vpc:::is_equal(sum(obj$aggr_obs$obs50), 11))

assert("vpc_cat plot succeeded", "ggplot" %in% class(plot_vpc(obj)))
