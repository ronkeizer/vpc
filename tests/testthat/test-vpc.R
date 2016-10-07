library(vpc)

context("vpc")

tmp <- simple_data
obj <- vpc(sim = tmp$sim, obs = tmp$obs, vpcdb=TRUE)

test_that("vpc_cat returned proper object", {
  expect_true(all(c("obs", "sim", "aggr_obs", "vpc_dat", "stratify", "bins") %in% names(obj)))
})

test_that("vpc_cat parsed data correctly", {
  expect_equal(sum(obj$vpc_dat$q50.med), 630.3304, tolerance = 0.001)
  expect_equal(sum(obj$vpc_dat$q50.low), 554.338, tolerance = 0.001)
  expect_equal(sum(obj$vpc_dat$q50.up), 708.7439, tolerance = 0.001)
  expect_equal(sum(obj$vpc_dat$q5.med), 296.3193, tolerance = 0.001)
  expect_equal(sum(obj$vpc_dat$q95.med), 1315.188, tolerance = 0.001)
  expect_equal(sum(obj$vpc_dat$bin_mid), 40.75, tolerance = 0.001)
  expect_equal(sum(obj$aggr_obs$obs50), 621.775, tolerance = 0.001)
})  

test_that("vpc_cat plot succeeded", {
  expect_equal(class(plot_vpc(obj)), c("gg", "ggplot"))
})  

## without obs:
obj2 <- vpc(sim = tmp$sim, vpcdb=TRUE)
plot_vpc(obj2)
expect_equal(class(plot_vpc(obj2)), c("gg", "ggplot"))
