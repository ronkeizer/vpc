context("vpc_cat")

tmp <- simple_data
cat <- cut(tmp$obs$DV, breaks = c(-1, 10, 40, 1000))
tmp$obs$DV <- match(cat, levels(cat))
cat2 <- cut(tmp$sim$DV, breaks = c(-1, 10, 40, 1000))
tmp$sim$DV <- match(cat2, levels(cat2))
obj <- vpc_cat(sim = tmp$sim, obs = tmp$obs, vpcdb = TRUE)

test_that("vpc_cat returned proper object", {
  expect_true(all(c("obs", "sim", "aggr_obs", "vpc_dat", "stratify", "bins") %in% names(obj)))
})

test_that("vpc_cat parsed data correctly", {
  expect_equal(sum(obj$vpc_dat$q50.med), 11, tolerance = 0.001)
  expect_equal(sum(obj$vpc_dat$q50.low), 9.598, tolerance = 0.001)
  expect_equal(sum(obj$vpc_dat$q50.up), 12.41069, tolerance = 0.001)
  expect_equal(sum(obj$vpc_dat$bin_mid), 122.25, tolerance = 0.001)
  expect_equal(sum(obj$aggr_obs$obs50), 11, tolerance = 0.001)
})  

plot_vpc(obj)
test_that("vpc_cat plot succeeded", {
  expect_equal(class(plot_vpc(obj)), c("gg", "ggplot"))
})  
