context("vpc_cens")

obj <- vpc_cens(sim = simple_data$sim, obs = simple_data$obs, lloq = 30, vpcdb = TRUE)

obs50 <- c(0.56, 0.08, 0.00, 0.0, 0.02, 0.14, 0.42, 0.52)
sim50 <- c(0.5, 0.08, 0.00, 0.00, 0.02, 0.18, 0.46, 0.52)
sim5  <- c(0.36, 0.02, 0.00, 0.00, 0.0, 0.10, 0.40, 0.50)
sim95 <- c(0.6, 0.14, 0.01, 0.01, 0.06, 0.26, 0.50, 0.55)

test_that("vpc_cense returned proper object", {
  expect_true(all(c("obs", "sim", "aggr_obs", "vpc_dat", "stratify", "bins") %in% names(obj)))
})

test_that("vpc_cens parsed data correctly", {
  expect_equal(obs50, obj$aggr_obs$obs50, tolerance = 0.001)
  expect_equal(sim50, obj$vpc_dat$q50.med, tolerance = 0.001)
  expect_equal(sim5, obj$vpc_dat$q50.low, tolerance = 0.001)
  expect_equal(sim95, obj$vpc_dat$q50.up, tolerance = 0.001)
})

test_that("vpc_cens plot succeeded", {
  expect_equal(class(plot_vpc(obj)), c("gg", "ggplot"))
})
