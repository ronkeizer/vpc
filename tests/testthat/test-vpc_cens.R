test_that("vpc_cens()", {
  ## Test regular vpc in presence of lloq / uloq data
  obj1 <- vpc(sim = vpc::simple_data$sim, obs = vpc::simple_data$obs, uloq = 120, vpcdb = TRUE)
  obj2 <- vpc(sim = vpc::simple_data$sim, obs = vpc::simple_data$obs, lloq = 20, vpcdb = TRUE)
  expect_equal(
    info="6 values in upper percentile flagged as >uloq",
    sum(is.na(obj1$aggr_obs$obs95)), 6
  )
  expect_equal(
    sum(is.na(obj2$aggr_obs$obs5)), 4,
    info="4 values in upper percentile flagged as <lloq"
  )
  expect_equal(
    sum(is.na(obj2$aggr_obs$obs50)), 1,
    info="1 values in upper percentile flagged as <lloq"
  )
  
  ## test vpc func
  obj <- vpc_cens(sim = vpc::simple_data$sim,
                  obs = vpc::simple_data$obs,
                  lloq = 30, n_bins = 8, vpcdb = TRUE)
  
  #vpc_cens(sim = vpc::simple_data$sim, obs = vpc::simple_data$obs, lloq = 30)
  
  obs50 <- c(0.56, 0.08, 0.00, 0.0, 0.02, 0.14, 0.42, 0.52)
  sim50 <- c(0.48, 0.07, 0.00, 0.00, 0.02, 0.18, 0.46, 0.52)
  sim5  <- c(0.34, 0.02, 0.00, 0.00, 0.0, 0.10, 0.40, 0.5)
  sim95 <- c(0.58, 0.12, 0.01, 0.01, 0.08, 0.26, 0.50, 0.55)
  
  expect_true(
    all(c("obs", "sim", "aggr_obs", "vpc_dat", "stratify", "bins") %in% names(obj)),
    info="vpc_cens returned proper object"
  )
  
  expect_equal(
    obs50, obj$aggr_obs$obs50,
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
  expect_equal(
    sim50, unname(obj$vpc_dat$q50.med),
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
  expect_equal(
    sim5, unname(obj$vpc_dat$q50.low),
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
  expect_equal(
    sim95, unname(obj$vpc_dat$q50.up),
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
})
