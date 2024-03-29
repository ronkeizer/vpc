test_that("vpc for continuous data works", {
  tmp <- simple_data
  obj <- vpc(sim = tmp$sim, obs = tmp$obs, vpcdb=TRUE)

  expect_true(
    all(c("obs", "sim", "aggr_obs", "vpc_dat", "stratify", "bins") %in% names(obj)),
    info="vpc returned proper object"
  )

# print(round(sum(obj$vpc_dat$q50.med),2))
# print(round(sum(obj$vpc_dat$q50.low),2))
# print(round(sum(obj$vpc_dat$q5.med),2))
# print(round(sum(obj$vpc_dat$q95.med),2))
# print(round(sum(obj$vpc_dat$bin_mid),2))
# print(round(sum(obj$aggr_obs$obs50),2))
  expect_equal(
    sum(obj$vpc_dat$q50.med),
    636.98,
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
  expect_equal(
    sum(obj$vpc_dat$q50.med),
    636.98,
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
  expect_equal(
    sum(obj$vpc_dat$q50.low),
    556.89,
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
  expect_equal(
    sum(obj$vpc_dat$q5.med),
    297.14,
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
  expect_equal(
    sum(obj$vpc_dat$q95.med),
    1324.86,
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
  expect_equal(
    sum(obj$vpc_dat$bin_mid),
    40.75,
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
  expect_equal(
    sum(obj$aggr_obs$obs50),
    621.77,
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
  expect_s3_class(
    plot_vpc(obj), "ggplot"
    # vpc plot succeeded
  )

  ## without obs:
  obj2 <- vpc(sim = tmp$sim, vpcdb=TRUE)
  expect_s3_class(
    plot_vpc(obj2), "ggplot"
    # "vpc plot succeeded"
  )
})

test_that("vpc for continuous data with pred-correction and censored data works", {
  tmp <- simple_data
  tmp$sim <- tmp$sim %>%
    dplyr::mutate(DV = IPRED)
  obj1 <- vpc(sim = tmp$sim, obs = tmp$obs, vpcdb=TRUE)
  obj2 <- vpc(sim = tmp$sim, obs = tmp$obs, lloq = 1.1, vpcdb=TRUE)
  expect_equal(as.numeric(tail(obj2$aggr_obs,1)), c(1, 11, NA_real_, 13.375, 67.56, 12, 10, 12.5875))
})
