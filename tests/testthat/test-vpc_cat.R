test_that("vpc_cat()", {
  tmp <- simple_data
  cat <- cut(tmp$obs$DV, breaks = c(-1, 10, 40, 1000))
  tmp$obs$DV <- match(cat, levels(cat))
  cat2 <- cut(tmp$sim$DV, breaks = c(-1, 10, 40, 1000))
  tmp$sim$DV <- match(cat2, levels(cat2))

  obj <- vpc_cat(sim = tmp$sim, obs = tmp$obs, vpcdb = TRUE)

  # plot_vpc(obj)
  expect_true(
    all(c("obs", "sim", "aggr_obs", "vpc_dat", "stratify", "bins") %in% names(obj)),
    info="vpc_cat returned proper object"
  )
  expect_equal(
    sum(obj$vpc_dat$q50.med),
    11.02,
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
  expect_equal(
    sum(obj$vpc_dat$q50.low), 9.597,
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
  expect_equal(
    sum(obj$vpc_dat$q50.up), 12.383,
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
  expect_equal(
    sum(obj$vpc_dat$bin_mid), 122.25,
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
  expect_equal(
    sum(obj$aggr_obs$obs50), 11,
    tolerance=1e-5,
    info="vpc parsed data correctly"
  )
  expect_s3_class(
    plot_vpc(obj), "ggplot"
    #"vpc_cat plot succeeded"
  )
})
