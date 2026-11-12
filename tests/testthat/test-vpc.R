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

test_that("missing DV is dropped with a warning and never treated as censored (#80)", {
  obs <- simple_data$obs[simple_data$obs$MDV == 0, ]
  sim <- simple_data$sim[simple_data$sim$MDV == 0, ]
  obs_na <- obs
  obs_na$DV[1:5] <- NA
  # without lloq: previously an error from quantile_cens()
  expect_warning(
    db <- vpc(sim = sim, obs = obs_na, vpcdb = TRUE),
    "5 record\\(s\\) with a missing \\(NA\\) dependent variable removed from observed data"
  )
  expect_equal(nrow(db$obs), nrow(obs) - 5)
  db_ref <- vpc(sim = sim, obs = obs[-(1:5), ], vpcdb = TRUE)
  expect_equal(db$aggr_obs, db_ref$aggr_obs)

  # with lloq: NA is not counted as censored, same result as dropping the rows
  expect_warning(
    db <- vpc(sim = sim, obs = obs_na, lloq = 20, vpcdb = TRUE),
    "removed from observed data"
  )
  db_ref <- vpc(sim = sim, obs = obs[-(1:5), ], lloq = 20, vpcdb = TRUE)
  expect_equal(db$aggr_obs, db_ref$aggr_obs)

  # missing simulated DV
  sim_na <- sim
  sim_na$DV[1:3] <- NA
  expect_warning(
    vpc(sim = sim_na, obs = obs, vpcdb = TRUE),
    "3 record\\(s\\) with a missing \\(NA\\) dependent variable removed from simulated data"
  )
})

test_that("censored values are returned as NA in the vpcdb (#80)", {
  obs <- simple_data$obs[simple_data$obs$MDV == 0, ]
  sim <- simple_data$sim[simple_data$sim$MDV == 0, ]
  db <- vpc(sim = sim, obs = obs, lloq = 20, vpcdb = TRUE)
  expect_false(any(is.infinite(db$obs$dv)))
  expect_equal(is.na(db$obs$dv), obs$DV < 20)
  # log_y floor does not overwrite censored values
  db_log <- vpc(sim = sim, obs = obs, lloq = 20, log_y = TRUE, log_y_min = 25, vpcdb = TRUE)
  expect_equal(is.na(db_log$obs$dv), obs$DV < 20)
  expect_true(all(db_log$obs$dv[!is.na(db_log$obs$dv)] >= 25))
  # pcVPC
  db_pc <- suppressWarnings(vpc(sim = sim, obs = obs, lloq = 20, pred_corr = TRUE, vpcdb = TRUE))
  expect_false(any(is.infinite(db_pc$obs$dv)))
  expect_false(any(is.infinite(db_pc$sim$dv)))
  expect_equal(is.na(db_pc$obs$dv), obs$DV < 20)
  # vpc_cens
  db_cens <- vpc_cens(sim = sim, obs = obs, lloq = 20, vpcdb = TRUE)
  expect_equal(is.na(db_cens$obs$dv), obs$DV < 20)
})
