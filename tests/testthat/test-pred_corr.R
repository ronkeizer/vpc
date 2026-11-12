test_that("multiplicative prediction-correction matches hand calculation", {
  dat <- data.frame(
    strat = 1, bin = 1,
    dv = c(10, 20, 30),
    PRED = c(5, 10, 20)
  )
  res <- pred_correction_core(dat, "PRED", pred_corr_lower_bnd = 0)
  # median(PRED) = 10, so pcDV = DV * 10 / PRED
  expect_equal(res$dv, c(20, 20, 15))
  res_lb <- pred_correction_core(dat, "PRED", pred_corr_lower_bnd = 2)
  expect_equal(res_lb$dv, 2 + (c(10, 20, 30) - 2) * (10 - 2) / (c(5, 10, 20) - 2))
})

test_that("additive (LTBS) prediction-correction matches hand-computed Bergstrand example", {
  # log-scale data, two bins; includes negative and zero log-predictions
  dat <- data.frame(
    strat = 1,
    bin = c(1, 1, 1, 2, 2, 2, 2),
    dv = c(-0.5, 0.2, 1.1, 2.0, 2.5, 3.1, NA),
    PRED = c(-0.4, 0, 0.9, 1.8, 2.2, 2.6, 3.0)
  )
  res <- pred_correction_core(dat, "PRED", pred_corr_method = "additive")
  # bin 1: median(PRED) = 0   -> pcDV = DV + (0 - PRED)
  # bin 2: median(PRED) = 2.4 -> pcDV = DV + (2.4 - PRED)
  expected <- c(
    -0.5 + (0 - -0.4),
     0.2 + (0 - 0),
     1.1 + (0 - 0.9),
     2.0 + (2.4 - 1.8),
     2.5 + (2.4 - 2.2),
     3.1 + (2.4 - 2.6),
     NA
  )
  expect_equal(res$dv, expected)
  # pred_corr_lower_bnd is ignored by the additive form
  res_lb <- pred_correction_core(dat, "PRED", pred_corr_lower_bnd = 1, pred_corr_method = "additive")
  expect_equal(res_lb$dv, expected)
})

test_that("additive correction on log scale equals log of multiplicative correction on linear scale when median is preserved", {
  # With an odd number of rows per bin, median(log(PRED)) == log(median(PRED)),
  # so exp(additive on log scale) == multiplicative on linear scale.
  dat <- data.frame(
    strat = 1, bin = rep(1:2, each = 3),
    dv = c(1.5, 3, 8, 12, 20, 40),
    PRED = c(1, 2, 6, 10, 15, 30)
  )
  lin <- pred_correction_core(dat, "PRED", pred_corr_method = "multiplicative")
  logdat <- dat
  logdat$dv <- log(logdat$dv)
  logdat$PRED <- log(logdat$PRED)
  lg <- pred_correction_core(logdat, "PRED", pred_corr_method = "additive")
  expect_equal(exp(lg$dv), lin$dv)
})

test_that("define_pred_corr_method resolves ltbs alias and checks arguments", {
  expect_equal(define_pred_corr_method(), "multiplicative")
  expect_equal(define_pred_corr_method("additive"), "additive")
  expect_equal(define_pred_corr_method(ltbs = TRUE), "additive")
  expect_equal(define_pred_corr_method(c("multiplicative", "additive"), ltbs = TRUE), "additive")
  expect_error(define_pred_corr_method("multiplicative", ltbs = TRUE), "ltbs")
  expect_error(define_pred_corr_method(ltbs = NA), "ltbs")
  expect_error(define_pred_corr_method("foo"))
  expect_warning(
    define_pred_corr_method("additive", pred_corr = TRUE, pred_corr_lower_bnd = 1),
    "pred_corr_lower_bnd"
  )
  expect_error(
    define_pred_corr_method("additive", pred_corr = TRUE, log_y = TRUE),
    "log_y"
  )
  # no checks needed when pred_corr is not performed
  expect_equal(define_pred_corr_method("additive", pred_corr = FALSE, log_y = TRUE), "additive")
})

test_that("vpc applies additive (LTBS) correction to both observed and simulated data", {
  obs <- simple_data$obs
  sim <- simple_data$sim
  obs <- obs[obs$MDV == 0, ]
  sim <- sim[sim$MDV == 0, ]
  # LTBS: dv and pred are supplied on the log scale
  obs$DV <- log(obs$DV)
  sim$DV <- log(sim$DV)
  sim$PRED <- log(sim$PRED)
  # observed PRED from first replicate (rows align with obs)
  obs$PRED <- sim$PRED[sim$REP == 1]

  db_add <- vpc(sim = sim, obs = obs, pred_corr = TRUE,
                pred_corr_method = "additive", vpcdb = TRUE)
  db_ltbs <- vpc(sim = sim, obs = obs, pred_corr = TRUE,
                 ltbs = TRUE, vpcdb = TRUE)
  db_none <- vpc(sim = sim, obs = obs, pred_corr = FALSE, vpcdb = TRUE)
  expect_equal(db_add$vpc_dat, db_ltbs$vpc_dat)
  expect_equal(db_add$aggr_obs, db_ltbs$aggr_obs)

  # observed: check against hand calculation
  o <- db_add$obs
  expected_obs <- db_none$obs$dv + (o$pred_bin - o$pred)
  expect_equal(o$dv, expected_obs)
  # simulated: check against hand calculation
  s <- db_add$sim
  expected_sim <- db_none$sim$dv + (s$pred_bin - s$pred)
  expect_equal(s$dv, expected_sim)

  # differs from multiplicative correction
  db_mult <- vpc(sim = sim, obs = obs, pred_corr = TRUE, vpcdb = TRUE)
  expect_false(isTRUE(all.equal(db_add$vpc_dat, db_mult$vpc_dat)))

  expect_error(
    vpc(sim = sim, obs = obs, pred_corr = TRUE, ltbs = TRUE, log_y = TRUE, vpcdb = TRUE),
    "log_y"
  )
})

test_that("calc_vpc_continuous treats censored values consistently for obs and sim with pred_corr", {
  loq <- define_loq(lloq = 1, require_loq = FALSE)
  # censored values are NA (determined on uncorrected scale); corrected values
  # may legitimately be below the lloq (0.5) and must not be censored again
  obs <- data.frame(strat = 1, bin = 1, idv = 1:5, dv = c(NA, NA, 0.5, 3, 4))
  sim <- data.frame(
    strat = 1, bin = 1, idv = rep(1:5, 2), sim = rep(1:2, each = 5),
    dv = c(NA, NA, 0.5, 3, 4, NA, NA, 0.5, 3, 4)
  )
  res <- calc_vpc_continuous(
    sim = sim, obs = obs, loq = loq, pi = c(0.05, 0.95), ci = c(0.05, 0.95),
    stratify = NULL, bins = c(0, 10), bin_mid = "mean", verbose = FALSE,
    pred_corr = TRUE
  )
  # median of c(-Inf, -Inf, 0.5, 3, 4) is 0.5 (not censored again)
  expect_equal(res$aggr_obs$obs50, 0.5)
  expect_true(is.na(res$aggr_obs$obs5))
  expect_equal(res$aggr_obs$obs95, quantile(c(-Inf, -Inf, 0.5, 3, 4), 0.95)[[1]])
  # simulated data get the same treatment (censored values not dropped)
  expect_equal(res$vpc_dat$q50.med, res$aggr_obs$obs50)
  expect_true(is.na(res$vpc_dat$q5.med))
  expect_equal(res$vpc_dat$q95.med, res$aggr_obs$obs95)

  # without pred_corr, behavior is unchanged: obs values < lloq are censored,
  # sim NAs are dropped
  res_npc <- calc_vpc_continuous(
    sim = sim, obs = obs, loq = loq, pi = c(0.05, 0.95), ci = c(0.05, 0.95),
    stratify = NULL, bins = c(0, 10), bin_mid = "mean", verbose = FALSE
  )
  expect_true(is.na(res_npc$aggr_obs$obs50))
  expect_equal(unname(res_npc$vpc_dat$q50.med), 3)
})

test_that("vpc with pred_corr and lloq uses the same censoring for obs and sim", {
  obs <- simple_data$obs[simple_data$obs$MDV == 0, ]
  sim <- simple_data$sim[simple_data$sim$MDV == 0, ]
  obs$PRED <- sim$PRED[sim$REP == 1]
  # use first simulated replicate as "observed" data: obs and sim statistics
  # must then be computed identically
  obs$DV <- sim$DV[sim$REP == 1]
  sim1 <- sim[sim$REP == 1, ]
  expect_warning(
    db <- vpc(sim = sim1, obs = obs, pred_corr = TRUE, lloq = 5, vpcdb = TRUE),
    "Censoring is determined on the uncorrected"
  )
  expect_true(any(is.na(db$obs$dv)))
  expect_equal(db$vpc_dat$q5.med, db$aggr_obs$obs5)
  expect_equal(db$vpc_dat$q50.med, db$aggr_obs$obs50)
  expect_equal(db$vpc_dat$q95.med, db$aggr_obs$obs95)
})
