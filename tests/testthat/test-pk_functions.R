test_that("pk_oral_1cmt follows the analytical 1-compartment oral model", {
  t <- c(0, 0.5, 1, 2, 4, 8, 24)
  dose <- 100; ka <- 1.5; ke <- 0.2; cl <- 4
  v <- cl / ke
  conc <- pk_oral_1cmt(t, dose = dose, ka = ka, ke = ke, cl = cl)
  expect_equal(conc, (dose / v) * (ka / (ka - ke)) * (exp(-ke * t) - exp(-ka * t)))
  expect_equal(conc[1], 0)
  # exposure (AUC0-inf) equals dose / clearance
  auc <- integrate(function(x) pk_oral_1cmt(x, dose = dose, ka = ka, ke = ke, cl = cl), 0, Inf)$value
  expect_equal(auc, dose / cl, tolerance = 1e-4)
})

test_that("pk_oral_1cmt handles equal absorption and elimination rates", {
  # the default ka = ke = 1 should not produce NaN
  t <- c(0, 1, 2, 6)
  conc <- pk_oral_1cmt(t, dose = 120, ka = 1, ke = 1, cl = 10)
  v <- 10 / 1
  expect_false(anyNA(conc))
  expect_equal(conc, (120 / v) * 1 * t * exp(-1 * t))
  # continuous with the general solution for ka close to ke
  expect_equal(conc, pk_oral_1cmt(t, dose = 120, ka = 1 + 1e-6, ke = 1, cl = 10), tolerance = 1e-5)
})

test_that("pk_iv_1cmt follows the analytical 1-compartment infusion model", {
  dose <- 100; CL <- 5; Vc <- 50; t_inf <- 2
  k <- CL / Vc
  t <- c(0, 1, 2, 4, 12)
  conc <- pk_iv_1cmt(t, t_inf = t_inf, dose = dose, CL = CL, Vc = Vc)
  expect_equal(conc[1], 0)
  c_end <- dose / (CL * t_inf) * (1 - exp(-k * t_inf))
  expect_equal(conc[3], c_end)
  expect_equal(conc[4:5], c_end * exp(-k * (t[4:5] - t_inf)))
  # concentration rises during, and declines after the infusion
  expect_true(all(diff(conc[1:3]) > 0))
  expect_true(all(diff(conc[3:5]) < 0))
  # exposure (AUC0-inf) equals dose / clearance
  auc <- integrate(function(x) pk_iv_1cmt(x, t_inf = t_inf, dose = dose, CL = CL, Vc = Vc), 0, Inf)$value
  expect_equal(auc, dose / CL, tolerance = 1e-4)
})

test_that("pk_iv_1cmt returns concentrations in the order of the supplied times", {
  t <- c(12, 0, 4, 1)
  conc <- pk_iv_1cmt(t, t_inf = 2, dose = 100, CL = 5, Vc = 50)
  ref <- sapply(t, function(x) pk_iv_1cmt(x, t_inf = 2, dose = 100, CL = 5, Vc = 50))
  expect_equal(conc, ref)
})

test_that("pk functions add residual variability when requested", {
  t <- 1:24
  ipred <- pk_oral_1cmt(t, ka = 1, ke = 0.1)
  set.seed(12)
  dv <- pk_oral_1cmt(t, ka = 1, ke = 0.1, ruv = list(proportional = 0.1, additive = 0.1))
  expect_length(dv, length(t))
  expect_false(isTRUE(all.equal(dv, ipred)))
  set.seed(12)
  dv_iv <- pk_iv_1cmt(t, ruv = list(proportional = 0.1))
  expect_length(dv_iv, length(t))
  expect_false(isTRUE(all.equal(dv_iv, pk_iv_1cmt(t))))
})

test_that("add_noise adds the specified type and magnitude of error", {
  x <- rep(100, 20000)
  expect_equal(add_noise(x, ruv = list()), x)
  set.seed(1)
  prop <- add_noise(x, ruv = list(proportional = 0.1))
  expect_equal(sd(prop) / mean(prop), 0.1, tolerance = 0.05)
  set.seed(1)
  add <- add_noise(x, ruv = list(additive = 2))
  expect_equal(mean(add), 100, tolerance = 0.01)
  expect_equal(sd(add), 2, tolerance = 0.05)
})
