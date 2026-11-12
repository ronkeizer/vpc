## Reference-corrected VPC (rcVPC), issue #105
## Anchor values generated with Pharmpy's PsN rcVPC implementation, see
## fixtures/refcorr/README.md

read_refcorr_fixture <- function(file) {
  utils::read.csv(test_path("fixtures", "refcorr", file))
}

refcorr_fixture <- function() {
  obs <- read_refcorr_fixture("obs.csv")
  sim <- read_refcorr_fixture("sim.csv")
  ref <- read_refcorr_fixture("sim_ref.csv")
  exp <- read_refcorr_fixture("expected_pharmpy.csv")
  list(obs = obs, sim = sim, ref = ref, exp = exp)
}

core_inputs <- function(fx, f = identity) {
  n_rec <- nrow(fx$obs)
  list(
    sim_dv   = matrix(f(fx$sim$dv), nrow = n_rec),
    sim_pred = matrix(f(fx$sim$pred), nrow = n_rec),
    ref_dv   = matrix(f(fx$ref$dv), nrow = n_rec),
    pred_ref = f(fx$obs$predref),
    obs_dv   = f(fx$obs$dv),
    obs_pred = f(fx$obs$pred)
  )
}

expected_case <- function(fx, case) {
  e <- fx$exp[fx$exp$case == case, ]
  n_rec <- nrow(fx$obs)
  list(
    obs = e$rc_dv[e$sim == 0][order(e$rec[e$sim == 0])],
    sim = matrix(e$rc_dv[e$sim > 0][order(e$sim[e$sim > 0], e$rec[e$sim > 0])], nrow = n_rec)
  )
}

test_that("ref_correction_core matches Pharmpy (normal scale, lb = 0)", {
  fx <- refcorr_fixture()
  res <- do.call(ref_correction_core, c(core_inputs(fx), lower_bnd = 0, log_dv = FALSE))
  exp <- expected_case(fx, "normal_lb0")
  expect_equal(res$obs, exp$obs, tolerance = 1e-8)
  expect_equal(res$sim, exp$sim, tolerance = 1e-8)
})

test_that("ref_correction_core matches Pharmpy (normal scale, lb = -0.5)", {
  fx <- refcorr_fixture()
  res <- do.call(ref_correction_core, c(core_inputs(fx), lower_bnd = -0.5, log_dv = FALSE))
  exp <- expected_case(fx, "normal_lbneg")
  expect_equal(res$obs, exp$obs, tolerance = 1e-8)
  expect_equal(res$sim, exp$sim, tolerance = 1e-8)
})

test_that("ref_correction_core matches Pharmpy (log scale)", {
  fx <- refcorr_fixture()
  res <- do.call(ref_correction_core, c(core_inputs(fx, log), lower_bnd = 0, log_dv = TRUE))
  exp <- expected_case(fx, "log")
  expect_equal(res$obs, exp$obs, tolerance = 1e-8)
  expect_equal(res$sim, exp$sim, tolerance = 1e-8)
})

test_that("ref_correction_core handles PRED == lb and zero SD", {
  sim_dv <- matrix(c(1, 2, 1, 3), nrow = 2)
  ref_dv <- matrix(c(1, 2, 1, 2), nrow = 2)  # record 1: zero SD
  res <- ref_correction_core(
    sim_dv = sim_dv, sim_pred = matrix(c(0, 2, 0, 2), nrow = 2),
    ref_dv = ref_dv, pred_ref = c(1, 2),
    obs_dv = c(1, 2.5), obs_pred = c(0, 2)
  )
  expect_true(all(is.finite(res$sim)))
  expect_true(all(is.finite(res$obs)))
  expect_equal(res$sim[1, ], c(1, 1))  # factor 1, var factor 1
})

## End-to-end through vpc()
refcorr_vpc_data <- function() {
  fx <- refcorr_fixture()
  names(fx$obs)[names(fx$obs) == "pred"] <- "PRED"
  fx$sim$PRED <- fx$sim$pred
  fx$ref$PRED <- fx$ref$pred
  fx$obs_cols <- list(id = "id", dv = "dv", idv = "time", pred = "PRED")
  fx$sim_cols <- c(fx$obs_cols, sim = "sim")
  fx
}

test_that("vpc() with sim_ref matches Pharmpy end-to-end", {
  fx <- refcorr_vpc_data()
  obj <- vpc(
    sim = fx$sim, obs = fx$obs, sim_ref = fx$ref,
    obs_cols = fx$obs_cols, sim_cols = fx$sim_cols,
    software = "nonmem", vpcdb = TRUE
  )
  exp <- expected_case(fx, "normal_lb0")
  expect_equal(obj$obs$dv[order(obj$obs$rec)], exp$obs, tolerance = 1e-8)
  sim_out <- obj$sim[order(obj$sim$sim, obj$sim$rec), ]
  expect_equal(sim_out$dv, as.vector(exp$sim), tolerance = 1e-8)
  expect_s3_class(plot_vpc(obj), "ggplot")
})

test_that("vpc() with sim_ref and ref_corr_log matches Pharmpy", {
  fx <- refcorr_vpc_data()
  fx$obs$dv <- log(fx$obs$dv); fx$obs$PRED <- log(fx$obs$PRED)
  fx$sim$dv <- log(fx$sim$dv); fx$sim$PRED <- log(fx$sim$PRED)
  fx$ref$dv <- log(fx$ref$dv); fx$ref$PRED <- log(fx$ref$PRED)
  obj <- vpc(
    sim = fx$sim, obs = fx$obs, sim_ref = fx$ref, ref_corr_log = TRUE,
    obs_cols = fx$obs_cols, sim_cols = fx$sim_cols,
    software = "nonmem", vpcdb = TRUE
  )
  exp <- expected_case(fx, "log")
  expect_equal(obj$obs$dv[order(obj$obs$rec)], exp$obs, tolerance = 1e-8)
})

test_that("identity reference leaves data unchanged", {
  # Reference = observed design, using sim itself as reference simulation:
  # PRED_ref == PRED and SD ratio == 1, so DV must be unchanged.
  fx <- refcorr_vpc_data()
  ref <- fx$sim
  obj_rc <- vpc(sim = fx$sim, obs = fx$obs, sim_ref = ref,
                obs_cols = fx$obs_cols, sim_cols = fx$sim_cols, software = "nonmem", vpcdb = TRUE)
  obj <- vpc(sim = fx$sim, obs = fx$obs,
             obs_cols = fx$obs_cols, sim_cols = fx$sim_cols, software = "nonmem", vpcdb = TRUE)
  expect_equal(obj_rc$obs$dv, obj$obs$dv)
  expect_equal(obj_rc$sim$dv, obj$sim$dv)
  expect_equal(obj_rc$vpc_dat, obj$vpc_dat)
})

test_that("vpc() with sim_ref validates input", {
  fx <- refcorr_vpc_data()
  args <- list(sim = fx$sim, obs = fx$obs, obs_cols = fx$obs_cols, sim_cols = fx$sim_cols,
               software = "nonmem", vpcdb = TRUE)
  expect_error(
    do.call(vpc, c(args, list(sim_ref = fx$ref, pred_corr = TRUE))),
    "cannot be combined"
  )
  expect_error(
    do.call(vpc, c(args, list(sim_ref = fx$ref[fx$ref$rec != 1, ]))),
    "number of records per replicate"
  )
  ref_bad_id <- fx$ref
  ref_bad_id$id[ref_bad_id$rec == 1] <- 99
  expect_error(
    do.call(vpc, c(args, list(sim_ref = ref_bad_id))),
    "id"
  )
  args_obs <- args
  args_obs$obs <- fx$obs[-1, ]
  expect_error(
    do.call(vpc, c(args_obs, list(sim_ref = fx$ref))),
    "number of records in `obs`"
  )
  ref_no_pred <- fx$ref
  ref_no_pred$PRED <- NULL
  expect_error(
    do.call(vpc, c(args, list(sim_ref = ref_no_pred))),
    "not found in reference simulation"
  )
})

test_that("vpc() with sim_ref and lloq keeps censored data out", {
  fx <- refcorr_vpc_data()
  expect_warning(
    obj <- vpc(sim = fx$sim, obs = fx$obs, sim_ref = fx$ref, lloq = 0.5,
               obs_cols = fx$obs_cols, sim_cols = fx$sim_cols, software = "nonmem", vpcdb = TRUE),
    "censored"
  )
  expect_true(all(is.na(obj$obs$dv[fx$obs$dv < 0.5])))
})
