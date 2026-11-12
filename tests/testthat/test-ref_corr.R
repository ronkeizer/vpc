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

## Regression tests from PR #117 review (round 1)
rc_vpc <- function(fx, ...) {
  vpc(sim = fx$sim, obs = fx$obs, sim_ref = fx$ref,
      obs_cols = fx$obs_cols, sim_cols = fx$sim_cols,
      software = "nonmem", vpcdb = TRUE, ...)
}
sim_sorted_dv <- function(obj) obj$sim$dv[order(obj$sim$sim, obj$sim$rec)]

test_that("interleaved simulation rows give same result", {
  fx <- refcorr_vpc_data()
  obj <- rc_vpc(fx)
  fx2 <- fx
  set.seed(1)
  fx2$sim <- fx$sim[sample(nrow(fx$sim)), ]
  fx2$sim <- fx2$sim[order(fx2$sim$rec), ]  # rows interleaved across replicates
  fx2$ref <- fx$ref[order(fx$ref$rec, -fx$ref$sim), ]
  obj2 <- rc_vpc(fx2)
  expect_equal(obj2$obs$dv, obj$obs$dv)
  expect_equal(sim_sorted_dv(obj2), sim_sorted_dv(obj))
})

test_that("obs PRED fallback uses first replicate with interleaved sims", {
  fx <- refcorr_vpc_data()
  obj <- rc_vpc(fx)
  fx$obs$PRED <- NULL
  fx$sim <- fx$sim[order(fx$sim$rec, fx$sim$sim), ]
  obj2 <- rc_vpc(fx)
  expect_equal(obj2$obs$dv, obj$obs$dv)
})

test_that("misaligned idv is detected for sim, but allowed for sim_ref", {
  fx <- refcorr_vpc_data()
  fx_bad <- fx
  swap <- fx_bad$sim$rec %in% c(1, 2)
  fx_bad$sim$time[swap] <- rev(fx_bad$sim$time[swap])[order(rev(fx_bad$sim$sim[swap]))]
  fx_bad$sim$time[fx_bad$sim$rec == 1 & fx_bad$sim$sim == 1] <- 99
  expect_error(rc_vpc(fx_bad), "idv")
  fx_obs <- fx
  fx_obs$obs$time[1:2] <- fx_obs$obs$time[2:1]
  expect_error(rc_vpc(fx_obs), "idv")
  fx_ref <- fx
  fx_ref$ref$time <- 500  # e.g. reference at steady state
  expect_equal(rc_vpc(fx_ref)$obs$dv, rc_vpc(fx)$obs$dv)
})

test_that("censoring does not change correction of uncensored values", {
  fx <- refcorr_vpc_data()
  obj <- rc_vpc(fx)
  suppressWarnings(obj_lloq <- rc_vpc(fx, lloq = 0.5))
  keep_obs <- fx$obs$dv >= 0.5
  expect_equal(obj_lloq$obs$dv[keep_obs], obj$obs$dv[keep_obs])
  expect_true(all(is.na(obj_lloq$obs$dv[!keep_obs])))
  raw_sim <- fx$sim$dv[order(fx$sim$sim, fx$sim$rec)]
  expect_equal(sim_sorted_dv(obj_lloq)[raw_sim >= 0.5], sim_sorted_dv(obj)[raw_sim >= 0.5])
  expect_true(all(is.na(sim_sorted_dv(obj_lloq)[raw_sim < 0.5])))
  suppressWarnings(obj_uloq <- rc_vpc(fx, uloq = 2))
  keep_obs <- fx$obs$dv <= 2
  expect_equal(obj_uloq$obs$dv[keep_obs], obj$obs$dv[keep_obs])
})

test_that("log_y floor is applied after correction only", {
  fx <- refcorr_vpc_data()
  obj <- rc_vpc(fx)
  obj_log <- rc_vpc(fx, log_y = TRUE, log_y_min = 0.5)
  above <- obj$obs$dv >= 0.5
  expect_equal(obj_log$obs$dv[above], obj$obs$dv[above])
  expect_true(all(obj_log$obs$dv[!above] == 0.5))
})

test_that("non-finite corrections are reported and excluded", {
  fx <- refcorr_vpc_data()
  fx$obs$dv[1] <- -1  # below lower bound 0 -> log of negative
  expect_warning(obj <- rc_vpc(fx), "non-finite")
  expect_false(1 %in% obj$obs$rec)
  expect_equal(nrow(obj$obs), nrow(fx$obs) - 1)
})

## Regression tests from PR #117 review (round 2)
test_that("corrected observations are not re-censored against raw LLOQ", {
  fx <- refcorr_vpc_data()
  suppressWarnings(obj <- rc_vpc(fx, lloq = 1, bins = c(0, 1.5, 3, 6, 10, 13)))
  manual <- obj$obs %>%
    dplyr::mutate(x = ifelse(is.na(dv), -Inf, dv)) %>%
    dplyr::group_by(strat, bin) %>%
    dplyr::summarise(obs50 = stats::quantile(x, 0.5), .groups = "drop")
  manual$obs50[is.infinite(manual$obs50)] <- NA
  expect_equal(obj$aggr_obs$obs50, unname(manual$obs50))
  ## at least one valid record is corrected to below the raw LLOQ
  expect_true(any(!is.na(obj$obs$dv) & obj$obs$dv < 1))
})

test_that("new vpc_vpc() arguments do not shift positional arguments", {
  f <- names(formals(vpc_vpc))
  expect_equal(f[(length(f) - 2):length(f)], c("sim_ref", "ref_corr_log", "..."))
  expect_equal(which(f == "pi"), 14)
})

test_that("obs PRED fallback warns on record mismatch (pcVPC)", {
  fx <- refcorr_vpc_data()
  fx$obs$PRED <- NULL
  first <- which(fx$sim$sim == 1)[1:2]
  fx$sim[first, ] <- fx$sim[rev(first), ]
  expect_warning(
    vpc(sim = fx$sim, obs = fx$obs, pred_corr = TRUE,
        obs_cols = fx$obs_cols, sim_cols = fx$sim_cols,
        software = "nonmem", vpcdb = TRUE),
    "do not match the observed data"
  )
})

## Regression test from PR #117 review (round 3)
test_that("one PRED vector is used for all replicates (as Pharmpy)", {
  fx <- refcorr_vpc_data()
  obj <- rc_vpc(fx)
  fx2 <- fx
  fx2$sim$PRED[fx2$sim$sim == 3] <- fx2$sim$PRED[fx2$sim$sim == 3] * 1.5
  expect_warning(obj2 <- rc_vpc(fx2), "PRED differs between `obs` and `sim` replicates")
  expect_equal(obj2$obs$dv, obj$obs$dv)
  expect_equal(sim_sorted_dv(obj2), sim_sorted_dv(obj))
  fx3 <- fx
  fx3$ref$PRED[fx3$ref$sim == 2] <- fx3$ref$PRED[fx3$ref$sim == 2] * 2
  expect_warning(rc_vpc(fx3), "PRED differs between `sim_ref` replicates")
})

## Regression tests from /code-review
test_that("replicate index is kept when failed rows are dropped", {
  fx <- refcorr_vpc_data()
  fx$sim$sim_orig <- fx$sim$sim
  fx$sim$sim <- NULL  # index derived from id runs
  fx$sim$dv[fx$sim$sim_orig == 2 & fx$sim$id == 1] <- -1  # whole id fails correction
  expect_warning(obj <- rc_vpc(fx), "non-finite")
  expect_equal(as.numeric(table(obj$sim$sim)), c(30, 25, rep(30, 18)))
  expect_equal(obj$sim$sim, obj$sim$sim_orig)
})

test_that("sim PRED is optional when obs has PRED", {
  fx <- refcorr_vpc_data()
  obj <- rc_vpc(fx)
  fx$sim$PRED <- NULL
  obj2 <- rc_vpc(fx)
  expect_equal(obj2$obs$dv, obj$obs$dv)
  expect_equal(sim_sorted_dv(obj2), sim_sorted_dv(obj))
})

test_that("PRED check detects a single misaligned value", {
  pred <- seq(1, 2, length.out = 1000)
  m <- matrix(pred, nrow = 1000, ncol = 50)
  expect_silent(check_pred_constant(m, pred, "x"))
  m[c(1, 2), 7] <- m[c(2, 1), 7]
  expect_warning(check_pred_constant(m, pred, "x"), "for 2 values")
})

test_that("vectorised row SD handles missing values (< 2 values gives NA)", {
  fx <- refcorr_fixture()
  inp <- core_inputs(fx)
  inp$ref_dv[1, 1:3] <- NA
  inp$ref_dv[2, ] <- NA
  inp$ref_dv[3, -1] <- NA
  res <- do.call(ref_correction_core, c(inp, lower_bnd = 0, log_dv = FALSE))
  expect_true(all(is.na(res$sim[2:3, ])))
  expect_false(anyNA(res$sim[-(2:3), ]))
  expect_equal(nrow(res$sim), nrow(inp$sim_dv))
})
