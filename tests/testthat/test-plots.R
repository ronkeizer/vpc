# Visual regression tests (issue #9).
#
# These use vdiffr to store SVG snapshots of the plots under
# tests/testthat/_snaps/plots/. When a plot changes, the test fails and the
# differences can be inspected and accepted with `testthat::snapshot_review()`
# (shows old and new plots side by side) or `testthat::snapshot_accept()`.
#
# vdiffr skips these tests on CRAN and when vdiffr is not installed.

skip_if_not_installed("vdiffr")

cat_data <- function() {
  tmp <- simple_data
  brk <- c(-1, 10, 40, 1000)
  tmp$obs$DV <- as.integer(cut(tmp$obs$DV, breaks = brk))
  tmp$sim$DV <- as.integer(cut(tmp$sim$DV, breaks = brk))
  tmp
}

test_that("continuous VPC plots are stable", {
  vdiffr::expect_doppelganger(
    "vpc default",
    vpc(sim = simple_data$sim, obs = simple_data$obs)
  )
  vdiffr::expect_doppelganger(
    "vpc pred_corr",
    vpc(sim = simple_data$sim, obs = simple_data$obs, pred_corr = TRUE)
  )
  vdiffr::expect_doppelganger(
    "vpc lloq log_y",
    vpc(sim = simple_data$sim, obs = simple_data$obs, lloq = 20, log_y = TRUE)
  )
  vdiffr::expect_doppelganger(
    "vpc stratified",
    vpc(sim = simple_data$sim, obs = simple_data$obs, stratify = "ISM")
  )
  vdiffr::expect_doppelganger(
    "vpc show options",
    vpc(
      sim = simple_data$sim, obs = simple_data$obs,
      show = list(obs_dv = TRUE, obs_ci = TRUE, bin_sep = FALSE),
      xlab = "Time (h)", ylab = "Concentration", title = "Custom VPC"
    )
  )
})

test_that("censored VPC plot is stable", {
  vdiffr::expect_doppelganger(
    "vpc_cens lloq",
    vpc_cens(sim = simple_data$sim, obs = simple_data$obs, lloq = 30, n_bins = 8)
  )
})

test_that("categorical VPC plot is stable", {
  tmp <- cat_data()
  vdiffr::expect_doppelganger(
    "vpc_cat",
    vpc_cat(sim = tmp$sim, obs = tmp$obs)
  )
})

test_that("time-to-event VPC plots are stable", {
  sim <- rtte_sim_nm[rtte_sim_nm$sim <= 20, ]
  vdiffr::expect_doppelganger(
    "vpc_tte",
    suppressWarnings(vpc_tte(
      sim = sim, obs = rtte_obs_nm, rtte = FALSE,
      sim_cols = list(dv = "dv", idv = "t"), obs_cols = list(idv = "t")
    ))
  )
  vdiffr::expect_doppelganger(
    "vpc_tte stratified",
    suppressWarnings(vpc_tte(
      sim = sim, obs = rtte_obs_nm, rtte = FALSE,
      stratify = c("sex", "drug"),
      sim_cols = list(dv = "dv", idv = "t"), obs_cols = list(idv = "t"),
      labeller = ggplot2::label_both
    ))
  )
})

test_that("outlier area plots are stable", {
  obs <- simple_data$obs
  obs$DV[obs$TIME > 4] <- obs$DV[obs$TIME > 4] * 1.6
  vdiffr::expect_doppelganger(
    "vpc outlier_areas",
    vpc(sim = simple_data$sim, obs = obs, show = list(outlier_areas = TRUE))
  )
  vdiffr::expect_doppelganger(
    "vpc outlier_areas binned",
    vpc(sim = simple_data$sim, obs = obs, show = list(outlier_areas = TRUE), smooth = FALSE)
  )
  vdiffr::expect_doppelganger(
    "vpc_cens outlier_areas",
    vpc_cens(sim = simple_data$sim, obs = obs, lloq = 30, show = list(outlier_areas = TRUE))
  )
})
