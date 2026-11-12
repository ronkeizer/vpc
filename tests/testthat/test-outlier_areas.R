test_that("outlier_areas_smooth finds exact crossing points", {
  # observed crosses the upper CI bound (constant at 1) halfway between x=1 and x=2
  res <- outlier_areas_smooth(
    x_sim = 0:3, low = rep(0, 4), up = rep(1, 4),
    x_obs = 0:3, obs = c(0.5, 0.5, 1.5, 1.5)
  )
  expect_equal(unique(res$group), "above_2")
  expect_equal(res$x, c(1.5, 2, 3))
  expect_equal(res$ymin, c(1, 1, 1))
  expect_equal(res$ymax, c(1, 1.5, 1.5))
})

test_that("outlier_areas_smooth handles below, inside and missing values", {
  expect_null(outlier_areas_smooth(
    x_sim = 0:3, low = rep(0, 4), up = rep(1, 4),
    x_obs = 0:3, obs = rep(0.5, 4)
  ))
  res <- outlier_areas_smooth(
    x_sim = 0:3, low = rep(0, 4), up = rep(1, 4),
    x_obs = 0:3, obs = c(-1, -1, NA, -1)
  )
  # The gap at x=2 breaks the area; the single point at x=3 has no area
  expect_equal(unique(res$group), "below_1")
  expect_equal(res$x, c(0, 1))
  expect_equal(res$ymin, c(-1, -1))
  expect_equal(res$ymax, c(0, 0))
})

test_that("outlier_areas_bins returns one rectangle per outlying bin", {
  sim_k <- data.frame(bin = 1:3, bin_min = 0:2, bin_max = 1:3,
                      q50.low = 0, q50.up = 1)
  obs_k <- data.frame(bin = 1:3, obs50 = c(2, 0.5, -1))
  res <- outlier_areas_bins(sim_k, obs_k, "q50", "obs50")
  expect_equal(res$xmin, c(0, 2))
  expect_equal(res$ymin, c(1, -1))
  expect_equal(res$ymax, c(2, 0))
  expect_equal(res$group, c("above", "below"))
})

test_that("calc_outlier_areas works per stratum and on log scale", {
  obs <- simple_data$obs
  obs$DV[obs$TIME > 4] <- obs$DV[obs$TIME > 4] * 1.6
  db <- vpc(sim = simple_data$sim, obs = obs, stratify = "ISM", vpcdb = TRUE)
  res <- calc_outlier_areas(db$vpc_dat, db$aggr_obs, smooth = TRUE)
  expect_true(all(c("x", "ymin", "ymax", "group", "strat") %in% names(res)))
  expect_true(all(res$ymax >= res$ymin - 1e-8))
  expect_setequal(unique(res$strat), unique(db$aggr_obs$strat))
  res_log <- calc_outlier_areas(db$vpc_dat, db$aggr_obs, smooth = TRUE, log_y = TRUE)
  expect_true(all(res_log$ymin > 0))
  res_bins <- calc_outlier_areas(db$vpc_dat, db$aggr_obs, smooth = FALSE)
  expect_true(all(c("xmin", "xmax", "ymin", "ymax") %in% names(res_bins)))
  expect_null(calc_outlier_areas(NULL, db$aggr_obs))
})

test_that("vpc shows outlier areas only when requested", {
  obs <- simple_data$obs
  obs$DV[obs$TIME > 4] <- obs$DV[obs$TIME > 4] * 1.6
  has_red <- function(p) {
    any(vapply(p$layers, function(l) identical(l$aes_params$fill, "#dd0000"), logical(1)))
  }
  p0 <- vpc(sim = simple_data$sim, obs = obs)
  expect_false(has_red(p0))
  p1 <- vpc(sim = simple_data$sim, obs = obs, show = list(outlier_areas = TRUE))
  expect_true(has_red(p1))
  p2 <- vpc(sim = simple_data$sim, obs = obs, show = list(outlier_areas = TRUE), smooth = FALSE)
  expect_s3_class(p2$layers[[which(vapply(p2$layers, function(l) identical(l$aes_params$fill, "#dd0000"), logical(1)))]]$geom, "GeomRect")
  theme <- new_vpc_theme(list(outlier_fill = "orange", outlier_alpha = 0.2))
  p3 <- vpc(sim = simple_data$sim, obs = obs, show = list(outlier_areas = TRUE), vpc_theme = theme)
  expect_true(any(vapply(p3$layers, function(l) identical(l$aes_params$fill, "orange"), logical(1))))
  expect_silent(ggplot2::ggplot_build(p1))
})

test_that("vpc_cens and vpc_cat show outlier areas when requested", {
  obs <- simple_data$obs
  obs$DV[obs$TIME > 4] <- obs$DV[obs$TIME > 4] * 1.6
  red_layers <- function(p) {
    p$layers[vapply(p$layers, function(l) identical(l$aes_params$fill, "#dd0000"), logical(1))]
  }
  p0 <- vpc_cens(sim = simple_data$sim, obs = obs, lloq = 30)
  expect_length(red_layers(p0), 0)
  p1 <- vpc_cens(sim = simple_data$sim, obs = obs, lloq = 30, show = list(outlier_areas = TRUE))
  expect_length(red_layers(p1), 1)
  expect_silent(ggplot2::ggplot_build(p1))
  p2 <- vpc_cens(sim = simple_data$sim, obs = obs, lloq = 30, stratify = "ISM",
                 smooth = FALSE, show = list(outlier_areas = TRUE))
  expect_s3_class(red_layers(p2)[[1]]$geom, "GeomRect")
  expect_true("ISM" %in% names(red_layers(p2)[[1]]$data))

  obs$DV_CAT <- cut(obs$DV, c(-Inf, 25, 75, Inf), labels = FALSE)
  sim <- simple_data$sim
  sim$DV_CAT <- cut(sim$DV, c(-Inf, 25, 75, Inf), labels = FALSE)
  p3 <- vpc_cat(sim = sim, obs = obs, obs_cols = list(dv = "DV_CAT"),
                sim_cols = list(dv = "DV_CAT"), show = list(outlier_areas = TRUE))
  areas <- red_layers(p3)[[1]]$data
  expect_true(all(areas$ymin >= 0 & areas$ymax <= 1))
  expect_true(length(unique(areas$strat)) > 1)
})
