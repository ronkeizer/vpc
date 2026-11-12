test_that("add_stratification orders combined strata by factor levels", {
  dat <- data.frame(
    x = factor(c("a", "b", "c", "a"), levels = c("b", "a", "c")),
    y = factor(c("d", "e", "f", "e"), levels = c("e", "d", "f"))
  )
  out <- add_stratification(dat, c("x", "y"))
  expect_s3_class(out$strat, "factor")
  expect_equal(levels(out$strat), c("b, e", "a, e", "a, d", "c, f"))
  expect_equal(as.character(out$strat), c("a, d", "b, e", "c, f", "a, e"))
})

test_that("add_stratification sorts non-factor columns", {
  dat <- data.frame(x = c(2, 1, 10), y = c("b", "a", "a"))
  out <- add_stratification(dat, c("x", "y"))
  expect_equal(levels(out$strat), c("1, a", "2, b", "10, a"))
})

test_that("add_strat_columns preserves factor levels of original columns", {
  dat <- data.frame(
    x = factor(c("a", "b", "c", "a"), levels = c("b", "a", "c")),
    y = factor(c("d", "e", "f", "e"), levels = c("e", "d", "f"))
  )
  dat <- add_stratification(dat, c("x", "y"))
  summ <- data.frame(strat = unique(dat$strat), val = 1:4)
  out <- add_strat_columns(summ, c("x", "y"), dat)
  expect_equal(levels(out$strat1), c("b", "a", "c"))
  expect_equal(levels(out$strat2), c("e", "d", "f"))
  expect_equal(as.character(out$strat1), c("a", "b", "c", "a"))
  expect_equal(as.character(out$strat2), c("d", "e", "f", "e"))
  expect_null(add_strat_columns(NULL, c("x", "y"), dat))
})

test_that("vpc with 2 stratification variables respects factor order (#101)", {
  obs <- simple_data$obs
  sim <- simple_data$sim
  ids <- sort(unique(obs$ID))
  set.seed(1)
  xfac <- factor(sample(c("a", "b", "c"), length(ids), replace = TRUE), levels = c("b", "a", "c"))
  yfac <- factor(sample(c("d", "e", "f"), length(ids), replace = TRUE), levels = c("e", "d", "f"))
  obs$xfac <- xfac[match(obs$ID, ids)]
  obs$yfac <- yfac[match(obs$ID, ids)]
  sim$xfac <- xfac[match(sim$ID, ids)]
  sim$yfac <- yfac[match(sim$ID, ids)]
  vpc_db <- vpc(sim = sim, obs = obs, stratify = c("xfac", "yfac"), vpcdb = TRUE)
  expect_equal(levels(vpc_db$vpc_dat$strat1), c("b", "a", "c"))
  expect_equal(levels(vpc_db$vpc_dat$strat2), c("e", "d", "f"))
  expect_equal(levels(vpc_db$aggr_obs$strat1), c("b", "a", "c"))
  expect_equal(levels(vpc_db$aggr_obs$strat2), c("e", "d", "f"))
  p <- vpc(sim = sim, obs = obs, stratify = c("xfac", "yfac"))
  built <- ggplot2::ggplot_build(p)
  expect_equal(levels(built$layout$layout$xfac), c("b", "a", "c"))
  expect_equal(levels(built$layout$layout$yfac), c("e", "d", "f"))
})

test_that("add_stratification keeps missing values distinct from literal \"NA\" in combined strata", {
  dat <- data.frame(
    x = c(NA, "NA", "<NA>", NA, "a"),
    y = c("X", "X", "X", NA, "X")
  )
  out <- add_stratification(dat, c("x", "y"))
  expect_equal(as.character(out$strat), c("<NA>, X", "NA, X", "<<NA>>, X", "<NA>, <NA>", "a, X"))
  expect_equal(nlevels(out$strat), 5)
  # labels do not depend on the other values in the data (obs vs sim)
  expect_equal(as.character(add_stratification(dat[1, ], c("x", "y"))$strat), "<NA>, X")
  # original values are recovered per stratum
  summ <- data.frame(strat = out$strat)
  cols <- add_strat_columns(summ, c("x", "y"), out)
  expect_equal(cols$strat1, dat$x)
  expect_equal(cols$strat2, dat$y)
})

test_that("add_stratification keeps strata distinct when values contain the separator", {
  dat <- data.frame(
    x = c("a, b", "a", "a\\", "a"),
    y = c("c", "b, c", ", c", "\\, c")
  )
  out <- add_stratification(dat, c("x", "y"))
  expect_equal(nlevels(out$strat), 4)
  expect_equal(as.character(out$strat)[1:2], c("a\\, b, c", "a, b\\, c"))
  # labels of values without commas or backslashes are unchanged
  expect_equal(as.character(add_stratification(data.frame(x = "a b", y = 1), c("x", "y"))$strat), "a b, 1")
  # original values are recovered per stratum
  cols <- add_strat_columns(data.frame(strat = out$strat), c("x", "y"), out)
  expect_equal(cols$strat1, dat$x)
  expect_equal(cols$strat2, dat$y)
})
