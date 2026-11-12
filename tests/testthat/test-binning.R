# Helper: assign each value to a bin the same way define_bins()/bin_data() does
n_unbinned <- function(x, bins) {
  sum(is.na(bin_data(data.frame(time = x), bins = bins)$bin))
}

test_that("auto_bin 'time' creates the requested number of equally wide bins", {
  x <- c(0:24, 48, 72)
  for (n in 2:6) {
    bins <- auto_bin(x, type = "time", n_bins = n)
    expect_length(bins, n + 1)
    expect_equal(diff(bins)[-n], rep(diff(bins)[1], n - 1), tolerance = 0.01)
    expect_equal(n_unbinned(x, bins), 0)
  }
})

test_that("auto_bin 'time' bins all data regardless of the time range", {
  # separators must not be rounded, which could exclude the first observations
  for (x in list(1006:1100, 1000:1010, seq(0.0013, 0.0099, by = 0.0001))) {
    bins <- auto_bin(x, type = "time", n_bins = 4)
    expect_equal(n_unbinned(x, bins), 0)
    expect_equal(bins[1], min(x))
    expect_equal(diff(bins)[1:3], rep(diff(range(x)) / 4, 3))
  }
  # a single bin covering all data
  bins <- auto_bin(1:10, type = "time", n_bins = 1)
  expect_length(bins, 2)
  expect_equal(n_unbinned(1:10, bins), 0)
})

test_that("auto_bin 'data' creates bins with equal numbers of observations and bins all data", {
  x <- 1:30
  for (n in 2:6) {
    bins <- auto_bin(x, type = "data", n_bins = n)
    expect_length(bins, n + 1)
    # the smallest observation must be included in the first bin
    expect_equal(bins[1], min(x))
    expect_equal(n_unbinned(x, bins), 0)
  }
  counts <- table(bin_data(data.frame(time = x), bins = auto_bin(x, type = "data", n_bins = 3))$bin)
  expect_equal(as.vector(counts), c(10, 10, 10))
  # works with unsorted input and ties
  set.seed(1)
  y <- sample(rep(c(0.5, 1, 2, 4, 8, 12, 24), each = 7))
  bins <- auto_bin(y, type = "data", n_bins = 4)
  expect_equal(n_unbinned(y, bins), 0)
})

test_that("auto_bin 'percentiles' uses quantiles of the independent variable", {
  x <- 1:101
  bins <- auto_bin(x, type = "percentiles", n_bins = 4)
  expect_equal(unname(bins), c(1, 26, 51, 76, 101))
  # number of bins scales with the number of data points when n_bins = "auto"
  expect_length(auto_bin(1:100, type = "percentiles", n_bins = "auto"), 3 + 1)
  expect_length(auto_bin(1:400, type = "percentiles", n_bins = "auto"), 10 + 1)
  expect_length(auto_bin(1:4000, type = "percentiles", n_bins = "auto"), 15 + 1)
})

test_that("auto_bin with classInt methods returns separators spanning the data", {
  # three clearly separated clusters of sampling times
  x <- c(rep(1, 10), rep(5, 10), rep(10, 10))
  for (type in c("kmeans", "jenks", "pretty", "quantile")) {
    bins <- auto_bin(x, type = type, n_bins = 3)
    expect_true(min(bins) <= min(x), info = type)
    expect_true(max(bins) >= max(x), info = type)
    expect_equal(n_unbinned(x, bins), 0, info = type)
  }
  # jenks/kmeans should separate the clusters into their own bins
  binned <- bin_data(data.frame(time = x), bins = auto_bin(x, type = "jenks", n_bins = 3))
  expect_equal(as.vector(table(binned$bin)), c(10, 10, 10))
  # n_bins = "auto" is passed on to classInt as default number of classes
  expect_type(auto_bin(x, type = "jenks", n_bins = "auto"), "double")
})

test_that("auto_bin 'density' places separators at nadirs between clusters", {
  set.seed(42)
  x <- c(rnorm(50, 1, 0.1), rnorm(50, 5, 0.1), rnorm(50, 10, 0.1))
  bins <- auto_bin(x, type = "density", n_bins = 3)
  binned <- bin_data(data.frame(time = x), bins = bins)
  expect_equal(n_unbinned(x, bins), 0)
  expect_equal(as.vector(table(binned$bin)), c(50, 50, 50))
})

test_that("automatic bins include all data for negative independent variables", {
  x <- -30:-1
  for (type in c("time", "data")) {
    for (n in 2:5) {
      bins <- auto_bin(x, type = type, n_bins = n)
      expect_equal(n_unbinned(x, bins), 0, info = paste(type, n))
    }
  }
  set.seed(7)
  y <- c(rnorm(50, -10, 0.1), rnorm(50, -5, 0.1), rnorm(50, -1, 0.1))
  bins <- auto_bin(y, type = "density", n_bins = 3)
  expect_equal(n_unbinned(y, bins), 0)
  # positive data keep the previous upper separator
  expect_equal(max(auto_bin(1:30, type = "data", n_bins = 3)), 30 * 1.001)
})

test_that("auto_bin with no binning returns unique values", {
  expect_equal(auto_bin(c(1, 1, 2, 3, 3), type = "none"), c(1, 2, 3))
  expect_message(auto_bin(1:3, type = "none", verbose = TRUE), "No binning")
})

test_that("auto_bin informs about unsupported options", {
  expect_error(auto_bin(1:10, type = "foo", n_bins = 3), "not recognized")
  expect_message(
    auto_bin(1:30, type = "time", n_bins = "auto", verbose = TRUE),
    "reverting to 8 bins"
  )
  expect_length(auto_bin(1:30, type = "time", n_bins = "auto"), 8 + 1)
})

test_that("auto_bin on a data.frame bins the 'idv' column", {
  df <- data.frame(idv = 1:30, time = 101:130)
  expect_equal(auto_bin(df, type = "data", n_bins = 3), auto_bin(1:30, type = "data", n_bins = 3))
})

test_that("bin_data assigns values to left-closed bins and includes the last separator", {
  dat <- data.frame(time = c(0, 2.9, 3, 5, 6.9, 7))
  res <- bin_data(dat, bins = c(0, 3, 5, 7))
  expect_equal(res$bin, c(1, 1, 2, 3, 3, 3))
  res_lab <- bin_data(dat, bins = c(0, 3, 5, 7), labeled = TRUE)
  expect_s3_class(res_lab$bin, "factor")
  expect_equal(levels(res_lab$bin), c("[0,3)", "[3,5)", "[5,7]"))
  # alternative idv column
  res_tad <- bin_data(data.frame(tad = c(1, 4)), bins = c(0, 3, 5), idv = "tad")
  expect_equal(res_tad$bin, c(1, 2))
})

test_that("vpc() with bins = 'data' keeps all observations in the VPC", {
  db <- vpc(sim = simple_data$sim, obs = simple_data$obs,
            bins = "data", n_bins = 5, vpcdb = TRUE)
  expect_false(any(is.na(db$obs$bin)))
  expect_equal(length(unique(db$obs$bin)), 5)
})
