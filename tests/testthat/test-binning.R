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

# Binning by stratum (#11) ####

strat_test_data <- function() {
  obs <- simple_data$obs
  sim <- simple_data$sim
  obs$GRP <- ifelse(obs$ID %% 2 == 0, "A", "B")
  sim$GRP <- ifelse(sim$ID %% 2 == 0, "A", "B")
  # stratum B has twice the time range of stratum A
  obs$TIME[obs$GRP == "B"] <- obs$TIME[obs$GRP == "B"] * 2
  sim$TIME[sim$GRP == "B"] <- sim$TIME[sim$GRP == "B"] * 2
  list(obs = obs, sim = sim)
}

test_that("bin_by_strata = TRUE determines automatic bins per stratum", {
  d <- strat_test_data()
  db <- vpc(sim = d$sim, obs = d$obs, stratify = "GRP", bins = "time",
            n_bins = 4, bin_by_strata = TRUE, vpcdb = TRUE)
  expect_type(db$bins, "list")
  expect_named(db$bins, c("A", "B"))
  obs_a <- d$obs$TIME[d$obs$GRP == "A" & d$obs$MDV == 0]
  obs_b <- d$obs$TIME[d$obs$GRP == "B" & d$obs$MDV == 0]
  expect_equal(db$bins$A, auto_bin(obs_a, type = "time", n_bins = 4))
  expect_equal(db$bins$B, auto_bin(obs_b, type = "time", n_bins = 4))
  # all data binned, bin boundaries taken from the stratum's own bins
  expect_false(any(is.na(db$obs$bin)))
  expect_false(any(is.na(db$sim$bin)))
  for (s in c("A", "B")) {
    v <- db$vpc_dat[db$vpc_dat$strat == s, ]
    expect_equal(v$bin_min, db$bins[[s]][v$bin])
    expect_equal(v$bin_max, db$bins[[s]][v$bin + 1])
    o <- db$aggr_obs[db$aggr_obs$strat == s, ]
    expect_equal(o$bin_min, db$bins[[s]][o$bin])
  }
  # row order of the data is retained
  expect_equal(db$obs$idv, d$obs$TIME[d$obs$MDV == 0])
})

test_that("default (bin_by_strata = FALSE) uses the same bins for all strata", {
  d <- strat_test_data()
  db <- vpc(sim = d$sim, obs = d$obs, stratify = "GRP", bins = "time",
            n_bins = 4, vpcdb = TRUE)
  expect_type(db$bins, "double")
  expect_length(db$bins, 5)
})

test_that("manual bins can be specified per stratum as a list", {
  d <- strat_test_data()
  bins <- list(A = c(0, 2, 5, 10, 30), B = c(0, 4, 10, 60))
  db <- vpc(sim = d$sim, obs = d$obs, stratify = "GRP", bins = bins, vpcdb = TRUE)
  expect_equal(db$bins, bins)
  expect_equal(sort(unique(db$obs$bin[db$obs$strat == "A"])), 1:4)
  expect_equal(sort(unique(db$obs$bin[db$obs$strat == "B"])), 1:3)
  expect_equal(db$aggr_obs$bin_max[db$aggr_obs$strat == "B"], c(4, 10, 60))
  # unnamed list, in order of the strata
  db2 <- vpc(sim = d$sim, obs = d$obs, stratify = "GRP", bins = unname(bins), vpcdb = TRUE)
  expect_equal(db2$aggr_obs, db$aggr_obs)
  # mixing methods and separators, and per-stratum n_bins
  db3 <- vpc(sim = d$sim, obs = d$obs, stratify = "GRP",
             bins = list(A = "time", B = c(0, 4, 10, 60)),
             n_bins = list(A = 3, B = 8), vpcdb = TRUE)
  expect_length(db3$bins$A, 4)
  expect_equal(db3$bins$B, c(0, 4, 10, 60))
})

test_that("per-stratum bins give informative errors", {
  d <- strat_test_data()
  expect_error(
    vpc(sim = d$sim, obs = d$obs, stratify = "GRP", bins = list(A = c(0, 10, 30)), vpcdb = TRUE),
    "No `bins` specified for stratum `B`"
  )
  expect_error(
    vpc(sim = d$sim, obs = d$obs, stratify = "GRP", bins = list(c(0, 10, 30)), vpcdb = TRUE),
    "one element per stratum"
  )
  expect_error(
    vpc(sim = d$sim, obs = d$obs, stratify = "GRP", bins = "percentiles",
        bin_by_strata = TRUE, vpcdb = TRUE),
    "not supported when binning by stratum"
  )
})

test_that("bin_by_strata works with two stratification variables, pred_corr and plotting", {
  d <- strat_test_data()
  d$obs$SEX <- ifelse(d$obs$ID %% 3 == 0, "M", "F")
  d$sim$SEX <- ifelse(d$sim$ID %% 3 == 0, "M", "F")
  db <- vpc(sim = d$sim, obs = d$obs, stratify = c("GRP", "SEX"),
            bin_by_strata = TRUE, pred_corr = TRUE, vpcdb = TRUE)
  expect_named(db$bins, c("A, F", "A, M", "B, F", "B, M"))
  expect_false(any(is.na(db$vpc_dat$bin_min)))
  bsep <- bin_separators_data(db)
  expect_true(all(c("x", "GRP", "SEX") %in% names(bsep)))
  expect_equal(nrow(bsep), length(unlist(db$bins)))
  pl <- plot_vpc(db)
  expect_s3_class(pl, "ggplot")
  expect_silent(ggplot2::ggplot_build(pl))
})

test_that("bin_by_strata works for vpc_cens()", {
  d <- strat_test_data()
  db <- vpc_cens(sim = d$sim, obs = d$obs, lloq = 30, stratify = "GRP",
                 bins = "time", n_bins = 5, bin_by_strata = TRUE, vpcdb = TRUE)
  expect_named(db$bins, c("A", "B"))
  expect_false(isTRUE(all.equal(db$bins$A, db$bins$B)))
  v <- db$vpc_dat[db$vpc_dat$strat == "B", ]
  expect_equal(v$bin_max, db$bins$B[v$bin + 1])
  expect_s3_class(plot_vpc(db), "ggplot")
})

test_that("vpc_cat() does not accept per-stratum bins", {
  tmp <- simple_data
  tmp$obs$DV <- as.integer(tmp$obs$DV > 40)
  tmp$sim$DV <- as.integer(tmp$sim$DV > 40)
  expect_error(vpc_cat(sim = tmp$sim, obs = tmp$obs, bins = list(c(0, 3, 30)), vpcdb = TRUE),
               "not supported")
})

test_that("bin_by_strata in vpc_cens() includes the color stratification", {
  d <- strat_test_data()
  d$obs$SEX <- ifelse(d$obs$ID %% 3 == 0, "M", "F")
  d$sim$SEX <- ifelse(d$sim$ID %% 3 == 0, "M", "F")
  # color stratification only: single panel, colored by GRP
  db <- vpc_cens(sim = d$sim, obs = d$obs, lloq = 30, stratify_color = "GRP",
                 bins = "time", n_bins = 4, bin_by_strata = TRUE, vpcdb = TRUE)
  expect_named(db$bins, c("A", "B"))
  expect_false(isTRUE(all.equal(db$bins$A, db$bins$B)))
  expect_null(db$stratify)
  expect_equal(db$stratify_color, "GRP")
  expect_setequal(db$vpc_dat$GRP, c("A", "B"))
  expect_equal(db$vpc_dat$bin_max[db$vpc_dat$GRP == "B"],
               db$bins$B[db$vpc_dat$bin[db$vpc_dat$GRP == "B"] + 1])
  pl <- plot_vpc(db)
  expect_s3_class(pl$facet, "FacetNull")
  built <- ggplot2::ggplot_build(pl)
  obs_layer <- which(vapply(pl$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))
  expect_length(unique(built$data[[obs_layer]]$colour), 2)
  ribbon_layer <- which(vapply(pl$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1)))
  expect_length(unique(built$data[[ribbon_layer]]$fill), 2)
  expect_equal(length(unique(built$data[[obs_layer]]$group)), 2)
  # facet (SEX) and color (GRP) stratification
  db <- vpc_cens(sim = d$sim, obs = d$obs, lloq = 30, stratify = "SEX",
                 stratify_color = "GRP", bins = "time", n_bins = 4,
                 bin_by_strata = TRUE, vpcdb = TRUE)
  expect_named(db$bins, c("F, A", "F, B", "M, A", "M, B"))
  expect_equal(db$bins[["M, B"]][5], max(d$obs$TIME[d$obs$MDV == 0 & d$obs$SEX == "M" & d$obs$GRP == "B"]) * 1.001)
  expect_equal(db$stratify, "SEX")
  v <- db$vpc_dat[db$vpc_dat$strat == "M, B", ]
  expect_equal(unique(v$SEX), "M")
  expect_equal(unique(v$GRP), "B")
  expect_equal(v$bin_max, db$bins[["M, B"]][v$bin + 1])
  pl <- plot_vpc(db)
  expect_s3_class(pl$facet, "FacetWrap")
  expect_equal(names(pl$facet$params$facets), "SEX")
  built <- ggplot2::ggplot_build(pl)
  expect_equal(nrow(built$layout$layout), 2)
  obs_layer <- which(vapply(pl$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))
  expect_length(unique(built$data[[obs_layer]]$colour), 2)
  # bin separators are drawn per facet and colored per color stratum
  rug_layer <- which(vapply(pl$layers, function(l) inherits(l$geom, "GeomRug"), logical(1)))
  rug <- built$data[[rug_layer]]
  expect_equal(nrow(rug), length(unlist(db$bins)))
  expect_length(unique(rug$colour), 2)
})

test_that("vpc_cens() color stratification without per-stratum binning", {
  d <- strat_test_data()
  db <- vpc_cens(sim = d$sim, obs = d$obs, lloq = 30, stratify_color = "GRP",
                 bins = "time", n_bins = 4, vpcdb = TRUE)
  expect_type(db$bins, "double")
  expect_setequal(db$aggr_obs$GRP, c("A", "B"))
  pl <- plot_vpc(db)
  expect_s3_class(pl$facet, "FacetNull")
  expect_s3_class(ggplot2::ggplot_build(pl), "ggplot_built")
})

test_that("missing values of the stratification variable are binned as their own stratum", {
  d <- strat_test_data()
  d$obs$GRP[d$obs$ID %in% c(1, 2)] <- NA
  d$sim$GRP[d$sim$ID %in% c(1, 2)] <- NA
  db <- vpc(sim = d$sim, obs = d$obs, stratify = "GRP", bins = "time",
            n_bins = 3, bin_by_strata = TRUE, vpcdb = TRUE)
  expect_equal(names(db$bins), c("A", "B", NA))
  expect_true(all(is.finite(unlist(db$bins))))
  expect_false(any(is.na(db$obs$bin)))
  expect_false(any(is.na(db$sim$bin)))
  expect_false(any(is.na(db$vpc_dat$bin_min)))
  expect_false(any(is.na(db$aggr_obs$bin_max)))
  bsep <- bin_separators_data(db)
  expect_true(anyNA(bsep$GRP))
  expect_s3_class(ggplot2::ggplot_build(plot_vpc(db)), "ggplot_built")
  # manual bins, with the missing stratum named "NA"
  db2 <- vpc(sim = d$sim, obs = d$obs, stratify = "GRP",
             bins = list(A = c(0, 5, 30), B = c(0, 10, 60), "NA" = c(0, 30)),
             vpcdb = TRUE)
  expect_false(any(is.na(db2$obs$bin)))
  expect_equal(db2$aggr_obs$bin_max[is.na(db2$aggr_obs$strat)], 30)
})

test_that("missing strata do not collide with a stratum with the literal value \"NA\"", {
  d <- strat_test_data()
  d$obs$GRP[d$obs$ID %in% c(1, 2)] <- NA
  d$sim$GRP[d$sim$ID %in% c(1, 2)] <- NA
  d$obs$GRP[d$obs$ID %in% c(3, 4)] <- "NA"
  d$sim$GRP[d$sim$ID %in% c(3, 4)] <- "NA"
  # literal "NA" stratum has a 2x longer time range than the missing stratum
  d$obs$TIME[d$obs$GRP %in% "NA"] <- d$obs$TIME[d$obs$GRP %in% "NA"] * 2
  d$sim$TIME[d$sim$GRP %in% "NA"] <- d$sim$TIME[d$sim$GRP %in% "NA"] * 2
  db <- vpc(sim = d$sim, obs = d$obs, stratify = "GRP", bins = "time",
            n_bins = 3, bin_by_strata = TRUE, vpcdb = TRUE)
  expect_length(db$bins, 4)
  expect_true("NA" %in% names(db$bins))
  expect_true(anyNA(names(db$bins)))
  lit <- db$bins[[which(names(db$bins) %in% "NA")]]
  mis <- db$bins[[which(is.na(names(db$bins)))]]
  expect_equal(max(lit), max(d$obs$TIME[d$obs$MDV == 0 & d$obs$GRP %in% "NA"]) * 1.001)
  expect_equal(max(mis), max(d$obs$TIME[d$obs$MDV == 0 & is.na(d$obs$GRP)]) * 1.001)
  expect_false(any(is.na(db$obs$bin)))
  # manual bins: separate definitions for the literal "NA" and missing strata
  bins <- list(A = c(0, 30), B = c(0, 60), "NA" = c(0, 10, 50), c(0, 5, 30))
  names(bins)[4] <- NA
  db2 <- vpc(sim = d$sim, obs = d$obs, stratify = "GRP", bins = bins, vpcdb = TRUE)
  expect_false(any(is.na(db2$obs$bin)))
  expect_equal(db2$aggr_obs$bin_max[is.na(db2$aggr_obs$strat)], c(5, 30))
  expect_equal(db2$aggr_obs$bin_max[db2$aggr_obs$strat %in% "NA"], c(10, 50))
  expect_s3_class(ggplot2::ggplot_build(plot_vpc(db2)), "ggplot_built")
})

test_that("outlier areas work with per-stratum bins and color stratification", {
  d <- strat_test_data()
  for (smooth in c(TRUE, FALSE)) {
    db <- vpc(sim = d$sim, obs = d$obs, stratify = "GRP", bins = "time", n_bins = 4,
              bin_by_strata = TRUE, vpcdb = TRUE)
    areas <- calc_outlier_areas(db$vpc_dat, db$aggr_obs, smooth = smooth)
    if (!smooth && !is.null(areas)) {
      # rectangles follow the bins of their own stratum
      for (s in unique(as.character(areas$strat))) {
        expect_true(all(areas$xmin[areas$strat == s] %in% db$bins[[s]]))
      }
    }
    pl <- plot_vpc(db, show = list(outlier_areas = TRUE), smooth = smooth)
    expect_s3_class(ggplot2::ggplot_build(pl), "ggplot_built")
    db <- vpc_cens(sim = d$sim, obs = d$obs, lloq = 30, stratify_color = "GRP",
                   bins = "time", n_bins = 4, bin_by_strata = TRUE, vpcdb = TRUE)
    areas <- calc_outlier_areas(db$vpc_dat, db$aggr_obs, smooth = smooth)
    if (!is.null(areas)) expect_true("GRP" %in% names(areas))
    pl <- plot_vpc(db, show = list(outlier_areas = TRUE), smooth = smooth)
    expect_s3_class(ggplot2::ggplot_build(pl), "ggplot_built")
  }
})
