test_that("compute_kmmc returns the mean covariate of subjects still at risk", {
  # one row per subject (event or censoring time), as for TTE data
  dat <- data.frame(
    id = 1:4,
    time = c(1, 2, 3, 4),
    dv = c(1, 1, 0, 1),
    WT = c(40, 60, 80, 100),
    strat = 1
  )
  res <- compute_kmmc(dat, strat = "strat", kmmc = "WT")
  expect_equal(res$time, c(1, 2, 3, 4))
  # at time t, all subjects with time >= t are still in the risk set
  expect_equal(res$surv, c(mean(c(40, 60, 80, 100)), mean(c(60, 80, 100)), mean(c(80, 100)), 100))
  res_rev <- compute_kmmc(dat, strat = "strat", kmmc = "WT", reverse_prob = TRUE)
  expect_equal(res_rev$surv, 1 - res$surv)
})

test_that("compute_kmmc computes curves separately for each stratum", {
  dat <- data.frame(
    id = 1:6,
    time = c(1, 2, 3, 1, 2, 3),
    dv = 1,
    WT = c(10, 20, 30, 100, 200, 300),
    strat = rep(c("a", "b"), each = 3)
  )
  res <- compute_kmmc(dat, strat = "strat", kmmc = "WT")
  expect_equal(res$surv[res$strat == "a"], c(20, 25, 30))
  expect_equal(res$surv[res$strat == "b"], c(200, 250, 300))
})

test_that("compute_kmmc without stratification averages the covariate per time point", {
  dat <- data.frame(id = 1:4, time = c(1, 1, 2, 2), WT = c(10, 20, 30, 50))
  res <- compute_kmmc(dat, kmmc = "WT")
  expect_equal(res$surv, c(15, 40))
  expect_equal(compute_kmmc(dat, kmmc = "WT", reverse_prob = TRUE)$surv, 1 - c(15, 40))
})

test_that("vpc_tte() creates stratified KMMC plots with separate curves per stratum", {
  obs <- vpc::rtte_obs_nm
  sim <- vpc::rtte_sim_nm[vpc::rtte_sim_nm$sim <= 5, ]
  # covariate that differs between strata
  obs$WT <- 50 + 20 * obs$sex + obs$id %% 7
  sim$WT <- 50 + 20 * sim$sex + sim$id %% 7
  db <- suppressWarnings(
    vpc_tte(sim = sim, obs = obs, kmmc = "WT", stratify = "sex",
            sim_cols = list(dv = "dv", idv = "t"), obs_cols = list(idv = "t"),
            vpcdb = TRUE)
  )
  strats <- unique(db$obs_km$strat)
  expect_length(strats, 2)
  first_mean <- tapply(db$obs_km$surv, db$obs_km$strat, function(x) x[1])
  # time-0 risk set contains all subjects of the stratum
  expect_equal(unname(first_mean[strats[1]] != first_mean[strats[2]]), TRUE)
  pl <- plot_vpc(db)
  expect_s3_class(pl, "ggplot")
  expect_equal(pl$labels$y, "Mean (WT)")
})

test_that("vpc_tte() errors when the KMMC covariate is missing", {
  sim <- vpc::rtte_sim_nm[vpc::rtte_sim_nm$sim <= 2, ]
  expect_error(
    suppressWarnings(vpc_tte(sim = sim, obs = vpc::rtte_obs_nm, kmmc = "WT",
            sim_cols = list(dv = "dv", idv = "t"), obs_cols = list(idv = "t"))),
    "not found among column names in observed data"
  )
  obs <- vpc::rtte_obs_nm
  obs$WT <- 70
  expect_error(
    suppressWarnings(vpc_tte(sim = sim, obs = obs, kmmc = "WT",
            sim_cols = list(dv = "dv", idv = "t"), obs_cols = list(idv = "t"))),
    "not found among column names in simulated data"
  )
})
