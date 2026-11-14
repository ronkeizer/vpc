test_that("vpc_tte()", {
  
  ## Test for bug AR 20171025 column name simulation dataset
  test <- vpc::rtte_sim_nm[1:200000,]
  test$bla <- test$t
  test$t <- NULL
  suppressWarnings(
    obj <- vpc_tte(sim = test,  # stratified for covariate and study arm
                   obs = vpc::rtte_obs_nm, 
                   rtte = FALSE,
                   sim_cols = list(dv = "dv", idv = "bla"), 
                   obs_cols = list(idv = "t"),
                   labeller = ggplot2::label_both)
  )
  expect_s3_class(
    obj, "ggplot"
    # vpc_tte no stratification succeeded"
  )
  
  suppressWarnings(
    obj <- vpc_tte(sim = vpc::rtte_sim_nm[1:200000,],  # stratified for covariate and study arm
                   obs = vpc::rtte_obs_nm, 
                   stratify = c("sex","drug"), 
                   rtte = FALSE,
                   show = list(obs_ci = TRUE),
                   vpc_theme = new_vpc_theme(list(obs_ci_fill = rgb(0.5,0,0,.7))),
                   sim_cols = list(dv = "dv", idv = "t"), 
                   obs_cols = list(idv = "t"),
                   labeller = ggplot2::label_both)
  )
  expect_s3_class(
    obj, "ggplot"
    # vpc_tte plot succeeded
  )

  ## without observations:
  suppressWarnings(
    obj2 <- vpc_tte(sim = vpc::rtte_sim_nm[1:500000,],  # stratified for covariate and study arm
                    stratify = c("sex"), 
                    rtte = FALSE,
                    sim_cols = list(dv = "dv", idv = "t"), 
                    obs_cols = list(idv = "t"), vpcdb=T)
  )
  pl2 <- plot_vpc(obj2)
  
  expect_s3_class(
    pl2, "ggplot"
    #vpc_tte without obs succeeded
  )
})

test_that("vpc_tte() ci_sim controls the simulated confidence interval", {
  sim <- vpc::rtte_sim_nm[vpc::rtte_sim_nm$sim <= 20, ]
  run <- function(...) {
    suppressWarnings(
      vpc_tte(sim = sim,
              obs = vpc::rtte_obs_nm,
              rtte = FALSE,
              sim_cols = list(dv = "dv", idv = "t"),
              obs_cols = list(idv = "t"),
              vpcdb = TRUE,
              ...)
    )
  }
  db_default <- run()
  db_90 <- run(ci_sim = c(0.05, 0.95))
  db_95 <- run(ci_sim = c(0.025, 0.975))
  expect_equal(db_default$sim_km$qmin, db_90$sim_km$qmin)
  expect_equal(db_default$sim_km$qmax, db_90$sim_km$qmax)
  # a wider interval must be at least as wide everywhere, and wider somewhere
  expect_true(all(db_95$sim_km$qmin <= db_90$sim_km$qmin))
  expect_true(all(db_95$sim_km$qmax >= db_90$sim_km$qmax))
  expect_true(any(db_95$sim_km$qmin < db_90$sim_km$qmin | db_95$sim_km$qmax > db_90$sim_km$qmax))
  # the ci argument (observed data) must not affect the simulated interval
  db_ci <- run(ci = c(0.025, 0.975))
  expect_equal(db_ci$sim_km$qmin, db_90$sim_km$qmin)
  expect_equal(db_ci$sim_km$qmax, db_90$sim_km$qmax)
})

test_that("vpc_tte() validates ci_sim", {
  expect_error(vpc_tte(sim = vpc::rtte_sim_nm, ci_sim = 0.9), "ci_sim")
  expect_error(vpc_tte(sim = vpc::rtte_sim_nm, ci_sim = c(0.95, 0.05)), "ci_sim")
  expect_error(vpc_tte(sim = vpc::rtte_sim_nm, ci_sim = c(-0.1, 0.9)), "ci_sim")
  expect_error(vpc_tte(sim = vpc::rtte_sim_nm, ci_sim = c("a", "b")), "ci_sim")
})

test_that("vpc_tte() keeps positional argument order (ci_sim appended last)", {
  expect_equal(
    names(formals(vpc_tte)),
    c("sim", "obs", "psn_folder", "rtte", "rtte_calc_diff", "rtte_conditional",
      "events", "bins", "n_bins", "software", "obs_cols", "sim_cols", "kmmc",
      "reverse_prob", "stratify", "stratify_color", "ci", "xlab", "ylab",
      "show", "as_percentage", "title", "smooth", "vpc_theme", "facet",
      "labeller", "verbose", "vpcdb", "ci_sim")
  )
  # xlab passed positionally (18th argument) must still be treated as xlab
  sim <- vpc::rtte_sim_nm[vpc::rtte_sim_nm$sim <= 5, ]
  pl <- suppressWarnings(
    vpc_tte(sim, vpc::rtte_obs_nm, NULL, FALSE, TRUE, TRUE, NULL, FALSE, 10,
            "auto", list(idv = "t"), list(dv = "dv", idv = "t"), NULL, FALSE,
            NULL, NULL, c(0.05, 0.95), "Positional time label")
  )
  expect_s3_class(pl, "ggplot")
  expect_equal(pl$labels$x, "Positional time label")
})

test_that("rtte event order follows event time, not gap length (#127)", {
  # One subject: events at 3, 10, 12, censored at 20 -> gaps 3, 7, 2, 8
  obs <- data.frame(id = 1, time = c(12, 3, 20, 10), dv = c(1, 1, 0, 1))
  f <- format_vpc_input_data_tte(
    obs, cols = list(id = "id", dv = "dv", idv = "time"),
    stratify = NULL, rtte = TRUE, rtte_calc_diff = TRUE
  )
  d <- as.data.frame(f$dat)
  expect_equal(d$rtte, 1:4)
  expect_equal(d$time, c(3, 7, 2, 8))
  expect_equal(d$dv, c(1, 1, 1, 0))

  # Without gap calculation, absolute times keep event order
  f2 <- format_vpc_input_data_tte(
    obs, cols = list(id = "id", dv = "dv", idv = "time"),
    stratify = NULL, rtte = TRUE, rtte_calc_diff = FALSE
  )
  d2 <- as.data.frame(f2$dat)
  expect_equal(d2$rtte, 1:4)
  expect_equal(d2$time, c(3, 10, 12, 20))

  # Simulated data: gaps computed within each sim & id separately
  sim <- data.frame(
    sim = rep(1:2, each = 4), id = 1,
    time = c(3, 10, 12, 20, 5, 6, 15, 20), dv = c(1, 1, 1, 0, 1, 1, 1, 0)
  )
  fs <- format_vpc_input_data_tte(
    sim, cols = list(id = "id", dv = "dv", idv = "time", sim = "sim"),
    stratify = NULL, rtte = TRUE, rtte_calc_diff = TRUE, what = "simulated"
  )
  ds <- as.data.frame(fs$dat)
  ds <- ds[order(ds$sim, ds$rtte), ]
  expect_equal(ds$rtte, rep(1:4, 2))
  expect_equal(ds$time, c(3, 7, 2, 8, 5, 1, 9, 5))
})

test_that("vpc_tte(rtte = TRUE) works without a `t` column (#128)", {
  obs <- data.frame(id = rep(1:2, each = 2), time = c(3, 10, 4, 8), dv = c(1, 0, 1, 0))
  sim <- rbind(cbind(obs, sim = 1), cbind(obs, sim = 2))
  for (args in list(
    list(rtte_calc_diff = FALSE, rtte_conditional = TRUE),
    list(rtte_calc_diff = TRUE, rtte_conditional = FALSE),
    list(rtte_calc_diff = FALSE, rtte_conditional = FALSE)
  )) {
    db <- suppressWarnings(
      vpc_tte(sim = sim, obs = obs, rtte = TRUE,
              rtte_calc_diff = args$rtte_calc_diff,
              rtte_conditional = args$rtte_conditional,
              sim_cols = list(sim = "sim"), vpcdb = TRUE)
    )
    # censoring marks at the original (absolute) times
    expect_equal(sort(db$cens_dat$time), c(8, 10))
    pl <- suppressWarnings(
      vpc_tte(sim = sim, obs = obs, rtte = TRUE,
              rtte_calc_diff = args$rtte_calc_diff,
              rtte_conditional = args$rtte_conditional,
              sim_cols = list(sim = "sim"))
    )
    expect_s3_class(pl, "ggplot")
  }
})
