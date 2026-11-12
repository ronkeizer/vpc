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
