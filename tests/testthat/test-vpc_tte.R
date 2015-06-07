context("vpc_tte")

obj <- vpc_tte(sim = rtte_sim_nm,  # stratified for covariate and study arm
               obs = rtte_obs_nm, 
               stratify = c("sex","drug"), 
               rtte = FALSE,
               sim_cols=list(dv = "dv", idv = "t"), obs_cols=list(idv = "t"))

test_that("vpc_tte plot succeeded", {
  expect_equal(class(obj), c("gg", "ggplot"))
})  