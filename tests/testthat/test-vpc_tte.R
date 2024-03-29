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
