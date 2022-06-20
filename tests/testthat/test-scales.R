test_that("scales", {
  tmp <- simple_data
  sex <- (tmp$obs$ID) %% 2
  race <- (tmp$obs$ID) %% 3
  tmp$obs$sex <- sex
  tmp$obs$race <- race
  tmp$sim$sex <- sex
  tmp$sim$race <- race

  v1 <- vpc(sim = tmp$sim, obs = tmp$obs, stratify = "race",
            facet = "wrap", scales = "free_y", vpcdb = F, labeller = ggplot2::label_both)
  expect_s3_class(v1, "ggplot")
  expect_equal(
    length(v1$facet$params$facets),
    1,
    info="facets succeeded"
  )
})
