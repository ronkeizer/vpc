test_that("labeller", {
  tmp <- simple_data
  sex <- (tmp$obs$ID) %% 2
  race <- (tmp$obs$ID) %% 3
  tmp$obs$sex <- sex
  tmp$obs$race <- race
  tmp$sim$sex <- sex
  tmp$sim$race <- race
  
  v1 <- vpc(sim = tmp$sim, obs = tmp$obs, stratify = "race",
            facet = "wrap", vpcdb = F, labeller = ggplot2::label_both)
  expect_s3_class(
    v1, "ggplot",
    # plot succeeded
  )
  expect_equal(
    length(v1$facet$params$facets),
    1,
    info="facets succeeded"
  )
})

test_that("label_vpc_tte orders numeric strata (e.g. event numbers) numerically", {
  labels <- data.frame(rtte = c("10", "2", "1"))
  expect_equal(
    label_vpc_tte(labels),
    list(c("rtte: 1", "rtte: 2", "rtte: 10"))
  )
  expect_equal(
    label_vpc_tte(labels, sep = " = "),
    list(c("rtte = 1", "rtte = 2", "rtte = 10"))
  )
  # multiple variables on a single line
  two <- data.frame(sex = c("1", "2"), rtte = c("1", "1"))
  expect_equal(
    label_vpc_tte(two, multi_line = FALSE),
    list(c("sex, rtte: 1, 1", "sex, rtte: 2, 1"))
  )
})

test_that("repeated TTE vpc uses event-number labels for facets by default", {
  sim <- vpc::rtte_sim_nm[vpc::rtte_sim_nm$sim <= 3, ]
  pl <- suppressWarnings(
    vpc_tte(sim = sim, obs = vpc::rtte_obs_nm, rtte = TRUE, events = 1:3,
            sim_cols = list(dv = "dv", idv = "t"), obs_cols = list(idv = "t"))
  )
  expect_s3_class(pl, "ggplot")
  facet_var <- names(pl$facet$params$facets)
  expect_equal(facet_var, "rtte")
  panels <- suppressWarnings(ggplot2::ggplot_build(pl))$layout$layout[, facet_var, drop = FALSE]
  facet_labels <- pl$facet$params$labeller(panels)
  expect_equal(unlist(facet_labels), paste0("rtte: ", 1:3))
})
