library(vpc)

context("vpc")

tmp <- simple_data
obj <- vpc(sim = tmp$sim, obs = tmp$obs, vpcdb=TRUE)

test_that("vpc_cat returned proper object", {
  expect_true(all(c("obs", "sim", "aggr_obs", "vpc_dat", "show", "stratify", "bins") %in% names(obj)))
})

test_that("vpc_cat parsed data correctly", {
  expect_equal(sum(obj$vpc_dat$q50.med), 630.3304, tolerance = 0.001)
  expect_equal(sum(obj$vpc_dat$q50.low), 554.338, tolerance = 0.001)
  expect_equal(sum(obj$vpc_dat$q50.up), 708.7439, tolerance = 0.001)
  expect_equal(sum(obj$vpc_dat$q5.med), 296.3193, tolerance = 0.001)
  expect_equal(sum(obj$vpc_dat$q95.med), 1315.188, tolerance = 0.001)
  expect_equal(sum(obj$vpc_dat$bin_mid), 40.75, tolerance = 0.001)
  expect_equal(sum(obj$aggr_obs$obs50), 621.775, tolerance = 0.001)
})  

test_that("vpc_cat plot succeeded", {
  expect_equal(class(plot_vpc(obj)), c("gg", "ggplot"))
})  


# ## snippet to show labeling 
# library(vpc)
# tmp <- simple_data
# vpc(sim = tmp$sim, obs = tmp$obs, stratify = c("ISM")) + 
#   theme(axis.text  = element_text(size = 12), # the numbers at the ticks
#         axis.title = element_text(size = 14,face="bold"), # the axis labels
#         strip.text.x = element_text(size = 8, colour = "orange")) # the strips
# 
# vpc(sim = tmp$sim, obs = tmp$obs, stratify = c("ISM"), facet_names = FALSE)
# 
