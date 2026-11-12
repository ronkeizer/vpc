test_that("as.num keeps a number numeric", {
  expect_equal(vpc:::as.num(1:3), 1:3)
})
test_that("as.num makes a character numeric", {
  expect_equal(vpc:::as.num(as.character(1:3)), 1:3)
})
test_that("as.num makes a factor numeric", {
  expect_equal(vpc:::as.num(factor(LETTERS[1:3])), 1:3)
})

test_that("locf carries the last observation forward (used for simulated KM curves)", {
  expect_equal(vpc:::locf(c(1, NA, NA, 0.8, NA, 0.5)), c(1, 1, 1, 0.8, 0.8, 0.5))
  # leading missing values are filled with the first available value
  expect_equal(vpc:::locf(c(NA, 0.9, NA)), c(0.9, 0.9, 0.9))
})

test_that("triangle_to_full converts a NONMEM-style lower triangle to a symmetric matrix", {
  # OMEGA BLOCK(3) as lower triangle, row-wise
  omega <- vpc:::triangle_to_full(c(0.1, 0.01, 0.2, 0.02, 0.03, 0.3))
  expect_equal(
    omega,
    matrix(c(0.10, 0.01, 0.02,
             0.01, 0.20, 0.03,
             0.02, 0.03, 0.30), nrow = 3)
  )
  expect_equal(omega, t(omega))
  expect_equal(vpc:::triangle_to_full(0.5), matrix(0.5))
  expect_error(vpc:::triangle_to_full(c(0.1, 0.01)), "lower triangle")
})

test_that("create_vpc_theme is deprecated in favour of new_vpc_theme", {
  expect_warning(
    theme <- create_vpc_theme(list(obs_color = "red")),
    "new_vpc_theme"
  )
  expect_equal(theme, new_vpc_theme(list(obs_color = "red")))
})

test_that("verbose = TRUE reports progress of the VPC calculation", {
  msgs <- capture_messages(
    vpc(sim = simple_data$sim, obs = simple_data$obs, verbose = TRUE, vpcdb = TRUE)
  )
  expect_match(msgs, "Binning", all = FALSE)
  expect_match(msgs, "Calculating statistics for observed data", all = FALSE)
  expect_silent(
    vpc(sim = simple_data$sim, obs = simple_data$obs, verbose = FALSE, vpcdb = TRUE)
  )
})
