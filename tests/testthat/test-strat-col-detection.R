test_that("strat column detection", {
  mock_dat <- data.frame(SITE=1, GENOTYPE=1, RACE=1, TRT=1)
  col_names <- names(mock_dat)

  all_correct <- c("SITE", "GENOTYPE", "RACE", "TRT")
  single_correct<- c("TRT")
  not_exist <- c("GENDER")
  some_exist <- c("GENOTYPE", "RACE", "GENDER")

  expect_equal(
    length(setdiff(some_exist, col_names)), 1,
    info="diffs captured properly"
  )
  expect_equal(
    length(setdiff(all_correct, col_names)), 0,
    info="diffs captured properly"
  )
  expect_equal(
    length(setdiff(not_exist, col_names)), 1,
    info="diffs captured properly"
  )
  expect_equal(
    length(setdiff(single_correct, col_names)), 0,
    info="diffs captured properly"
  )

  expect_error(
    check_stratification_columns_available(mock_dat, some_exist),
    regexp="GENDER"
  )

  expect_true(
    vpc:::check_stratification_columns_available(mock_dat, all_correct),
    info="expectation throws error if input not a named software type"
  )
})
