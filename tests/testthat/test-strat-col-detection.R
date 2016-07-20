context("stratification columns")
mock_dat <- data.frame(1, 1, 1, 1)
col_names <- c("SITE", "GENOTYPE", "RACE", "TRT")
names(mock_dat) <- col_names

all_correct <- c("SITE", "GENOTYPE", "RACE", "TRT")
single_correct<- c("TRT")
not_exist <- c("GENDER")
some_exist <- c("GENOTYPE", "RACE", "GENDER")

test_that("diffs captured properly", {
  expect_equal(length(setdiff(some_exist, col_names)), 1)
  expect_equal(length(setdiff(all_correct, col_names)), 0)
  expect_equal(length(setdiff(not_exist, col_names)), 1)
  expect_equal(length(setdiff(single_correct, col_names)), 0)
})

test_that("expectation throws error if input not a named software type", {
  expect_error(check_stratification_columns_available(mock_dat, some_exist)) 
  expect_true(check_stratification_columns_available(mock_dat, all_correct))
})