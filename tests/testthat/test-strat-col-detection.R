context("stratification columns")

col_names <- c("SITE", "GENOTYPE", "RACE", "TRT")
all_correct <- c("SITE", "GENOTYPE", "RACE", "TRT")
single_correct<- c("TRT")
not_exist <- c("GENDER")
some_exist <- c("GENOTYPE", "RACE", "GENDER")

setdiff(some_exist, col_names)
setdiff(single_correct, col_names)
setdiff(not_exist, col_names)
test_that("diffs captured properly", {
  expect_equal(length(setdiff(some_exist, col_names)), 1)
  expect_equal(length(setdiff(all_correct, col_names)), 0)
  expect_equal(length(setdiff(not_exist, col_names)), 1)
  expect_equal(length(setdiff(single_correct, col_names)), 0)
})

test_that("expectation throws error if input not a named software type", {
  expect_error(guess_software("nothing"), 
               "Please define one of the following software types: auto, nonmem, phoenix")
})