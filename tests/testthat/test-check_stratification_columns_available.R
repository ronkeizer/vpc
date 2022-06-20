test_that("NULL data returns TRUE", {
  expect_true(vpc:::check_stratification_columns_available(data=NULL, stratify="A"))
})
test_that("NULL stratify returns TRUE (regardless of data)", {
  expect_true(vpc:::check_stratification_columns_available(data="A", stratify=NULL))
})
test_that("Present columns work for one column", {
  expect_true(vpc:::check_stratification_columns_available(data=data.frame(A=1, B=2, C=3), stratify="A"))
})
test_that("Present columns work for multiple columns", {
  expect_true(vpc:::check_stratification_columns_available(data=data.frame(A=1, B=2, C=3), stratify=c("A", "B")))
})
