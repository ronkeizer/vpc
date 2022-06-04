test_that("as.num keeps a number numeric", {
  expect_equal(vpc:::as.num(1:3), 1:3)
})
test_that("as.num makes a character numeric", {
  expect_equal(vpc:::as.num(as.character(1:3)), 1:3)
})
test_that("as.num makes a factor numeric", {
  expect_equal(vpc:::as.num(factor(LETTERS[1:3])), 1:3)
})
