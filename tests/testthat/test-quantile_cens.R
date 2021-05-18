test_that("quantile_cens returns the expected quantile when no values are censored", {
  expect_equal(
    vpc::quantile_cens(x=1:10, probs=0.3, cens="left", limit=0),
    unname(quantile(x=1:10, probs=0.3))
  )
})
test_that("quantile_cens returns NA_real_ when the probs value is censored", {
  expect_equal(
    vpc::quantile_cens(x=1:10, probs=0.3, cens="left", limit=6),
    NA_real_
  )
})
test_that("quantile_cens returns NA_real_ when the probs value is censored", {
  expect_equal(
    vpc::quantile_cens(x=1:10, probs=0.3, cens="right", limit=6),
    unname(quantile(x=1:10, probs=0.3))
  )
})
test_that("quantile_cens returns NA_real_ when the probs value is censored and counts NA values as censored", {
  expect_equal(
    vpc::quantile_cens(x=c(1:10, rep(NA, 10)), probs=0.3, cens="right", limit=6),
    NA_real_
  )
})
test_that("quantile_cens returns the normal quantile value when neither direction is censored", {
  expect_equal(
    vpc::quantile_cens(x=1:10, probs=0.3, cens="neither"),
    unname(quantile(x=1:10, probs=0.3))
  )
})
test_that("quantile_cens returns an error when neither direction is censored and there is an NA in the data", {
  expect_error(
    vpc::quantile_cens(x=c(NA, 1), probs=0.3, cens="neither"),
    regexp="NA are not allowed when cens='neither'",
    fixed=TRUE
  )
})
test_that("quantile_cens returns an error for trying to censor both directions", {
  expect_error(
    vpc::quantile_cens(x=1:10, probs=0.3, cens="both", limit=6),
    regexp="cens='both' is not yet supported",
    fixed=TRUE
  )
})
test_that("quantile_cens returns an error for an invalid 'cens'", {
  expect_error(
    vpc::quantile_cens(x=1:10, probs=0.3, cens="foo", limit=6)
  )
})

test_that("loq_frac counts left values correctly", {
  expect_equal(vpc:::loq_frac(x=1:10, limit=3, cens="left"), 0.2)
})
test_that("loq_frac counts right values correctly", {
  expect_equal(vpc:::loq_frac(x=1:10, limit=3, cens="right"), 0.7)
})
test_that("loq_frac only allows valid `cens` values", {
  expect_error(
    vpc:::loq_frac(x=1:10, limit=3, cens="foo")
  )
})
test_that("loq_frac counts neither direction values correctly", {
  expect_equal(vpc:::loq_frac(x=1:10, limit=NULL, cens="neither"), 0)
})
test_that("loq_frac counts neither direction values correctly (and ignores `limit`)", {
  expect_equal(vpc:::loq_frac(x=1:10, limit=1, cens="neither"), 0)
})
test_that("loq_frac counts both values correctly", {
  expect_equal(vpc:::loq_frac(x=1:10, limit=c(3, 6), cens="both"), 0.6)
})

test_that("loq_frac errors", {
  expect_error(
    vpc:::loq_frac(x=1:10, limit=2:3, cens="left"),
    regexp= "limit must be a scalar if cens='left'",
    fixed=TRUE,
    info="loq_frac checks limit correctly with cens='left'"
  )

  expect_error(
    vpc:::loq_frac(x=1:10, limit=2:3, cens="right"),
    regexp="limit must be a scalar if cens='right'",
    info="loq_frac checks limit correctly with cens='right'"
  )
  
  expect_error(
    vpc:::loq_frac(x=1:10, limit=2, cens="both"),
    regexp="limit must have 2 elements if cens='both'",
    info="loq_frac checks limit correctly with cens='both'"
  )
})
