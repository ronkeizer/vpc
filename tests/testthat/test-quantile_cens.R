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
test_that("quantile_cens returns NA_real_ when the probs value is censored and counts Inf values as censored", {
  expect_equal(
    vpc::quantile_cens(x=c(1:10, rep(Inf, 10)), probs=0.3, cens="right", limit=6),
    NA_real_
  )
  expect_equal(
    vpc::quantile_cens(x=c(1:10, rep(-Inf, 10)), probs=0.3, cens="left", limit=-Inf),
    NA_real_
  )
  expect_equal(
    vpc::quantile_cens(x=c(rep(-Inf, 2), 0.5, 3, 4), probs=0.5, cens="left", limit=-Inf),
    0.5
  )
})
test_that("quantile_cens treats NA values as missing, not censored (#80)", {
  expect_equal(
    vpc::quantile_cens(x=c(1:10, rep(NA, 10)), probs=0.3, cens="right", limit=6),
    unname(quantile(x=1:10, probs=0.3))
  )
  expect_equal(
    vpc::quantile_cens(x=c(1:10, rep(NA, 10)), probs=0.3, cens="left", limit=0),
    unname(quantile(x=1:10, probs=0.3))
  )
  expect_equal(
    vpc::quantile_cens(x=c(NA, 1:10), probs=0.3, cens="neither"),
    unname(quantile(x=1:10, probs=0.3))
  )
})
test_that("quantile_cens returns the normal quantile value when neither direction is censored", {
  expect_equal(
    vpc::quantile_cens(x=1:10, probs=0.3, cens="neither"),
    unname(quantile(x=1:10, probs=0.3))
  )
})
test_that("quantile_cens_inf keeps censored quantiles as -Inf/Inf", {
  expect_equal(vpc:::quantile_cens_inf(x=1:10, probs=0.3, cens="left", limit=6), -Inf)
  expect_equal(vpc:::quantile_cens_inf(x=1:10, probs=0.8, cens="right", limit=6), Inf)
  expect_equal(vpc:::quantile_cens_inf(x=1:10, probs=0.8, cens="left", limit=6), unname(quantile(1:10, 0.8)))
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

test_that("loq_frac counts -Inf/Inf as censored and ignores NA (#80)", {
  expect_equal(vpc:::loq_frac(x=c(-Inf, -Inf, 3, 4), limit=1, cens="left"), 0.5)
  expect_equal(vpc:::loq_frac(x=c(-Inf, NA, 3, 4), limit=1, cens="left"), 1/3)
  expect_equal(vpc:::loq_frac(x=c(Inf, NA, 3, 4), limit=5, cens="right"), 1/3)
})

test_that("censor_dv and cens_to_na", {
  expect_equal(vpc:::censor_dv(c(NA, 0.5, 2, 10), lloq = 1), c(NA, -Inf, 2, 10))
  expect_equal(vpc:::censor_dv(c(NA, 0.5, 2, 10), uloq = 5), c(NA, 0.5, 2, Inf))
  expect_equal(vpc:::censor_dv(c(0.5, 2)), c(0.5, 2))
  expect_equal(vpc:::cens_to_na(data.frame(dv = c(-Inf, 1, Inf)))$dv, c(NA, 1, NA))
  expect_null(vpc:::cens_to_na(NULL))
})
