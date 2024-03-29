test_that("define_loq errors if both limits are given", {
  expect_equal(
    class(try(vpc:::define_loq(lloq=0, uloq=1), silent=TRUE)),
    "try-error"
  )
})
test_that("define_loq errors if neither limit is given, and a limit is required", {
  expect_equal(
    class(try(vpc:::define_loq(lloq=NULL, uloq=NULL, require_loq=TRUE), silent=TRUE)),
    "try-error"
  )
})
test_that("define_loq allows neither limit to be given", {
  expect_equal(
    vpc:::define_loq(lloq=NULL, uloq=NULL, require_loq=FALSE),
    list(
      lloq=NULL,
      uloq=NULL,
      cens_limit=NULL,
      pred_corr=FALSE,
      pred_corr_lower_bnd=0,
      cens_type="neither"
    )
  )
})

test_that("define_loq sets lloq, cens_limit, and cens_type correctly", {
  expect_equal(
    vpc:::define_loq(lloq=1, uloq=NULL, require_loq=FALSE),
    list(
      lloq=1,
      uloq=NULL,
      cens_limit=1,
      pred_corr=FALSE,
      pred_corr_lower_bnd=0,
      cens_type="left"
    )
  )
})

test_that("define_loq sets uloq, cens_limit, and cens_type correctly", {
  expect_equal(
    vpc:::define_loq(lloq=NULL, uloq=1, require_loq=FALSE),
    list(
      lloq=NULL,
      uloq=1,
      cens_limit=1,
      pred_corr=FALSE,
      pred_corr_lower_bnd=0,
      cens_type="right"
    )
  )
})
