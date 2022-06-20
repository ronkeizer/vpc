test_that("as_vpcdb requires type", {
  expect_equal(
    tryCatch(as_vpcdb(a=1), error=function(e) e$message),
    "`type` must be specified"
  )
})

test_that("as_vpcdb has a class for the type, vpcdb, and list", {
  expect_equal(
    as_vpcdb(type="foo"),
    structure(
      list(
        type="foo",
        labeller=NULL,
        facet=NULL,
        scales=NULL
      ),
      class=c("vpcdb_foo", "vpcdb", "list")
    )
  )
})

test_that("as_vpcdb requires valid facet, if given", {
  expect_equal(
    tryCatch(as_vpcdb(type="foo", facet="foo"), error=function(e) e$message),
    "`facet` argument needs to be one of `wrap`, `columns`, or `rows`."
  )
})

test_that("as_vpcdb changes facet 'grid' to 'rows'", {
  expect_equal(
    as_vpcdb(type="foo", facet="grid"),
    structure(
      list(
        type="foo",
        labeller=NULL,
        facet="rows",
        scales=NULL
      ),
      class=c("vpcdb_foo", "vpcdb", "list")
    )
  )
})

test_that("as_vpcdb requires valid scales, if given", {
  expect_equal(
    tryCatch(as_vpcdb(type="foo", scales="foo"), error=function(e) e$message),
    "`scales` argument needs to be one of `fixed`, `free_y`, `free_x` or `free`."
  )
})
