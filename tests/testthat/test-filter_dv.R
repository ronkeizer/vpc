normal_df <- data.frame(ID = 1, MDV = c(1, 0, 0), DV = 1)

nonmem_df <- data.frame(ID = 1, ADV = c(1, 0, 0), DV = 1)
nonmem_mdv <- normal_df
nonmem_evid <- data.frame(ID = 1, EVID = c(1, 0, 0), DV = 1)
nonmem <- data.frame(ID = 1, MDV = c(1, 0, 0), EVID = c(0, 1, 0), DV = 1)
nonmem_filtered_mdv <- nonmem_mdv[2:3, c("ID", "MDV", "DV")]
nonmem_filtered_evid <- nonmem_evid[2:3, c("ID", "EVID", "DV")]
nonmem_filtered <- nonmem[3, ]
class(nonmem_mdv)      <- c("nonmem", "data.frame")
class(nonmem_evid)     <- c("nonmem", "data.frame")
class(nonmem)          <- c("nonmem", "data.frame")
class(nonmem_filtered) <- c("nonmem", "data.frame")
class(nonmem_df)       <- c("nonmem", "data.frame")
class(nonmem_filtered_mdv) <- c("nonmem", "data.frame")
class(nonmem_filtered_evid) <- c("nonmem", "data.frame")

test_that("filtering values for nonmem works properly", {
  expect_equal(
    vpc:::filter_dv(nonmem_mdv),
    nonmem_filtered_mdv
  )
})

test_that("filtering values for nonmem works properly", {
  expect_equal(vpc:::filter_dv(nonmem_evid), nonmem_filtered_evid)
})

test_that("filtering values for nonmem works properly", {
  expect_equal(vpc:::filter_dv(nonmem), nonmem_filtered)
})

test_that("filtering values for nonmem works properly", {
  expect_equal(vpc:::filter_dv(nonmem_df), nonmem_df)
})

test_that("filtering values for unknown data gives a warning", {
  expect_warning(
    expect_equal(
      vpc:::filter_dv(normal_df), normal_df,
      info="filtering values for unknown data does nothing"
    ),
    regexp="No software packages matched for filtering values"
  )
})
