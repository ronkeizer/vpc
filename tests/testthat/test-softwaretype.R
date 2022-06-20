test_that("software type", {
  nonmem_df <- data.frame(ID = 0, MDV = 0, DV = 0)
  phx_df <- data.frame(ID = 0, COBS = 0)

  expect_equal(vpc:::guess_software("NONmem", nonmem_df), "nonmem")
  expect_equal(vpc:::guess_software("nonmem", phx_df), "nonmem")
  expect_equal(vpc:::guess_software("auto", nonmem_df), "nonmem")
  expect_equal(vpc:::guess_software("auto", phx_df), "phoenix")
})
