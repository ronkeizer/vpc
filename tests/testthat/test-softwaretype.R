context("softwaretype")

nonmem_df <- data.frame(ID = 0, MDV = 0, DV = 0)
phx_df <- data.frame(ID = 0, COBS = 0)

test_that("software type returned properly", {
  expect_equal(guess_software("NONmem", nonmem_df), "nonmem")
  expect_equal(guess_software("nonmem", phx_df), "nonmem")
  expect_equal(guess_software("auto", nonmem_df), "nonmem")
  expect_equal(guess_software("auto", phx_df), "phoenix")  
})

test_that("expectation throws error if input not a named software type", {
  expect_error(guess_software("nothing"), 
               "Please define one of the following software types: auto, nonmem, phoenix")
})