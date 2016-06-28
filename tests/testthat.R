Sys.setenv("R_TESTS" = "")
library(testthat)
library(vpc)

test_check("vpc")
