Sys.setenv("R_TESTS" = "")
library(testthat)
library(vpc)
library(ggplot2)
library(dplyr)

test_check("vpc")
