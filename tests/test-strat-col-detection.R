library(vpc)
library(testit)

mock_dat <- data.frame(1, 1, 1, 1)
col_names <- c("SITE", "GENOTYPE", "RACE", "TRT")
names(mock_dat) <- col_names

all_correct <- c("SITE", "GENOTYPE", "RACE", "TRT")
single_correct<- c("TRT")
not_exist <- c("GENDER")
some_exist <- c("GENOTYPE", "RACE", "GENDER")

assert("diffs captured properly",
   vpc:::is_equal(length(setdiff(some_exist, col_names)), 1)
)
assert("diffs captured properly",
   vpc:::is_equal(length(setdiff(all_correct, col_names)), 0, relative = F)
)
assert("diffs captured properly",
   vpc:::is_equal(length(setdiff(not_exist, col_names)), 1)
)
assert("diffs captured properly",
   vpc:::is_equal(length(setdiff(single_correct, col_names)), 0, relative=F)
)
# expect_error(check_stratification_columns_available(mock_dat, some_exist))

assert("expectation throws error if input not a named software type",
  vpc:::check_stratification_columns_available(mock_dat, all_correct)
)
