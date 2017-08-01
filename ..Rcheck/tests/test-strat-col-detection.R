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
   length(setdiff(some_exist, col_names)) == 1
)
assert("diffs captured properly", 
   length(setdiff(all_correct, col_names)) == 0
)
assert("diffs captured properly", 
   length(setdiff(not_exist, col_names)) == 1
)
assert("diffs captured properly", 
   length(setdiff(single_correct, col_names)) == 0
)
# expect_error(check_stratification_columns_available(mock_dat, some_exist)) 

assert("expectation throws error if input not a named software type",
  vpc:::check_stratification_columns_available(mock_dat, all_correct)
)
