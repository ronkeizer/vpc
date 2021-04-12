library(vpc)
library(testit)

assert(
  "loq_frac counts left values correctly",
  loq_frac(x=1:10, limit=3, cens="left") == 0.2
)
assert(
  "loq_frac counts right values correctly",
  loq_frac(x=1:10, limit=3, cens="right") == 0.7
)
assert(
  "loq_frac only allows `cens` values of 'left' or 'right'",
  class(try(loq_frac(x=1:10, limit=3, cens="foo"), silent=TRUE)) == "try-error"
)
