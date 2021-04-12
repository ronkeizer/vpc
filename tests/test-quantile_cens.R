library(vpc)
library(testit)

assert(
  "quantile_cens returns the expected quantile when no values are censored",
  vpc::quantile_cens(x=1:10, probs=0.3, cens="left", limit=0) ==
    quantile(x=1:10, probs=0.3)
)
assert(
  "quantile_cens returns NA_real_ when the probs value is censored",
  vpc::quantile_cens(x=1:10, probs=0.3, cens="left", limit=6) %==%
    NA_real_
)
assert(
  "quantile_cens returns NA_real_ when the probs value is censored",
  vpc::quantile_cens(x=1:10, probs=0.3, cens="right", limit=6) %==%
    unname(quantile(x=1:10, probs=0.3))
)
assert(
  "quantile_cens returns NA_real_ when the probs value is censored and counts NA values as censored",
  vpc::quantile_cens(x=c(1:10, rep(NA, 10)), probs=0.3, cens="right", limit=6) %==%
    NA_real_
)

assert(
  "loq_frac counts left values correctly",
  vpc:::loq_frac(x=1:10, limit=3, cens="left") == 0.2
)
assert(
  "loq_frac counts right values correctly",
  vpc:::loq_frac(x=1:10, limit=3, cens="right") == 0.7
)
assert(
  "loq_frac only allows `cens` values of 'left' or 'right'",
  class(try(vpc:::loq_frac(x=1:10, limit=3, cens="foo"), silent=TRUE)) == "try-error"
)
