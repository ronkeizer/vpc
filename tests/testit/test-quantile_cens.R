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
  "quantile_cens returns the normal quantile value when neither direction is censored",
  vpc::quantile_cens(x=1:10, probs=0.3, cens="neither") %==%
    unname(quantile(x=1:10, probs=0.3))
)
assert(
  "quantile_cens returns an error when neither direction is censored and there is an NA in the data",
  grepl(
    x=as.character(try(vpc::quantile_cens(x=c(NA, 1), probs=0.3, cens="neither"), silent=TRUE)),
    pattern="NA are not allowed when cens='neither'",
    fixed=TRUE
  )
)
assert(
  "quantile_cens returns an error for trying to censor both directions",
  tryCatch(
    vpc::quantile_cens(x=1:10, probs=0.3, cens="both", limit=6),
    error=function(e) e$message
  ) == "cens='both' is not yet supported"
)
assert(
  "quantile_cens returns an error for an invalid 'cens'",
  class(try(
    vpc::quantile_cens(x=1:10, probs=0.3, cens="foo", limit=6),
    silent=TRUE
  )) == "try-error"
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
  "loq_frac only allows valid `cens` values",
  class(try(vpc:::loq_frac(x=1:10, limit=3, cens="foo"), silent=TRUE)) == "try-error"
)
assert(
  "loq_frac counts neither direction values correctly",
  vpc:::loq_frac(x=1:10, limit=NULL, cens="neither") == 0
)
assert(
  "loq_frac counts neither direction values correctly (and ignores `limit`)",
  vpc:::loq_frac(x=1:10, limit=1, cens="neither") == 0
)
assert(
  "loq_frac counts both values correctly",
  vpc:::loq_frac(x=1:10, limit=c(3, 6), cens="both") == 0.6
)
# loq_frac errors
assert(
  "loq_frac checks limit correctly with cens='left'",
  tryCatch(
    vpc:::loq_frac(x=1:10, limit=2:3, cens="left"),
    error=function(e) e$message
  ) == "limit must be a scalar if cens='left'"
)
assert(
  "loq_frac checks limit correctly with cens='right'",
  tryCatch(
    vpc:::loq_frac(x=1:10, limit=2:3, cens="right"),
    error=function(e) e$message
  ) == "limit must be a scalar if cens='right'"
)
assert(
  "loq_frac checks limit correctly with cens='both'",
  tryCatch(
    vpc:::loq_frac(x=1:10, limit=2, cens="both"),
    error=function(e) e$message
  ) == "limit must have 2 elements if cens='both'"
)
