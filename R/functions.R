msg <- function(txt, verbose = FALSE) {
  if(verbose) {
    message(txt)
  }
}

add_recurs <- function(x, n, max) {
  x <- x + n
  n <- n + 1
  if (n <= max) {
    x <- add_recurs(x, n, max)
  }
  x
}

locf <- function(S) {
  L <- !is.na(S)
  c(S[L][1], S[L])[cumsum(L)+1]
}

as.num <- function(x) {
  if (is.numeric(x)) {
    x
  } else if (is.factor(x)) {
    as.integer(x)
  } else {
    as.numeric(as.character(x))
  }
}
