library(testit)

assert(
  "as.num keeps a number numeric",
  vpc:::as.num(1:3) == 1:3
)
assert(
  "as.num makes a character numeric",
  vpc:::as.num(as.character(1:3)) == 1:3
)
assert(
  "as.num makes a factor numeric",
  vpc:::as.num(factor(LETTERS[1:3])) == 1:3
)
