assert(
  "NULL data returns TRUE",
  vpc:::check_stratification_columns_available(data=NULL, stratify="A")
)
assert(
  "NULL stratify returns TRUE (regardless of data)",
  vpc:::check_stratification_columns_available(data="A", stratify=NULL)
)
assert(
  "Present columns work for one column",
  vpc:::check_stratification_columns_available(data=data.frame(A=1, B=2, C=3), stratify="A")
)
assert(
  "Present columns work for multiple columns",
  vpc:::check_stratification_columns_available(data=data.frame(A=1, B=2, C=3), stratify=c("A", "B"))
)
