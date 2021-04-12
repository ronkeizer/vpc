library(vpc)
library(testit)

nonmem_df <- data.frame(ID = 1, ADV = c(1, 0, 0), DV = 1)

# Check standardize_column ####

nonmem_df_id <- data.frame(id = 1, ADV = c(1, 0, 0), DV = 1)

str_contains <- function(x, value) {
  ret <- grepl(x=as.character(x), pattern=value, fixed=TRUE)
  if (!ret) {
    cat(
      "From: ", as.character(x), "\n",
      "Expected but did not find: ", value, "\n",
      sep=""
    )
  }
  ret
}

assert(
  "colname must be in names(cols)",
  str_contains(
    x=try(
      vpc:::standardize_column(dat=nonmem_df, cols=list(ID="ID"), colname="foo", coldesc="identifier"),
      silent=TRUE
    ),
    value="'colname' (foo) must be in 'names(cols)'.  Available names: ID"
  )
)
assert(
  "cols[[colname]] must be in names(dat), unless (see next test)",
  str_contains(
    x=try(
      vpc:::standardize_column(dat=nonmem_df, cols=list(foo="foo"), colname="foo", coldesc="identifier", what="errormsg"),
      silent=TRUE
    ),
    value="No column for identifier indicator found in errormsg data, can't continue! Available columns: ID, ADV, DV"
  )
)
assert(
  "When the column exists and it is named the same as colname even if cols is not the same, input is returned as output",
  vpc:::standardize_column(dat=nonmem_df, cols=list(ID="foo"), colname="ID", coldesc="identifier") %==%
    nonmem_df
)
assert(
  "When the column exists and it is named the same, input is returned as output",
  vpc:::standardize_column(dat=nonmem_df, cols=list(ID="ID"), colname="ID", coldesc="identifier") %==%
    nonmem_df
)
assert(
  "When the column exists and it is not named the same, input is returned with the column renamed",
  vpc:::standardize_column(dat=nonmem_df, cols=list(id="ID"), colname="id", coldesc="identifier") %==%
    nonmem_df_id
)

# Test format_vpc_input_data ####
formatted_nonmem_df <-
  dplyr::mutate(
    dplyr::rename(nonmem_df, id="ID", idv="ADV", dv="DV"),
    strat=factor(1)
  )

assert(
  "format_vpc_input_data returns the source data unmodified when lloq, uloq, strat, log_y, log_y_min, and pred_corr have no effect",
  vpc:::format_vpc_input_data(
    dat=nonmem_df,
    cols=list(id="ID", idv="ADV", dv="DV"),
    log_y=FALSE, log_y_min=1e-3,
    lloq=NULL, uloq=NULL,
    stratify=NULL,
    what="observed",
    verbose=FALSE,
    pred_corr=FALSE
  ) %==%
    formatted_nonmem_df
)

formatted_nonmem_df_lloq_nopredc <- formatted_nonmem_df
formatted_nonmem_df_lloq_nopredc$dv <- NA_real_
assert(
  "format_vpc_input_data returns the source data modified by lloq (NA if pred_corr == FALSE)",
  vpc:::format_vpc_input_data(
    dat=nonmem_df,
    cols=list(id="ID", idv="ADV", dv="DV"),
    log_y=FALSE, log_y_min=1e-3,
    lloq=2, uloq=NULL,
    stratify=NULL,
    what="observed",
    verbose=FALSE,
    pred_corr=FALSE
  ) %==%
    formatted_nonmem_df_lloq_nopredc
)

formatted_nonmem_df_lloq_yespredc <- formatted_nonmem_df
formatted_nonmem_df_lloq_yespredc$dv <- 2
assert(
  "format_vpc_input_data returns the source data modified by lloq (lloq if pred_corr == TRUE)",
  vpc:::format_vpc_input_data(
    dat=nonmem_df,
    cols=list(id="ID", idv="ADV", dv="DV"),
    log_y=FALSE, log_y_min=1e-3,
    lloq=2, uloq=NULL,
    stratify=NULL,
    what="observed",
    verbose=FALSE,
    pred_corr=TRUE
  ) %==%
    formatted_nonmem_df_lloq_yespredc
)

formatted_nonmem_df_uloq_nopredc <- formatted_nonmem_df
formatted_nonmem_df_uloq_nopredc$dv <- NA_real_
assert(
  "format_vpc_input_data returns the source data modified by uloq (NA if pred_corr == FALSE)",
  vpc:::format_vpc_input_data(
    dat=nonmem_df,
    cols=list(id="ID", idv="ADV", dv="DV"),
    log_y=FALSE, log_y_min=1e-3,
    lloq=NA, uloq=0.5,
    stratify=NULL,
    what="observed",
    verbose=FALSE,
    pred_corr=FALSE
  ) %==%
    formatted_nonmem_df_uloq_nopredc
)

formatted_nonmem_df_uloq_yespredc <- formatted_nonmem_df
formatted_nonmem_df_uloq_yespredc$dv <- 0.5
assert(
  "format_vpc_input_data returns the source data modified by uloq (uloq if pred_corr == TRUE)",
  vpc:::format_vpc_input_data(
    dat=nonmem_df,
    cols=list(id="ID", idv="ADV", dv="DV"),
    log_y=FALSE, log_y_min=1e-3,
    lloq=NA, uloq=0.5,
    stratify=NULL,
    what="observed",
    verbose=FALSE,
    pred_corr=TRUE
  ) %==%
    formatted_nonmem_df_uloq_yespredc
)

formatted_nonmem_df_logy_2 <- formatted_nonmem_df
formatted_nonmem_df_logy_2$dv <- 2
assert(
  "format_vpc_input_data returns the source data modified by log_y_min",
  vpc:::format_vpc_input_data(
    dat=nonmem_df,
    cols=list(id="ID", idv="ADV", dv="DV"),
    log_y=TRUE, log_y_min=2,
    lloq=NULL, uloq=NULL,
    stratify=NULL,
    what="observed",
    verbose=FALSE,
    pred_corr=TRUE
  ) %==%
    formatted_nonmem_df_logy_2
)
