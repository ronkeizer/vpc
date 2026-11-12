# Write a NONMEM-style output table ($TABLE with default ONEHEADER/NOTITLE off)
write_nm_table <- function(file, data, title = TRUE, header = TRUE) {
  fmt <- function(x) formatC(x, format = "E", digits = 4, width = 12)
  lines <- c(
    if (title) "TABLE NO.  1",
    if (header) trimws(paste0(" ", paste(formatC(names(data), width = -12), collapse = "")), "right"),
    apply(data, 1, function(row) paste(fmt(row), collapse = ""))
  )
  writeLines(lines, file)
}

nm_dat <- data.frame(
  ID = c(1, 1, 1, 2, 2, 2),
  TIME = c(0, 1, 2, 0, 1, 2),
  DV = c(0, 5.5, 3.25, 0, 7.1, 4.4),
  PRED = c(0, 5, 3, 0, 5, 3)
)

test_that("read_table_nm reads a NONMEM table with title and header", {
  f <- tempfile()
  on.exit(unlink(f))
  write_nm_table(f, nm_dat)
  res <- suppressMessages(read_table_nm(f))
  expect_s3_class(res, "data.frame")
  expect_equal(names(res), names(nm_dat))
  expect_equal(res, nm_dat, ignore_attr = TRUE)
})

test_that("read_table_nm auto-detects a table without title line", {
  f <- tempfile()
  on.exit(unlink(f))
  write_nm_table(f, nm_dat, title = FALSE)
  res <- suppressMessages(read_table_nm(f))
  expect_equal(res, nm_dat, ignore_attr = TRUE)
})

test_that("read_table_nm removes repeated table headers in simulation output", {
  # with $SIM (NSUBPROBLEMS > 1) NONMEM repeats the TABLE NO. / header lines
  f1 <- tempfile()
  f2 <- tempfile()
  on.exit(unlink(c(f1, f2)))
  write_nm_table(f1, nm_dat)
  write_nm_table(f2, nm_dat)
  writeLines(c(readLines(f1), readLines(f2)), f1)
  res <- suppressWarnings(suppressMessages(read_table_nm(f1)))
  expect_equal(nrow(res), 2 * nrow(nm_dat))
  expect_false(anyNA(res))
  expect_equal(res$DV, rep(nm_dat$DV, 2))
})

test_that("read_table_nm combines multiple tables from the same run", {
  sdtab <- tempfile()
  patab <- tempfile()
  on.exit(unlink(c(sdtab, patab)))
  write_nm_table(sdtab, nm_dat)
  write_nm_table(patab, data.frame(ID = nm_dat$ID, CL = c(5, 5, 5, 7, 7, 7)))
  res <- suppressMessages(read_table_nm(c(sdtab, patab)))
  expect_equal(ncol(res), 6)
  expect_equal(res$CL, c(5, 5, 5, 7, 7, 7))
  res_nodup <- suppressMessages(read_table_nm(c(sdtab, patab), rm_duplicates = TRUE))
  expect_equal(names(res_nodup), c("ID", "TIME", "DV", "PRED", "CL"))
  # non-existing files are skipped as long as one file exists
  res_missing <- suppressMessages(read_table_nm(c(sdtab, tempfile())))
  expect_equal(res_missing, nm_dat, ignore_attr = TRUE)
})

test_that("read_table_nm reads the final estimates from additional output files", {
  f <- tempfile(fileext = ".ext")
  on.exit(unlink(f))
  writeLines(c(
    "TABLE NO.     1: First Order Conditional Estimation: Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION",
    " ITERATION    THETA1       THETA2       OBJ",
    "            0  1.00000E+00  2.00000E+01    500.000",
    "  -1000000000  1.50000E+00  2.50000E+01    400.000",
    "TABLE NO.     2: Importance Sampling: Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION",
    " ITERATION    THETA1       THETA2       OBJ",
    "            0  1.40000E+00  2.40000E+01    410.000",
    "  -1000000000  1.60000E+00  2.60000E+01    390.000"
  ), f)
  res <- read_table_nm(f, nonmem_tab = FALSE)
  expect_equal(names(res), c("ITERATION", "THETA1", "THETA2", "OBJ"))
  expect_equal(res$THETA1, c(1.4, 1.6))
  expect_equal(res$OBJ, c(410, 390))
})

test_that("read_table_nm gives informative errors", {
  expect_error(read_table_nm(), "required")
  expect_error(read_table_nm(file.path(tempdir(), "does_not_exist")), "not found")
})

test_that("vpc() can read observed and simulated data from a PsN vpc folder", {
  psn <- file.path(tempfile(), "vpc_dir1")
  dir.create(file.path(psn, "m1"), recursive = TRUE)
  on.exit(unlink(dirname(psn), recursive = TRUE))
  obs <- simple_data$obs[simple_data$obs$MDV == 0, c("ID", "TIME", "DV")]
  sim <- simple_data$sim[simple_data$sim$MDV == 0 & simple_data$sim$REP <= 20, c("ID", "TIME", "DV")]
  write_nm_table(file.path(psn, "m1", "vpc_original.npctab.dta"), obs)
  # PsN writes all simulations in one table, repeating the ID sequence
  write_nm_table(file.path(psn, "m1", "vpc_simulation.1.npctab.dta"), sim)

  # NONMEM column names (ID, TIME, DV) are used by default
  db <- suppressWarnings(suppressMessages(vpc(psn_folder = psn, vpcdb = TRUE)))
  expect_equal(nrow(db$obs), nrow(obs))
  expect_equal(length(unique(db$sim$sim)), 20)
  expect_s3_class(plot_vpc(db), "ggplot")

  # supplied observed data takes precedence over the PsN folder
  db_obs <- suppressWarnings(suppressMessages(
    vpc(obs = obs[obs$ID <= 5, ], psn_folder = psn, vpcdb = TRUE)
  ))
  expect_equal(nrow(db_obs$obs), sum(obs$ID <= 5))
})
