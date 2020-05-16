format_vpc_input_data <- function(dat, cols, lloq, uloq, strat, bins, log_y, log_y_min, what = "observed", verbose = FALSE, pred_corr = FALSE) {
  if(cols[["id"]] %in% colnames(dat)) {
    if ("id" %in% colnames(dat) &! cols$id == "id") {
      colnames(dat)[match("id", colnames(dat))] <- "id.old"
    }
    colnames(dat)[match(cols$id, colnames(dat))] <- "id"
  }
  if(is.na(match("id", colnames(dat)))[1]) {
    stop (paste0("No column for id indicator found in ", what, " data, can't continue! Available columns: ", paste(colnames(dat), collapse = " ")))
  }
  if(cols$dv %in% colnames(dat)) {
    if ("dv" %in% colnames(dat) &! cols$dv == "dv") {
      colnames(dat)[match("dv", colnames(dat))] <- "dv.old"
    }
    colnames(dat)[match(cols$dv, colnames(dat))] <- "dv"
  }
  if(is.na(match("dv", colnames(dat)))[1]) {
    stop (paste0("No column for dependent variable found in ", what, " data, can't continue! Available columns: ", paste(colnames(dat), collapse = " ")))
  }
  if(cols$idv %in% colnames(dat)) {
    if ("idv" %in% colnames(dat) &! cols$idv == "idv") {
      colnames(dat)[match("idv", colnames(dat))] <- "idv.old"
    }
    colnames(dat)[match(cols$idv, colnames(dat))] <- "idv"
  }
  if(is.na(match("idv", colnames(dat)))[1]) {
    stop (paste0("No column for indepentent variable found in ", what, " data, can't continue! Available columns: ", paste(colnames(dat), collapse = " ")))
  }
  if(pred_corr) {
    if (!is.null(uloq)) { dat$dv[dat$dv > uloq] <- uloq }
    if (!is.null(lloq)) { dat$dv[dat$dv < lloq] <- lloq }
  } else {
    if (!is.null(uloq)) { dat$dv[dat$dv > uloq] <- NA }
    if (!is.null(lloq)) { dat$dv[dat$dv < lloq] <- NA }
  }
  if (log_y) {
    dat$dv[dat$dv < log_y_min] <- log_y_min
  }
  dat <- add_stratification(dat, strat)
  return(dat)
}
