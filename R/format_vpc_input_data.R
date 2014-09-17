format_vpc_input_data <- function(dat, dv, idv, id, lloq, uloq, strat, bins, log_y, log_y_min, nonmem) {
  if (nonmem) {
    dv = "DV"
    idv = "TIME"
    id = "ID"
    if("MDV" %in% colnames(dat)) {
      dat <- dat[dat$MDV == 0,]
    }
    if("EVID" %in% colnames(dat)) {
      dat <- dat[dat$EVID == 0,]
    }
  }
  if(id %in% colnames(dat)) {
    if ("id" %in% colnames(dat) &! id == "id") {
      colnames(dat)[match("id", colnames(dat))] <- "id.old"
    }
    colnames(dat)[match(id, colnames(dat))] <- "id"    
  }
  if(is.na(match("id", colnames(dat)))[1]) {
    cat ("No id column found in data, stopping!")
    stop()
  }  
  if(dv %in% colnames(dat)) {
    if ("dv" %in% colnames(dat) &! dv == "dv") {
      colnames(dat)[match("dv", colnames(dat))] <- "dv.old"
    }
    colnames(dat)[match(dv, colnames(dat))] <- "dv"    
  }
  if(is.na(match("dv", colnames(dat)))[1]) {
    cat ("No dv column found in data, stopping!")
    stop()
  }  
  if(idv %in% colnames(dat)) {
    if ("idv" %in% colnames(dat) &! idv == "idv") {
      colnames(dat)[match("idv", colnames(dat))] <- "idv.old"
    }
    colnames(dat)[match(idv, colnames(dat))] <- "idv"    
  }
  if(is.na(match("idv", colnames(dat)))[1]) {
    cat ("No idv column found in data, stopping!")
    stop()
  }  
  if (!is.null(uloq)) { dat$dv[dat$dv > uloq] <- uloq }
  if (!is.null(lloq)) { dat$dv[dat$dv < lloq] <- lloq }
  if (log_y) {
    dat$dv[dat$dv < log_y_min] <- log_y_min 
  }
  dat <- add_stratification(dat, strat)
  dat <- bin_data(dat, bins, "idv")  
  return(dat)
}
