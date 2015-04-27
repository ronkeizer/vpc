## checking whether stratification columns are available
check_stratification_columns_available <- function(data, stratify, type = "observation") {
  if(!all(stratify %in% names(data))) {
    stop(paste0("Not all specified stratification columns were found in ",type, " data"))
  }
}
