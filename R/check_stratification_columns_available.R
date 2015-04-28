## checking whether stratification columns are available
check_stratification_columns_available <- function(data, stratify, type = "observation") {
  diffs <- setdiff(stratify, names(data))
  if(length(diffs) >=1) {
    stop(paste0("The following specified stratification columns were NOT found in ",type, " data: \n", 
                  paste(diffs, collapse = ",")))
  }
  return(TRUE)
}
