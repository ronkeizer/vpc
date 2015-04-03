# guess software package based on data
# @param software string specifying type of software
# @param x analysis data from software
# dont document for Roxygen as internal function
guess_software <- function(software, x) {
  options <-  c("auto", "nonmem", "phoenix", "PKPDsim")
  software <- tolower(software)
  if(!software %in% options) {
    stop(paste("Please define one of the following software types:", paste(options, collapse=", ")))
  }
  if(software == "nonmem" | software == "phoenix") return(software)

  # nonmem typically will have MDV and DV
  software <- "other"
  if(all(c("ID", "TIME") %in% names(x)) || all(c("ID", "DV") %in% names(x)) || all(c("MDV", "DV") %in% names(x)) || all(c("EVID", "DV") %in% names(x))) {
    software <- "nonmem"
  }
  if("COBS" %in% names(x)) {
    software <- "phoenix"
  }
  if("PKPDsim" %in% class(x)) {
    software <- "PKPDsim"
  }
  return(software)
}
