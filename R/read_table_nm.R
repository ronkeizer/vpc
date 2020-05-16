#' NONMEM output table import function
#'
#' @description Quickly import NONMEM output tables into R.
#' Function taken from `modelviz` package by Benjamin Guiastrennec.
#' When both \code{skip} and \code{header} are \code{NULL},
#' \code{read_nmtab} will automatically detect the optimal
#' settings to import the tables. When more than one files are
#' provided for a same NONMEM run, they will be combined into
#' a single \code{data.frame}.
#'
#' @param file full file name
#' @param skip number of lines to skip before reading data
#' @param header logical value indicating whether the file contains the names
#' of the variables as its first line
#' @param rm_duplicates logical value indicating whether duplicated columns should be removed
#' @param nonmem_tab logical value indicating to the function whether the file is a
#' table or a nonmem additional output file.
#'
#' @return A \code{data.frame}
#' @examples
#' \dontrun{
#' data <- read_table_nm(file = '../models/pk/sdtab101')
#' }
#' @export
read_table_nm <- function(
  file = NULL,
  skip = NULL,
  header = NULL,
  rm_duplicates = FALSE,
  nonmem_tab = TRUE) {

    # Check inputs
    if(is.null(file)) {
      stop('Argument \"file\" required.')
    }

    if(!any(file.exists(file))) {
      stop('No file not found.')
    } else {
      file <- file[file.exists(file)]
    }

    if(nonmem_tab) {
      # If auto mode required
      if(is.null(skip) & is.null(header)) {
        test    <- readLines(file[1], n = 3)
        skip    <- ifelse(grepl('TABLE NO', test[1]), 1, 0)
        header  <- ifelse(grepl('[a-zA-Z]', test[2]), TRUE, FALSE)
      }

      # Import data
      tab_file <- do.call('cbind', lapply(file, readr::read_table,
                                          skip = skip, col_names = header))

      tab_file <- as.data.frame(apply(tab_file, MARGIN = 2, FUN = as.numeric))

      # Drop rows with NA (in simtab)
      tab_file <- na.omit(tab_file)

      # Correct bug in the headers
      if(header) {
        colnames(tab_file)[grepl('\n',colnames(tab_file))] <-
          gsub('\n.+', '', colnames(tab_file)[grepl('\n', colnames(tab_file))])
      }

    } else {
      # Search for final results only
      skip     <- max(grep('TABLE NO', readLines(file[1])))

      # Import all files
      tab_file <- do.call('cbind', lapply(file, read.table, skip = skip,
                                          header = FALSE, fill = TRUE, as.is = TRUE))
      colnames(tab_file) <- tab_file[1, ]
      tab_file <- suppressWarnings(as.data.frame(apply(tab_file[-1, ], 2, as.numeric)))
    }

    if(rm_duplicates) {
      tab_file <- tab_file[, !duplicated(colnames(tab_file))]
    }

    return(tab_file)

}
