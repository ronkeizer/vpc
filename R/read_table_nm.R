#' Read table file simulated with NONMEM
#' 
#' \code{read_table_nm} will read any table simulated from NONMEM in the $TABLE record. Tables can be simulated using either ONEHEADER or NOHEADER, and can be single or multiple subproblems.
#' @param file The file to be read
#' @param perl a boolean or NULL variable indicating whether to use Perl to read in the file. Default is NULL, which will make the function check whether Perl is available. Use of Perl is advised as it is much faster.
#' @export
#' @return a data.frame
#' @seealso \link{vpc}

read_table_nm <- function(file, perl = NULL) {
  file <- gsub("~", path.expand("~"), file)
  if (file.exists(file)) {
    tab_fr <- suppressWarnings(try(data.table::fread(file), silent = TRUE))
    if ("data.frame" %in% class(tab_fr)) {
      data.table::setnames(tab_fr, gsub(" ", "", colnames(tab_fr)))
      sel <- grep("[A-DF-Z]", tab_fr[[1]])
      if(length(sel)>0) {
        return(data.frame(apply(tab_fr[-sel,], 2, as.numeric)))    
      } else {
        return(data.frame(apply(tab_fr, 2, as.numeric)))
      }
    } else {
      cat ("Tip: NONMEM tables can be read much faster when 'NOTITLE FORMAT=,1PE11.4' is used in $TABLE")
      if(is.null(perl)) { 
        perl <- FALSE
        chk <- try(system("perl -v", intern = TRUE, ignore.stderr = TRUE), silent = TRUE)
        if(length(grep("Copyright", chk)) > 0) {
          perl <- TRUE
        }
      }
      if (perl) {
        cmd <- paste0("perl -e 'open (IN, \"<", file, "\"); my $i = 0; my $cols = 0; while (my $line = <IN>) { if ($line =~ m/[a-df-z]/i) { unless($line =~ m/^TABLE NO/ || $cols == 1) { print $line; $cols = 1; } } else { print $line } } ; close(IN);'")
        tab <- read.table(pipe(cmd), header=T);            
      } else {
        cat("Warning: Perl not found either, resorting to slowest method for reading tables.")
        tab <- readLines (file)      
        del_rows <- c(grep("TABLE", tab), grep ("TABLE", tab)[-1] + 1)
        if(length(del_rows) > 0) {
          tab <- tab[-del_rows]        
        }
        read.table(textConnection(tab), skip=1, header=T)               
      }        
    }    
  } else {
    stop("File does not exist!")
  }
}
