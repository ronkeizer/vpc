#' Read table file simulated with NONMEM
#' 
#' \code{read_table_nm} will read any table simulated from NONMEM in the $TABLE record. Tables can be simulated using either ONEHEADER or NOHEADER, and can be single or multiple subproblems.
#' @param file The file to be read
#' @param perl a boolean parameter indicating whether to use Perl to read in the file. Default is TRUE. Use of Perl is advised as it is much faster.
#' @export
#' @return a data.frame
#' @seealso \link{vpc}

read_table_nm <- function(file, perl = TRUE) {
  if (perl) {
    cmd <- paste0("perl -e 'open (IN, \"<", file, "\"); my $i = 0; my $cols = 0; while (my $line = <IN>) { if ($line =~ m/[a-df-z]/i) { unless($line =~ m/^TABLE NO/ || $cols == 1) { print $line; $cols = 1; } } else { print $line } } ; close(IN);'")
    tab <- read.table (pipe(cmd), header=T);    
  } else { # extremely slow....
    tab <- readLines (file)
    skip <- grep('/[a-z]/i', tab)[1] - 1
    del_rows <- c(grep("TABLE", tab)[-1] , grep ("TABLE", tab)[-1] + 1)
    tab <- tab[-del_rows]
    read.table(textConnection(tab), skip=1, header=T) 
  }    
}