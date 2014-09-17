add_step <- function(dat = ., vars) {
    dat$step <- 0
    tmp <- dat[-1,]
    tmp$step <- 1
    tmp[,vars] <- dat[-length(dat[,1]), vars]    
    newdat <- data.frame(rbind(dat, tmp))
    newdat %>% arrange(bin, -step)    
}

add_stratification <- function (dat, strat) {
  if(is.null(strat)) {
    dat$strat <- 1
  } else {
    dat$strat <- ""
    for(i in seq(strat)) {
      if(i > 1) { 
        dat$strat <- paste0(dat$strat, ", ")
      }
      dat$strat <- paste0(dat$strat, strat[i], "=", dat[,strat[i]])
    }
  }  
  dat$strat <- as.factor(dat$strat)
  return(dat)
}

triangle_to_full <- function (vect) {
  for (i in 1:100) { # find the size of the matrix
    if (length(vect) == add_recurs(0,0,i)) {
      nr = i    
    }
  }
  k_given_i_j <- function(x , y ) ifelse( y<x, x*(x-1)/2 + y, y*(y-1)/2 + x )
  k_mat <- function(p) outer( 1:p, 1:p, k_given_i_j )
  return (matrix(vect[ k_mat( nr ) ] , nr = nr ))
}

add_recurs <- function(x, n, max) {
  x <- x + n
  n <- n + 1
  if (n <= max) {
    x <- add_recurs(x, n, max)
  }
  x
}

find_nadirs <- function (x, thresh = 0) {
  pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) > 0) + 2
  if (!missing(thresh)) {
    pks[x[pks - 1] - x[pks] > thresh]
  }
  else pks
}

draw_params_mvr <- function(ids, n_sim, theta, omega_mat, par_names = NULL) {
  n_ids <- length(ids)
  if (!is.null(par_names)) {
    par <- data.frame(cbind(sim = rep(1:n_sim, each=n_ids), 
                            id = rep(ids, n_sim),
                            rep(theta, each=n_sim*n_ids) * exp (mvrnorm(n=n_sim*n_ids, c(0,0,0), omega_mat))))
    colnames(par) <- c("sim", "id", par_names)  
    return(par)    
  } else {
    cat("Parameter names have to be supplied!")
  }
}

#' Simulate PK data from a 1-compartment oral model
#' 
#' @param t Time after dose
#' @param tau Dosing interval
#' @param dose Dose
#' @param ka Absorption rate
#' @param ke Elimination rate
#' @param ruv Residual variability
#' @param par_names A vector describing 
#' @return A vector of predicted values, with or without added residual variability
#' @export
pk_oral_1cmt <- function (t, tau = 24, dose=120, ka = 1, ke = 1, cl = 10, ruv = NULL) {
  v = cl / ke
  tmp <- (dose/v) * (ka/(ka-ke)) * (exp(-ke*t) - exp(-ka*(t)))
  if(!is.null(ruv)) {
    tmp <- add_noise (tmp, ruv)
  }
  tmp
}

pk_iv_1cmt <- function (t, t_inf = 1, tau = 24, dose=120, CL = 0.345, Vc = 1.75, ruv = NULL) {
  k <- CL / Vc
  tmp <- c()
  tmp <- c(tmp, (dose / (CL * t_inf)) * (1-exp(-k*t[t < t_inf])) )
  tmp <- c(tmp, (dose / (CL * t_inf)) * (1-exp(-k*t_inf)) * exp(-k*(t[t >= t_inf] - t_inf)) )  
  if(!is.null(ruv)) {
    tmp <- add_noise (tmp, ruv)
  }
  tmp
}

#' @export
add_noise <- function(x, ruv = list(proportional = 0, additive = 0, exponential = 0)) {
  if (is.null(ruv$proportional)) { ruv$proportional <- 0 }
  if (is.null(ruv$additive)) { ruv$additive <- 0 }
  if (is.null(ruv$exponential)) { ruv$exponential <- 0 }
  x * (1 + rnorm(length(x), 0, ruv$proportional)) +  rnorm(length(x), 0, ruv$additive) * exp(rnorm(length(x), 0, ruv$exponential)) 
} 

#' @export
bin_data <- function(x, bins = c(0, 3, 5, 7), idv = "time") {
  x$bin <- cut(x[[idv]], bins, labels = FALSE, right=FALSE)
  return(x)
}

as.num <- function(x) { as.numeric(as.character(x)) }

themes <- list(
  "default" = list(
    pi_area = "#3388cc", pi_area_alpha = 0.2,  
    med_area = "#3388cc", med_area_alpha = 0.4  
  )
)

theme_plain <-  function () {
  theme(
    text = element_text(family="mono"),
    plot.title = element_text(family="sans", size = 16, vjust = 1.5),
    axis.title.x = element_text(family="sans",vjust=-0.25),
    axis.title.y = element_text(family="sans"),
    legend.background = element_rect(fill = "white"),
    legend.position = c(0.14, 0.80),
    panel.grid.major = element_line(colour = "#e5e5e5"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#efefef", colour = NA),
    strip.background = element_rect(fill = "#444444", colour = NA),
    strip.text = element_text(face="bold", colour = "white")
  )    
}

read.table.nm <- function(file, perl = TRUE) {
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

