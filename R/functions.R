#' Add noise / residual error to data
#' 
#' @param x data
#' @param ruv list describing the magnitude of errors. List arguments: "proportional", "additive", "exponential".
#' @export
add_noise <- function(x, ruv = list(proportional = 0, additive = 0, exponential = 0)) {
  if (is.null(ruv$proportional)) { ruv$proportional <- 0 }
  if (is.null(ruv$additive)) { ruv$additive <- 0 }
  if (is.null(ruv$exponential)) { ruv$exponential <- 0 }
  x * (1 + rnorm(length(x), 0, ruv$proportional)) +  rnorm(length(x), 0, ruv$additive) * exp(rnorm(length(x), 0, ruv$exponential)) 
} 

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

as.num <- function(x) { as.numeric(as.character(x)) }

add_sim_index_number <- function (sim, id = "id") { # for multiple simulations in a single dataframe, add an index number for every simulation
  sim[[id]] <- as.num(sim[[id]])
  sim_id <- unique(sim[[id]])
  sim$id_shift <- c(sim[[id]][2:length(sim[[id]])], 0) 
  idx <- c(1, (1:length(sim[[id]]))[sim[[id]] == tail(sim_id,1) & sim$id_shift == sim_id[1]], length(sim[[id]])+1)
  sim$sim <- 0
  for (i in 1:(length(idx)-1)) {
    sim$sim[idx[i] : (idx[i+1]-1)] <- i 
  }
  return(sim$sim)
}

triangle_to_full <- function (vect) {
  for (i in 1:100) { # find the size of the matrix
    if (length(vect) == add_recurs(0,0,i)) {
      nr = i    
    }
  }
  k_given_i_j <- function(x , y ) ifelse( y<x, x*(x-1)/2 + y, y*(y-1)/2 + x )
  k_mat <- function(p) outer( 1:p, 1:p, k_given_i_j )
  return (matrix(vect[ k_mat( nr ) ] , nrow = nr ))
}

add_recurs <- function(x, n, max) {
  x <- x + n
  n <- n + 1
  if (n <= max) {
    x <- add_recurs(x, n, max)
  }
  x
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

convert_to_dense_grid <- function(dat, t = "t", id = "id", t_start = 0, t_step = 1, add = NULL) {
  t = seq(from=t_start, to=max(dat$t), by=t_step)
  tmp <- data.frame(cbind(id = rep(unique(dat$id), each = length(t)), 
                          t  = rep(t, n = length(unique(dat$id))) ) )
  tmp$dv <- 0
  id_t <- paste0(dat$id, "-", dat$t)
  tmp[match(id_t, paste0(tmp$id,"-",tmp$t)),]$dv <- dat$dv
  tmp$rtte <- 0
  tmp[match(id_t, paste0(tmp$id,"-",tmp$t)),]$rtte <- 1
  if (!is.null(add)) {
    tmp2 <- merge(tmp, dat[,c("id", add)] %>% group_by(id) %>% do(.[1,]), by = "id", all.y = FALSE)
  }
  return(tmp2)
}

relative_times <- function (dat) { 
  tmp <- dat %>% group_by(id)
  if (dim(tmp %>% filter(length(time) > 1))[1] == 0) { # tte
    tmp2 <- tmp
  } else { # repeated tte
    tmp2 <- rbind(tmp %>% filter(length(time) > 1) %>% mutate(time = time - c(0,time[1:(length(time)-1)])),
                  tmp %>% filter(length(time) == 1) )
  }
  return(tmp2 %>% arrange(id, time))
}

convert_from_dense_grid <- function (dat) { # note: only for a single trial, requires a loop or ddply for multiple subproblems
  tmp <- dat %>% group_by(id)  
  if("rtte" %in% names(dat)) {
    tmp <- tmp %>% filter (rtte == 1)
  }
  #  filter (dv == 1 | time == max(time) )
  tmp2 <- rbind(tmp %>% filter(length(time) > 1) %>% mutate(time = time - c(0,time[1:(length(time)-1)])),
                tmp %>% filter(length(time) == 1) )
  return(tmp2 %>% arrange(id, time))
}

