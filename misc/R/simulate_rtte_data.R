library(dplyr)

## Simulation of RTTE data
sim_rtte <- function (n = 200, 
                      rate=0.01, 
                      rate_iiv_cv = 0.3, 
                      t_cens = 100,
                      id_offset = 0,
                      min_gap = 0,
                      round_t = FALSE) {
  comb <- c()
  rate_draw <- rate * exp(rnorm(n, 0, rate_iiv_cv))
  calc <- function ( x = . ) { 
    x[x$t > t_cens,]$t <- t_cens
    x$dt <- x$t
    x <- x[x$t < t_cens | !duplicated(x$t),]
    if (length(x$t) > 1) { 
      x$dt[2:length(x$t)] <- x$dt[2:length(x$t)] - x$dt[1:(length(x$t)-1)]
    }
    if (sum(x$t < t_cens) > 0) { 
      x[x$t < t_cens,]$dv <- 1 
    }
    return(x) 
  }
  for (i in 1:n) {
    tmp <- data.frame(
      cbind(id = i + id_offset, 
            t = cumsum(min_gap + rexp(50, rate = rate_draw[i])),  # add 1
            dv = 0)) %>% calc()
      
    comb <- rbind(comb, tmp)
  }
  if(round_t) {
    comb$t  <- round(comb$t)
    comb$dt <- round(comb$dt)
  }
  return(comb)
}

# simulate some trial data (two arms) + one covariate (sex) having 50% higher hazard rate
trial_dat <- tbl_df(rbind(cbind(sim_rtte(rate = 0.01*1.5, rate_iiv_cv = 0.3, n=50, id_offset=0, min_gap=1, round_t=TRUE), drug = 1, sex=1),
                          cbind(sim_rtte(rate = 0.01, rate_iiv_cv = 0.3, n=50, id_offset=50, min_gap=1, round_t=TRUE), drug = 1, sex=0),
                          cbind(sim_rtte(rate = 0.02*1.5, rate_iiv_cv = 0.3, n=50, id_offset=100, min_gap=1, round_t=TRUE), drug = 0, sex=1),
                          cbind(sim_rtte(rate = 0.02, rate_iiv_cv = 0.3, n=50, id_offset=150, min_gap=1, round_t=TRUE), drug = 0, sex=0)))
#rtte_obs_nm <- tbl_df(convert_to_dense_grid (trial_dat, t_start = 0, t_step = 1, t = "t", add = c("drug", "sex")))
rtte_obs_nm <- tbl_df(trial_dat)
save(rtte_obs_nm, file = "data/rtte_obs_nm.rda")

# repeated simulation
rtte_sim_nm <- c()
for (i in 1:100) {
  tmp <-  rbind(cbind(sim_rtte(rate = 0.01*1.5, rate_iiv_cv = 0.3, n=50, id_offset=0, min_gap=1, round_t=TRUE), drug = 1, sex=1),
                cbind(sim_rtte(rate = 0.01, rate_iiv_cv = 0.3, n=50, id_offset=50, min_gap=1, round_t=TRUE), drug = 1, sex=0),
                cbind(sim_rtte(rate = 0.02*1.5, rate_iiv_cv = 0.3, n=50, id_offset=100, min_gap=1, round_t=TRUE), drug = 0, sex=1),
                cbind(sim_rtte(rate = 0.02, rate_iiv_cv = 0.3, n=50, id_offset=150, min_gap=1, round_t=TRUE), drug = 0, sex=0))
  rtte_sim_nm <- bind_rows(rtte_sim_nm,
                    data.frame(sim = i, tmp))
  # rtte_sim_nm <- rbind(rtte_sim_nm,
  #                   data.frame(sim = i,
  #                              convert_to_dense_grid (tmp, t_start = 1, t_step = 1, t = "t", add = c("drug", "sex"))))
}
rtte_sim_nm <- tbl_df(rtte_sim_nm)
save(rtte_sim_nm, file = "data/rtte_sim_nm.rda")

tmp2 <- convert_to_dense_grid (tmp, t_start = 1, t_step = 1, t = "t", add = c("drug", "sex"))
