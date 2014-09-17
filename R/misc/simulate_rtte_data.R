## Simulation of RTTE data
library(dplyr)
sim_rtte <- function (n = 200, 
                      rate=0.01, 
                      rate_iiv_cv = 0.3, 
                      t_cens = 100,
                      id_offset = 0,
                      min_gap = 0,
                      round_t = FALSE) {
  comb <- c()
  rate_draw <- rate * exp(rnorm(n, 0, rate_iiv_cv))
  for (i in 1:n) {
    tmp <- data.frame(
      cbind(id = i + id_offset, 
            t = cumsum(min_gap + rexp(50, rate = rate_draw[i])),  # add 1
            dv = 0)) %>% 
      function ( x = . ) { 
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
    comb <- rbind(comb, tmp)
  }
  if(round_t) {
    comb$t  <- round(comb$t)
    comb$dt <- round(comb$dt)
  }
  return(comb)
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

convert_from_dense_grid <- function (dat) { # note: only for a single trial, requires a loop or ddply for multiple subproblems
  tmp <- dat %>%
    group_by(id) %>% 
    filter (dv == 1 | time == max(time))
  tmp2 <- rbind(tmp %>% filter(length(time) > 1) %>% mutate(time = time - c(0,time[1:(length(time)-1)])),
                tmp %>% filter(length(time) == 1) )
  return(tmp2 %>% arrange(id, time))
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
  rtte_sim_nm <- rbind(rtte_sim_nm,
                    data.frame(sim = i,
                               convert_to_dense_grid (tmp, t_start = 1, t_step = 1, t = "t", add = c("drug", "sex"))))
}
rtte_sim_nm <- tbl_df(rtte_sim_nm)
save(rtte_sim_nm, file = "data/rtte_sim_nm.rda")

tmp2 <- convert_to_dense_grid (tmp, t_start = 1, t_step = 1, t = "t", add = c("drug", "sex"))
