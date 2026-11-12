# Synthetic 1-cmt oral PK data for rcVPC fixtures
set.seed(105)
out <- commandArgs(TRUE)[1]
ids <- 1:6; times <- c(1, 2, 4, 8, 12)
dose <- c(50, 100, 200, 50, 100, 200)        # observed design
wt   <- c(50, 60, 70, 80, 90, 100)
dose_ref <- 100; wt_ref <- 70                   # reference design
pk <- function(t, dose, wt, eta_cl = 0, eta_v = 0) {
  cl <- 5 * (wt/70)^0.75 * exp(eta_cl); v <- 50 * (wt/70) * exp(eta_v); ka <- 1
  dose * ka / (v * (ka - cl/v)) * (exp(-cl/v * t) - exp(-ka * t))
}
grid <- expand.grid(time = times, id = ids)[, c("id", "time")]
grid$rec <- seq_len(nrow(grid))
grid$dose <- dose[grid$id]; grid$wt <- wt[grid$id]
grid$pred <- pk(grid$time, grid$dose, grid$wt)
grid$predref <- pk(grid$time, dose_ref, wt_ref)
simulate <- function(d, w, n) {
  do.call(rbind, lapply(seq_len(n), function(s) {
    eta_cl <- rnorm(length(ids), 0, 0.3)[grid$id]
    eta_v  <- rnorm(length(ids), 0, 0.2)[grid$id]
    ipred <- pk(grid$time, d, w, eta_cl, eta_v)
    data.frame(sim = s, rec = grid$rec, id = grid$id, time = grid$time,
               dv = ipred * exp(rnorm(nrow(grid), 0, 0.15)) + rnorm(nrow(grid), 0, 0.05))
  }))
}
obs <- simulate(grid$dose, grid$wt, 1)
obs <- data.frame(rec = grid$rec, id = grid$id, time = grid$time, dv = obs$dv,
                  pred = grid$pred, predref = grid$predref)
sim <- simulate(grid$dose, grid$wt, 20); sim$pred <- grid$pred[sim$rec]
ref <- simulate(dose_ref, wt_ref, 20);  ref$pred <- grid$predref[ref$rec]
stopifnot(all(obs$dv > 0), all(sim$dv > 0), all(ref$dv > 0))
write.csv(obs, file.path(out, "obs.csv"), row.names = FALSE)
write.csv(sim, file.path(out, "sim.csv"), row.names = FALSE)
write.csv(ref, file.path(out, "sim_ref.csv"), row.names = FALSE)
