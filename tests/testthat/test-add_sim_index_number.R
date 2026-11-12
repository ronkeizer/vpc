#' Simulate data based on a model and parameter distributions
#' Helper function to generate test data, not exposed to user.
#' 
sim_data <- function (design = cbind(id = c(1,1,1), idv = c(0,1,2)),
                      model = function(x) { return(x$alpha + x$beta) },
                      theta,
                      omega_mat,
                      par_names,
                      par_values = NULL,
                      draw_iiv = "mvrnorm",
                      error = list(proportional = 0, additive = 0, exponential = 0),
                      n=100) {
  if (is.null(par_values)) {
    param <- draw_params_mvr( # draw parameter values. can also be just population values, or specified manually ()
      ids = 1:n,
      n_sim = n,
      theta,
      omega_mat = triangle_to_full(omega_mat),
      par_names = par_names)
  } else {
    param <- par_values
  }
  sim_des <- do.call("rbind", replicate(n, design, simplify = FALSE))
  sim_des$sim  <- rep(1:n, each=nrow(design[,1]))
  sim_des$join <- paste(sim_des$sim, sim_des$id, sep="_")
  param$join   <- paste(param$sim, param$id, sep="_")
  tmp <- dplyr::as_tibble(merge(sim_des, param,
                                by.x="join", by.y="join"))
  tmp_pred <- cbind(data.frame(design), matrix(rep(theta, each=nrow(design[,1])), ncol=length(theta)))
  colnames(tmp_pred)[length(tmp_pred)-length(par_names)+1:3] <- par_names
  tmp$dv <- add_noise(model(tmp), ruv = error)
  tmp$pred <- rep(model(tmp_pred), n)
  
  colnames(tmp) <- gsub("\\.x", "", colnames(tmp))
  tmp %>%
    dplyr::arrange(sim, id, time)
}

sim_data_tte <- function (fit, t_cens = NULL, n = 100) {
  fit$coefficients <- as.list(fit$coefficients)
  dat <- data.frame(model.matrix(fit))
  for (i in seq(fit$coefficients)) { fit$coefficients[[i]] <- as.numeric (fit$coefficients[[i]]) }
  fact <- as.matrix(attr(fit$terms, "factors"))
  parm <- t(fact) %*% as.numeric(fit$coefficients)
  tmp.single <- data.frame (
    par = exp(as.numeric(fit$coefficients[1]) + as.matrix(dat[,rownames(parm)]) %*% parm),
    dv = 1
  )
  tmp <- do.call("rbind", replicate(n, tmp.single, simplify = FALSE))
  tmp$sim  <- rep(1:n, each=nrow(tmp.single[,1]))
  if (!fit$dist %in% c("exponential", "weibull")) {
    cat (paste("Simulation of ", fit$dist, "distribution not yet implemented, sorry."))
    return()
  }
  if (fit$dist == "exponential") {
    tmp$t = rweibull(nrow(dat[,1]) * n, shape = 1, scale = tmp$par)
    # or using: tmp$t = rexp(length(design$id), 1/tmp$par)
  }
  if (fit$dist == "weibull") {
    # annoyinly, the survreg and rweibull mix up the shape/scale parameter names and also take the inverse!!!
    tmp$t = rweibull(nrow(dat[,1]) * n, shape = 1/fit$scale, scale = tmp$par)
  }
  if (sum(tmp$t > t_cens) > 0) {
    tmp[tmp$t > t_cens,]$t <- t_cens
  }
  out <- c()
  for (i in 1:n) {
    km_fit <- compute_kaplan(tmp[tmp$sim == i,])
    idx_new <- idx + length(unique(tmp[tmp$sim == i,]$t))-1
    out <- rbind(out, cbind(i, km_fit$time, km_fit$surv))
  }
  colnames(out) <- c("sim", "time", "dv")
  dplyr::as_tibble(data.frame(out))
}
test_that("all simulated dataset-indices of equal length", {
  ## Load the theophylline PK dataset
  obs <- Theoph
  colnames(obs) <- c("id", "wt", "dose", "time", "dv")
  obs <- obs %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(sex = round(runif(1))) # randomly assign a "sex" covariate
  
  sim <- sim_data(obs, # the design of the dataset
                  model = function(x) { # the model
                    vpc:::pk_oral_1cmt (t = x$time, dose=x$dose * x$wt, ka = x$ka, ke = x$ke, cl = x$cl * x$wt)
                  },
                  error = list(additive = 0.1),
                  theta = c(2.774, 0.0718, .0361),                 # parameter values
                  omega_mat = c(0.08854,                           # specified as lower triangle by default;
                                0.02421, 0.02241,                  # note: assumed that every theta has iiv, set to 0 if no iiv.
                                0.008069, 0.008639, 0.02862),
                  par_names = c("ka", "ke", "cl"),                 # link the parameters in the model to the thetas/omegas
                  n = 500)
  
  s <- vpc:::add_sim_index_number(sim)
  h <- hist(s)
  expect_equal(length(unique(h$counts)), 1)
})

test_that("add_sim_index_number supports non-numeric ids (#103)", {
  sim <- data.frame(
    id = rep(rep(c(3, 1, 2), each = 2), times = 4),
    dv = 1
  )
  expected <- rep(1:4, each = 6)
  expect_equal(add_sim_index_number(sim), expected)

  sim_chr <- sim
  sim_chr$id <- paste0("subj_", sim$id)
  expect_equal(add_sim_index_number(sim_chr), expected)

  sim_fct <- sim
  sim_fct$id <- factor(paste0("subj_", sim$id))
  expect_equal(add_sim_index_number(sim_fct), expected)
})

test_that("vpc() works with character ids (#103)", {
  tmp <- simple_data
  obs <- tmp$obs
  sim <- tmp$sim
  obs_chr <- obs
  sim_chr <- sim
  obs_chr$ID <- paste0("subj_", obs$ID)
  sim_chr$ID <- paste0("subj_", sim$ID)

  set.seed(1)
  ref <- vpc(sim = sim, obs = obs, pred_corr = TRUE, vpcdb = TRUE)
  set.seed(1)
  obj <- vpc(sim = sim_chr, obs = obs_chr, pred_corr = TRUE, vpcdb = TRUE)
  expect_equal(obj$vpc_dat, ref$vpc_dat)
  expect_equal(obj$aggr_obs, ref$aggr_obs)
})

test_that("vpc_cat() works with character ids (#103)", {
  tmp <- simple_data
  cat <- cut(tmp$obs$DV, breaks = c(-1, 10, 40, 1000))
  tmp$obs$DV <- match(cat, levels(cat))
  cat2 <- cut(tmp$sim$DV, breaks = c(-1, 10, 40, 1000))
  tmp$sim$DV <- match(cat2, levels(cat2))
  ref <- vpc_cat(sim = tmp$sim, obs = tmp$obs, vpcdb = TRUE)

  tmp$obs$ID <- paste0("subj_", tmp$obs$ID)
  tmp$sim$ID <- paste0("subj_", tmp$sim$ID)
  obj <- vpc_cat(sim = tmp$sim, obs = tmp$obs, vpcdb = TRUE)
  expect_equal(obj$vpc_dat, ref$vpc_dat)
  expect_equal(obj$aggr_obs, ref$aggr_obs)
})

test_that("vpc_tte() works with character ids (#103)", {
  sim <- vpc::rtte_sim_nm[1:200000, ]
  sim$sim <- NULL # force derivation of the simulation index from the id column
  obs <- vpc::rtte_obs_nm
  suppressWarnings({
    set.seed(1)
    ref <- vpc_tte(sim = sim, obs = obs, rtte = FALSE,
                   sim_cols = list(dv = "dv", idv = "t"),
                   obs_cols = list(idv = "t"), vpcdb = TRUE)
  })
  sim$id <- paste0("subj_", sim$id)
  obs$id <- paste0("subj_", obs$id)
  suppressWarnings({
    set.seed(1)
    obj <- vpc_tte(sim = sim, obs = obs, rtte = FALSE,
                   sim_cols = list(dv = "dv", idv = "t"),
                   obs_cols = list(idv = "t"), vpcdb = TRUE)
  })
  expect_equal(obj$sim_km, ref$sim_km)
  expect_equal(obj$obs_km, ref$obs_km)
})
