library(dplyr)
library(vpc)
library(testit)
Sys.setenv("R_TESTS" = "")

## Load the theophylline PK dataset
obs <- Theoph
colnames(obs) <- c("id", "wt", "dose", "time", "dv")
obs <- obs %>%
  dplyr::group_by(id) %>%
  mutate(sex = round(runif(1))) # randomly assign a "sex" covariate
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
assert("all simulated dataset-indices of equal length",
  vpc:::is_equal(length(unique(h$counts)), 1))
