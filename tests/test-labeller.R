library(vpc)
library(testit)
Sys.setenv("R_TESTS" = "")

tmp <- simple_data
sex <- (tmp$obs$ID) %% 2
race <- (tmp$obs$ID) %% 3
tmp$obs$sex <- sex
tmp$obs$race <- race
tmp$sim$sex <- sex
tmp$sim$race <- race

v1 <- vpc(sim = tmp$sim, obs = tmp$obs, stratify = "race",
          facet = "wrap", vpcdb = F, labeller = ggplot2::label_both)
assert("plot succeeded", "ggplot" %in% class(v1))
assert("facets succeeded", length(v1$facet$params$facets) == 1)
