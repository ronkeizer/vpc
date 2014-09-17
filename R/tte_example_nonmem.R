library(vpc)
library(dplyr)
library(survival)

# single time to event
obs1 <- read.table.nm("nm/sdtab51", perl=FALSE)
sim1 <- readRDS(file="nm/simtab51.gz")

## create the VPC, stratified by dose
vpc1 <- vpc_tte(sim1, obs1, 
                n_bins = 16,
                stratify = "dose",
                facet = "wrap",
                nonmem = TRUE,  # use NONMEM common data labels
                smooth = TRUE) 

# repeated time to event
obs2 <- read.table.nm("nm/sdtab57", perl=FALSE)
sim2 <- readRDS(file="nm/simtab57.gz")
#sim <- read.table.nm("nm/simtab57")
#saveRDS(sim, "nm/simtab57.gz", compress=TRUE)

## create the VPC, stratified by dose
vpc2 <- vpc_tte(sim2, obs2, 
                rtte = TRUE, 
                occasions = c(1:4),
                n_bins = 25,
                stratify = "dose",
                facet = "column",
                nonmem = TRUE,  # use NONMEM common data labels
                smooth = TRUE) 
