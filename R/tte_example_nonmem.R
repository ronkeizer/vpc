library(vpc)
library(dplyr)
library(survival)

# single time to event
obs <- read.table.nm("nm/sdtab51")
sim <- readRDS(file="nm/simtab51.gz")

## create the VPC, stratified by dose
vpc1 <- vpc_tte(sim, obs, 
                n_bins = 16,
                stratify = "dose",
                facet = "wrap",
                nonmem = TRUE,  # use NONMEM common data labels
                smooth = TRUE) 

# repeated time to event
obs <- read.table.nm("nm/sdtab57")
#sim <- read.table.nm("nm/simtab57")
#saveRDS(sim, "nm/simtab57.gz", compress=TRUE)
sim <- readRDS(file="nm/simtab57.gz")

## create the VPC, stratified by dose
vpc2 <- vpc_tte(sim, obs, 
                rtte = TRUE, 
                rtte_show_occasions = c(1:12),
                n_bins = 25,
                stratify = "dose",
                facet = "wrap",
                nonmem = TRUE,  # use NONMEM common data labels
                smooth = TRUE) 
