## read obs en sim data from NONMEM tables
library(RCurl)
obs <- getURL("https://raw.githubusercontent.com/ronkeizer/vpc/master/nm/sdtab51")  
read.table.nm(obs, pipe=TRUE)
# sim <- tbl_df(read.table.nm("nm/simtab51"))
# saveRDS(sim, file="nm/simtab51.gz", compress = TRUE)
sim <- readRDS(file="nm/simtab51.gz")

## create the VPC, stratified by dose
vpc1 <- vpc_tte(sim, obs, 
                n_bins = 15,
                stratify = "dose",
                facet = "wrap",
                nonmem = TRUE,  # use NONMEM common data labels
                smooth = TRUE) + xlab("Test")
