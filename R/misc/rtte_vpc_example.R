## example for repeated) time-to-event data
## with NONMEM-like data (e.g. simulated using a dense grid)
library(vpc)
data(rtte_obs_nm) 
data(rtte_sim_nm) 

# treat RTTE as TTE, no stratification
vpc_tte(rtte_sim_nm, rtte_obs_nm, 
        rtte = FALSE, bin_method="obs",
        sim_dv = "dv", obs_idv = "t", sim_idv = "t", n_sim = 100)

# stratified for covariate and study arm
vpc_tte(rtte_sim_nm, rtte_obs_nm, 
        stratify = c("sex","drug"), 
        rtte = FALSE, bin_method = "spread", n_bins=16,
        sim_dv = "dv", obs_idv = "t", sim_idv = "t", n_sim = 100)

# stratified per event number (we'll only look at first 3 events) and stratify per arm
vpc_tte(rtte_sim_nm, rtte_obs_nm,
        rtte = TRUE, occasions = c(1:3),
        stratify = c("drug"), bin_method="obs", 
        sim_dv = "dv", obs_idv = "t", sim_idv = "t", n_sim = 100)
