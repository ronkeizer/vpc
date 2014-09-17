## example for repeated) time-to-event data
## with NONMEM-like data (e.g. simulated using a dense grid)
data(rtte_obs_nm) 
data(rtte_sim_nm) 

# treat RTTE as TTE, no stratification
vpc_tte(rtte_sim_nm, rtte_obs_nm, 
        rtte = FALSE, bins=16, 
        sim.dv = "dv", obs.idv = "t", sim.idv = "t", n_sim = 100)

# stratified for covariate and study arm
vpc_tte(rtte_sim_nm, rtte_obs_nm, 
        stratify = c("sex","drug"), 
        rtte = FALSE, bins=16,
        sim.dv = "dv", obs.idv = "t", sim.idv = "t", n_sim = 100)

# stratified per event number (we'll only look at first 3 events) and stratify per arm
vpc_tte(rtte_sim_nm, rtte_obs_nm,
        rtte = TRUE, occasions = c(1:3),
        stratify = c("drug"), bins=16, 
        sim.dv = "dv", obs.idv = "t", sim.idv = "t", n_sim = 100)
