library(testit)
library(vpc)

nonmem_df <- data.frame(ID = 0, MDV = 0, DV = 0)
phx_df <- data.frame(ID = 0, COBS = 0)

assert("software type returned properly", 
       vpc:::guess_software("NONmem", nonmem_df) == "nonmem")
assert("software type returned properly", 
       vpc:::guess_software("nonmem", phx_df) == "nonmem")
assert("software type returned properly", 
       vpc:::guess_software("auto", nonmem_df) == "nonmem")
assert("software type returned properly", 
       vpc:::guess_software("auto", phx_df) == "phoenix") 
