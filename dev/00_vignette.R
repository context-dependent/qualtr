library(tidyverse)

# LIST SURVEYS ------------------------------------------------------------

list_surveys("OTEC")



# GET SURVEY BY NUMBER ----------------------------------------------------

sc_exit_js <- get_survey(1)



# GET SURVEY BY ID --------------------------------------------------------

sc_exit_js <- get_survey("SV_0OJXn0xdyrgkl9P")



# PRINT SURVEY ------------------------------------------------------------

print_survey(sc_exit_js, "otec_sc-exit_js")
