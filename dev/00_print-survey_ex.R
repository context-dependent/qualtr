##ctrl shift l
library(tidyverse)

# LIST SURVEYS ------------------------------------------------------------

list_surveys("OTEC")



# GET SURVEY BY NUMBER ----------------------------------------------------

sc_exit_js <- get_survey(1)



# GET SURVEY BY ID --------------------------------------------------------

sc_exit_js <- get_survey("SV_0OJXn0xdyrgkl9P")


# PRINT SURVEY ------------------------------------------------------------

print_survey(sc_exit_js, "otec_sc-exit_js")



# PRINT WITH DISPLAY LOGIC ------------------------------------------------

sc_intake_js_dl <- read_qsf("qsf/otec_sc_intake_js.qsf")

print_survey(sc_intake_js_dl, "otec_sc-intake_dl")
