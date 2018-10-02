
# LIST SURVEYS ------------------------------------------------------------

list_surveys("INput surveys")



# GET RESPONSES -----------------------------------------------------------

dat_00_yss <-

  get_responses(2) %>%
  haven::as_factor()






# Q TAB -------------------------------------------------------------------

t_00_wellbg_raw <- qt_raw(

  dat_00_yss,
  vars(matches("wellbg_1"))

)

qt_print(t_00_wellbg_raw)


