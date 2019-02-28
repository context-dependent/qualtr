scc <- function() {

  list(
    prep = quos(
      matches("Not at all"),
      matches("A little"),
      matches("Somewhat"),
      matches("Mostly"),
      matches("Completely")),
    rec  = quos(
      matches("Definitely not"),
      matches("Probably not"),
      matches("No opinion"),
      matches("Probably rec"),
      matches("Definitely rec")),
    satis  = quos(
      matches("^Very dis"),
      matches("^Dis"),
      matches("Neutral"),
      matches("^Satis"),
      matches("^Very satis")),
    agree = quos(
      matches("^Strongly dis"),
      matches("^Dis"),
      matches("Neutral"),
      matches("^Agree"),
      matches("^Strongly ag")),
    effect_size = quos(
      matches("No effect"),
      matches("Small effect"),
      matches("Medium effect"),
      matches("Large effect"),
      matches("Very large effect")),
    likely = quos(
      matches("Very unlikely"),
      matches("Unlikely"),
      matches("Not sure"),
      matches("Likely"),
      matches("Very likely")

      )


  )

}



likert_scales <- list(
  agree = c(
    "Strongly disagree"                  = 1,
    "Disagree"                           = 2,
    "Neutral"                            = 3,
    "Agree"                              = 4,
    "Strongly agree"                     = 5
  ),
  confident = c(
    "Not at all confident"               = 1,
    "A little confident"                 = 2,
    "Somewhat confident"                 = 3,
    "Very confident"                     = 4,
    "Completely confident"               = 5
  ),
  difficult = c(
    "Extreme difficulty or can't do"     = 1,
    "A lot of difficulty"                = 2,
    "Some difficulty"                    = 3,
    "A little difficulty"                = 4,
    "No difficulty"                      = 5
  ),
  often = c(
    "Never"                              = 1,
    "Once or twice"                      = 2,
    "About once a week"                  = 3,
    "About 2 or 3 times a week"          = 4,
    "Almost every day"                   = 5,
    "Every day"                          = 6
  ),
  often_2 = c(
    "Almost never"                       = 1,
    "Rarely"                             = 2,
    "Sometimes"                          = 3,
    "Often"                              = 4,
    "Most of the time"                   = 5
  ),
  often_3 = c(
    "Never"                              = 1,
    "Rarely"                             = 2,
    "Sometimes"                          = 3,
    "Often"                              = 4,
    "All of the time"                    = 5
  ),
  effect_size = c(
    "No effect"                          = 1,
    "Small effect"                       = 2,
    "Medium effect"                      = 3,
    "Large effect"                       = 4,
    "Very large effect"                  = 5
  ),
  satisfied = c(
    "Extremely dissatisfied"             = 1,
    "Somewhat dissatisfied"              = 2,
    "Neither satisfied nor dissatisfied" = 3,
    "Somewhat satisfied"                 = 4,
    "Extremely satisfied"                = 5
  ),
  interested = c(
    "Not interested at all"              = 1,
    "Slightly interested"                = 2,
    "Somewhat interested"                = 3,
    "Very interested"                    = 4,
    "Extremely interested"               = 5

  )
)

#' Print the scales used in recoding functinos
#'
#' @return
#' @export
#'
#' @examples
print_scales <- function() {

  print(likert_scales)

}

#' Recode scale with numeric values
#'
#' @param dat
#' @param .vars
#' @param .rev
#' @param scale
#' @param fct
#'
#' @return
#' @export
#'
#' @examples
scr_num <- function(dat, .vars, .rev = NULL, scale = "agree", fct = FALSE) {



  cludge <- mean(likert_scales[[scale]])

  dat <-

    dat %>%
      dplyr::mutate_at(
        .vars,
        dplyr::funs(
          . %>%
            trimws() %>%
            purrr::map_dbl(~likert_scales[[scale]][.x]))
        )

  if(!is.null(.rev)) {

    dat <- dat %>%
      dplyr::mutate_at(
        .rev,
        dplyr::funs((. - cludge) * -1 + cludge)
      )

  }

  dat

}

#' Generate a scale score for a set of columns
#'
#' @param srv
#' @param var_name
#' @param .vars
#' @param .rev
#' @param scale
#'
#' @return
#' @export
#'
#' @examples
score_scale <- function(srv, var_name,
                        .vars,
                        .rev = vars(matches("_r")),
                        scale = "agree") {

  coded_scale <-

    srv %>%

    dplyr::select(!!!.vars) %>%
    scr_num(.vars, .rev, scale = scale) %>%
    dplyr::mutate(i = row_number()) %>%
    dplyr::group_by(i) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      items = data %>% purrr::map_dbl(~sum(!is.na(.x))),
      total = data %>% purrr::map_dbl(~sum(.x, na.rm = TRUE)),
      score = total / items
    ) %>%
    tidyr::unnest() %>%
    dplyr::ungroup() %>%
    dplyr::select(items, total, score) %>%
    dplyr::rename_all(funs(str_c(var_name, "_", .)))

  coded_scale
}


