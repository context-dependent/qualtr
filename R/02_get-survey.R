#' Get survey document as list
#'
#' @param survey_id
#'
#'
#' @return
#' @export
#'
#' @examples
get_survey <- function(survey_id) {

  survey_id <- env_id(survey_id)

  survey_url <- paste0(
    Sys.getenv("QUALTRICS_ROOT_URL"),
    "/API/v3/surveys/",
    survey_id
  )

  req <- httr::GET(survey_url, httr::add_headers(headers()))

  httr::content(req)

}
