#' Import qsf file for printing
#'
#' @param path
#' @param browse
#'
#' @return
#' @export
#'
#' @examples
read_qsf <- function(path, browse = FALSE) {

  if(browse) browser()

  qsf <- jsonlite::read_json(path)

  qs_raw <- qsf$SurveyElements[
    purrr::map_lgl(qsf$SurveyElements, ~.x$Element == "SQ")
  ]

  srv <- get_survey(qsf$SurveyEntry$SurveyID)

  bs <- srv$result$blocks

  qsf_body <- function(q) {

    res <- list(
      id           = q$PrimaryAttribute,
      questionName = q$Payload$DataExportTag,
      questionType = list(
        type         = q$Payload$QuestionType,
        selector     = q$Payload$Selector,
        subSelector  = q$Payload$subSelector %||% q$Payload$Selector
      ),
      questionText = q$Payload$QuestionText,
      choices      = q$Payload$Choices %>%
        map(
          ~ list(
            description = .x$Display,
            choiceText  = .x$Display
          )
        ),
      displayLogic = q$Payload$DisplayLogic[[1]] %>%
        map("Description") %>%
        map(strip_html) %>%
        map(strip_html) %>%
        unlist() %>%
        unname()
    )



    if(res$questionType$type == "SBS") {
      res$columns <- q$Payload$AdditionalQuestions %>%
        purrr::map(qsf_sbs_col)
      res$subQuestions <- res$columns[[1]]$subQuestions
    }

    if(res$questionType$type %in% c("Matrix")) {
      res$subQuestions <- res$choices
      res$choices <- q$Payload$Answers %>%
        purrr::map(
        ~ list(
            description = .x$Display,
            choiceText  = .x$Display
          )
        )
    }

    res

  }


  qsf_sbs_col <- function(col) {


    list(
      questionType = list(
        type         = col$QuestionType,
        selector     = col$Selector,
        subSelector  = col$SubSelector %||% col$Selector
      ),
      questionText = col$QuestionText,
      subQuestions = col$Choices %>%
        map(
          ~ list(
            description = .x$Display,
            choiceText  = .x$Display
          )
        ),
      choices = col$Answers %>%
        map(
          ~ list(
            description = .x$Display,
            choiceText  = .x$Display
          )
        )
    )


  }


  qs_00 <- qs_raw %>%
    map(qsf_body)

  qs_01 <- qs_00 %>%

    set_names(
      qs_00 %>% map_chr("id")
    )

  qs_01 <- qs_01[qs_01 %>% purrr::map_lgl(~.x$questionType$selector != "Profile")]


  srv <- list(
    result = list(
      questions = qs_01,
      blocks = bs,
      name = qsf$SurveyEntry$SurveyName
    )
  )

  srv



}

qp_display_logic <- function(q) {

  res <- c(
    "\\begin{dlbox}",
    q$displayLogic,
    "\\end{dlbox}"
  )

  res

}

filter_ls <- function(x, ...) {



}
