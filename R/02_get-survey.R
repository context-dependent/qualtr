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

  if(!is.character(survey_id)) {

    ids <- unlist(
      Sys.getenv(
        "QUALTR_LAST_SURVEYS"
      ) %>%
      stringr::str_split("::")
    )
    survey_id <- ids[[1]][survey_id]

  }

  survey_url <- paste0(
    Sys.getenv("QUALTRICS_ROOT_URL"),
    "/API/v3/surveys/",
    survey_id
  )

  req <- httr::GET(survey_url, httr::add_headers(headers()))

  httr::content(req)

}


#' Convert survey list object to markdown and compile as pdf
#'
#' @param srv
#' @param file_name
#' @param browse
#'
#' @return
#' @export
#'
#' @examples
print_survey <- function(srv, file_name, browse = FALSE) {


  rmd_head <- qp_head(srv)

  qs <- srv$result$questions %>%

    purrr::map(qp_print_question)

  if(browse) browser()
  bs <- srv$result$blocks %>%
    qp_set_blocks(qs) %>%
    unlist()

  res <- c(
    rmd_head,
    bs
  ) %>%

    purrr::map_chr(
    ~ stringi::stri_enc_toutf8(.x)
    )

  path <-

    paste0(
      "prints/",
      Sys.Date(), "-",
      file_name,
      ".Rmd"
    )

  write_lines(
    res,
    path
  )

  rmarkdown::render(path)

  res

}

qp_set_blocks <- function(bs, qs) {

  bs %>%

    purrr::map(
      ~qp_set_block(.x, qs)
    )
}

qp_set_block <- function(b, qs) {

  if(!is.list(b)) {

    return(NULL)

  }

  d <- b$description

  qs <- b$elements %>%
    purrr::map("questionId") %>%
    unlist() %>%
    purrr::map(~qs[[.x]])

  c(
    paste0("\\section{", d, "}"),
    "\n\n\\begin{mdframed}\n",
    qs,
     "\n\n\\end{mdframed}\n\n",
    "\\clearpage"
  )

}

qp_print_question <- function(q) {

  type <- q$questionType$type

  print_fun <-

    switch(
      type,

      "DB" = qp_title_text,
      "Matrix" = qp_likert,
      "MC" = qp_mc_single,
      "TE" = qp_text_entry

    )

  c(
    "\\begin{samepage}",
    print_fun(q),
    "\\end{samepage}"
  )

}


qp_title_text <- function(q) {

  # Title

  title <- q$questionName

  # Question text

  text <- q$questionText %>% strip_html()

  res <- c(
    paste0("\\subsection{", snakecase::to_upper_camel_case(title), "}"),
    "",
    text,
    ""
  )

  res

}

qp_mc_single <- function(q, browse = FALSE) {


  if(browse) browser()

  top <- qp_title_text(q)
  cb <- qp_cb(q, center = FALSE)
  choice <-
    q$choices %>%
      purrr::map_chr("choiceText") %>%
      purrr::map_chr(strip_html) %>%
      purrr::map_chr(
      ~ paste0(
          "\\item[", cb, "] ",
          .x,
          "\\\\",
          collapse = ""
        )
      )

  ## Construct table


  out <- c(
    "\n\n\n",
    top,
    "\\begin{itemize}",
    choice,
    "\\end{itemize}"
  )

  out

}

strip_html <- function(x) {

  if(is.null(x)) {

    return(NULL)

  }

  x %>%
    purrr::map_chr(
    ~ stringr::str_remove_all(.x, "<.+?>") %>%
        stringr::str_replace_all("\\n", " ") %>%
        stringr::str_replace_all("[\\`’]", "\\'") %>%
        trimws()
    )

}

qp_likert <- function(q) {


  top <- qp_title_text(q)

  cb <- qp_cb(q, center = TRUE)

  # Knitr table of choices by subquestions

  ## Get subquestions

  qsub <- q$subQuestions %>% purrr::map_chr("choiceText") %>% strip_html()

  ## Get choices

  choice <- q$choices %>% purrr::map_chr("choiceText") %>% strip_html()

  ## Generate checkboxes

  check <-

    as_tibble(
      matrix(
        cb,
        nrow = length(qsub),
        ncol = length(choice)
      )
    )


  align = c("l", rep("c", length(choice)))

  choice_width <- paste0(
    21 / length(choice),
    "em"
  )


  ## Construct table

  res <-

    check %>%
      set_names(choice) %>%
      mutate(item = qsub) %>%
      select(item, everything()) %>%
    knitr::kable(
      format = "latex",
      booktabs = TRUE,
      align = align,
      escape = FALSE
    ) %>%
    kableExtra::column_spec(1, width = "20em") %>%
    kableExtra::column_spec(1:length(choice) + 1, width = choice_width) %>%
    kableExtra::kable_styling(latex_options = "striped")


  out <- c(
    "\n\n\n",
    top,
    res
  )

  ## Return formatted question

  out

}

qp_text_entry <- function(q) {


  s <- q$questionType$selector
  print(s)

  if(s == "FORM") {

    res <- qp_form(q)

  } else {

    res <- c(qp_title_text(q), "\\Qline{8cm}\n\n\n")

  }

  res

}


qp_head <- function(srv) {

  title = srv$result$name


  tmp <- read_lines(
    "templates/00_survey-template.Rmd"
  ) %>%
    stringr::str_replace("SURVEY_TITLE", title) %>%
    stringr::str_replace("TODAY", as.character(Sys.Date()))

  tmp

}


qp_form <- function(q) {

  top <- qp_title_text(q)

  # Isolate labels
  cb <- qp_cb(q, center = FALSE)

  choice <-
    q$choices %>%
    purrr::map_chr("choiceText") %>%
    strip_html() %>%
    purrr::map_chr(
      ~ paste0(
        "\\item[", cb, "] ",
        .x,
        "\\Qline{5cm}",
        collapse = ""
      )
    )

  out <- c(
    "\n\n\n",
    top,
    "\\begin{itemize}",
    paste0(choice, "\\\\"),
    "\\end{itemize}"
  )

  out

}

qp_cb <- function(q, center = TRUE, browse = FALSE) {

  if(browse) browser()

  s <- q$questionType$selector

  if(s == "Likert") {

    s <- q$questionType$subSelector

  }

  ding <- case_when(

    s %in% c(
      "SingleAnswer",
      "SAVR"
    )
    ~ "\\ding{109}",

    s %in% c(
      "MAVR"
    )
    ~ "\\ding{113}",

    s %in% c(
      "FORM"
    )
    ~ "\\ding{47}",

    TRUE ~ "unknown"
  )

  if(center) {

    res <- ding

  } else {

    res <- ding

  }

  res



}
