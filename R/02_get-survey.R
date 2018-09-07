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
print_survey <- function(srv, file_name, print_internal = TRUE, browse = FALSE) {

  rmd_head <- qp_head(srv)

  qs <- srv$result$questions %>%

    purrr::imap(~qp_print_question(.x, .y, print_internal = print_internal))

  if(browse) browser()
  bs <- srv$result$blocks %>%
    qp_set_blocks(qs, print_internal = print_internal) %>%
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

  readr::write_lines(
    res,
    path
  )

  rmarkdown::render(path)

  res

}

qp_set_blocks <- function(bs, qs, print_internal = TRUE) {

  bs %>%

    purrr::map(
      ~qp_set_block(.x, qs, print_internal = TRUE)
    )
}

qp_set_block <- function(b, qs, print_internal = TRUE) {

  if(!is.list(b)) {

    return(NULL)

  }

  d <- b$description

  qs <- b$elements %>%
    purrr::map("questionId") %>%
    unlist() %>%
    purrr::map(~qs[[.x]])


  c(
    ifelse(
      print_internal,
      paste0(
        "\\section{\\textcolor{red}{",
        strip_html(d),
        "}}"
      ),
      NULL
    ),
    "\\begin{mdframed}\n",
    qs,
    "\n\n\\end{mdframed}\n\n",
    "\\clearpage"
  )

}

qp_header <- function(q) {



}

qp_print_question <- function(q, id, print_internal = TRUE) {

  type <- q$questionType$type
  nm <- q$questionName

  cat(stringr::str_c(c("ID: ", "Name: ", "Type: "), c(id, nm, type)), sep = "\n")
  cat("\n")

  print_fun <-

    switch(
      type,

      "DB" = function(q) {qp_title_text(q, print_internal = print_internal)},
      "Matrix" = qp_likert,
      "MC" = qp_mc_single,
      "TE" = qp_text_entry,
      "Timing" = function(q) {qp_title_text(q, print_internal = print_internal)},
      "SBS" = qp_sbs,
      "Slider" = qp_slider

    )

  c(
    "\\begin{minipage}{\\textwidth}",
    print_fun(q),
    "\\end{minipage}"
  )

}

qp_slider <- function(q) {

  return("[SLIDER]")

}

qp_sbs <- function(q, browse = FALSE) {

  if(browse) browser()

  top <- qp_title_text(q)

  # the first column should be the subquestions

  qsub <- q$subQuestions %>% purrr::map_chr("choiceText") %>% strip_html()

  # each column is a question, which should be represented by at least one
  # column in the final table

  cols <- q$columns

  t00 <- cols %>%

    map(~qp_sbs_col_print(.x, qsub)) %>%
    reduce(left_join)

  align <- c("l", rep("c", ncol(res) - 1))

  t01 <- t00 %>%
    knitr::kable(
      format = "latex",
      booktabs = TRUE,
      align = align,
      escape = FALSE
    )

  choice_width <- paste0(
    34 / (ncol(t00) - 1),
    "em"
  )

  t02 <- t01 %>%
    kableExtra::column_spec(1, width = "5em") %>%
    kableExtra::column_spec(2:ncol(t00), width = choice_width) %>%
    kableExtra::kable_styling(latex_options = "striped")

  out <- c(
    "\n\n\n",
    top,
    t02
  )

  ## Return formatted question

  out

}


qp_sbs_col_print <- function(col, qsub) {

  sel   <- col$questionType$selector
  sub <- col$questionType$subSelector
  col$questionText <- strip_html(col$questionText)
  col$choices <- col$choices %>% map(~map(.x, strip_html))

  print_fun <-

    ifelse(
      sub == "DL",
      qp_sbs_col_drop,
    ifelse(
      sel == "Likert",
      qp_sbs_col_likert,
    ifelse(
      sel == "TE",
      qp_sbs_col_txt,
      NA
    )))

  print_fun(col, qsub)

}


qp_sbs_col_drop <- function(col, qsub) {

  choices <-
    col$choices %>%
      map("description") %>%
      map(~stringr::str_trunc(.x, 8)) %>%
      paste0(collapse=",")


  dl <- stringr::str_c(
    "\\ChoiceMenu[print,combo,default=-,name=",
    snakecase::to_lower_camel_case(
      col$questionText
    ),
    1:length(qsub),
    "]{}{",
    choices,
    "}"
  )

  res <-

    tibble(
      item = qsub,
      !!sym(col$questionText) := dl
    )

  res

}

qp_sbs_col_txt <- function(col, qsub) {

  box <- paste0(
    qp_cb(col),
    "\\Qline{3cm}"
  )

  res <-

    tibble(
      item = qsub,
      !!sym(col$questionText) := box
    )

  res
}

qp_sbs_col_likert <- function(col, qsub) {

  choices <- col$choices %>% map_chr("description") %>% strip_html()

  cb <- qp_cb(col)


  ## Generate checkboxes

  check <-

    tibble::as_tibble(
      matrix(
        cb,
        nrow = length(qsub),
        ncol = length(choices)
      )
    )


  align = c("l", rep("c", length(choices)))

  choice_width <- paste0(
    21 / length(choices),
    "em"
  )


  ## Construct table

  res <-

    check %>%
    purrr::set_names(choices) %>%
    dplyr::mutate(item = qsub) %>%
    dplyr::select(item, dplyr::eve3rything())

  res

}


qp_title_text <- function(q, print_internal = TRUE) {

  # Title
  qn <- q$questionName

  # Question text
  text <- q$questionText %>% strip_html()

  if(!(qn %>% stringr::str_detect("head|desc|img"))) {
    title <- "\\question"
  } else if(qn %>% stringr::str_detect("head")) {
    return(
      c(
        paste0("\\subsection{", text, "}")
      )
    )
  } else if(qn %>% stringr::str_detect("desc")) {
    return(
      text
    )
  } else if(qn %>% stringr::str_detect("^cal")) {

    return(paste0("\\question", textm, "\\tcal"))

  } else {

    url <- stringr::str_extract(q$questionText, "(?<=img src\\=\")[^\\s\"]+")
    return(
      paste0("\\href{", url, "}")
    )
  }

  res <- c(
    title,
    ifelse(
      print_internal,
      paste0(
        "{\\color{red}\\begin{verbatim}",
        qn,
        "\\end{verbatim}}"
      ),
      NULL
    ),
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
        qx_embed_field() %>%
        stringr::str_replace_all("\\n|\\&nbsp;", " ") %>%
        stringr::str_replace_all("\\`|’|\\&#39;", "\'") %>%
        stringr::str_replace_all("\\&quot;", "\"") %>%
        stringr::str_replace_all("\\$", "\\\\$") %>%
        stringr::str_replace_all("\\&eacute;", "e") %>%
        stringr::str_replace_all("\\&", "\\\\&") %>%
        trimws()
    )

}


qp_likert <- function(q, top = TRUE, print = TRUE, browse = FALSE) {

  if(browse) browser()

  if(top) top <- qp_title_text(q)


  # Knitr table of choices by subquestions

  ## Get subquestions

  qsub <- q$subQuestions %>% purrr::map_chr("choiceText") %>% strip_html()

  ## Get choices

  choice <- q$choices %>% purrr::map_chr("choiceText") %>% strip_html()

  ncol <- length(choice)

  if(q$questionType$subSelector == "DL") {

    cb <- qp_drop(q)

    ncol <- 1
    choice <- q$questionText

  } else {

    cb <- qp_cb(q, center = TRUE)

  }
  ## Generate checkboxes

  check <-

    tibble::as_tibble(
      matrix(
        cb,
        nrow = length(qsub),
        ncol = ncol
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
      purrr::set_names(choice) %>%
      dplyr::mutate(item = qsub) %>%
      dplyr::select(item, dplyr::everything()) %>%
    knitr::kable(
      format = "latex",
      booktabs = TRUE,
      align = align,
      escape = FALSE
    )

  if(!print) return(res)

  res <- res %>%
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
  qn <- q$questionName
  print(s)

  if(qn %>% stringr::str_detect("^cal")) {

    res <- qp_title_text(q)

  } else if(s == "FORM") {

    res <- qp_form(q)

  } else {

    res <- c(qp_title_text(q), "\\Qline{8cm}\n\n\n")

  }

  res

}


qp_head <- function(srv) {

  title = srv$result$name


  tmp <- readr::read_lines(
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

  ding <- dplyr::case_when(

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
      "FORM",
      "TE"
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



qp_import_display_logic <- function(qsf) {

  srv <- jsonlite::read_json(qsf)

  qs <-

    srv[srv %>% purrr::map_lgl(~.x$


}
