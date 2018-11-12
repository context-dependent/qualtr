#' Get an export of responses from a survey
#'
#' @param id
#' @param con
#' @param format
#' @param labs
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_responses <- function(id,
                          folder = tempdir(),
                          fname = "qxre.zip",
                          format = "spss",
                          labs = TRUE,
                          ...) {

  if(Sys.getenv("QUALTRICS_API_KEY") == "") {

    register_options()

  }

  id <- env_id(id)

  pl <- create_payload(id = id, format = format, labs = labs, ...)


  root_url <- paste0(Sys.getenv("QUALTRICS_ROOT_URL"), "/API/v3/responseexports")


  post_content <-httr::VERB("POST",
    url = root_url,
    httr::add_headers(headers()),
    body = pl) %>%
    httr::content()


  check_url <- paste0(root_url, "/", post_content$result$id)

  check_request <- httr::VERB("GET", url = check_url, httr::add_headers(headers()))

  file_url <- paste0(check_url, "/file")

  progress <- 0
  cat("progress: \n")
  while(progress < 100) {

    check_request <- httr::VERB("GET", url = check_url, httr::add_headers(headers())) %>%
      httr::content()
    p <- floor(check_request$result$percentComplete)

    if(p > progress) {

      cat(paste0(rep(".", p - progress), collapse = ""))
      progress <- p

    }


  }

  req <- httr::GET(file_url, httr::add_headers(headers()))

  cat("\n\nget status: ", req$status_code, "\n")

  if(req$status_code == 404) {

    cat("Qualtrics can't find that ID right now, trying again...\n")
    get_responses(id, folder = folder, fname = fname, format = format, labs = labs, ...)

  }

  con = paste0(folder, "/", fname)



  writeBin(req$content, con = con)

  cat("zip file saved to", con, "\n")

  archive <- unzip(con, exdir = folder)

  cat("extracted to", archive, "\n")


  res <-

    dplyr::select(
      haven::read_spss(archive),
      -dplyr::matches("logo|header")
    )

  cat("file extracted", "\n")

  unlink(folder)

  # list.files(folder, full.names = TRUE) %>%
  #   purrr::map(file.remove)

  cat("cleanup complete", "\n")

  res <-

    res %>%
      purrr::map_dfc(
      ~ `attr<-`(.x, "label",
          attr(.x, "label") %>%
          stringr::str_remove("(?<=\\?)\\s+(?=-)") %>%
          qx_embed_field()
        )
      )

}


qx_mat_labs <- function(q) {

  sub_names <-

    q$subQuestions %>%
      purrr::map_chr(
      ~ paste0(q$questionName, "_", .x$recode)
      )

  res <- list(
    name = q$questionName,
    text = q$questionText %>%
      qx_embed_field() %>%
      strip_html(),
    subs = q$subQuestions %>%
      purrr::map(
      ~ stringr::str_c(
          q$questionText %>%
            qx_embed_field() %>%
            strip_html(),
          "::",
          strip_html(.x$description)
        )
      ) %>%
      purrr::set_names(sub_names)
  )

}

#' Get responses using current API version
#'
#' @param id Survey id as string or index from most recent list_surveys table
#' @param as_factor
#' @param clean_names
#' @param ...
#' @param browse
#'
#' @return
#' @export
#'
#' @examples
get_responses_v2 <- function(..., as_factor = TRUE, clean_names = TRUE, browse = FALSE) {

  dots <- rlang::list2(...)

  moja <- function(id) {

    folder <- tempdir()
    file <- tempfile(fileext = ".zip")
    if(Sys.getenv("QUALTRICS_API_KEY") == "") {

      register_options()

    }

    id <- env_id(id)

    pl <- create_payload_v2(
      format = "spss",
      labs = TRUE
      #,...
    )

    root_url <- paste0(
      Sys.getenv("QUALTRICS_ROOT_URL"),
      "/API/v3/surveys/",
      id,
      "/export-responses"
    )


    post_content <-httr::VERB("POST",
                              url = root_url,
                              httr::add_headers(headers()),
                              body = pl) %>%
      httr::content()


    check_url <- paste0(root_url, "/", post_content$result$progressId)

    check_request <- httr::VERB(
      "GET",
      url = check_url,
      httr::add_headers(headers())
    ) %>%
      httr::content()


    progress <- 0
    cat("progress: \n")
    while(progress < 100) {

      check_request <- httr::VERB("GET", url = check_url, httr::add_headers(headers())) %>%
        httr::content()
      p <- floor(check_request$result$percentComplete)

      if(p > progress) {

        cat(paste0(rep(".", p - progress), collapse = ""))
        progress <- p

      }

      file_url <- paste0(root_url, "/", check_request$result$fileId, "/file")

    }

    if(browse) browser()
    req <- httr::GET(file_url, httr::add_headers(headers()))

    cat("\n\nget status: ", req$status_code, "\n")

    if(req$status_code == 404) {
      cat("Qualtrics can't find that ID right now, trying again...\n")
      moja(id)
    }

    con = paste0(folder, "/", fname)



    writeBin(req$content, con = con)

    cat("zip file saved to", con, "\n")

    archive <- unzip(con, exdir = folder)

    cat("extracted to", archive, "\n")


    res <-

      dplyr::select(
        haven::read_spss(archive),
        -dplyr::matches("logo|header")
      )

    cat("file extracted", "\n")

    unlink(folder)

    # list.files(folder, full.names = TRUE) %>%
    #   purrr::map(file.remove)

    cat("cleanup complete", "\n")

    res <-

      res %>%
      purrr::map_dfc(
        ~ `attr<-`(.x, "label",
                   attr(.x, "label") %>%
                     stringr::str_remove("(?<=\\?)\\s+(?=-)") %>%
                     qx_embed_field()
        )
      )

    if(as_factor)   res <- haven::as_factor(res)
    if(clean_names) res <- janitor::clean_names(res)

    res
  }

  if(length(dots) > 1) {
    res <- dots %>% purrr::map(moja)
  } else {
    res <- moja(dots)
  }

  res
}

qx_embed_field <- function(qt) {

  # Replace qualtrics formatted embedded fields
  # with more legible alternatives

  qt %>%

    stringr::str_replace_all(
      "\\$\\{e://Field/([^\\s]+)?\\}",
      "[EMBEDDED VALUE: \\1]"
    ) %>%
    stringr::str_replace_all(
      "\\$\\{q://([^\\s]+)?/([^\\s]+)?\\}",
      "[RESPONSE VALUE: \\1]"
    )


}


