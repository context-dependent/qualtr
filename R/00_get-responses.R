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
get_responses <- function(id, folder = "Z:/R/temp", fname = "qxre.zip", format = "spss", labs = TRUE, ...) {

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
    get_survey(id, folder = folder, fname = fname, format = format, labs = labs, ...)

  }

  con = paste0(folder, "/", fname)



  writeBin(req$content, con = con)

  cat("zip file saved to", con, "\n")

  archive <- unzip(con, exdir = folder)

  cat("extracted to", archive, "\n")


  res <-

    select(
      haven::read_spss(archive),
      -matches("logo|header")
    )

  cat("file extracted", "\n")

  list.files(folder, full.names = TRUE) %>%
    map(file.remove)

  cat("cleanup complete", "\n")

  list(
    fct = haven::as_factor(res),
    num = haven::zap_labels(res),
    lab = purrr::map(res, ~ attr(.x, "label"))
  )


}

