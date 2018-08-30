#' register API KEY and root URL in env variables
#'
#'
#' @return
#' @export
#'
#' @examples
register_options <- function() {
  Sys.setenv(QUALTRICS_ROOT_URL = "ca1.qualtrics.com")
  Sys.setenv(QUALTRICS_API_KEY = decrypt_token("Z:/R/qxk.rds"))
}




#' Decrypt token with password
#'
#' @param path
#'
#' @return
#' @export
#'
#' @importFrom getPass getPass
#' @import sodium
#' @examples
decrypt_token <- function(path) {


  pass   <- getPass::getPass()
  cipher <- readRDS(path)
  key    <- sodium::hash(charToRaw(pass))

  unserialize(sodium::data_decrypt(cipher, key))


}


#' Create payload for post request body
#'
#' @param id
#' @param format
#' @param labs
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
create_payload <- function(id, format = "csv", labs = FALSE, ...) {


  list(format = format,
       surveyId = id,
       useLabels = labs,
       ...) %>%
    jsonlite::toJSON(auto_unbox = TRUE) %>%
    stringr::str_replace("\"false\"", "false") %>%
    stringr::str_replace("\"true\"", "true")
}


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




#' Return API request headers
#'
#' @return
#' @export
#'
#' @examples
headers <- function() {

  c(
    'X-API-TOKEN' = Sys.getenv("QUALTRICS_API_KEY"),
    'Content-Type' = "application/json",
    'Accept' = '*/*',
    'accept-encoding' = 'gzip, deflate'
  )


}





get_survey_json <- function(id, folder = "Z:/R/temp", fname = "qxre.zip", format = "json", labs = TRUE, ...) {

  if(Sys.getenv("QUALTRICS_API_KEY") == "") {

    register_options()

  }

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

  res <- jsonlite::read_json(archive)

  cat("file extracted", "\n")

  list.files(folder, full.names = TRUE) %>%
    map(file.remove)

  cat("cleanup complete", "\n")


  return(res)

}



