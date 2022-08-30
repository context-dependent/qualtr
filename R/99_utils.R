env_id <- function(id) {

  if(!is.character(id)) {

    ids <- unlist(
      Sys.getenv(
        "QUALTR_LAST_SURVEYS"
      ) %>%
        stringr::str_split(",")
    )
    res <- ids[id]

  } else {

    res <- id

  }

  res

}

qualtrics_response_codes <- function(res){
    interp <- switch(
        as.character(res$status_code),
        `200` = "Success: 200",
        `401` =
          c("Qualtrics API reported an authentication error (401):",
            "You may not have the required authorization",
            "Please check your API key and base URL."),
        `403` =
          c("Qualtrics API reported an forbidden error (403):",
            "You may have a valid API key that lacks API query permissions",
            "Please check your settings and/or talk to your administrators."),
        `400` =
          c("Qualtrics API reported a bad request error (400):",
            "Please report this on https://github.com/ropensci/qualtRics/issues"),
        `404` =
          c("Qualtrics API reported a not found error (404):",
            "Please check if you are using the correct survey ID."),
        `413` =
          c("Qualtrics API reported a 413 error:",
            "The request body was likely too large.",
            "Can also occur when a multipart/form-data request is malformed."),
        `429` =
          c("Qualtrics API reported a 429 error:",
            "You have reached the concurrent request limit."),
        `500` =
          c("After 4 attempts, Qualtrics API reported a temporary internal server error (500):",
            "Please contact Qualtrics Support or retry your query",
            glue::glue("instanceId: {httr::content(res)$meta$error$instanceId}"),
            glue::glue("errorCode: {httr::content(res)$meta$error$errorCode}")),
        `503` =
          c("After 4 attempts, Qualtrics API reported a temporary internal server error (503):",
            "Please contact Qualtrics Support or retry your query",
            glue::glue("instanceId: {httr::content(res)$meta$error$instanceId}"),
            glue::glue("errorCode: {httr::content(res)$meta$error$errorCode}")),
        `504` =
          c("After 4 attempts, Qualtrics API reported a gateway timeout error (504):",
            "Please contact Qualtrics Support or retry your query",
            glue::glue("instanceId: {httr::content(res)$meta$error$instanceId}"),
            glue::glue("errorCode: {httr::content(res)$meta$error$errorCode}")),
        # Default response for unknown status code:
        c(glue::glue("Qualtrics API reported the atypical status code {res$status_code}"),
          "A dictionary of status codes can be found here: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status",
          "Please check your request, and report at https://github.com/ropensci/qualtRics/issues if reoccurring:")
      )
    res <- interp |> paste0(collapse = "\n")
    return(res)
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

#' register API KEY and root URL in env variables
#'
#'
#' @return
#' @export
#'
#' @examples
register_options <- function(conf = decrypt_conf()) {
  Sys.setenv(QUALTRICS_ROOT_URL = conf$base_url)
  Sys.setenv(QUALTRICS_API_KEY = conf$api_token)
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
decrypt_conf <- function() {

  path <- paste0(find.package("qualtr"), "/.conf.rds")

  pass   <- getPass::getPass()
  cipher <- readRDS(path)
  key    <- sodium::hash(charToRaw(pass))

  unserialize(sodium::data_decrypt(cipher, key))


}

#' Encrypt qualtrics configuration details in project root
#'
#' @param api_token
#' @param base_url
#' @param key
#'
#' @return
#' @export
#'
#' @examples
encrypt_conf <- function(api_token,
                         base_url = "blueprintade.ca1.qualtrics.com",
                         key) {

  path <- paste0(find.package("qualtr"), "/.conf.rds")


  conf <- list(
    api_token = api_token,
    base_url = base_url
  )


  cipher <-

    sodium::data_encrypt(
      serialize(
        conf,
        NULL
      ),
      hash(
        charToRaw(key)
      )
    )

  saveRDS(cipher, path)

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

#' Create payload for updated api: no id in the payload
#'
#' @param format
#' @param labs
#' @param ...
#' @return
#' @export
#'
#' @examples
create_payload_v2 <- function(format = "csv", labs = FALSE, ...) {


  list(format = format,
       useLabels = labs,
       ...) |> list_to_json()
}

#' Convert R list into payload-usable json
#' 
#' @param d is a list of parameters and their values

list_to_json <- function(d) {
  d |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    stringr::str_replace("\"false\"", "false") |>
    stringr::str_replace("\"true\"", "true")
}


#' Simple scan for html formatting tags -> latex
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
qp_html_to_tex <- function(x) {

  x %>%
    stringr::str_replace_all("<i>(.+)</i>", "\\\\begin{itenv}\\1\\\\end{itenv}") %>%
    stringr::str_replace_all("<u>(.+)</u>", "\\\\underline{\\1}") %>%
    stringr::str_replace_all("<b>(.+)</b>", "\\\\begin{boldenv}\\1\\\\end{boldenv}") %>%
    stringr::str_replace_all("<strong>(.+)</strong>", "\\\\begin{boldenv}\\1\\\\end{boldenv}") %>%
    stringr::str_replace_all("(<br>|</div>|</p>)+", "\n\n\\\\vspace{2 mm}") %>%
    stringr::str_remove_all("(?<=\\})\\s+")


}
