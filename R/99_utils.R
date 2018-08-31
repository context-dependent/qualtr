env_id <- function(id) {

  if(!is.character(id)) {

    ids <- unlist(
      Sys.getenv(
        "QUALTR_LAST_SURVEYS"
      ) %>%
        stringr::str_split(", ")
    )
    res <- ids[[1]][id]

  } else {

    res <- id

  }

  res

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

"ca1.qualtrics.com"


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
decrypt_conf <- function(path = ".conf.rds") {

  pass   <- getPass::getPass()
  cipher <- readRDS(path)
  key    <- sodium::hash(charToRaw(pass))

  unserialize(sodium::data_decrypt(cipher, key))


}

encrypt_conf <- function(api_token, base_url, key) {

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

  saveRDS(cipher, ".conf.rds")

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