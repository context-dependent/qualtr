
#' List surveys on the license, arrange by search term
#'
#' @return
#' @export
#' @import sodium qualtRics tibble dplyr httr
#'
#' @examples
list_surveys <- function(search = "") {
  if (Sys.getenv("QUALTRICS_API_KEY") == "") {
    register_options()

  }

  req <-

    httr::VERB("GET",
               paste0(Sys.getenv("QUALTRICS_ROOT_URL"), "/API/v3/surveys"),
               httr::add_headers(headers())) %>%

    httr::content()

  res <- req$result$elements %>% bind_rows


  if (search == "") {
    res <- dplyr::select(res, id, name)

  } else {
    res <- dplyr::select(dplyr::arrange(dplyr::mutate(
      res,
      sd = stringdist::stringdist(
        tolower(search),
        tolower(name),
        weight = c(
          i = 0.01,
          d = 1,
          s = 0.9,
          t = 1
        )
      )
    ),
    sd),
    id, name)

  }

  Sys.setenv("QUALTR_LAST_SURVEYS" = paste0(res$id, collapse = ", "))

  res

}
