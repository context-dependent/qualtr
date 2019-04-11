
#' Title
#'
#' @param dat
#' @param .vars
#' @param .grp
#' @param title
#' @param browse
#'
#' @return
#' @export
#'
#' @examples
qt_raw <- function(dat,
                   .vars,
                   .grp = groups(dat),
                   title = NULL,
                   browse = FALSE) {

  if(browse) browser()

  if(is.null(.grp)) {

    grp <- quo(var)

  } else {

    grp <- c(quo(var), .grp)

  }

  g <- dat %>%
    dplyr::select(!!!.vars, -matches("text"))
  qs <- g %>%
    dplyr::ungroup() %>%
    dplyr::select(!!!.vars) %>%
    purrr::map(
    ~ attr(.x, "label") %>%
      stringr::str_remove("(?<=\\?)\\s+(?=-)") %>%
      qx_embed_field()
    )

  stem <- title %||% "Item text"

  names(qs) <- g %>%
    dplyr::ungroup() %>%
    dplyr::select(!!!.vars) %>%
    names()



  levs <- g %>% purrr::map(levels)

  levs <- levs[[which.max(levs %>% purrr::map(length))]]

  g <- g %>%
    tidyr::gather(var, val, !!!.vars) %>%
    dplyr::filter(!is.na(val)) %>%
    dplyr::count(var, val) %>%
    dplyr::group_by(!!!grp)

  g <- g %>%
    dplyr::mutate(
      p = n / sum(n),
      N = sum(n)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      var_lab = recode(var, !!!qs),
      val = forcats::fct_relevel(val, !!!levs, after = Inf)
    ) %>%
    dplyr::select(!!!grp, var_lab, everything()) %>%
    dplyr::rename(!!sym(stem) := var_lab) %>%
    dplyr::arrange(var, val)

  g <- g %>%
    dplyr::group_by(!!!grp) %>%
    dplyr::mutate(
      c = cumsum(p),
      cr = rev(cumsum(rev(p))),
      pn = stringr::str_c(round(p, 2) * 100, "% (", n, ")")
    )

  g %>% dplyr::ungroup()

}

#' format qt_raw for printing
#'
#' @param qt
#'
#' @return
#' @export
#'
#' @examples
qt_print <- function(qt) {

  qt %>%
    dplyr::select(-n, -p, -n, -c, -cr) %>%
    tidyr::spread(val, pn) %>%
    dplyr::mutate_if(is.character, funs(ifelse(is.na(.), " - ", .)))

}
