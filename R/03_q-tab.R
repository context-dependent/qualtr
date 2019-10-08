
#' Title
#'
#' @param dat
#' @param .vars
#' @param .grp
#' @param title
#' @param browse
#' @param explicit_na
#'
#' @return
#' @export
#'
#' @examples
qt_raw <- function(dat,
                   .vars,
                   .grp = groups(dat),
                   title = NULL,
                   browse = FALSE,
                   explicit_na = NULL) {

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
    tidyr::gather(var, val, !!!.vars)


  if(!is.null(explicit_na)) {
    g <- g %>% dplyr::mutate(val = as.factor(val) %>% forcats::fct_explicit_na(explicit_na))
  } else {
    g <- g %>% dplyr::filter(!is.na(val)) %>%
      dplyr::mutate(val = as.factor(val))
  }

  missing_levels <- levs[!(levs %in% levels(g$val))]

  if(!is.null(explicit_na)) {
    levs <- c(levs, explicit_na)
    missing_levels <- c(missing_levels, explicit_na)
  }

  g <- g %>%
    mutate(
      val = forcats::fct_expand(val, missing_levels) %>%
        forcats::fct_relevel(levs)
    ) %>%
    dplyr::count(var, val, .drop = FALSE) %>%
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
