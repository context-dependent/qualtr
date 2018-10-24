

#' Labelled table for qualtrics scales
#'
#' @param dat
#' @param ...
#' @param hh_agg
#' @param gender
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
    select(!!!.vars, -matches("TEXT"))
  qs <- g %>%
    ungroup() %>%
    select(!!!.vars) %>%
    purrr::map(
    ~ attr(.x, "label") %>%
      stringr::str_remove("(?<=\\?)\\s+(?=-)") %>%
      qx_embed_field()
    ) %>%
      str_split("(?<=[^\\s])-(?=[^\\s])", simplify = TRUE)

  if(ncol(qs) == 1) {

    stem <- title %||% "Item text"
    qs <- qs[,1]

  } else {

    stem <- unique(qs[, 1])
    qs <- qs[, 2]

  }

  names(qs) <- g %>%
    ungroup() %>%
    select(!!!.vars) %>%
    names()



  levs <- g %>% purrr::map(levels)

  levs <- levs[[which.max(levs %>% purrr::map(length))]]

  g <- g %>%
    gather(var, val, !!!.vars) %>%
    filter(!is.na(val)) %>%
    count(var, val) %>%
    group_by(!!!grp)

  g <- g %>%
    mutate(
      p = n / sum(n),
      N = sum(n)
    ) %>%
    ungroup() %>%
    mutate(
      var_lab = recode(var, !!!qs),
      val = fct_relevel(val, !!!levs, after = Inf)
    ) %>%
    select(!!!grp, var_lab, everything()) %>%
    rename(!!sym(stem) := var_lab) %>%
    arrange(var, val)

  g <- g %>%
    group_by(!!!grp) %>%
    mutate(
      c = cumsum(p),
      cr = rev(cumsum(rev(p))),
      pn = str_c(round(p, 2) * 100, "% (", n, ")")
    )

  g %>% ungroup()

}

#' format qt_raw for printing
#'
#' @param lt
#'
#' @return
#' @export
#'
#' @examples
qt_print <- function(qt) {

  qt %>% select(-n, -p, -n, -c, -cr) %>%
    spread(val, pn) %>%
    mutate_if(is.character, funs(ifelse(is.na(.), " - ", .)))

}
