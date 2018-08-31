

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
q_tab <- function(dat, ..., title = NULL, browse = FALSE) {

  if(browse) browser()
  x <- quos(...)
  g <- dat %>% select(!!!x)
  qs <- g %>%  map_chr(~attr(.x, "label"))
  stem <- ifelse(ncol(g) == 1, qs, q_stem(qs))

  if(nchar(stem) > 0) {
    g <- g %>%
      map_dfc(
      ~ `attr<-`(
          .x, "label",
          attr(.x, "label") %>%
          str_replace(stem %>% esc, "")
        )
      )
  } else {
    stem = title
  }

  nms <- g %>% map(~attr(.x, "label"))

  colnames(g) <- nms

  levs <- g %>% map(levels)

  levs <- levs[[which.max(levs %>% map(length))]]

  g <- g %>%
    filter(!is.na(val)) %>%
    mutate(p = n / sum(n),
           N = sum(n)) %>%
    ungroup() %>%
    mutate(val = fct_relevel(val, !!!levs, after = Inf)) %>%
    arrange(var, val)

  cnms <- c(esc(stem), "val", "n", "p", "N", "c", "cr", "pn")

  g <- g %>%
    group_by(var) %>%
    mutate(
      c = cumsum(p),
      cr = rev(cumsum(rev(p))),
      pn = str_c(round(p, 2) * 100, "% (", n, ")")
    ) %>%
    set_names(cnms)

  g %>% ungroup()

}

softly <- function(..., vars = current_vars()) {

  vars <- quos(...)
  names <- vars %>% map(quo_name)
  mstr <- str_c("^", names, "$") %>% paste0(collapse = "|")

  matches(mstr)

}


#' format latab for printing
#'
#' @param lt
#'
#' @return
#' @export
#'
#' @examples
laprint <- function(lt) {

  lt %>% select(1, N, matches("Gender"),  val, pn) %>%
    spread(val, pn) %>%
    mutate_at(vars(-1:-2), funs(ifelse(is.na(.), " - ", .)))

}

q_stem <- function(words) {
  #extract substrings from length 1 to length of shortest word
  subs <- sapply(seq_len(min(nchar(words))),
                 function(x, words) substring(words, 1, x),
                 words=words)
  #max length for which substrings are equal
  neqal <- max(cumsum(apply(subs, 2, function(x) length(unique(x)) == 1L)))
  #return substring
  substring(words[1], 1, neqal)
}

esc <- function(string) {
  str_remove_all(string, "([^[:alnum:]\\s])") %>%
    str_c("...")
}
