#' Labelled bar chart uses qualtrics data
#'
#' @param dat
#' @param x
#' @param title_wrap
#' @param col_wrap
#'
#' @return
#' @export
#'
#' @examples
q_bar <- function(dat, x, title_wrap = 40, col_wrap = 12) {

  x <- enquo(x)

  p <- pbar(dat, !!x, title_wrap = title_wrap, col_wrap = col_wrap) +
    labs(subtitle = dat %>% pull(!!x) %>% attr("label") %>%
           str_wrap(width = title_wrap), size = 8,
         x = NULL, y = NULL) +
    scale_x_discrete(
      limits = levels(dat %>% pull(!!x)),
      labels = function(x){str_wrap(x, width = col_wrap)})

  p
}


q_bars <- function(dat, ..., ylim = NULL) {
  x <- quos(...)
  g <- dat %>% select(!!!x)

  qs <- g %>% map_chr(~attr(.x, "label"))


  stem <- ifelse(ncol(g) == 1, qs, q_stem(qs))

  g <- g %>%

    map_dfc(
    ~ `attr<-`(
      .x, "label", attr(.x, "label") %>%
        str_replace(stem %>% esc, "")
      )
    )

  print(stem)


  pl <- g %>%

    names %>%
    map(
    ~ g %>%
      labar(!!sym(.x)) +
      scale_y_continuous(limits = ylim)
    )

  fig <- ggpubr::ggarrange(plotlist = pl, align = "hv")
  annotate_figure(fig, top = text_grob(stem, size = 12))
}



pbar <- function(dat, x, title_wrap = 40, col_wrap = 12) {

  x <- enquo(x)

  p <- dat %>% count(!!x) %>%
    filter(!is.na(!!x)) %>%
    ggplot(aes(!!x, n)) +
    geom_col(alpha = 0.9) +
    geom_text(aes(label = n,
                  vjust = ifelse(n > (0.7 * max(n)), "top", "bottom"),
                  colour = n > (0.7 * max(n))),
              size = 10) +
    scale_colour_manual(values = c("grey35", "white"), guide = FALSE) +
    theme_minimal(base_family = "Roboto Condensed", base_size = 14)

  p

}
harmonize_y <- function(plot_list) {
  y_max <- plot_list %>%
    map(~ggplot_build(.x)$data[[1]]$y) %>%
    unlist() %>% max()

  plot_list %>% map(~ .x + scale_y_continuous(limits = c(0, y_max)))
}
