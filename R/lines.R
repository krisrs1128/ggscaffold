#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Some functions for frequently used line plots with ggplot2.
## author: kriss1@stanford.edu

merge_line_opts <- function(opts = list()) {
  default_opts <- list(
    "x" = "x",
    "y" = "y",
    "group" = NULL,
    "col" = NULL,
    "color_scale" = scale_color_brewer(palette = "Set2"),
    "linetype" = NULL,
    "facet_terms" = NULL
    "row_order" = NULL,
    "col_order" = NULL,
    "theme_opts" = list()
  )
}

gglines <- function(plot_data, opts = list()) {
  ## merge and prepare opts
  opts <- merge_line_opts(opts)
  aes_opts <- list(
    "x" = opts$x,
    "y" = opts$y,
    "group" = opts$group,
    "col" = opts$col
  )
  aes_opts <- aes_opts[!is.null(aes_opts)]

  ## reorder levels for rows / columns
  plot_data <- plot_data %>%
    order_vars(
      opts$facet_terms,
      list(opts$row_order, opts$col_order)
    )

  ggplot(plot_data) +
    geom_line(do.call(aes, aes_opts))



}
