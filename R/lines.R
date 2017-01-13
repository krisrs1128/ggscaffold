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

