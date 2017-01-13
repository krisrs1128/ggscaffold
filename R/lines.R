#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Some functions for frequently used line plots with ggplot2.
## author: kriss1@stanford.edu

#' Merge default options for a lines plot
#' @param opts [list] A partially specified list used to customize appearance in
#'   ggplot theme(). Options that are already specified will not be changed,
#'   those that are not will be filled in with defaults.
#' @return opts [list]  A version of opts with unspecified options filled in
#'   with defaults.
#' @export
merge_line_opts <- function(opts = list()) {
  default_opts <- list(

    ## aesthetic options
    "x" = "x",
    "y" = "y",
    "group" = NULL,
    "col" = NULL,

    ## scale options
    "color_scale" = scale_color_brewer(palette = "Set2"),
    "linetype" = NULL,

    ## faceting
    "facet_terms" = NULL,
    "row_order" = NULL,
    "col_order" = NULL,

    ## themes
    "theme_opts" = list()
  )
  modifyList(default_opts, opts)
}

#' Make line plots with nice defaults
#' @param plot_data [data.frame] A data.frame with data to create the lines plot
#'   from. The variables for x, y axis, colors / linetype, and faceting must be
#'   provided by opts. Otherwise, they will be filled in with defaults, see
#'   merge_line_opts(). The order in which variables appear on the axes is
#'   controlled by opts$x_order and opts$y_order.
#' @param opts [list] A partially specified list used to customize appearance in
#'   ggplot theme(). Options that are already specified will not be changed,
#'   those that are not will be filled in with defaults.
#' @return p [ggplot] A ggplot geom_tile() heatmap with nice defaults.
#' @importFrom ggplot ggplot geom_tile aes_string scale_x_discrete
#'   scale_fill_gradient facet_grid
#' @importFrom maggritr %>%
#' @examples
gglines <- function(plot_data, opts = list()) {
  ## merge and prepare opts
  opts <- merge_line_opts(opts)
  aes_opts <- list(
    "x" = opts$x,
    "y" = opts$y,
    "group" = opts$group,
    "col" = opts$col
  )
  aes_opts <- aes_opts[!sapply(aes_opts, is.null)]

  ## reorder levels for rows / columns
  if (!is.null(opts$facet_terms)) {
    plot_data <- plot_data %>%
      order_vars(
        opts$facet_terms,
        list(opts$row_order, opts$col_order)
      )
  }

  p <- ggplot(plot_data) +
    geom_line(do.call(aes_string, aes_opts)) +
    opts$color_scale +
    min_theme(opts$theme_opts)

  add_facet(p, opts$facet_terms)
}
