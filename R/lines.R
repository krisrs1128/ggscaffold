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
#' @importFrom ggplot2 scale_color_brewer
#' @export
merge_line_opts <- function(opts = list()) {
  default_opts <- list(

    ## aesthetic options
    "x" = "x",
    "y" = "y",
    "group" = NULL,
    "col" = NULL,
    "linetype" = NULL,

    ## nonaesthetic options
    "size" = 0.5,
    "alpha" = 1,

    ## scale options
    "color_scale" = scale_color_brewer(palette = "Set2"),

    ## faceting
    "facet_terms" = NULL,
    "facet_order" = rep(list(NULL), length = opts$facet_terms),
    "facet_scales" = "fixed",
    "facet_space" = "fixed",

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
#' @importFrom ggplot2 ggplot geom_line aes_string
#' @importFrom magrittr %>%
#' @examples
#' data(lines)
#' opts <- list(col = "color", facet_terms = c("rows", "columns"),
#'              color_scale = ggplot2::scale_color_brewer(palette = "Set1"),
#'              theme_opts = list(border_size = .6))
#' gglines(lines, opts)
#' @export
gglines <- function(plot_data, opts = list()) {
  opts <- merge_line_opts(opts)
  aes_opts <- list(
    "x" = opts$x,
    "y" = opts$y,
    "group" = opts$group,
    "col" = opts$col,
    "linetype" = opts$linetype
  )
  aes_opts <- aes_opts[!sapply(aes_opts, is.null)]

  ## reorder levels for rows / columns
  for (i in seq_along(opts$facet_terms)) {
    plot_data[, opts$facet_terms[i]] <- order_vars(
      plot_data[, opts$facet_terms[i]],
      opts$facet_orders[[i]]
    )
  }

  p <- ggplot(plot_data) +
      geom_line(
          do.call(aes_string, aes_opts),
          size = opts$size, alpha = opts$alpha
      ) +
    opts$color_scale +
    min_theme(opts$theme_opts)

  add_facet(p, opts$facet_terms, opts$facet_scales, opts$facet_space)
}
