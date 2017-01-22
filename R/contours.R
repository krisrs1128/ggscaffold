#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Defaults built in to basic contour plots (useful for studying probabilistic
## ordinations).
##
## author: kriss1@stanford.edu

#' Merge default options for contour plots
#' @param opts [list] A partially specified list used to customize appearance in
#'   ggplot theme(). Options that are already specified will not be changed,
#'   those that are not will be filled in with defaults.
#' @return opts [list]  A version of opts with unspecified options filled in
#'   with defaults.
#' @importFrom RColorBrewer brewer.pal
#' @importFrom viridis viridis
#' @export
merge_contour_opts <- function(opts = list()) {
  default_opts <- list(
    ## discrete or gradient fill?
    "fill_type" = "gradient",

    ## aesthetic options
    "x" = "x",
    "y" = "y",
    "fill" = "..level..",
    "group" = NULL,

    ## contour appearance options
    "geom" = "polygon",
    "alpha" = 0.05,
    "h" = 0.1,
    "bins"= 40,

    ## scale_options
    "fill_colors" = viridis(256, option = "D"),
    "fill_breaks" = NULL,

    ## faceting
    "facet_terms" = c(NULL),
    "facet_orders" = rep(list(NULL), length(opts$facet_terms)),
    "facet_scales" = "fixed",
    "facet_space" = "fixed",
    "theme_opts" = list(),

    ## aspect ratio
    coord_ratio = 1
  )

  if (!is.null(opts$fill) && opts$fill_type != "gradient") {
    default_opts$fill_colors <- brewer.pal(8, "Set2")
  }

  modifyList(default_opts, opts)
}

#' Default Contour / Stat Density 2D plot
#'
#' This is a default for visualizing 2d density plots.
#'
#' @param plot_data [data.frame] A data.frame with data to create the density plot
#'   from. The variables for x, y axis, fill, and faceting must be
#'   provided by opts. Otherwise, they will be filled in with defaults, see
#'   merge_contour_opts(). The order in which variables appear on the axes is
#'   controlled by opts$x_order and opts$y_order.
#' @param opts [list] A partially specified list used to customize appearance in
#'   ggplot theme(). Options that are already specified will not be changed,
#'   those that are not will be filled in with defaults.
#' @return p [ggplot] A ggplot stat_density2d object with nice defaults.
#' @examples
#' plot_data <- expand.grid(
#'   "ix" = seq_len(1000),
#'   "row" = c("A", "B", "C"),
#'   "col" = c("alpha", "beta", "gamma")
#' )
#'
#' plot_data <- cbind(
#'   plot_data,
#'   x = rnorm(9000),
#'   y = rnorm(9000)
#' )
#' ggcontours(plot_data, list(facet_terms = c("row", "col")))
#' ggcontours(plot_data, list(facet_terms = c("row", ".")))
#' ggcontours(plot_data, list(facet_terms = c("row", "."), fill_type = "discrete", fill = "col"))
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot stat_density2d aes_string guides guide_legend coord_fixed
#' scale_fill_gradientn scale_fill_manual
#' @export
ggcontours <- function(plot_data, opts = list()) {
  opts <- merge_contour_opts(opts)
  aes_opts <- list(
    "x" = opts$x,
    "y" = opts$y,
    "fill" = opts$fill,
    "group" = opts$group
  )

  plot_data <- order_multiple(
    plot_data,
    opts$facet_terms,
    opts$facet_orders
  )

  p <- ggplot(plot_data) +
    stat_density2d(
      do.call(aes_string, aes_opts),
      geom = opts$geom,
      alpha = opts$alpha,
      h = opts$h,
      bins = opts$bins
    ) +
    min_theme(opts$theme_opts) +
    guides(fill = guide_legend(override.aes = list(alpha = 1))) +
    coord_fixed(opts$coord_ratio)

  if (opts$fill_type == "gradient") {
    p <- p +
      scale_fill_gradientn(
        colors = opts$fill_colors,
        breaks = opts$fill_breaks
      )
  } else {
    p <- p +
      scale_fill_manual(
        values = opts$fill_colors
      )
  }

  add_facet(p, opts$facet_terms, opts$facet_scales, opts$facet_space)
}
