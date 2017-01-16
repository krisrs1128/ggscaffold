#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Some functions for simplifying heatmaps using ggplot2.
## author: kriss1@stanford.edu


#' Merge default options for a heatmap
#' @param opts [list] A partially specified list used to customize appearance in
#'   ggplot theme(). Options that are already specified will not be changed,
#'   those that are not will be filled in with defaults.
#' @return opts [list]  A version of opts with unspecified options filled in
#'   with defaults.
#' @importFrom viridis viridis
#' @export
merge_heatmap_opts <- function(opts = list()) {
  default_opts <- list(
    "x" = "x",
    "y" = "y",
    "fill" = "fill",
    "fill_colors" = viridis(256, option = "D"),
    "fill_breaks" = NULL,
    "facet_terms" = NULL,
    "x_order" = NULL,
    "y_order" = NULL,
    "theme_opts" = list()
  )
  modifyList(default_opts, opts)
}

#' Make heatmaps with nice defaults
#' @param plot_data [data.frame] A data.frame with data to create the heatmap
#'   from. The variables for x, y axis, filling, and faceting must be provided
#'   by opts. Otherwise, they will be filled in with defaults, see
#'   merge_heatmap_opts(). The order in which variables appear on the axes is
#'   controlled by opts$x_order and opts$y_order.
#' @param opts [list] A partially specified list used to customize appearance in
#'   ggplot theme(). Options that are already specified will not be changed,
#'   those that are not will be filled in with defaults.
#' @return p [ggplot] A ggplot geom_tile() heatmap with nice defaults.
#' @importFrom ggplot2 ggplot geom_tile aes_string scale_x_discrete
#'   scale_fill_gradient
#' @importFrom magrittr %>%
#' @examples
#' X <- matrix(rnorm(10 * 10), 10, 10, dimnames = list(1:10, letters[1:10]))
#' plot_data <- reshape2::melt(X, varnames = c("x", "y"), value.name = "fill")
#' opts <- list(y_order = letters[10:1])
#' ggheatmap(plot_data, opts)
#' @export
ggheatmap <- function(plot_data, opts = list()) {
  # merge defaults and relevel variables
  opts <- merge_heatmap_opts(opts)
  plot_data <- plot_data %>%
    order_vars(
      c(opts$x, opts$y),
      list(opts$x_order, opts$y_order)
    )

  # create plot
  p <- ggplot(plot_data) +
    geom_tile(aes_string(x = opts$x, y = opts$y, fill = opts$fill)) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_gradientn(
      colors = opts$fill_colors,
      breaks = opts$fill_breaks
    ) +
    min_theme(opts$theme_opts)

  add_facet(p, opts$facet_terms)
}
