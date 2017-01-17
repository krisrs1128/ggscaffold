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
#' @importFrom viridis viridis
#' @export
merge_contour_opts <- function(opts = list()) {
  default_opts <- list(
    ## aesthetic options
    "x" = "x",
    "y" = "y",
    "fill" = "..level..",

    ## contour appearance options
    "geom" = "polygon",
    "alpha" = 0.05,
    "h" = 0.1,
    "bins"= 40,

    ## scale_options
    "fill_colors" = viridis(256, option = "D"),
    "fill_breaks" = NULL,

    ## faceting
    "facet_terms" = NULL,
    "facet_orders" = NULL,
    "theme_opts" = list()
  )

  modifyList(default_opts, opts)
}


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
#' p <- ggcontours(plot_data, list(facet_terms = c("row", "col")))
#' @importFrom magrittr %>%
ggcontours <- function(plot_data, opts = list()) {
  opts <- merge_contour_opts(opts)
  aes_opts <- list(
    "x" = opts$x,
    "y" = opts$y,
    "fill" = opts$fill
  )

  plot_data <- plot_data %>%
    order_vars(opts$facet_terms, opts$facet_orders)

  p <- ggplot(plot_data) +
    stat_density2d(
      do.call(aes_string, aes_opts),
      geom = opts$geom,
      alpha = opts$alpha,
      h = opts$h,
      bins = opts$bins
    ) +
    scale_fill_gradientn(
      colors = opts$fill_colors,
      breaks = opts$fill_breaks
    ) +
    min_theme(opts$theme_opts)

  add_facet(p, opts$facet_terms)
}
