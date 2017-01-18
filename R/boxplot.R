
#' @importFrom RColorBrewer brewer.pal
#' @importFrom viridis viridis
merge_boxplot_opts <- function(opts = list()) {
  default_opts <- list(
    ## discrete or gradient fill?
    "fill_type" = "discrete",

    ## aesthetic options
    "x" = "x",
    "y" = "y",
    "fill" = NULL,
    "fill_colors" = brewer.pal(8, "Set1"),
    "col" = NULL,
    "col_colors" = brewer.pal(8, "Set1") ,

    ## boxplot appearance options
    alpha = 0.6,
    outlier.size = 0.05,
    notchwidth = 0.1,
    size = 0.1,
    n_breaks = 3,

    ## faceting
    "facet_terms" = c(NULL),
    "facet_orders" = rep(list(NULL), length(opts$facet_terms)),
    "theme_opts" = list()
  )

  if (!is.null(opts$fill_type) && opts$fill_type != "discrete") {
    default_opts$fill_colors <- viridis(250, option = "D")
  }

  modifyList(default_opts, opts)
}

#' Nice default options for (grid of) boxplots
#'
#' @param plot_data [data.frame] A data.frame with data to create the boxplot.
#'   The variables for x, y, colors / fill types, and faceting must be provided
#'   by opts, see merge_boxplot_opts().
#' @param opts [list] A partially specified list used to customize the
#'   appareance in ggplot theme(). Options that are already specified will not
#'   be changed, those that are not will be filled in with defaults.
#' @return p [ggplot] A ggplot geom_boxplot() with nice defaults
#' @importFrom ggplot2 ggplot geom_boxplot aes_string scale_color_manual
#'   scale_fill_manual scale_y_continuous
#' @importFrom scales pretty_breaks
#' @export
ggboxplot <- function(plot_data, opts = list()) {
  opts <- merge_boxplot_opts(opts)
  aes_opts <- list(
    "x" = opts$x,
    "y" = opts$y,
    "fill" = opts$fill,
    "col" = opts$col
  )

  for (i in seq_along(opts$facet_terms)) {
    plot_data[, opts$facet_terms[i]] <- order_vars(
      plot_data[, opts$facet_terms[i]],
      opts$facet_orders[[i]]
    )
  }

  p <- ggplot(plot_data) +
    geom_boxplot(
      do.call(aes_string, aes_opts),
      alpha = opts$alpha,
      outlier.size = opts$outlier.size,
      notchwidth = opts$notchwidth,
      size = opts$size
    ) +
    scale_color_manual(values = opts$col_colors) +
    scale_fill_manual(values = opts$fill_colors) +
    scale_y_continuous(breaks = pretty_breaks(opts$n_breaks)) +
    min_theme(opts$theme_opts)

  add_facet(p, opts$facet_terms)
}