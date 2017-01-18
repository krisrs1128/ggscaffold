
##' Reorder levels in a vector
##'
##' @param x [vector] A vector whose factors we want to reorder (or create).
##' @param x_order [vectors] The new levels to use for each name in vars.
##' @return x [vector] version of x with levels in the correct order
##' @export
order_vars <- function(x, x_order = NULL) {
  if (is.null(x_order)) {
    x_order <- sort(unique(x))
  }
  factor(x, levels = x_order)
}

#' Facet a plot according to specified variables
#'
#' @param p [ggplot object] A plot object we might want to facet.
#' @param facet_terms [list of strings] The names of the columns of the data
#'    used in generating p which we want to facet by. Defaults to NULL, in which
#'    case no faceting is applied.
#' @importFrom ggplot2 facet_grid
#' @return p [ggplot object] The version of p including potential faceting.
#' @export
add_facet <- function(p, facet_terms = NULL, facet_scales = "fixed", facet_space = "fixed") {
  if (!is.null(facet_terms)) {
    p <- p + facet_grid(
               paste0(facet_terms, collapse = "~"),
               scales = facet_scales,
               space = facet_space
             )
  }
  p
}
