
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

#' Wrap order_vars()
#'
#' @param X [data.frame] A data.frame some whose columns we want to throw into
#'   order_vars
#' @param x_names [character vector] The names of the columns to reorder
#' @param x_orders [list of character vectors, or NULL] The new orders to input
#'   to order_vars
#' @return X [data.frame] The original X, but with columns x_names reordered
order_multiple <- function(X, x_names, x_orders) {
  for (i in seq_along(x_names)) {
    if (x_names[i] == ".") next
    X[, x_names[i]] <- order_vars(
      X[, x_names[i]],
      x_orders[[i]]
    )
  }
  X
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
