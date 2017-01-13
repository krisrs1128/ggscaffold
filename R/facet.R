
##' Reorder A collection of factors in a data.frame
##'
##' @param X [data.frame] A data.frame whose columns have levels we want to
##'   reorder.
##' @param vars [numeric vector] A vector of column names in X, for each column
##'   that we want to reorder.
##' @param vars [list of vectors] The new levels to use for each name in vars.
##'   If this has any nulls, we use the existing levels, or just the unique
##'   values.
##' @return X [data.frame] A version of X with the columns in vars already
##'   reordered.
##' @export
order_vars <- function(X, vars, var_orders) {
  for (i in seq_along(vars)) {

    ## If levels already exist, but are not specified in var_orders, use the
    ## existing ordering.
    if (is.null(var_orders[[i]])) {
      cur_lev <- levels(X[, vars[i]])

      if (!is.null(cur_lev)) {
        var_orders[[i]] <- cur_lev
      } else {
        var_orders[[i]] <- unique(X[, vars[i]])
      }
    }

    X[, vars[i]] <- factor(X[, vars[i]], levels = var_orders[[i]])
  }
  X
}

#' Facet a plot according to specified variables
#'
#' @param p [ggplot object] A plot object we might want to facet.
#' @param facet_terms [list of strings] The names of the columns of the data
#'    used in generating p which we want to facet by. Defaults to NULL, in which
#'    case no faceting is applied.
#' @return p [ggplot object] The version of p including potential faceting.
add_facet <- function(p, facet_terms = NULL) {
  if (!is.null(facet_terms)) {
    p <- p + facet_grid(paste0(facet_terms, collapse = "~"))
  }
  p
}
