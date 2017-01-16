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
    "col" = NULL,

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

ggcontours <- function() {

}
