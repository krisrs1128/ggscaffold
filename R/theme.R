#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Custom theme options not implemented in the usual theme packages.

#' Merge in Theme Defaults
#'
#' For a partially specified list opts, this merges in defaults
#' for those options that are not specified.
#'
#' @param opts [list] A partially specified list used to customize themes in
#'   ggplot theme(). Options that are already specified will not be changed,
#'   those that are not will be filled in with defaults.
#' @return opts [list]  A version of opts with unspecified options filled in
#'   with defaults.
#' @export
merge_theme_opts <- function(opts = list()) {
  default_opts <- list(
    border_size = 0,
    spacing = 0,
    subtitle_size = 7,
    text_size = 5
  )
  modifyList(default_opts, opts)
}

#' Provide a Custom, Minimal Theme
#'
#' @param opts [list] A partially specified list used to customize themes in
#'   ggplot theme(). Options that are already specified will not be changed,
#' @return theme_obj [ggplot theme] A theme object that can be used to modify
#'   the appearance of a ggplot object.
#' @importFrom ggplot2 theme_update element_blank element_text
#' @importFrom grid unit
#' @export
min_theme <- function(opts = list()) {
  opts <- merge_theme_opts(opts)
  theme_update(
    panel.border = element_rect("transparent", size = opts$border_size),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(opts$spacing, "line"),
    legend.title = element_text(size = opts$subtitle_size),
    legend.text = element_text(size = opts$text_size),
    legend.key = element_blank(),
    axis.text = element_text(size = opts$text_size),
    axis.title = element_text(size = opts$subtitle_size),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = opts$subtitle_size)
  )
}
