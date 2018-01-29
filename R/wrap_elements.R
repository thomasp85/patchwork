#' Wrap arbitrary graphics in a patchwork-compliant cell
#'
#' In order to add non-ggplot2 element to a patchwork assemble they can be
#' converted to a compliant representation using the `wrap_elements()` function.
#' This allows you to position either grobs, ggplot objects, or ggassemble
#' objects in either the full area, the full plotting area (anything between and
#' including the axis label), or the panel area (only the actual area where data
#' is drawn). Further you can still add title, subtitle, tag, and caption using
#' the same approach as with normal ggplots (using
#' [ggtitle()][ggplot2::ggtitle] and [labs()][ggplot2::labs]) as well as styling
#' using [theme()][ggplot2::theme]. For the latter, only the theme elements
#' targeting plot margins and background as well as title, subtitle, etc styling
#' will have an effect. If a ggassemble or ggplot object is wrapped, it will be
#' fixated in its state and will no longer respond to addition of styling,
#' geoms, etc..
#'
#' @param panel,plot,full A grob, ggplot, or ggassemble object to add to the
#' respective area.
#'
#' @param clip Should the grobs be clipped if expanding outside its area
#'
#' @param ignore_tag Should tags be ignored for this cell. This is relevant when
#' using automatic tagging of plots and the content of the cell does not qualify
#' for a tag.
#'
#' @return An el_wrapper object
#'
#' @export
wrap_elements <- function(panel = NULL, plot = NULL, full = NULL, clip = TRUE, ignore_tag = FALSE) {
  clip <- if (clip) 'on' else 'off'
  table <- make_cell()
  attr(table, 'grobs') <- list(panel = panel, plot = plot, full = full)
  attr(table, 'settings') <- list(clip = clip, ignore_tag = ignore_tag)
  class(table) <- c('el_wrapper', class(table))
  table
}
is.el_wrapper <- function(x) inherits(x, 'el_wrapper')
#' @importFrom ggplot2 ggplotGrob theme_get
#' @importFrom gtable gtable_add_grob
#' @importFrom grid grobHeight convertHeight
cellGrob.el_wrapper <- function(x) {
  gt <- ggplotGrob(x)
  table <- cell_table(x, gt)
  settings <- attr(x, 'settings')
  grobs <- attr(x, 'grobs')
  if (!is.null(grobs$full)) {
    table <- gtable_add_grob(table, list(as.grob(grobs$full)), 1, 1, nrow(table),
                             ncol(table), clip = settings$clip, name = 'full')
  }
  if (!is.null(grobs$plot)) {
    table <- gtable_add_grob(table, list(as.grob(grobs$plot)), 7, 5, 13, 11,
                             clip =  settings$clip, name = 'plot')
  }
  if (!is.null(grobs$panel)) {
    table <- gtable_add_grob(table, list(as.grob(grobs$panel)), 10, 8,
                             clip = settings$clip, name = 'panel')
  }
  title <- get_grob(gt, 'title')
  table <- gtable_add_grob(table, list(title), 3, 8, clip = settings$clip,
                           name = 'title')
  table$heights[3] <- convertHeight(grobHeight(title), 'mm')
  subtitle <- get_grob(gt, 'subtitle')
  table <- gtable_add_grob(table, list(subtitle), 4, 8, clip = settings$clip,
                           name = 'subtitle')
  table$heights[4] <- convertHeight(grobHeight(subtitle), 'mm')
  caption <- get_grob(gt, 'caption')
  table <- gtable_add_grob(table, list(caption), 16, 8, clip = settings$clip,
                           name = 'title')
  table$heights[16] <- convertHeight(grobHeight(caption), 'mm')
  if (!settings$ignore_tag) {
    table$widths[c(2, ncol(table)-1)] <- gt$widths[c(2, ncol(gt)-1)]
    table$heights[c(2, nrow(table)-1)] <- gt$heights[c(2, nrow(gt)-1)]
    tag <- get_grob(gt, 'tag')
    tag_pos <- x$theme$plot.tag.position
    if (is.null(tag_pos)) tag_pos <- theme_get()$plot.tag.position
    if (!is.character(tag_pos)) tag_pos <- 'manual'
    table <- switch(
      tag_pos,
      topleft = gtable_add_grob(table, tag, name = "tag", t = 2, l = 2, clip = "off"),
      top = gtable_add_grob(table, tag, name = "tag", t = 2, l = 2, r = ncol(table)-1, clip = "off"),
      topright = gtable_add_grob(table, tag, name = "tag", t = 2, l = ncol(table)-1, clip = "off"),
      left = gtable_add_grob(table, tag, name = "tag", t = 2, b = nrow(table)-1, l = 2, clip = "off"),
      right = gtable_add_grob(table, tag, name = "tag", t = 2, b = nrow(table)-1, l = ncol(table)-1, clip = "off"),
      bottomleft = gtable_add_grob(table, tag, name = "tag", t = nrow(table)-1, l = 2, clip = "off"),
      bottom = gtable_add_grob(table, tag, name = "tag", t = nrow(table)-1, l = 2, r = ncol(table)-1, clip = "off"),
      bottomright = gtable_add_grob(table, tag, name = "tag", t = nrow(table)-1, l = ncol(table)-1, clip = "off"),
      manual = gtable_add_grob(table, tag, name = 'tag', t = 2, l = 2, b = nrow(table)-1, r = ncol(table)-1, clip = "off")
    )
  }
  table
}

as.grob <- function(x, ...) {
  UseMethod('as.grob')
}
as.grob.grob <- function(x, ...) {
  x
}
#' @importFrom ggplot2 ggplotGrob
as.grob.ggplot <- function(x, ...) {
  ggplotGrob(x)
}
as.grob.ggassemble <- function(x, ...) {
  patchworkGrob(x)
}
#' @importFrom ggplot2 ggplotGrob
get_grob <- function(x, name) {
  x$grobs[[grep(paste0('^', name, '$'), x$layout$name)]]
}
