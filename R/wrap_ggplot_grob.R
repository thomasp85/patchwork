#' Make a gtable created from a ggplot object patchwork compliant
#'
#' This function converts a gtable, as produced by [ggplot2::ggplotGrob()] and
#' makes it ready to be added to a patchwork assemble. In contrast to passing
#' the gtable to [wrap_elements()], `wrap_ggplot_grob()` ensures proper
#' alignment as expected. On the other hand major restructuring of the gtable
#' will result in an object that doesn't work properly with
#' `wrap_ggplot_grob()`.
#'
#' @param x A gtable as produced by [ggplot2::ggplotGrob()]
#'
#' @return A `table_cell` object to be added to a patchwork assemble
#'
#' @export
wrap_ggplot_grob <- function(x) {
  stopifnot(inherits(x, 'gtable'))
  stopifnot(length(x$widths) <= 15)
  stopifnot(length(x$heights) <= 18)
  cell <- make_cell()
  class(cell) <- c('table_cell', class(cell))
  attr(cell, 'table') <- x
  cell
}
#' @export
cellGrob.table_cell <- function(x) {
  gt <- attr(x, 'table')
  gt <- add_strips(gt)
  add_guides(gt)
}
