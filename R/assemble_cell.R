#' @importFrom gtable gtable gtable_add_grob
#' @importFrom grid unit
#' @importFrom ggplot2 zeroGrob
make_cell <- function() {
  widths <- unit(rep(0, 15), 'mm')
  widths[8] <- unit(1, 'null')
  heights <- unit(rep(0, 18), 'mm')
  heights[10] <- unit(1, 'null')
  table <- gtable(widths, heights)
  table <- gtable_add_grob(table, list(zeroGrob()), 10, 8, z = -Inf, name = 'panel_cell')
  class(table) <- c('assemble_cell', class(table))
  table
}
is.assemble_cell <- function(x) inherits(x, 'assemble_cell')
