#' @importFrom gtable gtable gtable_add_grob
#' @importFrom grid unit
#' @importFrom ggplot2 zeroGrob
make_cell <- function() {
  widths <- unit(rep(0, TABLE_COLS), 'mm')
  widths[PANEL_COL] <- unit(1, 'null')
  heights <- unit(rep(0, TABLE_ROWS), 'mm')
  heights[PANEL_ROW] <- unit(1, 'null')
  table <- gtable(widths, heights)
  # Mark the panel cell
  table <- gtable_add_grob(table, list(zeroGrob()), PANEL_ROW, PANEL_COL,
                           z = -Inf, name = 'panel_cell')
  class(table) <- c('cellgrob', class(table))
  cell <- plot_filler()
  class(cell) <- c('assemble_cell', class(cell))
  attr(cell, 'table') <- table
  cell
}
is.assemble_cell <- function(x) inherits(x, 'assemble_cell')
is.cellgrob <- function(x) inherits(x, 'cellgrob')
#' @importFrom ggplot2 ggplotGrob
#' @importFrom gtable gtable_add_grob
cell_table <- function(x, grob = NULL) {
  table <- attr(x, 'table')
  if (is.null(grob)) grob <- ggplotGrob(x)
  table$widths[c(1, ncol(table))] <- grob$widths[c(1, ncol(grob))]
  table$heights[c(1, nrow(table))] <- grob$heights[c(1, nrow(grob))]
  gtable_add_grob(table, grob$grobs[grep('background', grob$layout$name)], 1, 1,
                  nrow(table), ncol(table), z = -100, clip = 'on',
                  name = 'background')
}
#' Get a grob describing the content of an assemble_cell object
#'
#' Methods for this generic should be defined for all `assemble_cell` subclasses
#' and should return a compliant `gtable` object ready to be combined with
#' regular plot objects. In general it is best to call `cell_table()` on the
#' object and add grobs to this as `cell_table()` will return a compliant
#' `gtable`
#'
#' @param x An `assemble_cell` object
#'
#' @return A `gtable` object
#'
#' @export
#' @keywords internal
#'
cellGrob <- function(x, guides = 'auto') {
  UseMethod('cellGrob')
}
#' @export
cellGrob.assemble_cell <- function(x, guides = 'auto') cell_table(x)
#' @importFrom grid grid.newpage grid.draw seekViewport pushViewport upViewport
#' @export
print.assemble_cell <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  if (newpage) grid.newpage()

  grDevices::recordGraphics(
    requireNamespace("patchwork", quietly = TRUE),
    list(),
    getNamespace("patchwork")
  )
  gt <- cellGrob(x)
  if (is.null(vp)) {
    grid.draw(gt)
  } else {
    if (is.character(vp)) {
      seekViewport(vp)
    } else {
      pushViewport(vp)
    }
    grid.draw(gt)
    upViewport()
  }
  invisible(x)
}
#' @export
plot.assemble_cell <- print.assemble_cell
