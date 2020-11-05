#' @importFrom gtable gtable gtable_add_grob
#' @importFrom grid unit
#' @importFrom ggplot2 zeroGrob
make_patch <- function() {
  widths <- unit(rep(0, TABLE_COLS), 'mm')
  widths[PANEL_COL] <- unit(1, 'null')
  heights <- unit(rep(0, TABLE_ROWS), 'mm')
  heights[PANEL_ROW] <- unit(1, 'null')
  table <- gtable(widths, heights)
  # Mark the panel patch
  table <- gtable_add_grob(table, list(zeroGrob()), PANEL_ROW, PANEL_COL,
                           z = -Inf, name = 'panel_patch')
  class(table) <- c('patchgrob', class(table))
  patch <- ggplot()
  class(patch) <- c('patch', class(patch))
  attr(patch, 'table') <- table
  patch
}
is_patch <- function(x) inherits(x, 'patch')
is_patchgrob <- function(x) inherits(x, 'patchgrob')
#' @importFrom ggplot2 ggplotGrob
#' @importFrom gtable gtable_add_grob
patch_table <- function(x, grob = NULL) {
  table <- attr(x, 'table')
  if (is.null(grob)) grob <- ggplotGrob(x)
  table$widths[c(1, ncol(table))] <- grob$widths[c(1, ncol(grob))]
  table$heights[c(1, nrow(table))] <- grob$heights[c(1, nrow(grob))]
  gtable_add_grob(table, grob$grobs[grep('background', grob$layout$name)], 1, 1,
                  nrow(table), ncol(table), z = -100, clip = 'on',
                  name = 'background')
}
#' Get a grob describing the content of a patch object
#'
#' Methods for this generic should be defined for all `patch` subclasses
#' and should return a compliant `gtable` object ready to be combined with
#' regular plot objects. In general it is best to call `patch_table()` on the
#' object and add grobs to this as `patch_table()` will return a compliant
#' `gtable`
#'
#' @param x An `patch` object
#'
#' @return A `gtable` object
#'
#' @export
#' @keywords internal
#'
patchGrob <- function(x, guides = 'auto') {
  UseMethod('patchGrob')
}
#' @export
patchGrob.patch <- function(x, guides = 'auto') patch_table(x)
#' @importFrom grid grid.newpage grid.draw seekViewport pushViewport upViewport
#' @export
print.patch <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  if (newpage) grid.newpage()

  grDevices::recordGraphics(
    requireNamespace("patchwork", quietly = TRUE),
    list(),
    getNamespace("patchwork")
  )
  gt <- patchGrob(x)
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
plot.patch <- print.patch

#' @export
has_tag.ggplot <- function(x) !is_empty(x)
