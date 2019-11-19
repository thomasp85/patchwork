#' Add an area to hold collected guides
#'
#' Using the `guides` argument in [plot_layout()] you can collect and collapse
#' guides from plots. By default these guides will be put on the side like with
#' regular plots, but by adding a `guide_area()` to the plot you can tell
#' patchwork to place the guides in that area instead. If guides are not
#' collected or no guides exists to collect it behaves as a standard
#' [plot_spacer()] instead.
#'
#' @export
guide_area <- function() {
  table <- make_patch()
  class(table) <- c('guide_area', class(table))
  table
}
#' @importFrom gtable gtable_add_grob
patchGrob.guide_area <- function(x, guides = 'auto') {
  table <- NextMethod()
  gtable_add_grob(table, zeroGrob(), PANEL_ROW, PANEL_COL, name = 'panel-guide_area')
}
