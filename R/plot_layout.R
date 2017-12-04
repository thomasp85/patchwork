plot_layout <- function(ncol = NULL, nrow = NULL, byrow = FALSE, widths = 1, heights = 1) {
  structure(list(
    ncol = ncol,
    nrow = nrow,
    byrow = byrow,
    widths = widths,
    heights = heights
  ), class = 'plot_layout')
}
