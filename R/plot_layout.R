#' Define the grid to compose plots in
#'
#' In order to control how different plots are layed out, you need to add a
#' layout specification. If you are nesting grids, the layout is scoped to the
#' current nesting level.
#'
#' @param ncol,nrow The dimensions of the grid to create - if both are `NULL` it
#' will use the same logic as [facet_wrap()][ggplot2::facet_wrap] to set the
#' dimensions
#' @param byrow Analogous to `byrow` in [matrix()][base::matrix]. If `FALSE` the
#' plots will be filled in in column-major order
#' @param widths,heights The relative widths and heights of each column and row
#' in the grid. Will get repeated to match the dimensions of the grid.
#' @param guides A string specifying how guides should be treated in the layout
#' @param tag_level A string (`'keep'` or `'new'`) to indicate whether
#' auto-tagging should behave. See [plot_annotation()].
#'
#' @return A `plot_layout` object to be added to a `ggassmble` object
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
#' p4 <- ggplot(mtcars) + geom_bar(aes(carb))
#' p5 <- ggplot(mtcars) + geom_violin(aes(cyl, mpg, group = cyl))
#'
#' # The plots are layed out automatically by default
#' p1 + p2 + p3 + p4 + p5
#'
#' # Use byrow to change how the grid is filled out
#' p1 + p2 + p3 + p4 + p5 + plot_layout(byrow = FALSE)
#'
#' # Change the grid dimensions
#' p1 + p2 + p3 + p4 + p5 + plot_layout(ncol = 2, widths = c(1, 2))
#'
#' # Define layout at different nesting levels
#' p1 +
#'   p2 +
#'   (p3 +
#'      p4 +
#'      plot_layout(ncol = 1)
#'   ) +
#'   p5 +
#'   plot_layout(widths = c(2, 1))
plot_layout <- function(ncol = NULL, nrow = NULL, byrow = NULL, widths = NULL, heights = NULL, guides = NULL, tag_level = NULL) {
  if (!is.null(guides)) guides <- match.arg(guides, c('auto', 'collect', 'keep'))
  if (!is.null(tag_level)) tag_level <- match.arg(tag_level, c('keep', 'new'))
  structure(list(
    ncol = ncol,
    nrow = nrow,
    byrow = byrow,
    widths = widths,
    heights = heights,
    guides = guides,
    tag_level = tag_level
  ), class = 'plot_layout')
}
default_layout <- plot_layout(byrow = TRUE, widths = 1, heights = 1, guides = 'auto', tag_level = 'keep')
#' @importFrom utils modifyList
#' @export
ggplot_add.plot_layout <- function(object, plot, object_name) {
  if (!is.ggassemble(plot)) stop('plot_layout must be added to an assemble of plots', call. = FALSE)
  plot$assemble$layout <- modifyList(plot$assemble$layout, object[!vapply(object, is.null, logical(1))])
  plot
}
