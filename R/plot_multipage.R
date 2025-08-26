#' Align plots across multiple pages
#'
#' Sometimes it is necessary to make sure that separate plots are aligned, with
#' each other, but still exists as separate plots. That could e.g. be if they
#' need to be part of a slideshow and you don't want titles and panels jumping
#' around as you switch between slides. patchwork provides a range of utilities
#' to achieve that. Currently it is only possible to align ggplots, but aligning
#' patchworks will be supported in the future.
#'
#' @param plot A ggplot object
#' @param dim A plot_dimension object as created by `get_dim()`
#' @param ... ggplot objects or a single list of them
#'
#' @return `get_dim()` and `get_max_dim()` return a plot_dimension object.
#' `set_dim()` returns a modified ggplot object with fixed outer dimensions and
#' `align_patches()` return a list of such. The modified ggplots still behaves
#' like a standard ggplot and new layers, scales, etc can be added to them.
#'
#' @name multipage_align
#' @rdname multipage_align
#'
#' @examples
#' library(ggplot2)
#' p1 <- ggplot(mtcars) +
#'   geom_point(aes(mpg, disp)) +
#'   ggtitle('Plot 1')
#'
#' p2 <- ggplot(mtcars) +
#'   geom_boxplot(aes(gear, disp, group = gear)) +
#'   ggtitle('Plot 2')
#'
#' p3 <- ggplot(mtcars) +
#'   geom_point(aes(hp, wt, colour = mpg)) +
#'   ggtitle('Plot 3')
#'
#' p4 <- ggplot(mtcars) +
#'   geom_bar(aes(gear)) +
#'   facet_wrap(~cyl) +
#'   ggtitle('Plot 4')
#'
#' # Align a plot to p4
#' p4_dim <- get_dim(p4)
#' set_dim(p1, p4_dim)
#'
#' # Align a plot to the maximum dimensions of a list of plots
#' max_dims <- get_max_dim(p1, p2, p3, p4)
#' set_dim(p2, max_dims)
#'
#' # Align a list of plots with each other
#' aligned_plots <- align_patches(p1, p2, p3, p4)
#' aligned_plots[[3]]
#'
#' # Aligned plots still behave like regular ggplots
#' aligned_plots[[3]] + theme_bw()
#'
NULL

#' @rdname multipage_align
#' @export
get_dim <- function(plot) {
  UseMethod('get_dim')
}
is_plot_dimension <- function(x) inherits(x, 'plot_dimension')
#' @export
print.plot_dimension <- function(x, ...) {
  cat('A plot dimension object to be applied to a ggplot or patchwork with `set_dim()`')
  invisible(x)
}
#' @importFrom ggplot2 ggplot_build ggplot_gtable geom_blank
#' @export
get_dim.ggplot <- function(plot) {
  table <- plot_table(plot, 'auto')
  panel_pos <- find_panel(table)
  widths <- convertWidth(table$widths, 'mm', TRUE)
  heights <- convertHeight(table$heights, 'mm', TRUE)
  dims <- list(l = widths[seq_len(panel_pos$l - 1)],
               r = widths[seq(panel_pos$r + 1, ncol(table))],
               t = heights[seq_len(panel_pos$t - 1)],
               b = heights[seq(panel_pos$b + 1, nrow(table))])
  class(dims) <- c('ggplot_dimension', 'plot_dimension')
  dims
}
is_ggplot_dimension <- function(x) inherits(x, 'ggplot_dimension')
#' @export
get_dim.patchwork <- function(plot) {
  cli_abort('Getting dimensions on patchworks are currently unsupported')
}

#' @rdname multipage_align
#' @export
set_dim <- function(plot, dim) {
  if (!is_plot_dimension(dim)) {
    cli_abort('{.arg dim} must be a {.cls plot_dimension} object created with {.fun get_dim}')
  }
  UseMethod('set_dim')
}
#' @export
set_dim.ggplot <- function(plot, dim) {
  plot$fixed_dimensions <- dim
  class(plot) <- c('fixed_dim_ggplot', class(plot))
  plot
}
#' @export
set_dim.patchwork <- function(plot, dim) {
  cli_abort('Setting dimensions on patchworks are currently unsupported')
}
#' @importFrom ggplot2 ggplot_build
#' @export
ggplot_build.fixed_dim_ggplot <- function(plot, ...) {
  plot <- NextMethod()
  class(plot) <- c('fixed_dim_build', class(plot))
  plot
}
#' @importFrom ggplot2 ggplot_gtable
#' @export
ggplot_gtable.fixed_dim_build <- function(data) {
  dim <- data$plot$fixed_dimensions
  table <- NextMethod()
  table <- add_strips(table, data$plot$theme)
  table <- add_guides(table, FALSE)
  panel_pos <- find_panel(table)
  table$widths[seq_len(panel_pos$l - 1)] <- unit(dim$l, 'mm')
  table$widths[seq(panel_pos$r + 1, ncol(table))] <- unit(dim$r, 'mm')
  table$heights[seq_len(panel_pos$t - 1)] <- unit(dim$t, 'mm')
  table$heights[seq(panel_pos$b + 1, nrow(table))] <- unit(dim$b, 'mm')
  table
}
#' @rdname multipage_align
#' @export
get_max_dim <- function(...) {
  if (is_ggplot(..1)) {
    plots <- list(...)
  } else if (is.list(..1)) {
    plots <- ..1
  } else {
    cli_abort('Can only get dimensions from {.cls ggplot} objects or a list of them')
  }
  dims <- lapply(plots, get_dim)
  dims <- list(
    l = exec(pmax, !!!lapply(dims, `[[`, 'l')),
    r = exec(pmax, !!!lapply(dims, `[[`, 'r')),
    t = exec(pmax, !!!lapply(dims, `[[`, 't')),
    b = exec(pmax, !!!lapply(dims, `[[`, 'b'))
  )
  class(dims) <- c('ggplot_dimension', 'plot_dimension')
  dims
}
#' @rdname multipage_align
#' @export
align_patches <- function(...) {
  if (is_ggplot(..1)) {
    plots <- list(...)
  } else if (is.list(..1)) {
    plots <- ..1
  } else {
    cli_abort('Can only align {.cls ggplot} objects or a list of them')
  }
  lapply(plots, set_dim, get_max_dim(plots))
}
#' Deprecated functions
#'
#' These functions are deprecated and should not be used.
#'
#' @export
#' @keywords internal
#' @usage NULL
align_plots <- function(...) {
  .Deprecated('align_patches')
  align_patches(...)
}
