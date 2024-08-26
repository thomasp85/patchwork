#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggplot <- function(object, plot, object_name) {
  patches <- get_patches(plot)
  add_patches(object, patches)
}
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.grob <- function(object, plot, object_name) {
  plot + wrap_elements(full = object)
}
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.formula <- ggplot_add.grob
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.raster <- ggplot_add.grob
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.nativeRaster <- ggplot_add.grob
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.gt_tbl <- ggplot_add.grob

#' @importFrom grid is.grob
#' @importFrom grDevices is.raster
should_autowrap <- function(x) {
  is.grob(x) || inherits(x, 'formula') || is.raster(x) || inherits(x, 'nativeRaster') || inherits(x, 'gt_tbl')
}

# Convert a plot with a (possible) list of patches into a self-contained
# patchwork to be attached to another plot
get_patches <- function(plot) {
  empty <- is_empty(plot)
  if (is_patchwork(plot)) {
    patches <- plot$patches
    plot$patches <- NULL
    class(plot) <- setdiff(class(plot), 'patchwork')
  } else {
    patches <- new_patchwork()
  }
  if (!empty) {
    patches$plots <- c(patches$plots, list(plot))
  }
  patches
}
is_patchwork <- function(x) inherits(x, 'patchwork')
as_patchwork <- function(x) {
  UseMethod('as_patchwork')
}
#' @export
as_patchwork.default <- function(x) {
  cli_abort('Don\'t know how to convert an object of class {.cls {class(x)}} to a patchwork')
}
#' @export
as_patchwork.ggplot <- function(x) {
  class(x) <- c('patchwork', class(x))
  x$patches <- new_patchwork()
  # Will ensure serialisation includes a link to the patchwork namespace
  attr(x, 'patchwork_link') <- patchwork_namespace_link
  x
}
#' @export
as_patchwork.patchwork <- function(x) x

add_patches <- function(plot, patches) {
  UseMethod('add_patches')
}
#' @export
add_patches.ggplot <- function(plot, patches) {
  plot <- as_patchwork(plot)
  plot$patches <- patches
  plot
}
#' @export
add_patches.patchwork <- function(plot, patches) {
  patches$plots <- c(patches$plots, list(plot))
  add_patches(plot_filler(), patches)
}
new_patchwork <- function() {
  list(
    plots = list(),
    layout = plot_layout(),
    annotation = plot_annotation()
  )
}
#' @importFrom ggplot2 ggplot
plot_filler <- function() {
  p <- ggplot()
  class(p) <- c('plot_filler', class(p))
  p
}
is_empty <- function(x) inherits(x, 'plot_filler')
#' @export
has_tag.plot_filler <- function(x) FALSE
