#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggplot <- function(object, plot, object_name) {
  patchwork <- get_patchwork(plot)
  as.patchwork(object, patchwork)
}
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.grob <- function(object, plot, object_name) {
  plot + wrap_elements(full = object)
}
# Convert a plot with a (possible) list of patches into a selfcontained
# patchwork to be attached to another plot
get_patchwork <- function(plot) {
  empty <- is.empty(plot)
  if (is.patchwork(plot)) {
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
is.patchwork <- function(x) inherits(x, 'patchwork')
as.patchwork <- function(plot, patches) {
  UseMethod('as.patchwork')
}
as.patchwork.ggplot <- function(plot, patches) {
  class(plot) <- c('patchwork', class(plot))
  plot$patches <- patches
  plot
}
as.patchwork.patchwork <- function(plot, patches) {
  patches$plots <- c(patches$plots, list(plot))
  as.patchwork(plot_filler(), patches)
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
  ggplot()
}
is.empty <- function(x) !is.patch(x) && length(x$layers) == 0 && inherits(x$data, 'waiver')

