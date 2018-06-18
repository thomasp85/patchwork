#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggplot <- function(object, plot, object_name) {
  assemble <- get_assemble(plot)
  as.ggassemble(object, assemble)
}
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.grob <- function(object, plot, object_name) {
  plot + wrap_elements(full = object)
}
# Convert a plot with a (possible) assemble into a selfcontained assemble to be
# attached to another plot
get_assemble <- function(plot) {
  empty <- is.empty(plot)
  if (is.ggassemble(plot)) {
    assemble <- plot$assemble
    plot$assemble <- NULL
    class(plot) <- setdiff(class(plot), 'ggassemble')
  } else {
    assemble <- new_assemble()
  }
  if (!empty) {
    assemble$plots <- c(assemble$plots, list(plot))
  }
  assemble
}
is.ggassemble <- function(x) inherits(x, 'ggassemble')
as.ggassemble <- function(plot, assemble) {
  UseMethod('as.ggassemble')
}
as.ggassemble.ggplot <- function(plot, assemble) {
  class(plot) <- c('ggassemble', class(plot))
  plot$assemble <- assemble
  plot
}
as.ggassemble.ggassemble <- function(plot, assemble) {
  assemble$plots <- c(assemble$plots, list(plot))
  as.ggassemble(plot_filler(), assemble)
}
new_assemble <- function() {
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
is.empty <- function(x) !is.assemble_cell(x) && length(x$layers) == 0 && inherits(x$data, 'waiver')

