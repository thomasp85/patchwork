#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggplot <- function(object, plot, object_name) {
  assemble <- get_assemble(plot)
  as.ggassemble(object, assemble)
}

get_assemble <- function(plot) {
  empty <- is.empty(plot)
  if (is.ggassemble(plot)) {
    assemble <- plot$assemble
    plot$assemble <- NULL
    class(plot) <- c('gg', 'ggplot')
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
#' @importFrom ggplot2 ggplot
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

plot_filler <- function() {
  ggplot()
}
is.empty <- function(x) !is.assemble_cell(x) && length(x$layers) == 0 && inherits(x$data, 'waiver')

