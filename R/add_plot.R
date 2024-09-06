#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggplot <- function(object, plot, object_name) {
  patches <- get_patches(plot)
  add_patches(object, patches)
}
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.grob <- function(object, plot, object_name) {
  table <- as_patch(object)
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
ggplot_add.gt_tbl <- function(object, plot, object_name) {
  plot + wrap_table(object)
}

#' @importFrom grid is.grob
#' @importFrom grDevices is.raster
should_autowrap <- function(x) {
  is.grob(x) || inherits(x, 'formula') || is.raster(x) || inherits(x, 'nativeRaster')
}

# Convert a plot with a (possible) list of patches into a self-contained
# patchwork to be attached to another plot
get_patches <- function(plot) {
  empty <- is_empty(plot)
  if (is_patchwork(plot)) {
    patches <- plot$patches
    plot$patches <- NULL
    class(plot) <- setdiff(class(plot), 'patchwork')
    if (is_free_plot(plot)) {
      attr(plot, "patchwork_free_settings") <- NULL
      if (is.null(attr(plot, "free_settings"))) {
        class(plot) <- setdiff(class(plot), 'free_plot')
      }
    }
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
    # We need to initialise layout and annotation with NULL values rather than waivers
    layout = plot_layout(
      ncol = NULL,
      nrow = NULL,
      byrow = NULL,
      widths = NULL,
      heights = NULL,
      guides = NULL,
      tag_level = NULL,
      design = NULL,
      axes = NULL,
      axis_titles = NULL
    ),
    annotation = plot_annotation(
      title = NULL,
      subtitle = NULL,
      caption = NULL,
      tag_levels = NULL,
      tag_prefix = NULL,
      tag_suffix = NULL,
      tag_sep = NULL,
      theme = NULL
    )
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
