
#' @export
#'
merge.patchwork <- function(x, ...) {
  patchwork <- new_patchwork()
  patchwork$plots <- list(x)
  add_patches(plot_filler(), patchwork)
}
#' @export
#'
merge.ggplot <- function(x, ...) {
  x
}
