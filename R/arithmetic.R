#' Plot arithmetic
#'
#' In addition to the `+` operator known in `ggplot2`, `patchwork` defines logic
#' for some of the other operators that aids in building up your plot
#' composition and reduce code-reuse.
#'
#' @details
#' `patchwork` augment the `+` operator from `ggplot2` and allows the user to
#' add full `ggplot` objects together in order to compose them into the same
#' view. The last added plot is always the active one where new geoms etc. are
#' added to. Another operator that is much like it, but no quite, is `/`. It
#' also adds plots together but instead of adding the right hand side to the
#' assemble defined in the left hand side, it puts the left hand side besides
#' the right hand side in a new assemble. This might sound confusing, but in
#' essence `/` ensures that the right and left side are put in the same nesting
#' level (`+` puts the right side *into* the left side).
#'
#' In order to reduce code repetition `patchwork` provides two operators for
#' adding ggplot elements (geoms, themes, facets, etc.) to multiple/all plots in
#' an assemble. `* ` will add the element to all plots in the current nesting
#' level, while `^` will recurse into nested cells.
#'
#' @usage
#' p + p
#' p / p
#' p * gg
#' p ^ gg
#'
#' @param p A `ggplot` or `ggassemble` object
#' @param gg A `gg` object such as a geom or theme specification
#'
#' @return A `ggassemble` object
#'
#' @name plot_arithmetic
#' @rdname plot_arithmetic
#'
#' @examples
#' library(ggplot2)
#'
#' p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
#' p4 <- ggplot(mtcars) + geom_bar(aes(carb))
#'
#' # Standard addition vs division
#' (p1 + p2) + p3 + plot_layout(ncol = 1) # same as p1 + p2 + p3...
#' (p1 + p2) / p3 + plot_layout(ncol = 1)
#'
#' # Add elements to the same nesting level
#' (p1 + (p2 + p3) + p4 + plot_layout(ncol = 1)) * theme_bw()
#'
#' # Recurse into nested plots as well
#' (p1 + (p2 + p3) + p4 + plot_layout(ncol = 1)) ^ theme_bw()
#'
NULL

#' @rdname plot_arithmetic
#' @usage NULL
#' @export
"/.ggplot" <- function(e1, e2) {
  if (!is.ggplot(e2)) stop("Only knows how to fold ggplot objects together", call. = FALSE)
  assemble <- new_assemble()
  if (is.ggassemble(e2)) {
    plot <- plot_filler()
    assemble$plots <- list(e1, e2)
  } else {
    plot <- e2
    assemble$plots <- list(e1)
  }
  as.ggassemble(plot, assemble)
}
#' @rdname plot_arithmetic
#' @usage NULL
#' @export
"*.gg" <- function(e1, e2) {
  if (is.ggassemble(e1)) {
    e1$assemble$plots <- lapply(e1$assemble$plots, function(p) {
      if (!is.ggassemble(p)) p <- p + e2
      p
    })
  }
  e1 + e2
}
#' @rdname plot_arithmetic
#' @usage NULL
#' @export
"^.gg" <- function(e1, e2) {
  if (is.ggassemble(e1)) {
    e1$assemble$plots <- lapply(e1$assemble$plots, function(p) {
      if (is.ggassemble(p)) {
        p <- p ^ e2
      } else {
        p <- p + e2
      }
      p
    })
  }
  e1 + e2
}
