#' Wrap plots into an assemble
#'
#' While the use of `+` is a natural way to add plots together, it can be
#' difficult to string together multiple plots programmatically if the number
#' of plots is not known beforehand. `wrap_plots` makes it easy to take a list
#' of plots and add them into one composition, along with layout specifications.
#'
#' @param ... multiple `ggplot`s or a list containing `ggplot` objects
#' @inheritParams plot_layout
#'
#' @return A `ggassemble` object
#'
#' @importFrom ggplot2 is.ggplot
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
#' # Either add the plots as single arguments
#' wrap_plots(p1, p2, p3, p4, p5)
#'
#' # Or add them as a list...
#' plots <- list(p1, p2, p3, p4, p5)
#' wrap_plots(plots)
#'
wrap_plots <- function(..., ncol = NULL, nrow = NULL, byrow = TRUE, widths = 1, heights = 1) {
  if (is.ggplot(..1)) {
    plots <- list(...)
  } else if (is.list(..1)) {
    plots <- ..1
  } else {
    stop('Can only wrap ggplot objects or a list of them', call. = FALSE)
  }
  if (!all(vapply(plots, is.ggplot, logical(1)))) stop('Only know how to add ggplots', call. = FALSE)
  Reduce(`+`, plots) + plot_layout(ncol = ncol, nrow = nrow, byrow = byrow,
                                   widths = widths, heights = heights)
}
