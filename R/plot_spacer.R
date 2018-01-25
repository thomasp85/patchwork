#' Add a completely blank area
#'
#' This simple wrapper creates an empty transparant cell that can be added to
#' push your other plots apart. The cell responds to adding
#' [theme()][ggplot2::theme] specifications, but only `plot.background` will
#' have an effect.
#'
#' @return A `ggplot` object containing an empty plot
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
#'
#' p1 + plot_spacer() + p2
#'
plot_spacer <- function() {
  table <- make_cell()
  class(table) <- c('spacer', class(table))
  table
}
is.spacer <- function(x) inherits(x, 'spacer')
