#' Add a completely blank area
#'
#' This simple wrapper creates an empty transparant plot that can be added to
#' push your other plots apart
#'
#' @return A `ggplot` object containing an empty plot
#'
#' @importFrom ggplot2 ggplot theme_void theme element_rect
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
  p <- ggplot() +
    theme_void() +
    theme(plot.background = element_rect(fill = NA, colour = NA))
  class(p) <- c('spacer', class(p))
  p
}
is.spacer <- function(x) inherits(x, 'spacer')
