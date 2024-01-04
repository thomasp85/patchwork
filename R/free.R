#' Free a plot from alignment
#'
#' While the purpose of patchwork is often to align plots by their panels,
#' sometimes this doesn't cut it and we want to compose plots without alignment.
#' The `free()` function tells patchwork to treat the content (which can either
#' be a ggplot or a patchwork) specially and not align it with the remaining
#' panels in the composition. It works much like using [wrap_elements()] but has
#' a few niceties. For starter, it is less verbose, both with a shorter name,
#' but also without the need to use the `full` argument rather than the first.
#' Second, A plot wrapped with `free()` retains all of it's behavior from
#' before. You can still add stuff to it, change it's theme, etc., but more
#' importantly you can still collect guides and recurse tags as usual. A further
#' nicety is that margins of the plot behave as expected and is aligned with the
#' other plots in the composition.
#'
#' @param x A ggplot or patchwork object
#'
#' @return A modified version of `x` with a `free_plot` class
#'
#' @importFrom ggplot2 is.ggplot
#' @export
#'
#' @examples
#' # Sometimes you have a plot that defies good composition alginment, e.g. due
#' # to long axis labels
#' p1 <- ggplot(mtcars) +
#'   geom_bar(aes(y = factor(gear), fill = factor(gear))) +
#'   scale_y_discrete(
#'     "",
#'     labels = c("3 gears are often enough",
#'                "But, you know, 4 is a nice number",
#'                "I would def go with 5 gears in a modern car")
#'   )
#'
#' # When combined with other plots it ends up looking bad
#' p2 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
#'
#' p1 / p2
#'
#' # We can fix this be using free
#' free(p1) / p2
#'
#' # We can still collect guides like before
#' free(p1) / p2 + plot_layout(guides = "collect")
#'
free <- function(x) {
  check_object(x, function(x) is.ggplot(x) || is_patchwork(x), "a <ggplot> or <patchwork> object")
  class(x) <- c("free_plot", class(x))
  x
}
is_free_plot <- function(x) inherits(x, "free_plot")
