#' @section Overview:
#' The use and premise of `patchwork` is simple: Just add `ggplot2` plots
#' together to compose multiplot layouts. Because of this simplicity there is
#' not much more to say. Still, a few functions allow you to modify the
#' behaviour:
#'
#' - [plot_layout()] allows you to define the grid that plots are put into
#' - [plot_spacer()] inserts a completely empty plot thus pushing the subsequent
#'   plots one cell.
#'
#' That's it...
#'
#' @examples
#' library(ggplot2)
#' # You can add plots saved to variables
#'
#' p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
#'
#' p1 + p2
#'
#' # Or build it up in one step
#' ggplot(mtcars) +
#'   geom_point(aes(mpg, disp)) +
#'   ggplot(mtcars) +
#'   geom_boxplot(aes(gear, disp, group = gear))
#'
#' @name patchwork
#' @docType package
'_PACKAGE'
