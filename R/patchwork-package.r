#' @section Overview:
#' The use and premise of `patchwork` is simple: Just add `ggplot2` plots
#' together to compose multiplot layouts. Because of this simplicity there is
#' not much more to say. Still, a few functions allow you to modify the
#' behaviour, e.g.:
#'
#' - [plot_layout()] allows you to define the grid that plots are put into
#' - [plot_annotation()] allows you to add titles, tags etc.
#'
#' @section Learn more:
#' The guides below will teach you all about what you can do with patchwork.
#'
#' - [Getting Started](https://patchwork.data-imaginist.com/articles/patchwork.html)
#' - [Assembling Plots](https://patchwork.data-imaginist.com/articles/guides/assembly.html)
#' - [Defining Layouts](https://patchwork.data-imaginist.com/articles/guides/layout.html)
#' - [Adding Annotation](https://patchwork.data-imaginist.com/articles/guides/annotation.html)
#' - [Aligning across pages](https://patchwork.data-imaginist.com/articles/guides/multipage.html)
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
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @import cli
## usethis namespace: end
NULL
