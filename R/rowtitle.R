#' Create a Row Title for Patchwork
#'
#' Creates a plot with a centered text label designed to be used as a row
#' title in a multi-row patchwork composition. The plot has a minimal theme
#' with a customizable background and bold text.
#'
#' @param label Character string for the row title label. Must be a single value.
#' @param x Numeric x position of text. Must be between 0 and 1. Default: 0.5 (center).
#' @param y Numeric y position of text. Must be between 0 and 1. Default: 0.5 (center).
#' @param size Numeric text size. Must be positive. Default: 5.5.
#' @param hjust Numeric horizontal justification. Must be between 0 and 1. Default: 0.5 (center).
#' @param vjust Numeric vertical justification. Must be between 0 and 1. Default: 0.5 (center).
#' @param fill Character color for plot background. Must be a single value. Default: "grey92".
#' @param color Character color for plot border. Must be a single value. Default: "black".
#' @param linewidth Numeric width of plot border. Must be positive. Default: 1.5.
#' @param margin_top Numeric top margin in points. Must be non-negative. Default: 6.
#' @param margin_right Numeric right margin in points. Must be non-negative. Default: 0.
#' @param margin_bottom Numeric bottom margin in points. Must be non-negative. Default: 0.
#' @param margin_left Numeric left margin in points. Must be non-negative. Default: 0.
#'
#' @return A ggplot object suitable for use in a patchwork composition.
#'
#' @examples
#' ggplot2::theme_set(theme_bw2())
#'
#' # Create sample plots
#' p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "Plot 1")
#' p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, hp)) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "Plot 2")
#' p3 <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
#'   ggplot2::geom_boxplot() +
#'   ggplot2::labs(title = "Plot 3")
#' p4 <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), hp)) +
#'   ggplot2::geom_boxplot() +
#'   ggplot2::labs(title = "Plot 4")
#'
#' # Use case 1: Row title as an entire row (spanning all columns)
#' row_title_1 <- rowtitle("Group A")
#' row_title_2 <- rowtitle("Group B")
#' patchwork1 <- p1 + p2
#' patchwork2 <- p3 + p4
#'
#' row_title_1 +
#'   patchwork1 +
#'   row_title_2 +
#'   patchwork2 +
#'   patchwork::plot_layout(ncol = 1, heights = c(0.2, 1, 0.2, 1))
#'
#' # Use case 2: Row title as a box to the left (narrow column)
#' row_title_left_1 <- rowtitle("Group\nA")
#' row_title_left_2 <- rowtitle("Group\nB")
#'
#' row_title_left_1 + patchwork1 +
#'   row_title_left_2 + patchwork2 +
#'   patchwork::plot_layout(ncol = 2, widths = c(0.2, 1))
#'
#' @export
rowtitle <- function(
  label,
  x = 0.5,
  y = 0.5,
  size = 5.5,
  hjust = 0.5,
  vjust = 0.5,
  fill = "grey92",
  color = "black",
  linewidth = 1.5,
  margin_top = 6,
  margin_right = 0,
  margin_bottom = 0,
  margin_left = 0
) {
  # Validate arguments
  stopifnot(
    is.character(label),
    length(label) == 1
  )
  stopifnot(
    is.numeric(x),
    length(x) == 1,
    x >= 0,
    x <= 1
  )
  stopifnot(
    is.numeric(y),
    length(y) == 1,
    y >= 0,
    y <= 1
  )
  stopifnot(
    is.numeric(hjust),
    length(hjust) == 1,
    hjust >= 0,
    hjust <= 1
  )
  stopifnot(
    is.numeric(vjust),
    length(vjust) == 1,
    vjust >= 0,
    vjust <= 1
  )
  stopifnot(
    is.numeric(size),
    length(size) == 1,
    size > 0
  )
  stopifnot(
    is.numeric(linewidth),
    length(linewidth) == 1,
    linewidth > 0
  )
  stopifnot(
    is.character(fill),
    length(fill) == 1
  )
  stopifnot(
    is.character(color),
    length(color) == 1
  )
  stopifnot(
    is.numeric(margin_top),
    length(margin_top) == 1,
    margin_top >= 0
  )
  stopifnot(
    is.numeric(margin_right),
    length(margin_right) == 1,
    margin_right >= 0
  )
  stopifnot(
    is.numeric(margin_bottom),
    length(margin_bottom) == 1,
    margin_bottom >= 0
  )
  stopifnot(
    is.numeric(margin_left),
    length(margin_left) == 1,
    margin_left >= 0
  )

  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = x,
      y = y,
      label = label,
      size = size,
      fontface = "bold",
      hjust = hjust,
      vjust = vjust
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = fill,
        color = color,
        linewidth = linewidth
      ),
      plot.margin = ggplot2::margin(
        margin_top,
        margin_right,
        margin_bottom,
        margin_left
      )
    )
}
