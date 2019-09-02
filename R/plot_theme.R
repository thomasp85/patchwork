#' Modify global aesthetics
#'
#' @param background The background color of everything, including everything around the plots. (default: NULL -> white or transparent)
#' @param border_color The color of a border around the plot (default: "transparent")
#' @param border_thickness The thickness of the border (default: NULL -> 0)
#' @param border_linetype The line type of the border (default: NULL -> solid)
#'
#' @return A `plot_layout` object to be added to a `ggassmble` object
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#'p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp)) + theme_bw()
#'p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear)) + theme_bw()
#'
#'p1 + p2 + plot_theme(background = "gray92")
#'
plot_theme <- function(background = NULL, border_color = "transparent", border_thickness = NULL, border_linetype = NULL) {
  structure(list(
    background = background,
    border_color = border_color,
    border_thickness = border_thickness,
    border_linetype = border_linetype
  ), class = 'plot_theme')
}
default_theme <- plot_theme()

#' @export
ggplot_add.plot_theme <- function(object, plot, object_name) {
  if (!is.ggassemble(plot)) stop('plot_theme must be added to an assemble of plots', call. = FALSE)
  plot$assemble$theme <- object
  plot
}

