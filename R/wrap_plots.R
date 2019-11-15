#' Wrap plots into an assemble
#'
#' While the use of `+` is a natural way to add plots together, it can be
#' difficult to string together multiple plots programmatically if the number
#' of plots is not known beforehand. `wrap_plots` makes it easy to take a list
#' of plots and add them into one composition, along with layout specifications.
#'
#' If `cells` is specified as a text string *and* the plots are named (e.g.
#' `wrap_plots(A = p1, ...)`) *and* all plot names are single characters
#' represented in the cell layout string, the plots will be matched to their
#' respective area by name. Otherwise the cell areas will be filled out
#' sequentially in the same manner as using the `+` operator. See the examples
#' for more.
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
#' # Match plots to cells by name
#' cell_layout <- "#BB
#'                 AA#"
#' wrap_plots(B = p1, A = p2, cells = cell_layout)
#'
#' # Compare to not using named plot arguments
#' wrap_plots(p1, p2, cells = cell_layout)
#'
wrap_plots <- function(..., ncol = NULL, nrow = NULL, byrow = NULL, widths = NULL, heights = NULL, guides = NULL, tag_level = NULL, cells = NULL) {
  if (is.valid_plot(..1)) {
    plots <- list(...)
  } else if (is.list(..1)) {
    plots <- ..1
  } else {
    stop('Can only wrap ggplot and/or grob objects or a list of them', call. = FALSE)
  }
  if (!all(vapply(plots, is.valid_plot, logical(1)))) stop('Only know how to add ggplots and/or grobs', call. = FALSE)
  if (!is.null(names(plots)) && !is.null(cells) && is.character(cells)) {
    cell_names <- unique(trimws(strsplit(cells, '')[[1]]))
    cell_names <- sort(setdiff(cell_names, c('', '#')))
    if (all(names(plots) %in% cell_names)) {
      plot_list <- vector('list', length(cell_names))
      names(plot_list) <- cell_names
      plot_list[names(plots)] <- plots
      plot_list[vapply(plot_list, is.null, logical(1))] <- list(plot_spacer())
      plots <- plot_list
    }
  }
  Reduce(`+`, plots, init = ggplot()) + plot_layout(
    ncol = ncol, nrow = nrow, byrow = byrow, widths = widths, heights = heights,
    guides = guides, tag_level = tag_level, cells = cells
  )
}

#' @importFrom ggplot2 is.ggplot
#' @importFrom grid is.grob
is.valid_plot <- function(x) is.ggplot(x) || is.grob(x)
