#' Wrap plots into a patchwork
#'
#' While the use of `+` is a natural way to add plots together, it can be
#' difficult to string together multiple plots programmatically if the number
#' of plots is not known beforehand. `wrap_plots` makes it easy to take a list
#' of plots and add them into one composition, along with layout specifications.
#'
#' If `design` is specified as a text string *and* the plots are named (e.g.
#' `wrap_plots(A = p1, ...)`) *and* all plot names are single characters
#' represented in the design layout string, the plots will be matched to their
#' respective area by name. Otherwise the areas will be filled out
#' sequentially in the same manner as using the `+` operator. See the examples
#' for more.
#'
#' @param ... multiple `ggplot`s or a list containing `ggplot` objects
#' @inheritParams plot_layout
#'
#' @return A `patchwork` object
#'
#' @importFrom ggplot2 is.ggplot
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) + geom_bar(aes(gear)) + facet_wrap(~cyl)
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
#' # Match plots to areas by name
#' design <- "#BB
#'            AA#"
#' wrap_plots(B = p1, A = p2, design = design)
#'
#' # Compare to not using named plot arguments
#' wrap_plots(p1, p2, design = design)
#'
wrap_plots <- function(..., ncol = NULL, nrow = NULL, byrow = NULL,
                       widths = NULL, heights = NULL, guides = NULL,
                       tag_level = NULL, design = NULL, axes = NULL,
                       axis_titles = axes) {
  if (is_valid_plot(..1)) {
    plots <- list(...)
  } else if (is.list(..1)) {
    plots <- ..1
  } else {
    cli_abort('Can only wrap {.cls ggplot} and/or {.cls grob} objects or a list of them')
  }
  if (!all(vapply(plots, is_valid_plot, logical(1)))) cli_abort('Only know how to add {.cls ggplot} and/or {.cls grob} objects')
  if (!is.null(names(plots)) && !is.null(design) && is.character(design)) {
    area_names <- unique(trimws(strsplit(design, '')[[1]]))
    area_names <- sort(setdiff(area_names, c('', '#')))
    if (all(names(plots) %in% area_names)) {
      plot_list <- vector('list', length(area_names))
      names(plot_list) <- area_names
      plot_list[names(plots)] <- plots
      plot_list[vapply(plot_list, is.null, logical(1))] <- list(plot_spacer())
      plots <- plot_list
    }
  }
  Reduce(`+`, plots, init = plot_filler()) + plot_layout(
    ncol = ncol, nrow = nrow, byrow = byrow, widths = widths, heights = heights,
    guides = guides, tag_level = tag_level, design = design, axes = axes,
    axis_titles = axis_titles
  )
}

#' @importFrom ggplot2 is.ggplot
#' @importFrom grid is.grob
is_valid_plot <- function(x) is.ggplot(x) || is.grob(x)
