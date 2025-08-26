#' Make a gtable created from a ggplot object patchwork compliant
#'
#' This function converts a gtable, as produced by [ggplot2::ggplotGrob()] and
#' makes it ready to be added to a patchwork. In contrast to passing
#' the gtable to [wrap_elements()], `wrap_ggplot_grob()` ensures proper
#' alignment as expected. On the other hand major restructuring of the gtable
#' will result in an object that doesn't work properly with
#' `wrap_ggplot_grob()`.
#'
#' @param x A gtable as produced by [ggplot2::ggplotGrob()]
#'
#' @return A `table_patch` object to be added to a patchwork
#'
#' @export
#'
#' @examples
#' library(grid)
#' library(gtable)
#' library(ggplot2)
#'
#' p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp)) + ggtitle('disp and mpg seems connected')
#' p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
#'
#' # Convert p2 so we can add new stuff to it
#' p2_table <- ggplotGrob(p2)
#' stamp <- textGrob('TOP SECRET', rot = 35,
#'   gp = gpar(fontsize = 72, fontface = 'bold')
#' )
#' p2_table <- gtable_add_grob(p2_table, stamp,
#'   t = 1, l = 1, b = nrow(p2_table), r = ncol(p2_table)
#' )
#'
#' # Adding it directly will loose alignment
#' p1 + p2_table
#'
#' # Use wrap_ggplot_grob to keep alignment
#' p1 + wrap_ggplot_grob(p2_table)
#'
wrap_ggplot_grob <- function(x) {
  check_object(x, is.gtable, "a <gtable> object")
  if (length(x$widths) > TABLE_COLS || length(x$heights) > TABLE_ROWS) {
    cli_abort("{.arg x} does not appear to be a gtable created from a {.cls ggplot} object")
  }
  patch <- make_patch()
  class(patch) <- c('table_patch', class(patch))
  attr(patch, 'table') <- x
  patch
}

#' @importFrom ggplot2 ggplot_build ggplot_gtable
#' @importFrom gtable gtable_add_grob
#' @export
patchGrob.table_patch <- function(x, guides = 'auto') {
  data <- ggplot_build(x)
  gt <- attr(x, 'table')
  # use the completed theme when adding strips
  gt <- add_strips(gt, data$plot$theme)
  gt <- add_guides(gt, guides == 'collect')
  if ("tag" %in% names(x$labels)) {
    # use the completed theme when adding strips
    plot <- add_guides(add_strips(ggplot_gtable(data), data$plot$theme))
    tag_idx <- which(plot$layout$name == "tag")
    gt <- gtable_add_grob(
      gt,
      grobs = plot$grobs[[tag_idx]],
      t = plot$layout$t[tag_idx],
      l = plot$layout$l[tag_idx],
      b = plot$layout$b[tag_idx],
      r = plot$layout$r[tag_idx],
      z = max(gt$layout$z) + 1,
      clip = plot$layout$clip[tag_idx],
      name = "tag"
    )
  }
  gt
}
