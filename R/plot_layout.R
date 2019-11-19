#' Define the grid to compose plots in
#'
#' In order to control how different plots are layed out, you need to add a
#' layout specification. If you are nesting grids, the layout is scoped to the
#' current nesting level.
#'
#' @param ncol,nrow The dimensions of the grid to create - if both are `NULL` it
#' will use the same logic as [facet_wrap()][ggplot2::facet_wrap] to set the
#' dimensions
#' @param byrow Analogous to `byrow` in [matrix()][base::matrix]. If `FALSE` the
#' plots will be filled in in column-major order
#' @param widths,heights The relative widths and heights of each column and row
#' in the grid. Will get repeated to match the dimensions of the grid.
#' @param guides A string specifying how guides should be treated in the layout.
#' `'collect'` will collect guides below to the given nesting level, removing
#' duplicates. `'keep'` will stop collection at this level and let guides be
#' placed alongside their plot. `auto` will allow guides to be collected if a
#' upper level tries, but place them alongside the plot if not.
#' @param tag_level A string (`'keep'` or `'new'`) to indicate how
#' auto-tagging should behave. See [plot_annotation()].
#' @param design Specification of the location of areas in the layout. Can either
#' be specified as a text string or by concatenating calls to [area()] together.
#' See the examples for further information on use.
#'
#' @return A `plot_layout` object to be added to a `ggassmble` object
#'
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
#' # The plots are layed out automatically by default
#' p1 + p2 + p3 + p4 + p5
#'
#' # Use byrow to change how the grid is filled out
#' p1 + p2 + p3 + p4 + p5 + plot_layout(byrow = FALSE)
#'
#' # Change the grid dimensions
#' p1 + p2 + p3 + p4 + p5 + plot_layout(ncol = 2, widths = c(1, 2))
#'
#' # Define layout at different nesting levels
#' p1 +
#'   p2 +
#'   (p3 +
#'      p4 +
#'      plot_layout(ncol = 1)
#'   ) +
#'   p5 +
#'   plot_layout(widths = c(2, 1))
#'
#' # Complex layouts can be created with the `design` argument
#' design <- c(
#'   area(1, 1, 2),
#'   area(1, 2, 1, 3),
#'   area(2, 3, 3),
#'   area(3, 1, 3, 2),
#'   area(2, 2)
#' )
#' p1 + p2 + p3 + p4 + p5 + plot_layout(design = design)
#'
#' # The same can be specified as a character string:
#' design <- "
#'   122
#'   153
#'   443
#' "
#' p1 + p2 + p3 + p4 + p5 + plot_layout(design = design)
#'
#' # When using strings to define the design `#` can be used to denote empty
#' # areas
#' design <- "
#'   1##
#'   123
#'   ##3
#' "
#' p1 + p2 + p3 + plot_layout(design = design)
#'
plot_layout <- function(ncol = NULL, nrow = NULL, byrow = NULL, widths = NULL,
                        heights = NULL, guides = NULL, tag_level = NULL,
                        design = NULL) {
  if (!is.null(guides)) guides <- match.arg(guides, c('auto', 'collect', 'keep'))
  if (!is.null(tag_level)) tag_level <- match.arg(tag_level, c('keep', 'new'))
  structure(list(
    ncol = ncol,
    nrow = nrow,
    byrow = byrow,
    widths = widths,
    heights = heights,
    guides = guides,
    tag_level = tag_level,
    design = as_areas(design)
  ), class = 'plot_layout')
}
#' Specify a plotting area in a layout
#'
#' This is a small helper used to specify a single area in a rectangular grid
#' that should contain a plot. Objects constructed with `area()` can be
#' concatenated together with `c()` in order to specify multiple areas.
#'
#' The grid that the areas are specified in reference to enumerate rows from top
#' to bottom, and coloumns from left to right. This means that `t` and `l`
#' should always be less or equal to `b` and `r` respectively. Instead of
#' specifying area placement with a combination of `area()` calls, it is
#' possible to instead pass in a single string
#'
#' ```
#' areas <- c(area(1, 1, 2, 1),
#'            area(2, 3, 3, 3))
#' ```
#'
#' is equivalent to
#'
#' ```
#' areas < -"A##
#'           A#B
#'           ##B"
#' ```
#'
#' For an example of this, see the [plot_layout()] examples.
#'
#' @param t,b The top and bottom bounds of the area in the grid
#' @param l,r The left and right bounds of the area int the grid
#'
#' @return A `patch_area` object
#'
#' @export
area <- function(t, l, b = t, r = l) {
  if (missing(t) || missing(l)) {
    one_area <- list(
      t = integer(0),
      l = integer(0),
      b = integer(0),
      r = integer(0)
    )
  } else {
    len <- max(length(t), length(l), length(b), length(r))
    one_area <- list(
      t = rep_len(as.integer(t), len),
      l = rep_len(as.integer(l), len),
      b = rep_len(as.integer(b), len),
      r = rep_len(as.integer(r), len)
    )
    if (any(t > b)) {
      stop('`t` must be less than `b`', call. = FALSE)
    }
    if (any(l > r)) {
      stop('`l` must be less than `r`', call. = FALSE)
    }
  }
  class(one_area) <- 'patch_area'
  one_area
}
as_areas <- function(x) {
  if (is.null(x)) return(NULL)
  if (is_area(x)) return(x)
  if (!is.character(x)) {
    stop("Don't know how to convert ", class(x)[1], " into area positions", call. = FALSE)
  }
  x <- strsplit(x, split = '\n')[[1]]
  x <- lapply(x, trimws)
  if (identical(x[[1]], '')) x[1] <- NULL
  if (identical(x[[length(x)]], '')) x[length(x)] <- NULL
  x <- lapply(x, function(x) strsplit(x, '')[[1]])
  ncols <- vapply(x, length, integer(1))
  if (length(unique(ncols)) != 1) {
    stop("character layout must be rectangular", call. = FALSE)
  }
  row <- rep(seq_along(x), each = ncols[1])
  col <- rep(seq_len(ncols[1]), length(x))
  x <- unlist(x)
  area_names <- unique(sort(x))
  area_names[area_names == '#'] <- NA
  x <- match(x, area_names)
  do.call(c, lapply(split(seq_along(x), x), function(i) {
    if (is.na(x[i[1]])) return(area())
    area_rows <- range(row[i])
    area_cols <- range(col[i])
    if (!all(x[row >= area_rows[1] & row <= area_rows[2] & col >= area_cols[1] & col <= area_cols[2]] == x[i[1]])) {
      stop('Patch areas must be rectangular', call. = FALSE)
    }
    area(area_rows[1], area_cols[1], area_rows[2], area_cols[2])
  }))
}
is_area <- function(x) inherits(x, 'patch_area')
#' @export
length.patch_area <- function(x) length(x$t)
#' @export
print.patch_area <- function(x, ...) {
  cat(length(x), 'patch areas, spanning', max(x$r), 'columns and', max(x$b), 'rows\n\n')
  print(as.data.frame(unclass(x), row.names = paste0(seq_along(x), ': ')))
}
#' @export
c.patch_area <- function(..., recursive = FALSE) {
  all_areas <- list(...)

  if (length(all_areas) == 0) return(area())

  if (any(!vapply(all_areas, is_area, logical(1)))) {
    stop('Areas can only be combined with each other', call. = FALSE)
  }
  area <- all_areas[[1]]
  area$t <- unlist(lapply(all_areas, `[[`, 't'))
  area$l <- unlist(lapply(all_areas, `[[`, 'l'))
  area$b <- unlist(lapply(all_areas, `[[`, 'b'))
  area$r <- unlist(lapply(all_areas, `[[`, 'r'))
  area
}
default_layout <- plot_layout(byrow = TRUE, widths = 1, heights = 1, guides = 'auto', tag_level = 'keep')
#' @importFrom utils modifyList
#' @export
ggplot_add.plot_layout <- function(object, plot, object_name) {
  if (!is.patchwork(plot)) stop('plot_layout must be added to a patchwork of plots', call. = FALSE)
  plot$patches$layout <- modifyList(plot$patches$layout, object[!vapply(object, is.null, logical(1))])
  plot
}
