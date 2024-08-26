#' Wrap arbitrary graphics in a patchwork-compliant patch
#'
#' In order to add non-ggplot2 element to a patchwork they can be
#' converted to a compliant representation using the `wrap_elements()` function.
#' This allows you to position either grobs, ggplot objects, patchwork
#' objects, or even base graphics (if passed as a formula) in either the full
#' area, the full plotting area (anything between and
#' including the axis label), or the panel area (only the actual area where data
#' is drawn). Further you can still add title, subtitle, tag, and caption using
#' the same approach as with normal ggplots (using
#' [ggtitle()][ggplot2::ggtitle] and [labs()][ggplot2::labs]) as well as styling
#' using [theme()][ggplot2::theme]. For the latter, only the theme elements
#' targeting plot margins and background as well as title, subtitle, etc styling
#' will have an effect. If a patchwork or ggplot object is wrapped, it will be
#' fixated in its state and will no longer respond to addition of styling,
#' geoms, etc.. When grobs and formulas are added directly, they will implicitly
#' be converted to `wrap_elements(full = x)`.
#'
#' @param panel,plot,full A grob, ggplot, patchwork, formula, raster,
#' nativeRaster, or gt object to add to the respective area.
#'
#' @param clip Should the grobs be clipped if expanding outside its area
#'
#' @param ignore_tag Should tags be ignored for this patch. This is relevant
#' when using automatic tagging of plots and the content of the patch does not
#' qualify for a tag.
#'
#' @return A wrapped_patch object
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(grid)
#'
#' # Combine grobs with each other
#' wrap_elements(panel = textGrob('Here are some text')) +
#'   wrap_elements(
#'     panel = rectGrob(gp = gpar(fill = 'steelblue')),
#'     full = rectGrob(gp = gpar(fill = 'goldenrod'))
#'   )
#'
#' # wrapped elements can still get titles etc like ggplots
#' wrap_elements(panel = textGrob('Here are some text')) +
#'   wrap_elements(
#'     panel = rectGrob(gp = gpar(fill = 'steelblue')),
#'     full = rectGrob(gp = gpar(fill = 'goldenrod'))
#'   ) +
#'   ggtitle('Title for the amazing rectangles')
#'
#' # You can also pass in ggplots or patchworks to e.g. have it fill out the
#' # panel area
#' p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
#' p1 + wrap_elements(panel = p1 + ggtitle('Look at me shrink'))
#'
#' # You can even add base graphics if you pass it as a formula (requires gridGraphics package)
#' if (requireNamespace("gridGraphics", quietly = TRUE)) {
#'   p1 + wrap_elements(full = ~ plot(mtcars$mpg, mtcars$disp))
#'
#'   # Adding a grob or formula directly is equivalent to placing it in `full`
#'   p1 + ~ plot(mtcars$mpg, mtcars$disp)
#' }
#'
wrap_elements <- function(panel = NULL, plot = NULL, full = NULL, clip = TRUE, ignore_tag = FALSE) {
  clip <- if (clip) 'on' else 'off'
  table <- make_patch()
  attr(table, 'grobs') <- list(panel = panel, plot = plot, full = full)
  attr(table, 'settings') <- list(clip = clip, ignore_tag = ignore_tag)
  class(table) <- c('wrapped_patch', class(table))
  table
}
is_wrapped_patch <- function(x) inherits(x, 'wrapped_patch')
#' @importFrom ggplot2 ggplotGrob theme_get
#' @importFrom gtable gtable_add_grob
#' @importFrom grid grobHeight convertHeight
#' @export
patchGrob.wrapped_patch <- function(x, guides = 'auto') {
  gt <- ggplotGrob(x)
  table <- patch_table(x, gt)
  settings <- attr(x, 'settings')
  grobs <- attr(x, 'grobs')
  if (!is.null(grobs$full)) {
    table <- gtable_add_grob(table, list(as_patch(grobs$full)), 1, 1, nrow(table),
                             ncol(table), clip = settings$clip, name = 'full')
  }
  if (!is.null(grobs$plot)) {
    table <- gtable_add_grob(table, list(as_patch(grobs$plot)), PLOT_TOP,
                             PLOT_LEFT, PLOT_BOTTOM, PLOT_RIGHT,
                             clip =  settings$clip, name = 'plot')
  }
  if (!is.null(grobs$panel)) {
    table <- gtable_add_grob(table, list(as_patch(grobs$panel)), PANEL_ROW,
                             PANEL_COL, clip = settings$clip, name = 'panel')
  }
  title <- get_grob(gt, 'title')
  table <- gtable_add_grob(table, list(title), TITLE_ROW, PANEL_COL,
                           clip = settings$clip, name = 'title')
  table$heights[TITLE_ROW] <- convertHeight(grobHeight(title), 'mm')
  subtitle <- get_grob(gt, 'subtitle')
  table <- gtable_add_grob(table, list(subtitle), SUBTITLE_ROW, PANEL_COL,
                           clip = settings$clip, name = 'subtitle')
  table$heights[SUBTITLE_ROW] <- convertHeight(grobHeight(subtitle), 'mm')
  caption <- get_grob(gt, 'caption')
  table <- gtable_add_grob(table, list(caption), CAPTION_ROW, PANEL_COL,
                           clip = settings$clip, name = 'caption')
  table$heights[CAPTION_ROW] <- convertHeight(grobHeight(caption), 'mm')
  if (!settings$ignore_tag) {
    table$widths[c(2, ncol(table)-1)] <- gt$widths[c(2, ncol(gt)-1)]
    table$heights[c(2, nrow(table)-1)] <- gt$heights[c(2, nrow(gt)-1)]
    tag <- get_grob(gt, 'tag')
    tag_pos <- calc_element("plot.tag.position", x$theme %||% theme_get())
    if (is.null(tag_pos)) tag_pos <- theme_get()$plot.tag.position
    if (!is.character(tag_pos)) tag_pos <- 'manual'
    table <- switch(
      tag_pos,
      topleft = gtable_add_grob(table, tag, name = "tag", t = 2, l = 2, clip = "off"),
      top = gtable_add_grob(table, tag, name = "tag", t = 2, l = 2, r = ncol(table)-1, clip = "off"),
      topright = gtable_add_grob(table, tag, name = "tag", t = 2, l = ncol(table)-1, clip = "off"),
      left = gtable_add_grob(table, tag, name = "tag", t = 2, b = nrow(table)-1, l = 2, clip = "off"),
      right = gtable_add_grob(table, tag, name = "tag", t = 2, b = nrow(table)-1, l = ncol(table)-1, clip = "off"),
      bottomleft = gtable_add_grob(table, tag, name = "tag", t = nrow(table)-1, l = 2, clip = "off"),
      bottom = gtable_add_grob(table, tag, name = "tag", t = nrow(table)-1, l = 2, r = ncol(table)-1, clip = "off"),
      bottomright = gtable_add_grob(table, tag, name = "tag", t = nrow(table)-1, l = ncol(table)-1, clip = "off"),
      manual = gtable_add_grob(table, tag, name = 'tag', t = 2, l = 2, b = nrow(table)-1, r = ncol(table)-1, clip = "off")
    )
  }
  table
}

as_patch <- function(x, ...) {
  UseMethod('as_patch')
}
#' @export
as_patch.grob <- function(x, ...) {
  x
}
#' @importFrom grid gTree
#' @export
as_patch.gList <- function(x, ...) {
  gTree(children = x)
}
#' @importFrom ggplot2 ggplotGrob
#' @export
as_patch.ggplot <- function(x, ...) {
  ggplotGrob(x)
}
#' @export
as_patch.patchwork <- function(x, ...) {
  patchworkGrob(x)
}
#' @export
as_patch.formula <- function(x, ...) {
  rlang::check_installed('gridGraphics', 'to add base plots to patchworks')
  gp <- graphics::par(no.readonly = TRUE)
  plot_call <- function() {
    old_gp <- graphics::par(no.readonly = TRUE)
    graphics::par(gp)
    on.exit(try(graphics::par(old_gp)))
    res <- suppressMessages(eval(x[[2]], attr(x, '.Environment')))
    invisible(NULL)
  }
  gridGraphics::echoGrob(plot_call, name = 'patchwork_base', device = offscreen_dev())
}
#' @export
#' @importFrom grid rasterGrob
as_patch.raster <- function(x, ...) {
  as_patch(rasterGrob(x), ...)
}
#' @export
as_patch.nativeRaster <- as_patch.raster
#' @export
#' @importFrom grid viewport grobWidth grobHeight grobTree
as_patch.gt_tbl <- function(x, ...) {
  check_installed("gt", version = "0.11.0")
  grob <- gt::as_gtable(x)
  grob$vp <- viewport(
    x = 0,
    y = 1,
    width = grobWidth(grob),
    height = grobHeight(grob),
    default.units = "npc",
    just = c(0, 1)
  )
  grob
}

#' @importFrom ggplot2 ggplotGrob
get_grob <- function(x, name) {
  ind <- grep(paste0('^', name, '$'), x$layout$name)
  if (length(ind) == 0) return(ggplot2::zeroGrob())
  x$grobs[[grep(paste0('^', name, '$'), x$layout$name)]]
}
offscreen_dev <- function() {
  if (requireNamespace('ragg', quietly = TRUE)) {
    function(width, height) {
      ragg::agg_capture(width = width, height = height, units = 'in')
      grDevices::dev.control("enable")
    }
  } else {
    function(width, height) {
      grDevices::pdf(NULL, width = width, height = height)
      grDevices::dev.control("enable")
    }
  }
}
#' @export
has_tag.wrapped_patch <- function(x) !attr(x, 'settings')$ignore_tag
