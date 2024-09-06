#' Wrap a table in a patchwork compliant patch
#'
#' This function works much like [wrap_elements()] in that it turns the input
#' into patchwork compliant objects that can be added to a composition. However,
#' `wrap_table()` uses the knowledge that the input is a table to provide some
#' very nifty layout options that makes it generally better to use than
#' [wrap_elements()] for this type of object.
#'
#' @param table A gt table or an object coercible to a data frame
#' @param panel what portion of the table should be aligned with the panel
#' region? `"body"` means that any column and row headers will be placed outside
#' the panel region, i.e. the topleft corner of the panel region will be aligned
#' with the topleft data cell. `"full"` means that the whole table will be
#' placed inside the panel region. `"rows"` means that all rows (including column
#' headers) will be placed inside the panel region but row headers will be
#' placed to the left. `"cols"` is the opposite, placing all columns within the
#' panel region but keeping the column header on top of it.
#' @param space How should the dimension of the table influence the final
#' composition? `"fixed"` means that the table width will set the width of the
#' column it occupies and the table height will set the height of the row it
#' occupies. `"free"` is the opposite meaning that the table dimension will not
#' have any influence on the sizing. `"free_x"` and `"free_y"` allows you to
#' free either direction while keeping the remaining fixed. Do note that if you
#' set a specific width or height in [plot_layout()] it will have higher
#' priority than the table dimensions
#' @inheritParams wrap_elements
#'
#' @return A wrapped_table object
#'
#' @export
#'
#' @note This functionality requires v0.11.0 or higher of the gt package
#'
#' @examplesIf requireNamespace("gt", quietly = TRUE) && packageVersion("gt") >= "0.11.0"
#' library(ggplot2)
#' library(gt)
#'
#' p1 <- ggplot(airquality) +
#'   geom_line(aes(x = Day, y = Temp, colour = month.name[Month])) +
#'   labs(colour = "Month")
#'
#' table <- data.frame(
#'   Month = month.name[5:9],
#'   "Mean temp." = tapply(airquality$Temp, airquality$Month, mean),
#'   "Min temp." = tapply(airquality$Temp, airquality$Month, min),
#'   "Max temp." = tapply(airquality$Temp, airquality$Month, max)
#' )
#' gt_tab <- gt(table, rowname_col = "Month")
#'
#' # Default addition usees wrap_table
#' p1 + gt_tab
#'
#' # Default places column and row headers outside panel area. Use wrap_table
#' # to control this
#' p1 + wrap_table(gt_tab, panel = "full")
#'
#' # Tables generally have fixed dimensions and these can be used to control
#' # the size of the area they occupy
#' p2 <- ggplot(airquality) +
#'   geom_boxplot(aes(y = month.name[Month], x = Temp)) +
#'   scale_y_discrete(name = NULL, limits = month.name[9:5], guide = "none")
#'
#' wrap_table(gt_tab, space = "fixed") + p2
#'
wrap_table <- function(table, panel = c("body", "full", "rows", "cols"), space = c("free", "free_x", "free_y", "fixed"), ignore_tag = FALSE) {
  check_installed("gt", version = "0.11.0")
  if (!inherits(table, "gt_tbl")) {
    table <- try_fetch(
      gt::gt(as.data.frame(table)),
      error = function(cnd, ...) cli::cli_abort("Unable to convert input table to {.cls gt_tbl}", parent = cnd)
    )
  }
  n_row_headers <- (!all(is.na(table[["_stub_df"]]$row_id))) + (!all(is.na(table[["_stub_df"]]$group_id)))
  if (n_row_headers == 2 && !table[["_options"]]$value[[which(table[["_options"]]$parameter == "row_group_as_column")]]) {
    n_row_headers <- 1
  }
  table <- wrap_elements(table, ignore_tag = ignore_tag)
  attr(table, "patch_settings")$panel <- arg_match(panel)
  attr(table, "patch_settings")$n_row_headers <- n_row_headers
  attr(table, "patch_settings")$space <- c(space %in% c("free", "free_x"), space %in% c("free", "free_y"))
  class(table) <- c("wrapped_table", class(table))
  table
}

#' @export
patchGrob.wrapped_table <- function(x, guides = 'auto') {
  panel <- attr(x, "patch_settings")$panel
  row_head <- attr(x, "patch_settings")$n_row_headers
  space <- attr(x, "patch_settings")$space

  x <- NextMethod()

  table_loc <- which(x$layout$name == "panel")
  table_width <- x$grobs[[table_loc]]$widths
  table_height <- x$grobs[[table_loc]]$heights

  if (panel %in% c("body", "rows")) {
    col_head <- x$grobs[[table_loc]]$layout$t[x$grobs[[table_loc]]$layout$name == "table_body"] - 1
    if (col_head > 0) {
      height <- sum(x$grobs[[table_loc]]$heights[1:col_head])
      x$grobs[[table_loc]]$vp$y <- x$grobs[[table_loc]]$vp$y + height
      x$heights[PANEL_ROW - 2] <- height

      table_height <- table_height[-(1:col_head)]
    }
  }
  if (panel %in% c("body", "cols") && row_head > 0) {
    width <- sum(x$grobs[[table_loc]]$widths[1:row_head])
    x$grobs[[table_loc]]$vp$x <- x$grobs[[table_loc]]$vp$x - width
    x$widths[PANEL_COL - 2] <- width

    table_width <- table_width[-(1:row_head)]
  }
  if (!space[1]) {
    # Something wonky is going on with unit addition sometimes where it looses
    # it's unit type. So we make a dance to make sure
    w <- if (inherits(table_width, "simpleUnit")) sum(table_width) else Reduce(`+`, table_width)
    if (!is.unit(w)) w <- unit(w, unitType(table_width)[1])
    x$widths[PANEL_COL] <- w
  }
  if (!space[2]) {
    h <- if (inherits(table_height, "simpleUnit")) sum(table_height) else Reduce(`+`, table_height)
    if (!is.unit(h)) h <- unit(h, unitType(table_height)[1])
    x$heights[PANEL_ROW] <- h
  }
  x
}

#' @export
#' @importFrom grid viewport grobWidth grobHeight grobTree
as_patch.gt_tbl <- function(x, ...) {
  check_installed("gt", version = "0.11.0")
  grob <- gt::as_gtable(x)
  loc <- grob$layout[grob$layout$name == "table",]
  grob <- grob[loc$t:loc$b, loc$l:loc$r]
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
