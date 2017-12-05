#' @importFrom ggplot2 wrap_dims
#' @importFrom grid grid.newpage grid.draw seekViewport pushViewport upViewport
#' @export
print.ggassemble <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  assemble <- get_assemble(x)
  gtable <- assemble_grob(assemble)
  if (newpage) grid.newpage()
  if (is.null(vp)) {
    grid.draw(gtable)
  } else {
    if (is.character(vp)) {
      seekViewport(vp)
    } else {
      pushViewport(vp)
    }
    grid.draw(gtable)
    upViewport()
  }
  invisible(x)
}
#' @importFrom ggplot2 ggplot_build ggplot_gtable panel_rows panel_cols
#' @importFrom stats na.omit
assemble_grob <- function(x) {
  pb <- lapply(x$plots, ggplot_build)
  gt <- lapply(pb, ggplot_gtable)
  gt <- lapply(gt, simplify_gt)
  dims <- wrap_dims(length(x$plots), nrow = x$layout$nrow, ncol = x$layout$ncol)
  index_mat <- matrix(NA_integer_, ncol = dims[2], nrow = dims[1])
  if (x$layout$byrow) {
    index_mat <- t(index_mat)
    index_mat[seq_along(gt)] <- seq_along(gt)
    index_mat <- t(index_mat)
  } else {
    index_mat[seq_along(gt)] <- seq_along(gt)
  }
  gt_new <- lapply(seq_len(nrow(index_mat)), function(i) {
    ind <- na.omit(index_mat[i, ])
    do.call(cbind, c(gt[ind], list(size = 'first')))
  })
  ncol_total <- max(vapply(gt_new, ncol, integer(1)))
  gt_new <- lapply(gt_new, function(gt_tmp) {
    extra_col <- ncol_total - ncol(gt_tmp)
    if (extra_col == 0) return(gt_tmp)
    gtable_add_cols(gt_tmp, unit(rep(0, extra_col), 'mm'))
  })
  gt_new <- do.call(rbind, c(gt_new, list(size = 'first')))
  p_cols <- panel_cols(gt_new)$l
  p_rows <- panel_rows(gt_new)$t
  t_dims <- table_dims(gt, index_mat)
  gt_new$widths[-p_cols] <- t_dims$widths[-p_cols]
  gt_new$widths[p_cols] <- unit(rep(x$layout$widths, lengths.out = dims[2]), 'null')
  gt_new$heights[-p_rows] <- t_dims$heights[-p_rows]
  gt_new$heights[p_rows] <- unit(rep(x$layout$heights, lengths.out = dims[1]), 'null')
  gt_new
}
#' @importFrom gtable gtable_add_grob gtable_add_rows gtable_add_cols
#' @importFrom ggplot2 find_panel
#' @importFrom grid unit convertWidth convertHeight
simplify_gt <- function(gt) {
  gt <- add_strips(gt)
  panel_pos <- find_panel(gt)
  rows <- c(panel_pos$t, panel_pos$b)
  cols <- c(panel_pos$l, panel_pos$r)
  p_rows <- seq(rows[1], rows[2])
  p_cols <- seq(cols[1], cols[2])
  panels <- gt[p_rows, p_cols]
  gt_new <- gt[-p_rows, -p_cols]
  gt_new$widths <- convertWidth(gt$widths, 'mm')[-p_cols]
  gt_new$heights <- convertHeight(gt$heights, 'mm')[-p_rows]
  gt_new <- gtable_add_rows(gt_new, unit(1, 'null'), rows[1] - 1)
  gt_new <- gtable_add_cols(gt_new, unit(1, 'null'), cols[1] - 1)
  gt$grobs[gt$layout$name == 'background'] <- NULL
  gt$layout <- gt$layout[gt$layout$name != 'background', ]
  for (i in seq_len(nrow(gt))) {
    if (i >= rows[1]) {
      if (i <= rows[2]) next
      ii <- i - diff(rows)
    } else {
      ii <- i
    }
    table <- gt[i, p_cols]
    if (length(table$grobs) != 0) gt_new <- gtable_add_grob(gt_new, table, ii, cols[1], clip = 'off', name = paste(table$layout$name, collapse = ', '))
  }
  for (i in seq_len(ncol(gt))) {
    if (i >= cols[1]) {
      if (i <= cols[2]) next
      ii <- i - diff(cols)
    } else {
      ii <- i
    }
    table <- gt[p_rows, i]
    if (length(table$grobs) != 0) gt_new <- gtable_add_grob(gt_new, table, rows[1], ii, clip = 'off', name = paste(table$layout$name, collapse = ', '))
  }
  gtable::gtable_add_grob(gt_new, panels, rows[1], cols[1], clip = 'off', name = 'panels')
}

#' @importFrom grid convertHeight convertWidth unit unit.c
#' @importFrom stats na.omit
table_dims <- function(grobs, mat) {
  heights <- do.call(unit.c, lapply(seq_len(nrow(mat)), function(i) {
    ind <- na.omit(mat[i, ])
    heights <- lapply(grobs[ind], function(x) {
      convertHeight(x$heights, 'mm', TRUE)
    })
    unit(do.call(pmax, heights), 'mm')
  }))
  widths <- do.call(unit.c, lapply(seq_len(ncol(mat)), function(i) {
    ind <- na.omit(mat[, i])
    widths <- lapply(grobs[ind], function(x) {
      convertWidth(x$widths, 'mm', TRUE)
    })
    unit(do.call(pmax, widths), 'mm')
  }))
  list(heights = heights, widths = widths)
}
#' @importFrom gtable gtable_add_rows gtable_add_cols
#' @importFrom grid unit
#' @importFrom ggplot2 find_panel
add_strips <- function(gt) {
  panel_loc <- find_panel(gt)
  if (!any(grepl('strip-b', gt$layout$name))) {
    gt <- gtable_add_rows(gt, unit(0, 'mm'), panel_loc$b)
  }
  if (!any(grepl('strip-t', gt$layout$name))) {
    gt <- gtable_add_rows(gt, unit(0, 'mm'), panel_loc$t - 1)
  }
  if (!any(grepl('strip-r', gt$layout$name))) {
    gt <- gtable_add_cols(gt, unit(0, 'mm'), panel_loc$r)
  }
  if (!any(grepl('strip-l', gt$layout$name))) {
    gt <- gtable_add_cols(gt, unit(0, 'mm'), panel_loc$l - 1)
  }
  gt
}
