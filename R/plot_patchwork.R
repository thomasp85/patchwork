#' @importFrom grid grid.newpage grid.draw seekViewport pushViewport upViewport
#' @importFrom utils modifyList
#' @importFrom ggplot2 set_last_plot
#' @export
print.patchwork <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  if (newpage) grid.newpage()

  grDevices::recordGraphics(
    requireNamespace("patchwork", quietly = TRUE),
    list(),
    getNamespace("patchwork")
  )
  annotation <- modifyList(
    default_annotation,
    x$patches$annotation[!vapply(x$patches$annotation, is.null, logical(1))]
  )
  x <- recurse_tags(x, annotation$tag_levels, annotation$tag_prefix,
                    annotation$tag_suffix, annotation$tag_sep)$patches
  plot <- get_patchwork(x)
  gtable <- build_patchwork(plot, plot$layout$guides %||% 'auto')
  gtable <- resolve_background(gtable)
  gtable <- annotate_table(gtable, annotation)

  set_last_plot(x)

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
#' @export
plot.patchwork <- print.patchwork
#' @importFrom ggplot2 ggplot_build ggplot_gtable panel_rows panel_cols wrap_dims
#' @importFrom gtable gtable
#' @importFrom grid unit unit.pmax is.unit
#' @importFrom utils modifyList
#' @importFrom stats na.omit
build_patchwork <- function(x, guides = 'auto') {
  x$layout <- modifyList(default_layout, x$layout[!vapply(x$layout, is.null, logical(1))])

  guides <- if (guides == 'collect' && x$layout$guides != 'keep') {
    'collect'
  } else {
    x$layout$guides
  }
  gt <- lapply(x$plots, plot_table, guides = guides)
  guide_grobs <- unlist(lapply(gt, `[[`, 'collected_guides'), recursive = FALSE)
  gt <- lapply(gt, simplify_gt)
  if (!is.null(x$layout$design)) {
    if (is.null(x$layout$ncol)) x$layout$ncol <- max(x$layout$design$r)
    if (is.null(x$layout$nrow)) x$layout$nrow <- max(x$layout$design$b)
  }
  dims <- wrap_dims(length(x$plots), nrow = x$layout$nrow, ncol = x$layout$ncol)
  gt_new <- gtable(unit(rep(0, TABLE_COLS * dims[2]), 'null'),
                   unit(rep(0, TABLE_ROWS * dims[1]), 'null'))
  if (is.null(x$layout$design)) {
    x$layout$design <- create_design(dims[2], dims[1], x$layout$byrow)
  } else {
  }
  design <- as.data.frame(unclass(x$layout$design))
  if (nrow(design) < length(gt)) {
    warning('Too few patch areas to hold all plots. Dropping plots', call. = FALSE)
    gt <- gt[seq_len(nrow(design))]
  } else {
    design <- design[seq_along(gt), ]
  }
  if (any(design$t < 1)) design$t[design$t < 1] <- 1
  if (any(design$l < 1)) design$l[design$l < 1] <- 1
  if (any(design$b > dims[1])) design$b[design$b > dims[1]] <- dims[1]
  if (any(design$r > dims[2])) design$r[design$r > dims[2]] <- dims[2]
  gt_new$layout <- do.call(rbind, lapply(seq_along(gt), function(i) {
    loc <- design[i, ]
    lay <- gt[[i]]$layout
    lay$t <- lay$t + ifelse(lay$t <= PANEL_ROW, (loc$t - 1) * TABLE_ROWS, (loc$b - 1) * TABLE_ROWS)
    lay$l <- lay$l + ifelse(lay$l <= PANEL_COL, (loc$l - 1) * TABLE_COLS, (loc$r - 1) * TABLE_COLS)
    lay$b <- lay$b + ifelse(lay$b < PANEL_ROW, (loc$t - 1) * TABLE_ROWS, (loc$b - 1) * TABLE_ROWS)
    lay$r <- lay$r + ifelse(lay$r < PANEL_COL, (loc$l - 1) * TABLE_COLS, (loc$r - 1) * TABLE_COLS)
    lay
  }))
  table_dimensions <- table_dims(
    lapply(gt, `[[`, 'widths'),
    lapply(gt, `[[`, 'heights'),
    design
  )
  gt_new$grobs <- unlist(lapply(gt, `[[`, 'grobs'), recursive = FALSE)
  gt_new$widths <- table_dimensions$widths
  if (!is.unit(x$layout$widths)) x$layout$widths <- unit(x$layout$widths, 'null')
  gt_new$widths[seq(PANEL_COL, by = TABLE_COLS, length.out = dims[2])] <- rep(x$layout$widths, length.out = dims[2])
  gt_new$heights <- table_dimensions$heights
  if (!is.unit(x$layout$heights)) x$layout$heights <- unit(x$layout$heights, 'null')
  gt_new$heights[seq(PANEL_ROW, by = TABLE_ROWS, length.out = dims[1])] <- rep(x$layout$widths, length.out = dims[1])

  if (x$layout$guides == 'collect') {
    guide_grobs <- collapse_guides(guide_grobs)
    theme <- x$annotation$theme
    if (!attr(theme, 'complete')) {
      theme <- theme_get() + theme
    }
    guide_grobs <- assemble_guides(guide_grobs, theme)
    gt_new <- attach_guides(gt_new, guide_grobs, theme)
  } else {
    gt_new$collected_guides <- guide_grobs
  }

  class(gt_new) <- c('gtable_patchwork', class(gt_new))
  gt_new
}
#' Convert a patchwork to a gtable
#'
#' This function is the patchwork analogue of [ggplot2::ggplotGrob()] in that it
#' takes an unevaluated patchwork object and fixate it into a gtable object to
#' further manipulate directly.
#'
#' @param x A `patchwork` object
#'
#' @return A `gtable` object
#'
#' @keywords internal
#' @importFrom utils modifyList
#' @export
#'
patchworkGrob <- function(x) {
  annotation <- modifyList(
    default_annotation,
    x$patches$annotation[!vapply(x$patches$annotation, is.null, logical(1))]
  )
  x <- recurse_tags(x, annotation$tag_levels, annotation$tag_prefix,
                    annotation$tag_suffix, annotation$tag_sep)$patches
  plot <- get_patchwork(x)
  gtable <- build_patchwork(plot)
  annotate_table(gtable, annotation)
}
plot_table <- function(x, guides) {
  UseMethod('plot_table')
}
#' @importFrom ggplot2 ggplotGrob
#' @export
plot_table.ggplot <- function(x, guides) {
  gt <- ggplotGrob(x)
  gt <- add_strips(gt)
  add_guides(gt, guides == 'collect')
}
#' @export
plot_table.patchwork <- function(x, guides) {
  build_patchwork(get_patchwork(x), guides)
}
#' @export
plot_table.patch <- function(x, guides) {
  patchGrob(x, guides)
}
simplify_gt <- function(gt) {
  UseMethod('simplify_gt')
}
#' @importFrom gtable gtable_add_grob gtable_add_rows gtable_add_cols gtable_col gtable_row
#' @importFrom ggplot2 find_panel
#' @importFrom grid unit convertWidth convertHeight grobWidth grobHeight
#' @export
simplify_gt.gtable <- function(gt) {
  gt <- simplify_base(gt)
  id <- sample.int(1e10, size = 1)
  gt <- add_marker_rects(gt, id)
  gt$layout[gt$layout$name == 'background', c('t', 'l','b', 'r')] <- 1
  gt$layout$name[gt$layout$name == 'background'] <- paste0('background_', id)
  gt
}
#' @export
simplify_gt.gtable_patchwork <- function(gt) {
  simplify_base(gt)
}
#' @export
simplify_gt.patchgrob <- function(gt) gt

simplify_base <- function(gt) {
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
  # gt$grobs[gt$layout$name == 'background'] <- NULL
  # gt$layout <- gt$layout[gt$layout$name != 'background', ]
  gt <- if (gt$respect) {
    simplify_fixed(gt, gt_new, panels, rows, cols)
  } else {
    simplify_free(gt, gt_new, panels, rows, cols)
  }
  # keep <- gt$layout$name != 'background'
  # gt$grobs <- gt$grobs[keep]
  # gt$layout <- gt$layout[keep, , drop = FALSE]
  gt
}
#' @importFrom gtable gtable_add_grob
#' @importFrom grid viewport
simplify_free <- function(gt, gt_new, panels, rows, cols) {
  p_rows <- seq(rows[1], rows[2])
  p_cols <- seq(cols[1], cols[2])
  for (i in seq_len(nrow(gt))) {
    if (i >= rows[1]) {
      if (i <= rows[2]) next
      ii <- i - diff(rows)
      pos <- 'bottom'
    } else {
      ii <- i
      pos <- 'top'
    }
    table <- gt[i, p_cols]
    if (length(table$grobs) != 0) {
      grobname <- paste(table$layout$name, collapse = ', ')
      if (pos == 'top') {
        table$vp <- viewport(y = 0, just = 'bottom', height = table$heights)
      } else {
        table$vp <- viewport(y = 1, just = 'top', height = table$heights)
      }
      gt_new <- gtable_add_grob(gt_new, table, ii, cols[1], clip = 'off', name = grobname, z = max(table$layout$z))
    }
  }
  for (i in seq_len(ncol(gt))) {
    if (i >= cols[1]) {
      if (i <= cols[2]) next
      ii <- i - diff(cols)
      pos <- 'right'
    } else {
      ii <- i
      pos <- 'left'
    }
    table <- gt[p_rows, i]
    if (length(table$grobs) != 0) {
      grobname <- paste(table$layout$name, collapse = ', ')
      if (pos == 'left') {
        table$vp <- viewport(x = 1, just = 'right', width = table$widths)
      } else {
        table$vp <- viewport(x = 0, just = 'left', width = table$widths)
      }
      gt_new <- gtable_add_grob(gt_new, table, rows[1], ii, clip = 'off', name = grobname, z = max(table$layout$z))
    }
  }
  panel_name <- paste0('panel; ', paste(panels$layout$name, collapse = ', '))
  gtable_add_grob(gt_new, panels, rows[1], cols[1], clip = 'off', name = panel_name, z = 1)
}
#' @importFrom grid viewport unit convertWidth convertHeight
#' @importFrom gtable gtable_add_grob
simplify_fixed <- function(gt, gt_new, panels, rows, cols) {
  p_rows <- seq(rows[1], rows[2])
  p_cols <- seq(cols[1], cols[2])
  left <- gt$layout$l[grep('-l(-|$)', gt$layout$name)]
  right <- gt$layout$r[grep('-r(-|$)', gt$layout$name)]
  top <- gt$layout$t[grep('-t(-|$)', gt$layout$name)]
  bottom <- gt$layout$b[grep('-b(-|$)', gt$layout$name)]
  # Add strips, axes and labels to panel grob
  if (length(left) != 0 && min(left) < cols[1]) {
    left_grob <- gt[p_rows, seq(min(left), cols[1] - 1)]
    h_width <- unit(sum(convertWidth(left_grob$widths, 'mm', TRUE))/2, 'mm')
    left_grob$vp <- viewport(x = unit(0, 'npc') - h_width)
    panels <- gtable_add_grob(panels,  grobs = list(left_grob),
                              t = 1, l = 1, b = nrow(panels), r = ncol(panels),
                              z = Inf, clip = 'off', name = 'left-l')
  }
  if (length(right) != 0 && max(right) > cols[2]) {
    right_grob <- gt[p_rows, seq(cols[2] + 1, max(right))]
    h_width <- unit(sum(convertWidth(right_grob$widths, 'mm', TRUE))/2, 'mm')
    right_grob$vp <- viewport(x = unit(1, 'npc') + h_width)
    panels <- gtable_add_grob(panels,  grobs = list(right_grob),
                              t = 1, l = 1, b = nrow(panels), r = ncol(panels),
                              z = Inf, clip = 'off', name = 'right-r')
  }
  if (length(top) != 0 && min(top) < rows[1]) {
    top_grob <- gt[seq(min(top), rows[1] - 1), p_cols]
    h_height <- unit(sum(convertHeight(top_grob$heights, 'mm', TRUE))/2, 'mm')
    top_grob$vp <- viewport(y = unit(1, 'npc') + h_height)
    panels <- gtable_add_grob(panels,  grobs = list(top_grob),
                              t = 1, l = 1, b = nrow(panels), r = ncol(panels),
                              z = Inf, clip = 'off', name = 'top-t')
  }
  if (length(bottom) != 0 && max(bottom) > rows[2]) {
    bottom_grob <- gt[seq(rows[2] + 1, max(bottom)), p_cols]
    h_height <- unit(sum(convertHeight(bottom_grob$heights, 'mm', TRUE))/2, 'mm')
    bottom_grob$vp <- viewport(y = unit(0, 'npc') - h_height)
    panels <- gtable_add_grob(panels,  grobs = list(bottom_grob),
                              t = 1, l = 1, b = nrow(panels), r = ncol(panels),
                              z = Inf, clip = 'off', name = 'bottom-b')
  }
  # Add remaining grobs to gt_new
  left <- if (length(left) != 0) min(left) else cols[1]
  for (i in seq_len(left - 1)) {
    table <- gt[p_rows, i]
    if (length(table$grobs) != 0) {
      grobname <- paste(table$layout$name, collapse = ', ')
      gt_new <- gtable_add_grob(gt_new, table, rows[1], i, clip = 'off', name = grobname, z = max(table$layout$z))
    }
  }
  right <- if (length(right) != 0) max(right) else cols[2]
  for (i in seq_len(ncol(gt) - right)) {
    table <- gt[p_rows, i + right]
    if (length(table$grobs) != 0) {
      grobname <- paste(table$layout$name, collapse = ', ')
      gt_new <- gtable_add_grob(gt_new, table, rows[1], i + cols[1] + right - cols[2], clip = 'off', name = grobname, z = max(table$layout$z))
    }
  }
  top <- if (length(top) != 0) min(top) else rows[1]
  for (i in seq_len(top - 1)) {
    table <- gt[i, p_cols]
    if (length(table$grobs) != 0) {
      grobname <- paste(table$layout$name, collapse = ', ')
      gt_new <- gtable_add_grob(gt_new, table, i, cols[1], clip = 'off', name = grobname, z = max(table$layout$z))
    }
  }
  bottom <- if (length(bottom) != 0) max(bottom) else rows[2]
  for (i in seq_len(nrow(gt) - bottom)) {
    table <- gt[i + bottom, p_cols]
    if (length(table$grobs) != 0) {
      grobname <- paste(table$layout$name, collapse = ', ')
      gt_new <- gtable_add_grob(gt_new, table, i + rows[1] + bottom - rows[2], cols[1], clip = 'off', name = grobname, z = max(table$layout$z))
    }
  }
  panel_name <- paste0('panel; ', paste(panels$layout$name, collapse = ', '))
  gtable_add_grob(gt_new, panels, rows[1], cols[1], clip = 'off', name = panel_name, z = 1)
}
#' @importFrom gtable gtable_add_grob
#' @importFrom grid rectGrob gpar
#' @importFrom ggplot2 find_panel
add_marker_rects <- function(gt, id) {
  panel_pos <- find_panel(gt)
  gt <- gtable_add_grob(gt, rectGrob(gp = gpar(fill = NA, col = NA)),
                        t = 1, l = panel_pos$l, b = 1,
                        r = panel_pos$r, z = 0, name = paste0('tm_marker_', id))
  gt <- gtable_add_grob(gt, rectGrob(gp = gpar(fill = NA, col = NA)),
                        t = nrow(gt), l = panel_pos$l, b = nrow(gt),
                        r = panel_pos$r, z = 0, name = paste0('bm_marker_', id))
  gt <- gtable_add_grob(gt, rectGrob(gp = gpar(fill = NA, col = NA)),
                        t = panel_pos$t, l = 1, b = panel_pos$b,
                        r = 1, z = 0, name = paste0('lm_marker_', id))
  gt <- gtable_add_grob(gt, rectGrob(gp = gpar(fill = NA, col = NA)),
                        t = panel_pos$t, l = ncol(gt), b = panel_pos$b,
                        r = ncol(gt), z = 0, name = paste0('rm_marker_', id))
  gt <- gtable_add_grob(gt, rectGrob(gp = gpar(fill = NA, col = NA)),
                        t = 2, l = panel_pos$l, b = panel_pos$t - 1,
                        r = panel_pos$r, z = 0, name = paste0('t_marker_', id))
  gt <- gtable_add_grob(gt, rectGrob(gp = gpar(fill = NA, col = NA)),
                        t = panel_pos$b + 1, l = panel_pos$l, b = nrow(gt) - 1,
                        r = panel_pos$r, z = 0, name = paste0('b_marker_', id))
  gt <- gtable_add_grob(gt, rectGrob(gp = gpar(fill = NA, col = NA)),
                        t = panel_pos$t, l = 2, b = panel_pos$b,
                        r = panel_pos$l - 1, z = 0, name = paste0('l_marker_', id))
  gt <- gtable_add_grob(gt, rectGrob(gp = gpar(fill = NA, col = NA)),
                        t = panel_pos$t, l = panel_pos$r + 1, b = panel_pos$b,
                        r = ncol(gt) - 1, z = 0, name = paste0('r_marker_', id))
  gt <- gtable_add_grob(gt, rectGrob(gp = gpar(fill = NA, col = NA)),
                        t = panel_pos$t, l = panel_pos$l, b = panel_pos$b,
                        r = panel_pos$r, z = 0, name = paste0('p_marker_', id))
  gt
}
get_background_table <- function(gt) {
  ncols <- round(ncol(gt) / TABLE_COLS)
  nrows <- round(nrow(gt) / TABLE_ROWS)
  col <- if (ncols == 1) rep(1, length(gt)) else cut(gt$layout$l, ncols)
  row <- if (nrows == 1) rep(1, length(gt)) else cut(gt$layout$t, nrows)
  rows <- lapply(split(seq_along(gt), row), function(i) {
    cols <- lapply(split(i, col[i]), function(ii) {
      ii_m <- ii[grep('_marker$', gt$layout$name[ii])]
      heights <- lapply(gt$grobs[ii_m[grep('^[tbp]', gt$layout$name[ii_m])]], grobHeight)
      height <- do.call(sum, heights)
      widths <- lapply(gt$grobs[ii_m[grep('^[lrp]', gt$layout$name[ii_m])]], grobWidth)
      width <- do.call(sum, widths)
      bg <- gt$grobs[[ii[grep('^background$', gt$layout$name[ii])]]]
      bg$width <- width
      bg$height <- height
      gtable_add_grob(gtable(width, height), bg, 1, 1)
    })
    if (length(cols) < ncols) {
      cols <- c(cols, rep(list(gtable(unit(0, 'mm'), unit(0, 'mm'))), ncols - length(cols)))
    }
    do.call(cbind, c(cols, list(size = 'first')))
  })
  do.call(rbind, c(rows, list(size = 'first')))
}
create_design <- function(width, height, byrow) {
  mat <- matrix(seq_len(width * height), nrow = height, ncol = width, byrow = byrow)
  ind <- as.vector(mat)
  area(
    t = row(mat)[ind],
    l = col(mat)[ind]
  )
}
#' @importFrom grid convertHeight convertWidth unit
table_dims <- function(widths, heights, areas) {
  widths <- lapply(widths, convertWidth, 'mm', valueOnly = TRUE)
  widths <- vapply(seq_len(max(areas$r) * TABLE_COLS), function(i) {
    area <- (i - 1) %/% TABLE_COLS + 1
    col_loc <- i %% TABLE_COLS
    if (col_loc == 0) col_loc <- TABLE_COLS
    area_side <- if (col_loc <= PANEL_COL) 'l' else 'r'
    tables <- which(areas[[area_side]] == area)
    if (length(tables) == 0) {
      0
    } else {
      max(vapply(widths[tables], `[[`, numeric(1), col_loc))
    }
  }, numeric(1))
  heights <- lapply(heights, convertHeight, 'mm', valueOnly = TRUE)
  heights <- vapply(seq_len(max(areas$b) * TABLE_ROWS), function(i) {
    area <- (i - 1) %/% TABLE_ROWS + 1
    row_loc <- i %% TABLE_ROWS
    if (row_loc == 0) row_loc <- TABLE_ROWS
    area_side <- if (row_loc <= PANEL_ROW) 't' else 'b'
    tables <- which(areas[[area_side]] == area)
    if (length(tables) == 0) {
      0
    } else {
      max(vapply(heights[tables], `[[`, numeric(1), row_loc))
    }
  }, numeric(1))
  list(widths = unit(widths, 'mm'), heights = unit(heights, 'mm'))
}
#' @importFrom gtable gtable_add_rows gtable_add_cols
#' @importFrom grid unit
#' @importFrom ggplot2 find_panel
add_strips <- function(gt) {
  panel_loc <- find_panel(gt)
  strip_pos <- switch(
    find_strip_pos(gt),
    inside = 0,
    outside = 2
  )
  if (!any(grepl('strip-b', gt$layout$name))) {
    gt <- gtable_add_rows(gt, unit(0, 'mm'), panel_loc$b + strip_pos)
  }
  if (!any(grepl('strip-t', gt$layout$name))) {
    gt <- gtable_add_rows(gt, unit(0, 'mm'), panel_loc$t - 1 - strip_pos)
  }
  if (!any(grepl('strip-r', gt$layout$name))) {
    gt <- gtable_add_cols(gt, unit(0, 'mm'), panel_loc$r + strip_pos)
  }
  if (!any(grepl('strip-l', gt$layout$name))) {
    gt <- gtable_add_cols(gt, unit(0, 'mm'), panel_loc$l - 1 - strip_pos)
  }
  gt
}
#' @importFrom gtable gtable_add_rows gtable_add_cols
#' @importFrom grid unit
add_guides <- function(gt, collect = FALSE) {
  panel_loc <- find_panel(gt)
  guide_ind <- which(gt$layout$name == 'guide-box')
  guide_loc <- gt$layout[guide_ind, ]
  guide_pos <- if (nrow(guide_loc) == 0) {
    'none'
  } else if (all(unlist(guide_loc[, c('t', 'l', 'b', 'r')] == panel_loc))) {
    'inside'
  } else {
    if (panel_loc$t == guide_loc$t) {
      if (panel_loc$l > guide_loc$l) {
        'left'
      } else {
        'right'
      }
    } else {
      if (panel_loc$t > guide_loc$t) {
        'top'
      } else {
        'bottom'
      }
    }
  }
  if (guide_pos != 'right') {
    gt <- gtable_add_cols(gt, unit(c(0, 0), 'mm'), panel_loc$r + 3)
  }
  if (guide_pos != 'left') {
    gt <- gtable_add_cols(gt, unit(c(0, 0), 'mm'), panel_loc$l - 4)
  }
  if (guide_pos != 'bottom') {
    gt <- gtable_add_rows(gt, unit(c(0, 0), 'mm'), panel_loc$b + 5)
  }
  if (guide_pos != 'top') {
    gt <- gtable_add_rows(gt, unit(c(0, 0), 'mm'), panel_loc$t - 4)
  }
  if (collect && guide_pos != 'none') {
    guide_grob <- gt$grobs[[guide_ind]]
    guide_loc <- gt$layout[guide_ind, ] # May have changed above
    space_pos <- if (guide_pos %in% c('left', 'top')) 1 else -1
    if (guide_pos %in% c('right', 'left')) {
      gt$widths[c(guide_loc$l, guide_loc$l + space_pos)] <- unit(c(0, 0), 'mm')
    } else if (guide_pos %in% c('bottom', 'top')) {
      gt$widths[c(guide_loc$t, guide_loc$t + space_pos)] <- unit(c(0, 0), 'mm')
    }
    gt$grobs[guide_ind] <- NULL
    gt$layout <- gt$layout[-guide_ind, ]
    gt$collected_guides <- guide_grob$grobs[guide_grob$layout$name == 'guides']
  }
  gt
}
find_strip_pos <- function(gt) {
  panel_loc <- find_panel(gt)
  ind <- grep('strip-t', gt$layout$name)
  if (length(ind) != 0 && panel_loc$t - min(gt$layout$t[ind]) != 1) {
    return('outside')
  }
  ind <- grep('strip-r', gt$layout$name)
  if (length(ind) != 0 && max(gt$layout$r[ind]) - panel_loc$r != 1) {
    return('outside')
  }
  ind <- grep('strip-b', gt$layout$name)
  if (length(ind) != 0 &&  max(gt$layout$b[ind]) - panel_loc$b != 1) {
    return('outside')
  }
  ind <- grep('strip-l', gt$layout$name)
  if (length(ind) != 0 && panel_loc$l - min(gt$layout$l[ind]) != 1) {
    return('outside')
  }
  'inside'
}
resolve_background <- function(gt) {
  gt
}
#' @importFrom gtable is.gtable
find_markers <- function(gt, id) {
  ind <- grep(paste0('marker_', id), gt$layout$name)
  recurse <- vapply(gt$grobs[ind], is.gtable, logical(1))
  markers <- gt$grobs[ind[!recurse]]
  names(markers) <- gt$layout$name[ind[!recurse]]
  for (i in ind[recurse]) {
    markers <- c(markers, find_markers(gt$grobs[[i]], id))
  }
  markers
}
