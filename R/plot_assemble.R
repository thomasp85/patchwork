#' @importFrom grid grid.newpage grid.draw seekViewport pushViewport upViewport
#' @importFrom utils modifyList
#' @importFrom ggplot2 set_last_plot
#' @export
print.ggassemble <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  if (newpage) grid.newpage()

  grDevices::recordGraphics(
    requireNamespace("patchwork", quietly = TRUE),
    list(),
    getNamespace("patchwork")
  )
  annotation <- modifyList(
    default_annotation,
    x$assemble$annotation[!vapply(x$assemble$annotation, is.null, logical(1))]
  )
  x <- recurse_tags(x, annotation$tag_levels, annotation$tag_prefix,
                    annotation$tag_suffix, annotation$tag_sep)$assemble
  assemble <- get_assemble(x)
  gtable <- build_assemble(assemble)
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
plot.ggassemble <- print.ggassemble
#' @importFrom ggplot2 ggplot_build ggplot_gtable panel_rows panel_cols wrap_dims
#' @importFrom gtable gtable_add_cols
#' @importFrom grid unit
#' @importFrom utils modifyList
#' @importFrom stats na.omit
build_assemble <- function(x, guides = 'auto') {
  x$layout <- modifyList(default_layout, x$layout[!vapply(x$layout, is.null, logical(1))])
  guides <- if (guides == 'collect' && x$layout$guides != 'keep') {
    'collect'
  } else {
    x$layout$guides
  }
  gt <- lapply(x$plots, plot_table, guides = guides)
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
  gt_new$respect <- FALSE
  gt_new
}
#' Convert a patchwork assemble to a gtable
#'
#' This function is the patchwork analogue of [ggplot2::ggplotGrob()] in that it
#' takes an unevaluated patchwork plot object (a `ggassemble`) and fixate it
#' into a gtable object to further manipulate directly.
#'
#' @param x A `ggassemble` object
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
    x$assemble$annotation[!vapply(x$assemble$annotation, is.null, logical(1))]
  )
  x <- recurse_tags(x, annotation$tag_levels, annotation$tag_prefix,
                    annotation$tag_suffix, annotation$tag_sep)$assemble
  assemble <- get_assemble(x)
  gtable <- build_assemble(assemble)
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
  add_guides(gt)
}
#' @export
plot_table.ggassemble <- function(x, guides) {
  build_assemble(get_assemble(x))
}
#' @export
plot_table.assemble_cell <- function(x, guides) {
  cellGrob(x)
}
#' @importFrom gtable gtable_add_grob gtable_add_rows gtable_add_cols gtable_col gtable_row
#' @importFrom ggplot2 find_panel
#' @importFrom grid unit convertWidth convertHeight grobWidth grobHeight
simplify_gt <- function(gt) {
  if (is.cellgrob(gt)) return(gt)
  fixed_asp <- gt$respect
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
  gt <- if (gt$respect) {
    simplify_fixed(gt, gt_new, panels, rows, cols)
  } else {
    simplify_free(gt, gt_new, panels, rows, cols)
  }
  keep <- gt$layout$name != 'background'
  gt$grobs <- gt$grobs[keep]
  gt$layout <- gt$layout[keep, , drop = FALSE]
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
  gtable_add_grob(gt_new, panels, rows[1], cols[1], clip = 'off', name = 'panels', z = 1)
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
  gtable_add_grob(gt_new, panels, rows[1], cols[1], clip = 'off', name = 'panels', z = 1)
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
add_guides <- function(gt) {
  panel_loc <- find_panel(gt)
  guide_loc <- gt$layout[gt$layout$name == 'guide-box', ]
  guide_pos <- if (nrow(guide_loc) == 0) {
    'none'
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
