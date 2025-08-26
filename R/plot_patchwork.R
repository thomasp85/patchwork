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
  plot <- get_patches(x)
  gtable <- build_patchwork(plot, plot$layout$guides %||% 'auto')
  gtable <- annotate_table(gtable, annotation)

  set_last_plot(x)

  if (!is.null(vp)) {
    if (is.character(vp)) {
      seekViewport(vp)
    } else {
      pushViewport(vp)
    }
  }

  tryCatch(
    grid.draw(gtable),
    error = function(e) {
      if (inherits(e, 'simpleError') && deparse(conditionCall(e)[[1]]) == 'grid.Call') {
        if (Sys.getenv("RSTUDIO") == "1") {
          cli_abort(c("The RStudio {.field Plots} window may be too small to show this patchwork.",
                    i = "Please make the window larger.")
          )
        } else {
          cli_abort(c("The viewport may be too small to show this patchwork.",
                      i = "Please make the window larger.")
          )
        }
      }
    }
  )

  if (!is.null(vp)) {
    upViewport()
  }

  invisible(x)
}
#' @export
plot.patchwork <- print.patchwork
#' @export
length.patchwork <- function(x) {
  length(x$patches$plots) + !is_empty(x)
}
#' @export
names.patchwork <- function(x) NULL
#' @export
`[[.patchwork` <- function(x, ..., exact = TRUE) {
  ind <- ..1
  if (!is.numeric(ind)) {
    cli_abort('Patchworks can only be indexed with numeric indices')
  }

  n_patches <- length(x$patches$plots)
  if (!is_empty(x) && ind[1] == n_patches + 1) {
    plot <- x
    plot$patches <- NULL
    class(plot) <- setdiff(class(plot), 'patchwork')
  } else {
    if (ind > n_patches) {
      cli_abort('Index out of bounds')
    }
    plot <- x$patches$plots[[ind[1]]]
  }
  if (length(ind) > 1) {
    if (!is_patchwork(plot)) {
      cli_abort('Can only do nested indexing into patchworks')
    }
    plot <- plot[[ind[-1]]]
  }
  plot
}
#' @export
`[[<-.patchwork` <- function(x, ..., value) {
  ind <- ..1
  if (!is.numeric(ind)) {
    cli_abort('Patchworks can only be indexed with numeric indices')
  }

  if (!is_ggplot(value)) {
    value <- wrap_elements(value)
  }
  n_patches <- length(x$patches$plots)
  if (!is_empty(x) && ind == n_patches + 1) {
    if (length(ind) != 1) {
      cli_abort('Can only do nested indexing into patchworks')
    }
    return(add_patches(value, x$patches))
  }
  if (length(ind) > 1) {
    if (!is_patchwork(x$patches$plots[[ind[1]]])) {
      cli_abort('Can only do nested indexing into patchworks')
    }
    x$patches$plots[[ind[1]]][[ind[-1]]] <- value
  } else {
    x$patches$plots[[ind]] <- value
  }
  x
}
#' @export
as.list.patchwork <- function(x, ...) {
  get_patches(x)$plots
}
#' @importFrom utils str
#' @export
str.patchwork <- function(object, ...) {
  n_patches <- length(object$patches$plots)
  if (!is_empty(object)) n_patches <- n_patches + 1
  cat('A patchwork composed of ', n_patches, ' patches\n', sep = '')
  cat('- Autotagging is turned ', if (is.null(object$patches$annotation$tag_levels)) 'off' else 'on', '\n', sep = '')
  cat('- Guides are ', if (isTRUE(object$patches$layout$guides == 'collect')) 'collected' else 'kept', '\n', sep = '')
  cat('\n')
  cat('Layout:\n')
  if (is.null(object$layout$design)) {
    l <- object$layout
    if (is.null(l$ncol) && !is.null(l$widths) && length(l$widths) > 1) {
      l$ncol <- length(l$widths)
    }
    if (is.null(l$nrow) && !is.null(l$heights) && length(l$heights) > 1) {
      l$nrow <- length(l$heights)
    }
    dims <- wrap_dims(n_patches, nrow = l$nrow, ncol = l$ncol)
    print(create_design(dims[2], dims[1], isTRUE(l$byrow)))
  } else {
    print(object$layout$design)
  }
}
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
  gt <- add_insets(gt)
  fixed_asp <- vapply(gt, function(x) isTRUE(x$respect), logical(1))
  if (is.null(x$layout$design)) {
    if (is.null(x$layout$ncol) && !is.null(x$layout$widths) && length(x$layout$widths) > 1) {
      x$layout$ncol <- length(x$layout$widths)
    }
    if (is.null(x$layout$nrow) && !is.null(x$layout$heights) && length(x$layout$heights) > 1) {
      x$layout$nrow <- length(x$layout$heights)
    }
    dims <- wrap_dims(length(gt), nrow = x$layout$nrow, ncol = x$layout$ncol)
    x$layout$design <- create_design(dims[2], dims[1], isTRUE(x$layout$byrow))
  } else {
    dims <- c(
      max(x$layout$design$b),
      max(x$layout$design$r)
    )
  }
  gt_new <- gtable(unit(rep(0, TABLE_COLS * dims[2]), 'null'),
                   unit(rep(0, TABLE_ROWS * dims[1]), 'null'))
  design <- as.data.frame(unclass(x$layout$design))
  if (nrow(design) < length(gt)) {
    warning('Too few patch areas to hold all plots. Dropping plots', call. = FALSE)
    gt <- gt[seq_len(nrow(design))]
    fixed_asp <- fixed_asp[seq_len(nrow(design))]
  } else {
    design <- design[seq_along(gt), ]
  }
  if (any(design$t < 1)) design$t[design$t < 1] <- 1
  if (any(design$l < 1)) design$l[design$l < 1] <- 1
  if (any(design$b > dims[1])) design$b[design$b > dims[1]] <- dims[1]
  if (any(design$r > dims[2])) design$r[design$r > dims[2]] <- dims[2]
  max_z <- lapply(gt, function(x) max(x$layout$z))
  max_z <- c(0, cumsum(max_z))
  gt_new$layout <- exec(rbind, !!!lapply(seq_along(gt), function(i) {
    loc <- design[i, ]
    lay <- gt[[i]]$layout
    lay$z <- lay$z + ifelse(lay$name == "background", 0, max_z[i])
    lay$t <- lay$t + ifelse(lay$t <= PANEL_ROW, (loc$t - 1) * TABLE_ROWS, (loc$b - 1) * TABLE_ROWS)
    lay$l <- lay$l + ifelse(lay$l <= PANEL_COL, (loc$l - 1) * TABLE_COLS, (loc$r - 1) * TABLE_COLS)
    lay$b <- lay$b + ifelse(lay$b < PANEL_ROW, (loc$t - 1) * TABLE_ROWS, (loc$b - 1) * TABLE_ROWS)
    lay$r <- lay$r + ifelse(lay$r < PANEL_COL, (loc$l - 1) * TABLE_COLS, (loc$r - 1) * TABLE_COLS)
    lay$name <- paste0(lay$name, '-', i)
    lay
  }))
  table_dimensions <- table_dims(
    lapply(gt, `[[`, 'widths'),
    lapply(gt, `[[`, 'heights'),
    design,
    dims[2],
    dims[1]
  )
  gt_new$grobs <- set_grob_sizes(gt, table_dimensions$widths, table_dimensions$heights, design)
  gt_new$widths <- table_dimensions$widths
  gt_new$heights <- table_dimensions$heights
  widths <- rep(x$layout$widths, length.out = dims[2])
  heights <- rep(x$layout$heights, length.out = dims[1])
  gt_new <- set_panel_dimensions(gt_new, gt, widths, heights, fixed_asp, design)
  if (x$layout$guides == 'collect') {
    guide_grobs <- collapse_guides(guide_grobs)
    if (length(guide_grobs) != 0) {
      theme <- x$annotation$theme
      if (!attr(theme, 'complete')) {
        theme <- theme_get() + theme
      }
      position <- theme$legend.position %||% "right"
      if (length(position) == 2) {
        warning("Manual legend position not possible for collected guides. Defaulting to 'right'", call. = FALSE)
        position <- "right"
      }
      guide_grobs <- assemble_guides(guide_grobs, position, theme)
      gt_new <- attach_guides(gt_new, guide_grobs, position, theme)
    }
  } else {
    gt_new$collected_guides <- guide_grobs
  }

  axes <- x$layout$axes %||% default_layout$axes
  if (axes %in% c('collect', 'collect_x')) {
    gt_new <- collect_axes(gt_new, "x")
  }
  if (axes %in% c('collect', 'collect_y')) {
    gt_new <- collect_axes(gt_new, "y")
  }

  titles <- x$layout$axis_titles %||% default_layout$axis_titles
  if (titles %in% c('collect', 'collect_x')) {
    gt_new <- collect_axis_titles(gt_new, "x", merge = TRUE)
  }
  if (titles %in% c('collect', 'collect_y')) {
    gt_new <- collect_axis_titles(gt_new, "y", merge = TRUE)
  }

  gt_new <- gtable_add_grob(
    gt_new, zeroGrob(),
    t = PANEL_ROW,
    l = PANEL_COL,
    b = PANEL_ROW + TABLE_ROWS * (dims[1] - 1),
    r = PANEL_COL + TABLE_COLS * (dims[2] - 1),
    z = -1,
    name = "panel-area"
  )

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
  plot <- get_patches(x)
  gtable <- build_patchwork(plot)
  gtable <- annotate_table(gtable, annotation)
  class(gtable) <- setdiff(class(gtable), 'gtable_patchwork')
  gtable
}

#' @importFrom gtable as.gtable
#' @export
as.gtable.patchwork <- function(x, ...) patchworkGrob(x)

plot_table <- function(x, guides) {
  UseMethod('plot_table')
}
#' @importFrom ggplot2 ggplotGrob
#' @export
plot_table.ggplot <- function(x, guides) {
  data <- ggplot_build(x)
  gt <- ggplot_gtable(data)
  # use the completed theme when adding strips
  gt <- add_strips(gt, data$plot$theme)
  add_guides(gt, guides == 'collect')
}
#' @export
plot_table.patchwork <- function(x, guides) {
  if (is_free_plot(x)) {
    plot_table.free_plot(x, guides)
  } else {
    build_patchwork(get_patches(x), guides)
  }
}
#' @export
plot_table.patch <- function(x, guides) {
  patchGrob(x, guides)
}
#' @export
plot_table.inset_patch <- function(x, guides) {
  settings <- attr(x, 'inset_settings')
  class(x) <- setdiff(class(x), 'inset_patch')
  table <- plot_table(x, guides)
  table$vp <- viewport(x = settings$left, y = settings$bottom,
                       width = settings$right - settings$left,
                       height = settings$top - settings$bottom,
                       just = c(0, 0))
  attr(table, 'inset_settings') <- settings
  class(table) <- c('inset_table', class(table))
  table
}
#' @export
plot_table.free_plot <- function(x, guides) {
  if (is_patchwork(x)) {
    settings <- attr(x, 'patchwork_free_settings')
    # We do this directly because the last plot in the patchwork might be free
    # so we don't want to remove the free class and dispatch
    table <- build_patchwork(get_patches(x), guides)
  } else {
    settings <- attr(x, 'free_settings')
    class(x) <- setdiff(class(x), 'free_plot')
    table <- plot_table(x, guides)
  }
  attr(table, 'free_settings') <- settings
  class(table) <- c('free_table', class(table))
  table
}
simplify_gt <- function(gt) {
  UseMethod('simplify_gt')
}
#' @importFrom gtable gtable_add_grob gtable_add_rows gtable_add_cols
#' @importFrom ggplot2 find_panel
#' @importFrom grid unit convertWidth convertHeight
#' @export
simplify_gt.gtable <- function(gt) {
  guides <- gt$collected_guides
  gt$collected_guides <- NULL
  panel_pos <- find_panel(gt)
  rows <- c(panel_pos$t, panel_pos$b)
  cols <- c(panel_pos$l, panel_pos$r)
  if (!gt$respect && rows[1] == rows[2] && cols[1] == cols[2] && !any(grepl('^strip-', gt$layout$name))) {
    gt$widths <- convertWidth(gt$widths, 'mm')
    gt$heights <- convertHeight(gt$heights, 'mm')
    return(gt)
  }
  p_rows <- seq(rows[1], rows[2])
  p_cols <- seq(cols[1], cols[2])
  panels <- gt[p_rows, p_cols]
  gt_new <- gt[-p_rows, -p_cols]
  gt_new$widths <- convertWidth(gt$widths[-p_cols], 'mm')
  if (all(is_abs_unit(gt$widths[p_cols]))) {
    new_width <- sum(convertWidth(gt$widths[p_cols], 'mm'))
  } else {
    new_width <- unit(1, 'null')
  }
  gt_new$heights <- convertHeight(gt$heights[-p_rows], 'mm')
  if (all(is_abs_unit(gt$heights[p_rows]))) {
    new_height <- sum(convertHeight(gt$heights[p_rows], 'mm'))
  } else {
    new_height <- unit(1, 'null')
  }
  gt_new <- gtable_add_rows(gt_new, new_height, rows[1] - 1)
  gt_new <- gtable_add_cols(gt_new, new_width, cols[1] - 1)
  if (gt$respect) {
    gt_new <- simplify_fixed(gt, gt_new, panels, rows, cols)
  } else {
    gt_new <- simplify_free(gt, gt_new, panels, rows, cols)
  }
  gt_new$collected_guides <- guides
  gt_new
}
#' @importFrom grid unit.c unit
#' @importFrom ggplot2 find_panel
#' @importFrom gtable gtable gtable_add_grob
#' @export
simplify_gt.gtable_patchwork <- function(gt) {
  guides <- gt$collected_guides
  gt$collected_guides <- NULL
  panel_pos <- find_panel(gt)
  if (all(is_abs_unit(gt$widths[panel_pos$l:panel_pos$r]))) {
    new_width <- sum(convertWidth(gt$widths[panel_pos$l:panel_pos$r], 'mm'))
  } else {
    new_width <- unit(1, 'null')
  }
  if (all(is_abs_unit(gt$heights[panel_pos$t:panel_pos$b]))) {
    new_height <- sum(convertHeight(gt$widths[panel_pos$t:panel_pos$b], 'mm'))
  } else {
    new_height <- unit(1, 'null')
  }
  widths <- unit.c(gt$widths[seq_len(panel_pos$l - 1)], new_width, gt$widths[seq(panel_pos$r + 1, ncol(gt))])
  heights <- unit.c(gt$heights[seq_len(panel_pos$t - 1)], new_height, gt$heights[seq(panel_pos$b + 1, nrow(gt))])
  gt_new <- gtable(widths = widths, heights = heights)
  gt_new <- gtable_add_grob(gt_new, zeroGrob(), PANEL_ROW, PANEL_COL, name = 'panel-nested-patchwork')
  gt_new <- gtable_add_grob(gt_new, gt, 1, 1, nrow(gt_new), ncol(gt_new), clip = 'off', name = 'patchwork-table')
  class(gt_new) <- c('gtable_patchwork_simple', class(gt_new))
  gt_new$collected_guides <- guides
  gt_new
}
#' @export
simplify_gt.patchgrob <- function(gt) gt
#' @export
simplify_gt.inset_table <- function(gt) gt
#' @export
simplify_gt.free_table <- function(gt) {
  settings <- attr(gt, "free_settings")
  settings <- split(names(settings), settings)
  gt_new <- NextMethod()
  if (!is.null(settings$label)) {
    gt_new <- free_label(gt_new, c("t", "r", "b", "l") %in% settings$label)
  }
  if (!is.null(settings$space)) {
    gt_new <- free_space(gt_new, c("t", "r", "b", "l") %in% settings$space)
  }
  if (!is.null(settings$panel)) {
    gt_new <- free_panel(gt_new, c("t", "r", "b", "l") %in% settings$panel)
  }
  gt_new
}

#' @importFrom gtable gtable_add_grob is.gtable
#' @importFrom grid viewport
simplify_free <- function(gt, gt_new, panels, rows, cols) {
  p_cols <- seq(cols[1], cols[2])
  if (length(p_cols) == 1) {
    top <- which(gt$layout$l == p_cols & gt$layout$r == p_cols & gt$layout$b < rows[1])
    gt_new <- gtable_add_grob(gt_new, gt$grobs[top], gt$layout$t[top], p_cols,
                              gt$layout$b[top], z = gt$layout$z[top],
                              clip = gt$layout$clip[top], name = gt$layout$name[top])
    bottom <- which(gt$layout$l == p_cols & gt$layout$r == p_cols & gt$layout$t > rows[2])
    b_mod <- rows[2] - rows[1]
    gt_new <- gtable_add_grob(gt_new, gt$grobs[bottom], gt$layout$t[bottom] - b_mod,
                              p_cols, gt$layout$b[bottom] - b_mod, z = gt$layout$z[bottom],
                              clip = gt$layout$clip[bottom], name = gt$layout$name[bottom])
    t_strips <- grepl('^strip-t-', gt_new$layout$name)
    if (any(t_strips)) {
      gt_new$grobs[t_strips] <- lapply(gt_new$grobs[t_strips], function(g) {
        if (is.gtable(g)) {
          g$vp <- viewport(y = 0, just = 'bottom', height = sum(g$heights))
        }
        g
      })
    }
    b_strips <- grepl('^strip-b-', gt_new$layout$name)
    if (any(b_strips)) {
      gt_new$grobs[b_strips] <- lapply(gt_new$grobs[b_strips], function(g) {
        if (is.gtable(g)) {
          g$vp <- viewport(y = 1, just = 'top', height = sum(g$heights))
        }
        g
      })
    }
  } else {
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
        gt_new <- gtable_add_grob(gt_new, table, ii, cols[1], clip = 'off',
                                  name = grobname, z = max(table$layout$z))
      }
    }
  }

  p_rows <- seq(rows[1], rows[2])
  if (length(p_rows) == 1) {
    left <- which(gt$layout$t == p_rows & gt$layout$b == p_rows & gt$layout$r < cols[1])
    gt_new <- gtable_add_grob(gt_new, gt$grobs[left], p_rows, gt$layout$l[left], p_rows,
                              gt$layout$r[left], z = gt$layout$z[left],
                              clip = gt$layout$clip[left], name = gt$layout$name[left])
    right <- which(gt$layout$t == p_rows & gt$layout$b == p_rows & gt$layout$l > cols[2])
    r_mod <- cols[2] - cols[1]
    gt_new <- gtable_add_grob(gt_new, gt$grobs[right], p_rows, gt$layout$l[right] - r_mod,
                              p_rows, gt$layout$r[right] - r_mod, z = gt$layout$z[right],
                              clip = gt$layout$clip[right], name = gt$layout$name[right])
    l_strips <- grepl('^strip-l-', gt_new$layout$name)
    if (any(l_strips)) {
      gt_new$grobs[l_strips] <- lapply(gt_new$grobs[l_strips], function(g) {
        if (is.gtable(g)) {
          g$vp <- viewport(x = 1, just = 'right', width = sum(g$widths))
        }
        g
      })
    }
    r_strips <- grepl('^strip-r-', gt_new$layout$name)
    if (any(r_strips)) {
      gt_new$grobs[r_strips] <- lapply(gt_new$grobs[r_strips], function(g) {
        if (is.gtable(g)) {
          g$vp <- viewport(x = 0, just = 'left', width = sum(g$widths))
        }
        g
      })
    }
  } else {
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
        gt_new <- gtable_add_grob(gt_new, table, rows[1], ii, clip = 'off',
                                  name = grobname, z = max(table$layout$z))
      }
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
      if (length(table$grobs) == 1) {
        grobname <- table$layout$name
        grob <- table$grobs[[1]]
      } else {
        grobname <- paste(table$layout$name, collapse = ', ')
        grob <- table
      }
      gt_new <- gtable_add_grob(gt_new, grob, rows[1], i, clip = 'off', name = grobname, z = max(table$layout$z))
    }
  }
  right <- if (length(right) != 0) max(right) else cols[2]
  for (i in seq_len(ncol(gt) - right)) {
    table <- gt[p_rows, i + right]
    if (length(table$grobs) != 0) {
      if (length(table$grobs) == 1) {
        grobname <- table$layout$name
        grob <- table$grobs[[1]]
      } else {
        grobname <- paste(table$layout$name, collapse = ', ')
        grob <- table
      }
      gt_new <- gtable_add_grob(gt_new, grob, rows[1], i + cols[1] + right - cols[2], clip = 'off', name = grobname, z = max(table$layout$z))
    }
  }
  top <- if (length(top) != 0) min(top) else rows[1]
  for (i in seq_len(top - 1)) {
    table <- gt[i, p_cols]
    if (length(table$grobs) != 0) {
      if (length(table$grobs) == 1) {
        grobname <- table$layout$name
        grob <- table$grobs[[1]]
      } else {
        grobname <- paste(table$layout$name, collapse = ', ')
        grob <- table
      }
      gt_new <- gtable_add_grob(gt_new, grob, i, cols[1], clip = 'off', name = grobname, z = max(table$layout$z))
    }
  }
  bottom <- if (length(bottom) != 0) max(bottom) else rows[2]
  for (i in seq_len(nrow(gt) - bottom)) {
    table <- gt[i + bottom, p_cols]
    if (length(table$grobs) != 0) {
      if (length(table$grobs) == 1) {
        grobname <- table$layout$name
        grob <- table$grobs[[1]]
      } else {
        grobname <- paste(table$layout$name, collapse = ', ')
        grob <- table
      }
      gt_new <- gtable_add_grob(gt_new, grob, i + rows[1] + bottom - rows[2], cols[1], clip = 'off', name = grobname, z = max(table$layout$z))
    }
  }
  panel_name <- paste0('panel; ', paste(panels$layout$name, collapse = ', '))
  gtable_add_grob(gt_new, panels, rows[1], cols[1], clip = 'off', name = panel_name, z = 1)
}

free_panel <- function(gt, has_side) {
  nested <- grep("patchwork-table", gt$layout$name)
  for (i in nested) {
    loc <- gt$layout[i, ]
    loc <- c(loc$t, loc$r, loc$b, loc$l) == c(1, ncol(gt), nrow(gt), 1) & has_side
    if (!any(loc)) next
    gt$grobs[[i]] <- free_panel(gt$grobs[[i]], loc)
  }
  top <- if (has_side[1]) 3 else PANEL_ROW
  right <- ncol(gt) - if (has_side[2]) 2 else TABLE_COLS - PANEL_COL
  bottom <- nrow(gt) - if (has_side[3]) 2 else TABLE_ROWS - PANEL_ROW
  left <- if (has_side[4]) 3 else PANEL_COL

  panel_col_pos <- seq(0, by = TABLE_COLS, length.out = floor(ncol(gt) / TABLE_COLS)) + PANEL_COL
  panel_row_pos <- seq(0, by = TABLE_ROWS, length.out = floor(nrow(gt) / TABLE_ROWS)) + PANEL_ROW
  panel_width <- gt$widths[panel_col_pos]
  panel_height <- gt$heights[panel_row_pos]

  gt$widths[panel_col_pos][as.numeric(panel_width) == 0] <- unit(1, "null")
  gt$heights[panel_row_pos][as.numeric(panel_height) == 0] <- unit(1, "null")

  # Fixed aspect plots needs special treatment
  if (isTRUE(gt$respect)) {
    p_i <- grep("panel;", gt$layout$name)
    if (has_side[1]) {
      h <- gt$grobs[[p_i]]$grobs[[grep("top", gt$grobs[[p_i]]$layout$name)]]
      gt$grobs[[p_i]] <- gtable_add_rows(gt$grobs[[p_i]], sum(h$heights), pos = 0)
      gt$layout$t[p_i] <- top
    }
    if (has_side[2]) {
      w <- gt$grobs[[p_i]]$grobs[[grep("right", gt$grobs[[p_i]]$layout$name)]]
      gt$grobs[[p_i]] <- gtable_add_cols(gt$grobs[[p_i]], sum(w$widths), pos = -1)
      gt$layout$r[p_i] <- right
    }
    if (has_side[3]) {
      h <- gt$grobs[[p_i]]$grobs[[grep("bottom", gt$grobs[[p_i]]$layout$name)]]
      gt$grobs[[p_i]] <- gtable_add_rows(gt$grobs[[p_i]], sum(h$heights), pos = -1)
      gt$layout$b[p_i] <- bottom
    }
    if (has_side[4]) {
      w <- gt$grobs[[p_i]]$grobs[[grep("left", gt$grobs[[p_i]]$layout$name)]]
      gt$grobs[[p_i]] <- gtable_add_cols(gt$grobs[[p_i]], sum(w$widths), pos = 0)
      gt$layout$l[p_i] <- left
    }
  } else {
    gt <- liberate_area(gt, top, right, bottom, left, "free_panel")
  }

  if (!has_side[1] && (has_side[2] || has_side[4])) {
    gt <- liberate_rows(gt, 3, right, top - 1, left, align = 0, "free_row")
  }
  if (!has_side[2] && (has_side[1] || has_side[3])) {
    gt <- liberate_cols(gt, top, ncol(gt) - 2, bottom, right + 1, align = 0, "free_col")
  }
  if (!has_side[3] && (has_side[2] || has_side[4])) {
    gt <- liberate_rows(gt, bottom + 1, right, nrow(gt) - 2, left, align = 1, "free_row")
  }
  if (!has_side[4] && (has_side[1] || has_side[3])) {
    gt <- liberate_cols(gt, top, left - 1, bottom, 3, align = 1, "free_col")
  }

  old_free <- grepl("free_panel-", gt$layout$name) | grepl("free_row-", gt$layout$name) | grepl("free_col-", gt$layout$name)
  if (any(old_free)) {
    for (i in which(old_free)) {
      loc <- unlist(gt$layout[i, c("t", "r", "b", "l")])
      loc[has_side] <- c(top, right, bottom, left)[has_side]
      gt_old <- gt
      gt_old$grobs <- gt_old$grobs[i]
      gt_old$layout <- gt_old$layout[i, ]
      gt$grobs[[i]] <- gt_old[loc[1]:loc[3], loc[4]:loc[2]]
      gt$layout[i, c("t", "r", "b", "l")] <- loc
    }
  }

  gt$widths[setdiff(left:right, min(panel_col_pos):max(panel_col_pos))] <- unit(0, "mm")
  gt$widths[panel_col_pos] <- panel_width
  gt$heights[setdiff(top:bottom, min(panel_row_pos):max(panel_row_pos))] <- unit(0, "mm")
  gt$heights[panel_row_pos] <- panel_height

  gt
}
grob_in_rect <- function(gt, top, right, bottom, left) {
  gt$layout$l >= left & gt$layout$t >= top & gt$layout$r <= right & gt$layout$b <= bottom
}
liberate_area <- function(gt, top, right, bottom, left, name = NULL, vp = NULL) {
  liberated <- gt[top:bottom, left:right]
  remove <- grob_in_rect(gt, top, right, bottom, left)
  if (any(remove)) {
    if (!is.null(vp)) liberated$vp <- vp
    name <- name %||% paste(liberated$layout$name, collapse ="; ")
    gt$grobs <- gt$grobs[!remove]
    gt$layout <- gt$layout[!remove,]
    gt  <- gtable_add_grob(gt, liberated, top, left, bottom, right, max(liberated$layout$z), "inherit", name)
  }
  gt
}
liberate_rows <- function(gt, top, right, bottom, left, align = 0.5, name = NULL) {
  liberate <- which(grob_in_rect(gt, top, right, bottom, left))
  unique_rows <- unique(gt$layout[liberate, c("t", "b")])
  for (i in seq_len(nrow(unique_rows))) {
    gt <- liberate_area(gt, unique_rows$t[i], right, unique_rows$b[i], left, name, vp = viewport(y = align, height = sum(gt$heights[unique_rows$t[i]:unique_rows$b[i]]), just = c(0.5, align)))
  }
  gt
}
liberate_cols <- function(gt, top, right, bottom ,left, align = 0.5, name = NULL) {
  liberate <- which(grob_in_rect(gt, top, right, bottom, left))
  unique_cols <- unique(gt$layout[liberate, c("l", "r")])
  for (i in seq_len(nrow(unique_cols))) {
    gt <- liberate_area(gt, top, unique_cols$r[i], bottom, unique_cols$l[i], name, vp = viewport(x = align, width = sum(gt$widths[unique_cols$l[i]:unique_cols$r[i]]), just = c(align, 0.5)))
  }
  gt
}
free_label <- function(gt, has_side) {
  # Fixed aspect plots already have this behaviour
  if (isTRUE(gt$respect)) return(gt)
  nested <- grep("patchwork-table", gt$layout$name)
  for (i in nested) {
    loc <- gt$layout[i, ]
    loc <- c(loc$t, loc$r, loc$b, loc$l) == c(1, ncol(gt), nrow(gt), 1) & has_side
    if (!any(loc)) next
    gt$grobs[[i]] <- free_label(gt$grobs[[i]], loc)
  }

  panel_col_pos <- seq(0, by = TABLE_COLS, length.out = floor(ncol(gt) / TABLE_COLS)) + PANEL_COL
  panel_row_pos <- seq(0, by = TABLE_ROWS, length.out = floor(nrow(gt) / TABLE_ROWS)) + PANEL_ROW
  panel_width <- gt$widths[panel_col_pos]
  panel_height <- gt$heights[panel_row_pos]

  gt$widths[panel_col_pos][as.numeric(panel_width) == 0] <- unit(1, "null")
  gt$heights[panel_row_pos][as.numeric(panel_height) == 0] <- unit(1, "null")

  top <- PANEL_ROW
  right <- ncol(gt) - (TABLE_COLS - PANEL_COL)
  bottom <- nrow(gt) - (TABLE_ROWS - PANEL_ROW)
  left <- PANEL_COL

  if (has_side[1]) {
    gt <- liberate_area(gt, top - 3, right, top - 1, left, vp = viewport(y = 0, height = sum(gt$heights[(top - 3):(top - 1)]), just = c(0.5, 0)))
  }
  if (has_side[2]) {
    gt <- liberate_area(gt, top, right + 3, bottom, right + 1, vp = viewport(x = 0, width = sum(gt$widths[(right + 1):(right + 3)]), just = c(0, 0.5)))
  }
  if (has_side[3]) {
    gt <- liberate_area(gt, bottom + 1, right, bottom + 3, left, vp = viewport(y = 1, height = sum(gt$heights[(bottom + 1):(bottom + 3)]), just = c(0.5, 1)))
  }
  if (has_side[4]) {
    gt <- liberate_area(gt, top, left - 1, bottom, left - 3, vp = viewport(x = 1, width = sum(gt$widths[(left - 3):(left - 1)]), just = c(1, 0.5)))
  }

  gt$widths[panel_col_pos] <- panel_width
  gt$heights[panel_row_pos] <- panel_height

  gt
}
free_space <- function(gt, has_side) {
  nested <- grep("patchwork-table", gt$layout$name)
  for (i in nested) {
    loc <- gt$layout[i, ]
    loc <- c(loc$t, loc$r, loc$b, loc$l) == c(1, ncol(gt), nrow(gt), 1) & has_side
    if (!any(loc)) next
    gt$grobs[[i]] <- free_space(gt$grobs[[i]], loc)
  }

  panel_col_pos <- seq(0, by = TABLE_COLS, length.out = floor(ncol(gt) / TABLE_COLS)) + PANEL_COL
  panel_row_pos <- seq(0, by = TABLE_ROWS, length.out = floor(nrow(gt) / TABLE_ROWS)) + PANEL_ROW
  panel_width <- gt$widths[panel_col_pos]
  panel_height <- gt$heights[panel_row_pos]

  gt$widths[panel_col_pos][as.numeric(panel_width) == 0] <- unit(1, "null")
  gt$heights[panel_row_pos][as.numeric(panel_height) == 0] <- unit(1, "null")

  top <- PANEL_ROW
  right <- ncol(gt) - (TABLE_COLS - PANEL_COL)
  bottom <- nrow(gt) - (TABLE_ROWS - PANEL_ROW)
  left <- PANEL_COL

  if (has_side[1]) {
    gt <- liberate_area(gt, 3, right, top - 1, left, vp = viewport(y = 0, height = sum(gt$heights[3:(top - 1)]), just = c(0.5, 0)))
    gt$heights[3:(top - 1)] <- unit(0, "mm")
  }
  if (has_side[2]) {
    gt <- liberate_area(gt, top, ncol(gt) - 2, bottom, right + 1, vp = viewport(x = 0, width = sum(gt$widths[(right + 1):(ncol(gt) - 2)]), just = c(0, 0.5)))
    gt$widths[(right + 1):(ncol(gt) - 2)] <- unit(0, "mm")
  }
  if (has_side[3]) {
    gt <- liberate_area(gt, bottom + 1, right, nrow(gt) - 2, left, vp = viewport(y = 1, height = sum(gt$heights[(bottom + 1):(nrow(gt) - 2)]), just = c(0.5, 1)))
    gt$heights[(bottom + 1):(nrow(gt) - 2)] <- unit(0, "mm")
  }
  if (has_side[4]) {
    gt <- liberate_area(gt, top, left - 1, bottom, 3, vp = viewport(x = 1, width = sum(gt$widths[3:(left - 1)]), just = c(1, 0.5)))
    gt$widths[3:(left - 1)] <- unit(0, "mm")
  }

  gt$widths[panel_col_pos] <- panel_width
  gt$heights[panel_row_pos] <- panel_height

  gt
}


create_design <- function(width, height, byrow) {
  mat <- matrix(seq_len(width * height), nrow = height, ncol = width, byrow = byrow)
  ind <- as.vector(mat)
  ind <- match(seq_along(ind), ind)
  area(
    t = row(mat)[ind],
    l = col(mat)[ind]
  )
}
#' @importFrom grid convertHeight convertWidth unit
table_dims <- function(widths, heights, areas, ncol, nrow) {
  widths <- lapply(widths, convertWidth, 'mm', valueOnly = TRUE)
  widths <- vapply(seq_len(ncol * TABLE_COLS), function(i) {
    area <- (i - 1) %/% TABLE_COLS + 1
    col_loc <- i %% TABLE_COLS
    if (col_loc == 0) col_loc <- TABLE_COLS
    area_side <- if (col_loc <= PANEL_COL) 'l' else 'r'
    tables <- which(areas[[area_side]] == area)
    if (length(tables) == 0) {
      0
    } else {
      max(vapply(widths[tables], `[[`, numeric(1), col_loc), 0)
    }
  }, numeric(1))
  heights <- lapply(heights, convertHeight, 'mm', valueOnly = TRUE)
  heights <- vapply(seq_len(nrow * TABLE_ROWS), function(i) {
    area <- (i - 1) %/% TABLE_ROWS + 1
    row_loc <- i %% TABLE_ROWS
    if (row_loc == 0) row_loc <- TABLE_ROWS
    area_side <- if (row_loc <= PANEL_ROW) 't' else 'b'
    tables <- which(areas[[area_side]] == area)
    if (length(tables) == 0) {
      0
    } else {
      max(vapply(heights[tables], `[[`, numeric(1), row_loc), 0)
    }
  }, numeric(1))
  list(widths = unit(widths, 'mm'), heights = unit(heights, 'mm'))
}

set_grob_sizes <- function(tables, widths, heights, design) {
  unlist(lapply(seq_along(tables), function(i) {
    gt <- tables[[i]]
    if (!inherits(gt, 'gtable_patchwork_simple')) {
      return(gt$grobs)
    }
    table_loc <- design[i, , drop = FALSE]
    l <- (table_loc$l - 1) * TABLE_COLS
    l_widths <- widths[seq(l + 1, l + PANEL_COL - 1)]
    r <- (table_loc$r - 1) * TABLE_COLS
    r_widths <- widths[seq(r + PANEL_COL + 1, r + TABLE_COLS)]
    t <- (table_loc$t - 1) * TABLE_ROWS
    t_heights <- heights[seq(t + 1, t + PANEL_ROW - 1)]
    b <- (table_loc$b - 1) * TABLE_ROWS
    b_heights <- heights[seq(b + PANEL_ROW + 1, b + TABLE_ROWS)]
    nested <- grep("patchwork-table", gt$layout$name)
    gt$grobs[[nested]] <- set_border_sizes(gt$grobs[[nested]], l_widths, r_widths, t_heights, b_heights)
    gt$grobs
  }), recursive = FALSE)
}

set_border_sizes <- function(gt, l = NULL, r = NULL, t = NULL, b = NULL) {
  if (is.null(l) && is.null(r) && is.null(t) && is.null(b)) return(gt)

  if (!is.null(l)) gt$widths[seq_along(l)] <- l
  if (!is.null(r)) gt$widths[seq(ncol(gt) - length(r) + 1, ncol(gt))] <- r
  if (!is.null(t)) gt$heights[seq_along(t)] <- t
  if (!is.null(b)) gt$heights[seq(nrow(gt) - length(b) + 1, nrow(gt))] <- b

  gt$grobs <- lapply(seq_along(gt$grobs), function(i) {
    grob <- gt$grobs[[i]]
    if (!inherits(grob, 'gtable_patchwork')) {
      return(grob)
    }
    set_border_sizes(
      grob,
      if (gt$layout$l[i] == 1) l else NULL,
      if (gt$layout$r[i] == ncol(gt)) r else NULL,
      if (gt$layout$t[i] == 1) t else NULL,
      if (gt$layout$b[i] == nrow(gt)) b else NULL
    )
  })
  gt
}
#' @importFrom gtable gtable_add_rows gtable_add_cols
#' @importFrom grid unit
#' @importFrom ggplot2 find_panel
add_strips <- function(gt, theme = NULL) {
  panel_loc <- find_panel(gt)
  strip_pos <- switch(
    find_strip_pos(gt, theme),
    inside = 0,
    outside = 2
  )
  if (!any(grepl('strip-b', gt$layout$name))) {
    gt <- gtable_add_rows(gt, unit(0, 'mm'), panel_loc$b + strip_pos)
  } else if (strip_pos == 2 && !any(gt$layout$b == panel_loc$b + 2)) {
    # Merge the strip-gap height into the axis and remove it. Only performed if
    # an axis exist
    gt$heights[panel_loc$b + 1] <- sum(gt$heights[panel_loc$b + c(1, 2)])
    gt <- gt[-(panel_loc$b + 2), ]
  }
  if (!any(grepl('strip-t', gt$layout$name))) {
    gt <- gtable_add_rows(gt, unit(0, 'mm'), panel_loc$t - 1 - strip_pos)
  } else if (strip_pos == 2 && !any(gt$layout$t == panel_loc$t - 2)) {
    gt$heights[panel_loc$t - 1] <- sum(gt$heights[panel_loc$t - c(1, 2)])
    gt <- gt[-(panel_loc$t - 2), ]
  }
  if (!any(grepl('strip-r', gt$layout$name))) {
    gt <- gtable_add_cols(gt, unit(0, 'mm'), panel_loc$r + strip_pos)
  } else if (strip_pos == 2 && !any(gt$layout$r == panel_loc$r + 2)) {
    gt$widths[panel_loc$r + 1] <- sum(gt$widths[panel_loc$r + c(1, 2)])
    gt <- gt[, -(panel_loc$r + 2)]
  }
  if (!any(grepl('strip-l', gt$layout$name))) {
    gt <- gtable_add_cols(gt, unit(0, 'mm'), panel_loc$l - 1 - strip_pos)
  } else if (strip_pos == 2 && !any(gt$layout$l == panel_loc$l - 2)) {
    gt$widths[panel_loc$l - 1] <- sum(gt$widths[panel_loc$l - c(1, 2)])
    gt <- gt[, -(panel_loc$l - 2)]
  }
  gt
}
#' @importFrom gtable gtable_add_rows gtable_add_cols
#' @importFrom grid unit
add_guides <- function(gt, collect = FALSE) {
  panel_loc <- find_panel(gt)[, c('t', 'l', 'b', 'r')]
  guide_ind <- which(grepl('guide-box', gt$layout$name))

  if (length(guide_ind) == 5) {
    # For ggplot2 >3.5.0, we don't need to add extra space for missing legends,
    # as every position already has relevant cells in the gtable.
    if (!collect) {
      return(gt)
    }
    # We need to collect guides from multiple cells in the gtable instead.
    guide_loc <- gt$layout[guide_ind, ]
    guide_pos <- gsub("guide-box-", "", guide_loc$name)

    # Set space for guides to zero
    space_pos <- ifelse(guide_pos %in% c('left', 'top'), 1L, -1L)
    lr  <- guide_pos %in% c('left', 'right')
    col <- guide_loc$l[lr]
    gt$widths[c(col, col + space_pos[lr])] <- unit(0, "mm")
    tb  <- guide_pos %in% c('top', 'bottom')
    row <- guide_loc$t[tb]
    gt$heights[c(row, row + space_pos[tb])] <- unit(0, "mm")

    # Collect guides
    collection <- lapply(gt$grobs[guide_ind], function(box) {
      box$grobs[grepl('guides', box$layout$name)] # NULL if legend is empty
    })
    collection <- unlist(collection, recursive = FALSE) # drops NULL
    gt$collected_guides <- collection

    # Remove guides from gtable
    gt$grobs[guide_ind] <- NULL
    gt$layout <- gt$layout[-guide_ind, ]
    return(gt)
  }

  guide_loc <- gt$layout[guide_ind, c('t', 'l', 'b', 'r')]
  guide_pos <- if (nrow(guide_loc) == 0) {
    'none'
  } else if (all(unlist(guide_loc == panel_loc))) {
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
      gt$heights[c(guide_loc$t, guide_loc$t + space_pos)] <- unit(c(0, 0), 'mm')
    }
    gt$grobs[guide_ind] <- NULL
    gt$layout <- gt$layout[-guide_ind, ]
    gt$collected_guides <- guide_grob$grobs[grepl('guides', guide_grob$layout$name)]
  }
  gt
}
find_strip_pos <- function(gt, theme) {
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
  if (is.null(theme)) {
    'inside'
  } else {
    calc_element("strip.placement", theme) %||% "inside"
  }
}

set_panel_dimensions <- function(gt, panels, widths, heights, fixed_asp, design) {
  width_ind <- seq(PANEL_COL, by = TABLE_COLS, length.out = length(widths))
  height_ind <- seq(PANEL_ROW, by = TABLE_ROWS, length.out = length(heights))
  if (!is.unit(widths)) {
    widths[is.na(widths)] <- -1
    widths <- unit(widths, 'null')
  }
  width_strings <- as.character(widths)
  if (!is.unit(heights)) {
    heights[is.na(heights)] <- -1
    heights <- unit(heights, 'null')
  }
  height_strings <- as.character(heights)

  panel_widths <- do.call(unit.c, lapply(panels, function(x) x$widths[PANEL_COL]))
  absolute_col <- is_abs_unit(panel_widths) & as.numeric(panel_widths) != 0
  if (any(absolute_col)) {
    pos <- ifelse(absolute_col & design$l == design$r & width_strings[design$l] == "-1null", design$l, NA)
    fixed_widths <- lapply(split(panel_widths, pos), "max")
    widths[as.numeric(names(fixed_widths))] <- do.call(unit.c, fixed_widths)
    width_strings <- as.character(widths)
  }
  panel_heights <- do.call(unit.c, lapply(panels, function(x) x$heights[PANEL_ROW]))
  absolute_row <- is_abs_unit(panel_heights) & as.numeric(panel_heights) != 0
  if (any(absolute_row)) {
    pos <- ifelse(absolute_row & design$t == design$b & height_strings[design$t] == "-1null", design$t, NA)
    fixed_heights <- lapply(split(panel_heights, pos), "max")
    heights[as.numeric(names(fixed_heights))] <- do.call(unit.c, fixed_heights)
    height_strings <- as.character(heights)
  }

  if (any(width_strings == '-1null') && any(height_strings == '-1null')) {
    respect <- matrix(0, nrow = length(gt$heights), ncol = length(gt$widths))
    fixed_areas <- lapply(which(fixed_asp), function(i) {
      list(
        rows = seq(design$t[i], design$b[i]),
        cols = seq(design$l[i], design$r[i])
      )
    })
    can_fix <- vapply(fixed_areas, function(x) length(x$rows) == 1 && length(x$cols), logical(1))
    can_fix_row <- vapply(fixed_areas, function(x) all(grepl('null$', height_strings[x$rows])), logical(1))
    can_fix_col <- vapply(fixed_areas, function(x) all(grepl('null$', width_strings[x$cols])), logical(1))
    fixed_areas <- fixed_areas[can_fix & (can_fix_row & can_fix_col)]
    fixed_gt <- which(fixed_asp)[can_fix & (can_fix_row & can_fix_col)]
    all_fixed_rows <- table(unlist(lapply(fixed_areas, `[[`, 'rows')))
    all_fixed_cols <- table(unlist(lapply(fixed_areas, `[[`, 'cols')))
    controls_dim <- vapply(fixed_areas, function(a) {
      all(all_fixed_rows[as.character(a$rows)] == 1) || all(all_fixed_cols[as.character(a$cols)] == 1)
    }, logical(1))
    for (i in order(controls_dim)) {
      panel_ind <- grep('panel', panels[[fixed_gt[i]]]$layout$name)[1]
      # Guard against rows and cols added by free_panel()
      content_cols <- range(panels[[fixed_gt[i]]]$grobs[[panel_ind]]$layout$l, panels[[fixed_gt[i]]]$grobs[[panel_ind]]$layout$r)
      content_rows <- range(panels[[fixed_gt[i]]]$grobs[[panel_ind]]$layout$t, panels[[fixed_gt[i]]]$grobs[[panel_ind]]$layout$b)
      w <- panels[[fixed_gt[i]]]$grobs[[panel_ind]]$widths[content_cols[1]:content_cols[2]]
      h <- panels[[fixed_gt[i]]]$grobs[[panel_ind]]$heights[content_rows[1]:content_rows[2]]
      can_set_width <- all(width_strings[fixed_areas[[i]]$cols] == '-1null') && length(w) == 1 && length(h) == 1
      can_set_height <- all(height_strings[fixed_areas[[i]]$rows] == '-1null') && length(w) == 1 && length(h) == 1
      will_be_fixed <- TRUE
      if (can_set_width && can_set_height) {
        widths[fixed_areas[[i]]$cols] <- w
        width_strings[fixed_areas[[i]]$cols] <- ''
        heights[fixed_areas[[i]]$rows] <- h
        height_strings[fixed_areas[[i]]$rows] <- ''
      } else if (can_set_width) {
        widths[fixed_areas[[i]]$cols] <- heights[fixed_areas[[i]]$rows] * (as.numeric(w) / as.numeric(h))
        width_strings[fixed_areas[[i]]$cols] <- ''
      } else if (can_set_height) {
        heights[fixed_areas[[i]]$rows] <- widths[fixed_areas[[i]]$cols] * (as.numeric(h) / as.numeric(w))
        height_strings[fixed_areas[[i]]$rows] <- ''
      } else {
        will_be_fixed <- FALSE
      }
      if (will_be_fixed) {
        respect[height_ind[fixed_areas[[i]]$rows], width_ind[fixed_areas[[i]]$cols]] <- 1
      }
    }
    if (all(respect == 0)) respect <- FALSE
    gt$respect <- respect
  }
  widths[width_strings == '-1null'] <- unit(1, 'null')
  heights[height_strings == '-1null'] <- unit(1, 'null')
  gt$widths[width_ind] <- widths
  gt$heights[height_ind] <- heights
  gt
}

add_insets <- function(gt) {
  is_inset <- vapply(gt, inherits, logical(1), 'inset_table')
  if (!any(is_inset)) {
    return(gt)
  }
  canvas <- rank(cumsum(!is_inset), ties.method = "min")[is_inset]
  if (canvas[1] == 0) {
    cli_abort("insets cannot be the first plot in a patchwork")
  }
  insets <- which(is_inset)
  name <- paste0('inset_', insets)
  for (i in seq_along(insets)) {
    ins <- gt[[insets[i]]]
    can <- gt[[canvas[i]]]
    setting <- attr(ins, 'inset_settings')
    if (setting$on_top) {
      z <- max(can$layout$z) + 1
    } else {
      bg <- which(grepl('background', can$layout$name))
      if (length(bg) != 0) {
        z <- can$layout$z[bg[1]]
      } else {
        z <- min(can$layout$z) - 1
      }
    }
    gt[[canvas[i]]] <- switch(setting$align_to,
           panel = gtable_add_grob(can, list(ins), PANEL_ROW, PANEL_COL, z = z,
                                   clip = setting$clip, name = name[i]),
           plot = gtable_add_grob(can, list(ins), PLOT_TOP, PLOT_LEFT, PLOT_BOTTOM,
                                  PLOT_RIGHT, z = z, clip =  setting$clip, name = name[i]),
           full = gtable_add_grob(can, list(ins), 1, 1, nrow(can), ncol(can), z = z,
                                  clip = setting$clip, name = name[i]),
           cli_abort('Unknown alignment setting: {.arg {setting$align_to}}')
    )
  }
  gt[!is_inset]
}
