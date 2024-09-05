unname_vp <- function(x) {
  if (inherits(x, 'vpTree')) {
    x$parent <- unname_vp(x$parent)
    x$children <- lapply(x$children, unname_vp)
  } else if (inherits(x, 'viewport')) {
    x$name <- ''
    if (!is.null(x$layout$widths)) {
      x$layout$widths <- absolute.size(x$layout$widths)
    }
    if (!is.null(x$layout$heights)) {
      x$layout$heights <- absolute.size(x$layout$heights)
    }
  }
  unit_elements <- vapply(x, is.unit, logical(1))
  x[unit_elements] <- lapply(.subset(x, unit_elements), absolute.size)
  x
}
#' @importFrom grid is.grob is.unit absolute.size
#' @importFrom gtable is.gtable
#' @importFrom farver set_channel get_channel
unname_grob <- function(x) {
  if (is.gtable(x)) {
    x$name <- ''
    x$rownames <- NULL
    x$vp <- unname_vp(x$vp)
    names(x$grobs) <- NULL
    x$grobs <- lapply(x$grobs, unname_grob)
  } else if (is.grob(x)) {
    x$name <- ''
    x$vp <- unname_vp(x$vp)
    x$children <- unname(lapply(x$children, unname_grob))
    x$childrenOrder <- rep_len('', length(x$childrenOrder))
  }
  unit_elements <- vapply(x, is.unit, logical(1))
  x[unit_elements] <- lapply(.subset(x, unit_elements), absolute.size)
  if (!is.null(x$gp)) {
    if (is.character(x$gp$col)) x$gp$col <- set_channel(x$gp$col, "r", get_channel(x$gp$col, "r"))
    if (is.character(x$gp$fill)) x$gp$fill <- set_channel(x$gp$fill, "r", get_channel(x$gp$fill, "r"))
    if (is.numeric(x$gp$lty)) x$gp$lty <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[x$gp$lty + 1]
    if (is.character(x$gp$lty)) {
      rename <- match(x$gp$lty, c("44", "13", "1343", "73", "2262"))
      matched <- !is.na(rename)
      x$gp$lty[matched] <- c("dashed", "dotted", "dotdash", "longdash", "twodash")[rename[matched]]
    }
    if (is.numeric(x$gp$lineend)) x$gp$lineend <- c("round", "butt", "square")[x$gp$lineend]
    if (is.numeric(x$gp$linejoin)) x$gp$linejoin <- c("round", "mitre", "bevel")[x$gp$linejoin]
  }
  x
}
collapse_guides <- function(guides) {
  unnamed <- lapply(guides, unname_grob)
  for (i in rev(seq_along(unnamed)[-1])) {
    for (j in seq_len(i - 1)) {
      if (isTRUE(all.equal(unnamed[[i]], unnamed[[j]], check.names = FALSE, check.attributes = FALSE))) {
        guides[i] <- NULL
        break
      }
    }
  }
  guides
}

#' @importFrom ggplot2 calc_element
assemble_guides <- function(guides,  theme) {
  position <- theme$legend.position %||% "right"
  if (length(position) == 2) {
    warning("Manual legend position not possible for collected guides. Defaulting to 'right'", call. = FALSE)
    position <- "right"
  }
  # https://github.com/tidyverse/ggplot2/blob/57ba97fa04dadc6fd73db1904e39a09d57a4fcbe/R/guides-.R#L512
  theme$legend.spacing <- theme$legend.spacing %||% unit(0.5, "lines")
  theme$legend.spacing.y <- calc_element("legend.spacing.y", theme)
  theme$legend.spacing.x <- calc_element("legend.spacing.x", theme)
  # for every position, collect all individual guides and arrange them
  # into a guide box which will be inserted into the main gtable
  Guides <- utils::getFromNamespace("Guides", "ggplot2")
  Guides$package_box(guides, position, theme)
}

#' @importFrom ggplot2 calc_element find_panel
#' @importFrom gtable gtable_width gtable_height
#' @importFrom grid unit.c
attach_guides <- function(table, guides, theme) {
  guide_areas <- grepl('panel-guide_area', table$layout$name)
  if (any(guide_areas)) {
    area_ind <- which(guide_areas)
    if (length(area_ind) != 1) {
      warning("Only using the first guide area", call. = FALSE)
    }
    table$grobs[[area_ind[1]]] <- guides
    return(table)
  }
  p_loc <- find_panel(table)
  position <- theme$legend.position %||% "right"
  if (length(position) == 2) {
    warning('Manual position of collected guides not supported', call. = FALSE)
    position <- "right"
  }

  spacing <- calc_element("legend.box.spacing", theme) %||% unit(0.2, 'cm')
  legend_width  <- gtable_width(guides)
  legend_height <- gtable_height(guides)
  if (position == "left") {
    table <- gtable_add_grob(table, guides, clip = "off", t = p_loc$t,
                             l = p_loc$l - 5, b = p_loc$b, name = "guide-box")
    table <- set_border_sizes(table, l = unit.c(table$widths[seq_len(p_loc$l - 6)], legend_width, spacing))
  } else if (position == "right") {
    table <- gtable_add_grob(table, guides, clip = "off", t = p_loc$t,
                             l = p_loc$r + 5, b = p_loc$b, name = "guide-box")
    table <- set_border_sizes(table, r = unit.c(spacing, legend_width, table$widths[seq(p_loc$r + 6, ncol(table))]))
  } else if (position == "bottom") {
    table <- gtable_add_grob(table, guides, clip = "off", t = p_loc$b + 5,
                             l = p_loc$l, r = p_loc$r, name = "guide-box")
    table <- set_border_sizes(table, b = unit.c(spacing, legend_height, table$heights[seq(p_loc$b + 6, nrow(table))]))
  } else if (position == "top") {
    table <- gtable_add_grob(table, guides, clip = "off", t = p_loc$t - 5,
                             l = p_loc$l, r = p_loc$r, name = "guide-box")
    table <- set_border_sizes(table, t = unit.c(table$heights[seq_len(p_loc$t - 6)], legend_height, spacing))
  }

  table
}
