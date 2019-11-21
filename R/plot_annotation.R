#' Annotate the final patchwork
#'
#' The result of this function can be added to a patchwork using `+` in the same
#' way as [plot_layout()], but unlike [plot_layout()] it will only have an
#' effect on the top level plot. As the name suggests it controls different
#' aspects of the annotation of the final plot, such as titles and tags.
#'
#' @details
#' Tagging of subplots is done automatically and following the order of the
#' plots as they are added. When the plot contains nested layouts the
#' `tag_level` argument in the nested [plot_layout] will define whether
#' enumeration should continue as usual or add a new level. The format of the
#' levels are defined with `tag_levels` argument in `plot_annotation`
#'
#' @param title,subtitle,caption Text strings to use for the various plot
#' annotations.
#'
#' @param tag_levels A character vector defining the enumeration format to use
#' at each level. Possible values are `'a'` for lowercase letters, `'A'` for
#' uppercase letters, `'1'` for numbers, `'i'` for lowercase Roman numerals, and
#' `'I'` for uppercase Roman numerals.
#'
#' @param tag_prefix,tag_suffix Strings that should appear before or after the
#' tag.
#'
#' @param tag_sep A separator between different tag levels
#'
#' @param theme A ggplot theme specification to use for the plot. Only elements
#' related to the titles as well as plot margin and background is used.
#'
#' @return A `plot_annotation` object
#'
#' @export
plot_annotation <- function(title = NULL, subtitle = NULL, caption = NULL,
                            tag_levels = NULL, tag_prefix = NULL, tag_suffix = NULL,
                            tag_sep = NULL, theme = NULL) {
  th <- if (is.null(theme)) ggplot2::theme() else theme
  structure(list(
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag_levels = tag_levels,
    tag_prefix = tag_prefix,
    tag_suffix = tag_suffix,
    tag_sep = tag_sep,
    theme = th
  ), class = 'plot_annotation')
}
default_annotation <- plot_annotation(tag_levels = character(), tag_prefix = '', tag_suffix = '', tag_sep = '')
#' @importFrom utils modifyList
#' @export
ggplot_add.plot_annotation <- function(object, plot, object_name) {
  plot <- as_patchwork(plot)
  object$theme <- plot$patches$annotation$theme + object$theme
  plot$patches$annotation <- modifyList(plot$patches$annotation, object[!vapply(object, is.null, logical(1))])
  plot
}
#' @importFrom ggplot2 is.ggplot labs
#' @importFrom utils as.roman
recurse_tags <- function(x, levels, prefix, suffix, sep, offset = 1) {
  if (length(levels) == 0) return(list(patches = x, tab_ind = offset))
  level <- switch(
    as.character(levels[1]),
    a = letters,
    A = LETTERS,
    "1" = 1:100,
    i = tolower(as.roman(1:100)),
    I = as.roman(1:100),
    stop('Unknown tag type: ', levels[1], call. = FALSE)
  )
  patches <- x$patches$plots
  tag_ind <- offset
  for (i in seq_along(patches)) {
    if (is_patchwork(patches[[i]])) {
      tag_level <- patches[[i]]$patches$layout$tag_level
      tag_level <- if (is.null(tag_level)) default_layout$tag_level else tag_level
      if (tag_level == 'keep') {
        new_plots <- recurse_tags(patches[[i]], levels, prefix, suffix, sep, tag_ind)
        patches[[i]] <- new_plots$patches
        tag_ind <- new_plots$tag_ind
      } else {
        patches[[i]] <- recurse_tags(patches[[i]], levels[-1],
                                     prefix = paste0(prefix, level[tag_ind], sep),
                                     suffix, sep)$patches
        tag_ind <- tag_ind + 1
      }
    } else {
      patches[[i]] <- patches[[i]] + labs(tag = paste0(prefix, level[tag_ind], suffix))
      if ((is.ggplot(patches[[i]]) && !is_empty(patches[[i]])) ||
          (is_wrapped_patch(patches[[i]]) && !attr(patches[[i]], 'settings')$ignore_tag)) {
        tag_ind <- tag_ind + 1
      }
    }
  }
  x$patches$plots <- patches
  x <- x + labs(tag = paste0(prefix, level[tag_ind], suffix))
  if ((is.ggplot(x) && !is_empty(x)) || (is_wrapped_patch(x) && !attr(x, 'settings')$ignore_tag)) {
    tag_ind <- tag_ind + 1
  }
  list(
    patches = x,
    tag_ind = tag_ind
  )
}
#' @importFrom ggplot2 ggplot labs ggplotGrob
#' @importFrom gtable gtable_add_rows gtable_add_cols
#' @importFrom grid unit
#' @importFrom utils tail
annotate_table <- function(table, annotation) {
  p <- ggplot() + annotation$theme + do.call(labs, annotation[c('title', 'subtitle', 'caption')])
  p <- ggplotGrob(p)
  max_z <- max(table$layout$z)
  if (!is.null(annotation$title) || !is.null(annotation$subtitle)) {
    table <- gtable_add_rows(table, p$heights[c(1, 3, 4)], 0)
    table <- gtable_add_grob(table, get_grob(p, 'title'), 2, 2, r = ncol(table) - 1,
                             z = max_z + 3, name = 'title', clip = 'off')
    table <- gtable_add_grob(table, get_grob(p, 'subtitle'), 3, 2, r = ncol(table) - 1,
                             z = max_z + 2, name = 'subtitle', clip = 'off')
  } else {
    table <- gtable_add_rows(table, p$heights[1], 0)
  }
  if (!is.null(annotation$caption)) {
    table <- gtable_add_rows(table, tail(p$heights, 3)[-2])
    table <- gtable_add_grob(table, get_grob(p, 'caption'), nrow(table) - 1, 2,
                             r = ncol(table) - 1, z = max_z + 1, name = 'caption',
                             clip = 'off')
  } else {
    table <- gtable_add_rows(table, tail(p$heights, 1))
  }
  table <- gtable_add_cols(table, p$widths[1], 0)
  table <- gtable_add_cols(table, tail(p$widths, 1))
  table <- gtable_add_grob(table, get_grob(p, 'background'), 1, 1, nrow(table), ncol(table),
                           z = -Inf, name = 'background')
  table
}