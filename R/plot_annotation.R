has_tag <- function(x) {
    UseMethod('has_tag')
}

#' Annotate the final patchwork
#'
#' The result of this function can be added to a patchwork using `+` in the same
#' way as [plot_layout()], but unlike [plot_layout()] it will only have an
#' effect on the top level plot. As the name suggests it controls different
#' aspects of the annotation of the final plot, such as titles and tags. Already
#' added annotations can be removed by setting the relevant argument to `NULL`.
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
#' `'I'` for uppercase Roman numerals. It can also be a list containing
#' character vectors defining arbitrary tag sequences. If any element in the
#' list is a scalar and one of `'a'`, `'A'`, `'1'`, `'i`, or `'I'`, this level
#' will be expanded to the expected sequence.
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
#' @importFrom ggplot2 waiver
#'
#' @examples
#' library(ggplot2)
#'
#' p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) + geom_bar(aes(gear)) + facet_wrap(~cyl)
#'
#' # Add title, etc. to a patchwork
#' p1 + p2 + plot_annotation('This is a title', caption = 'made with patchwork')
#'
#' # Change styling of patchwork elements
#' p1 + p2 +
#'   plot_annotation(
#'     title = 'This is a title',
#'     caption = 'made with patchwork',
#'     theme = theme(plot.title = element_text(size = 16))
#'   )
#'
#' # Add tags to plots
#' p1 / (p2 | p3) +
#'   plot_annotation(tag_levels = 'A')
#'
#' # Add multilevel tagging to nested layouts
#' p1 / ((p2 | p3) + plot_layout(tag_level = 'new')) +
#'   plot_annotation(tag_levels = c('A', '1'))
#'
#' # Use a custom tag sequence (mixed with a standard one)
#' p1 / ((p2 | p3) + plot_layout(tag_level = 'new')) +
#'   plot_annotation(tag_levels = list(c('&', '%'), '1'))
#'
plot_annotation <- function(title = waiver(), subtitle = waiver(), caption = waiver(),
                            tag_levels = waiver(), tag_prefix = waiver(), tag_suffix = waiver(),
                            tag_sep = waiver(), theme = waiver()) {
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

default_annotation <- NULL
on_load({
  default_annotation <- plot_annotation(
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    tag_levels = character(),
    tag_prefix = '',
    tag_suffix = '',
    tag_sep = '',
    theme = NULL
  )
})

#' @importFrom utils modifyList
#' @export
ggplot_add.plot_annotation <- function(object, plot, object_name) {
  plot <- as_patchwork(plot)
  if (is.null(object$theme)) {
    plot$patches$annotation$theme <- NULL
  } else if (!is_waiver(object$theme)) {
    plot$patches$annotation$theme <- plot$patches$annotation$theme + object$theme
  }
  object$theme <- NULL
  do_change <- object[!vapply(object, is_waiver, logical(1))]
  plot$patches$annotation[names(do_change)] <- do_change
  plot
}
#' @importFrom ggplot2 is.ggplot labs
recurse_tags <- function(x, levels, prefix, suffix, sep, offset = 1) {
  if (length(levels) == 0) return(list(patches = x, tab_ind = offset))
  level <- get_level(levels[1])
  patches <- x$patches$plots
  tag_ind <- offset
  for (i in seq_along(patches)) {
    this_level <- if (length(level) < tag_ind) '' else level[tag_ind]
    if (is_patchwork(patches[[i]])) {
      if (is_inset_patch(patches[[i]]) && !has_tag(patches[[i]])) {
        next
      }
      tag_level <- patches[[i]]$patches$layout$tag_level
      tag_level <- if (is.null(tag_level)) default_layout$tag_level else tag_level
      if (tag_level == 'keep') {
        new_plots <- recurse_tags(patches[[i]], levels, prefix, suffix, sep, tag_ind)
        patches[[i]] <- new_plots$patches
        tag_ind <- new_plots$tag_ind
      } else if (length(levels) > 1) {
        patches[[i]] <- recurse_tags(patches[[i]], levels[-1],
                                     prefix = paste0(prefix, this_level, sep),
                                     suffix, sep)$patches
        tag_ind <- tag_ind + 1
      }
    } else if (has_tag(patches[[i]])) {
      patches[[i]] <- patches[[i]] + labs(tag = paste0(prefix, this_level, suffix))
      tag_ind <- tag_ind + 1
    }
  }
  x$patches$plots <- patches
  if (has_tag(x)) {
    this_level <- if (length(level) < tag_ind) '' else level[tag_ind]
    x <- x + labs(tag = paste0(prefix, this_level, suffix))
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
  p <- ggplot() + annotation$theme + exec(labs, !!!annotation[c('title', 'subtitle', 'caption')])
  p <- ggplotGrob(p)
  max_z <- max(table$layout$z)
  fix_respect <- is.matrix(table$respect)
  if (!is.null(annotation$title) || !is.null(annotation$subtitle)) {
    table <- gtable_add_rows(table, p$heights[c(1, 3, 4)], 0)

    table <- gtable_add_grob(table, get_grob(p, 'title'), 2, 2, r = ncol(table) - 1,
                             z = max_z + 3, name = 'title', clip = 'off')
    table <- gtable_add_grob(table, get_grob(p, 'subtitle'), 3, 2, r = ncol(table) - 1,
                             z = max_z + 2, name = 'subtitle', clip = 'off')
    if (fix_respect) table$respect <- rbind(matrix(0, nrow = 3, ncol = ncol(table$respect)), table$respect)
  } else {
    table <- gtable_add_rows(table, p$heights[1], 0)
    if (fix_respect) table$respect <- rbind(0, table$respect)
  }
  if (!is.null(annotation$caption)) {
    table <- gtable_add_rows(table, tail(p$heights, 3)[-2])
    table <- gtable_add_grob(table, get_grob(p, 'caption'), nrow(table) - 1, 2,
                             r = ncol(table) - 1, z = max_z + 1, name = 'caption',
                             clip = 'off')
    if (fix_respect) table$respect <- rbind(table$respect, matrix(0, nrow = 2, ncol = ncol(table$respect)))
  } else {
    table <- gtable_add_rows(table, tail(p$heights, 1))
    if (fix_respect) table$respect <- rbind(table$respect, 0)
  }
  table <- gtable_add_cols(table, p$widths[1], 0)
  table <- gtable_add_cols(table, tail(p$widths, 1))
  if (fix_respect) table$respect <- cbind(0, table$respect, 0)
  table <- gtable_add_grob(table, get_grob(p, 'background'), 1, 1, nrow(table), ncol(table),
                           z = -Inf, name = 'background')
  table
}

#' @importFrom utils as.roman
get_level <- function(x) {
  if (is.list(x)) {
    if (length(x[[1]]) == 1 && x[[1]] %in% c('a', 'A', '1', 'i', 'I')) {
      x <- x[[1]]
    } else {
      return(x[[1]])
    }
  }
  switch(
    as.character(x),
    a = letters,
    A = LETTERS,
    "1" = 1:100,
    i = tolower(as.roman(1:100)),
    I = as.roman(1:100),
    cli_abort('Unknown tag type: {.val {x}}')
  )
}
