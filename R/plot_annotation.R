#' Annotate the final plot assemble
#'
#' The result of this function can be added to an assemble using `+` in the same
#' way as [plot_layout()], but unlike [plot_layout()] it will only have an
#' effect on the top level plot. As the name suggests it controls different
#' aspects of the annotation of the final plot, such as
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
  if (!is.ggassemble(plot)) stop('plot_annotation must be added to an assemble of plots', call. = FALSE)
  theme <- plot$assemble$annotation$theme + object$theme
  plot$assemble$annotation <- modifyList(plot$assemble$annotation, object[!vapply(object, is.null, logical(1))])
  plot
}
#' @importFrom ggplot2 is.ggplot labs
recurse_tags <- function(x, levels, prefix, suffix, sep, offset = 1) {
  if (length(levels) == 0) return(list(assemble = x, tab_ind = offset))
  level <- switch(
    as.character(levels[1]),
    a = letters,
    A = LETTERS,
    "1" = 1:100,
    i = tolower(as.roman(1:100)),
    I = as.roman(1:100),
    stop('Unknown tag type: ', levels[1], call. = FALSE)
  )
  assemble <- x$assemble$plots
  tag_ind <- offset
  for (i in seq_along(assemble)) {
    if (is.ggassemble(assemble[[i]])) {
      tag_level <- assemble[[i]]$assemble$layout$tag_level
      tag_level <- if (is.null(tag_level)) default_layout$tag_level else tag_level
      if (tag_level == 'keep') {
        new_plots <- recurse_tags(assemble[[i]], levels, prefix, suffix, sep, tag_ind)
        assemble[[i]] <- new_plots$assemble
        tag_ind <- new_plots$tag_ind
      } else {
        assemble[[i]] <- recurse_tags(assemble[[i]], levels[-1],
                                            prefix = paste0(prefix, level[tag_ind], sep),
                                            suffix, sep)$assemble
      }
    } else {
      assemble[[i]] <- assemble[[i]] + labs(tag = paste0(prefix, level[tag_ind], suffix))
      if ((is.ggplot(assemble[[i]]) && !is.empty(assemble[[i]])) ||
          (is.el_wrapper(assemble[[i]]) && !attr(assemble[[i]], 'settings')$ignore_tag)) {
        tag_ind <- tag_ind + 1
      }
    }
  }
  x$assemble$plots <- assemble
  x <- x + labs(tag = paste0(prefix, level[tag_ind], suffix))
  if ((is.ggplot(x) && !is.empty(x)) || (is.el_wrapper(x) && !attr(x, 'settings')$ignore_tag)) {
    tag_ind <- tag_ind + 1
  }
  list(
    assemble = x,
    tag_ind = tag_ind
  )
}
