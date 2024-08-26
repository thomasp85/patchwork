
collect_axis_titles <- function(gt, dir = "x", merge = TRUE) {

  names <- paste0(dir, "lab", switch(dir, x = c("-t", "-b"), y = c("-l", "-r")))

  delete <- integer()

  for (name in names) {

    # Find titles
    idx <- which(grepl(paste0("^", name), gt$layout$name))
    if (length(idx) < 2) {
      # No titles to collapse, leave as-is
      next
    }

    if (all(is_zero(gt$grobs[idx]))) {
      # No need to bother with non-existing titles
      next
    }

    # We want patches to be able to break title runs
    patch_index <- grep("panel-nested-patchwork", gt$layout$name)

    # Simplify layout of grobs to matrix
    layout <- grob_layout(gt, c(idx, patch_index))
    nested <- layout %in% patch_index
    layout[nested] <- NA # Remove patches

    # Mark duplicated grobs
    structure <- grob_id(gt$grobs, layout, byrow = dir == "x", merge = merge, unpack = TRUE)

    # If all title grobs are unique, there is nothing to collapse
    if (anyDuplicated(structure[!is.na(structure)]) == 0) {
      next
    }

    structure[nested] <- 0

    # Identify 'run'-rectangles in the structure
    runs <- rle_2d(structure, byrow = dir == "y", ignore.na = TRUE)
    runs <- runs[!is.na(runs$value) & runs$value != 0, , drop = FALSE]

    # Get all panels in each run and put the keeper first
    panels <- lapply(seq_len(nrow(runs)), function(i) {
      rows <- runs$row_start[i]:runs$row_end[i]
      cols <- runs$col_start[i]:runs$col_end[i]
      first <- switch(name,
        "xlab-t" = layout[runs$row_start[i], cols],
        "xlab-b" = layout[runs$row_end[i], cols],
        "ylab-l" = layout[rows, runs$col_start[i]],
        "ylab-r" = layout[rows, runs$col_end[i]]
      )
      first <- first[!is.na(first)][1]
      panels <- as.vector(layout[rows , cols])
      panels <- panels[!is.na(panels)]
      unique(c(first, panels))
    })

    title_grob <- vapply(panels, `[[`, numeric(1), 1)

    # Mark every non-start grob for deletion
    delete <- c(delete, setdiff(idx, title_grob))

    if ((dir == "x" && all(runs$col_start == runs$col_end)) ||
        (dir == "y" && all(runs$row_start == runs$row_end))) {
      next
    }

    # Stretch titles over span
    if (dir == "y") {
      gt$layout$t[title_grob] <- vapply(panels, function(i) min(gt$layout$t[i]), numeric(1))
      gt$layout$b[title_grob] <- vapply(panels, function(i) max(gt$layout$b[i]), numeric(1))
      gt$layout$z[title_grob] <- max(gt$layout$z[idx])
    } else {
      gt$layout$l[title_grob] <- vapply(panels, function(i) min(gt$layout$l[i]), numeric(1))
      gt$layout$r[title_grob] <- vapply(panels, function(i) max(gt$layout$r[i]), numeric(1))
      gt$layout$z[title_grob] <- max(gt$layout$z[idx])
    }
  }
  delete_grobs(gt, delete)
}

# Very similar to `collect_titles`, except there is no merging step involved
# and rows/columns are resized afterwards.
collect_axes <- function(gt, dir = "x") {

  if (dir == "x") {
    names <- c("axis-b", "axis-t")
  } else {
    names <- c("axis-l", "axis-r")
  }

  delete <- integer()

  for (name in names) {

    # Find axes
    idx <- which(grepl(paste0("^", name), gt$layout$name))
    if (length(idx) < 2) {
      # No axes to collapse, leave as-is
      next
    }

    if (all(is_zero(gt$grobs[idx]))) {
      # No need to bother with non-existing axes
      next
    }

    # We want patches to be able to break axis runs
    patch_index <- grep("panel-nested-patchwork", gt$layout$name)

    # Simplify layout of grobs to matrix
    layout <- grob_layout(gt, c(idx, patch_index))
    layout[layout %in% patch_index] <- NA # Remove patches

    # Mark duplicated grobs
    structure <- grob_id(gt$grobs, layout, byrow = dir == "x", merge = FALSE)

    # If all grobs are unique, there is nothing to collapse
    if (anyDuplicated(structure[!is.na(structure)]) == 0) {
      next
    }

    # Identify 'run'-rectangles in the structure
    runs <- rle_2d(structure, byrow = dir == "y")
    runs <- runs[!is.na(runs$value), , drop = FALSE]

    # Find first grob in run
    start_runs <- c("row_start", "col_start")
    if (name == "axis-b") start_runs[1] <- "row_end"
    if (name == "axis-r") start_runs[2] <- "col_end"
    start_idx <- layout[as.matrix(runs[, start_runs])]

    # Mark every non-start grob for deletion
    delete <- c(delete, setdiff(idx, start_idx))
  }

  deleted_rows <- unique(c(gt$layout$t[delete], gt$layout$b[delete]))
  deleted_cols <- unique(c(gt$layout$l[delete], gt$layout$r[delete]))

  new <- delete_grobs(gt, delete)
  new <- retrofit_rows(new, deleted_rows, pattern = "^axis")
  new <- retrofit_cols(new, deleted_cols, pattern = "^axis")
  new
}

# For every given row, check if all non-zero grobs occupying that row have a
# name that has a pattern. If all these grobs in that row do, measure the
# grob heights and put that into the gtable's heights.
#' @importFrom ggplot2 max_height
retrofit_rows <- function(gt, rows, pattern = NULL) {
  if (is.null(pattern) || length(rows) == 0) {
    return(gt)
  }

  # zeroGrobs are ignored for fitting
  layout <- gt$layout[!is_zero(gt$grobs), , drop = FALSE]

  # Grab grob index and their rows
  grob_idx <- which(layout$t %in% rows | layout$b %in% rows)
  row_idx  <- layout$t[grob_idx] # 'layout$b' is ignored, but that is probably fine

  # Check if any grob in row does not have the pattern.
  # If all grobs in a row have the pattern, include for resizing
  is_pattern <- grepl(pattern, layout$name[grob_idx])
  resize_row <- rowsum(as.integer(!is_pattern), group = row_idx) == 0
  resize_row <- as.integer(rownames(resize_row)[resize_row[, 1]])

  # Do resizing
  for (row in resize_row) {
    grobs <- gt$grobs[gt$layout$t == row | gt$layout$b == row]
    size  <- max_height(grobs[!is_zero(grobs)])
    gt$heights[row] <- size
  }
  gt
}

# For every given column, check if all non-zero grobs occupying that column
# have a name that has a pattern. If all these grobs in that column do, measure
# the grob widths and put that into the gtable's widths.
#' @importFrom ggplot2 max_width
retrofit_cols <- function(gt, cols, pattern = NULL) {
  if (is.null(pattern) || length(cols) == 0) {
    return(gt)
  }

  # zeroGrobs are ignored for fitting
  layout <- gt$layout[!is_zero(gt$grobs), , drop = FALSE]

  # Grab grob index and their columns
  grob_idx <- which(layout$l %in% cols | layout$r %in% cols)
  col_idx  <- layout$l[grob_idx] # 'layout$r' is ignored, but that is probably fine

  # Check if any grob in column does not have the pattern.
  # If all grobs in a column have the pattern, include for resizing
  is_pattern <- grepl(pattern, layout$name[grob_idx])
  resize_col <- rowsum(as.integer(!is_pattern), group = col_idx) == 0
  resize_col <- as.integer(rownames(resize_col)[resize_col[, 1]])

  # Do resizing
  for (col in resize_col) {
    grobs <- gt$grobs[gt$layout$l == col | gt$layout$r == col]
    size  <- max_width(grobs[!is_zero(grobs)])
    gt$widths[col] <- size
  }
  gt
}

# Delete grobs from the gtable while preserving dimensions.
# If a row or column in the gtable becomes empty, optionally set size to 0.
delete_grobs <- function(gt, idx, resize = TRUE) {
  if (length(idx) == 0) {
    return(gt)
  }

  if (resize) {
    # Candidate rows/cols for resizing
    resize_rows <- unique(gt$layout[idx, "t"])
    resize_cols <- unique(gt$layout[idx, "l"])
  }

  gt$layout <- gt$layout[-idx, , drop = FALSE]
  gt$grobs  <- gt$grobs[-idx]

  if (!resize) {
    return(gt)
  }

  # Only resize rows/columns that don't have any (non-zero) grobs associated
  # with them.
  # Note that this ignores grobs that 'span' the rows/columns, but these are
  # typically background rectangles.
  zero <- is_zero(gt$grobs)
  resize_rows <- setdiff(resize_rows, unlist(gt$layout[!zero, c("t", "b")]))
  resize_cols <- setdiff(resize_cols, unlist(gt$layout[!zero, c("l", "r")]))

  if (length(resize_rows) > 0) {
    gt$heights[resize_rows] <- unit(0, "pt")
  }
  if (length(resize_cols) > 0) {
    gt$widths[resize_cols] <- unit(0, "pt")
  }
  gt
}

# Check if 'x' is 'empty': a zeroGrob or NULL
is_zero <- function(x) {
  if (is_bare_list(x)) {
    vapply(x, inherits, logical(1), what = "zeroGrob") | lengths(x) == 0
  } else {
    is.null(x) || inherits(x, "zeroGrob")
  }
}

# Determine uniqueness of grobs
#' @importFrom stats ave
grob_id <- function(grobs, layout, byrow, merge = FALSE, unpack = FALSE) {

  # Hash the grobs to determine unique grobs
  valid <- !is.na(layout)
  idx  <- as.vector(layout)[valid]
  hash <- vapply(grobs[idx], function(x) {
    if (unpack && inherits(x, "gtable") && length(x$grobs) == 1) {
      x <- x$grobs[[1]]
    }
    hash(unname_grob(x))
  }, character(1))

  # For multi-cell grobs, compute an extra identifier
  if (!merge) {
    index <- if (byrow) col(layout) else row(layout)
    min <- ave(index, layout, FUN = min)
    max <- ave(index, layout, FUN = max)
    identifier <- paste0(min, ";", max)
    # Include the multi-cell identifier in the hash
    hash <- paste0(hash, identifier[valid])
  }

  layout[valid] <- match(hash, unique(hash))
  layout
}

# Representing grob indices in a simplified layout matrix
# Assumes cell can be uniquely mapped to a grob, so no overlapping grobs
grob_layout <- function(gt, idx) {

  layout <- gt$layout[idx, , drop = FALSE]
  top    <- sort(unique(c(layout$t, layout$b)))
  left   <- sort(unique(c(layout$l, layout$r)))

  new <- matrix(NA_integer_, length(top), length(left))

  # Account for fact that grobs may span multiple cells
  right  <- match(layout$r, left)
  bottom <- match(layout$b, top)
  top    <- match(layout$t, top)
  left   <- match(layout$l, left)

  for(i in seq_along(idx)) {
    new[top[i]:bottom[i], left[i]:right[i]] <- idx[i]
  }
  new
}

# Backports of hash table functionality
hashtab <- function(type, size) {
  new_environment()
}
gethash <- function(h, key, nomatch = NULL) {
  get0(hash(key), envir = h, ifnotfound = nomatch)
}
sethash <- function(h, key, value) {
  assign(hash(key), value, envir = h)
}
on_load({
  if ("hashtab" %in% getNamespaceExports("utils")) {
    hashtab <- utils::hashtab
  }
  if ("gethash" %in% getNamespaceExports("utils")) {
    gethash <- utils::gethash
  }
  if ("sethash" %in% getNamespaceExports("utils")) {
    sethash <- utils::sethash
  }
})

# 2D equivalent of run-length encoding.
# Essentially, it tries to look for rectangular arrangements of cells in a
# matrix that have the same values, and reports back their positions.
#
# Worked example:
#
# # Let's say we have the following matrix
# (m <- matrix(c(1, 1, 2, 1, 1, 2, 3, 3, 1), 3,  3))
# #>      [,1] [,2] [,3]
# #> [1,]    1    1    3
# #> [2,]    1    1    3
# #> [3,]    2    2    1
#
# # The `rle_2d()` function finds the `i` and `j` arguments that define the
# # rectangular areas with the same values. For this example so this finds:
# # m[1:2, 1:2], m[1:2, 3], m[3, 1:2] and m[3, 3] as runs.
#
# rle_2d(m)
# #>   col_start col_end row_start row_end value
# #> 1         1       2         1       2     1
# #> 2         1       2         3       3     2
# #> 5         3       3         1       2     3
# #> 6         3       3         3       3     1
rle_2d <- function(m, byrow = FALSE, ignore.na = FALSE) {

  n <- length(m)

  # Return 0-row data.frame if matrix is empty
  if (n == 0L) {
    ans <- data.frame(
      col_start = integer(),
      col_end   = integer(),
      row_start = integer(),
      row_end   = integer(),
      value     = as.vector(m)
    )
    return(ans)
  }

  if (isTRUE(byrow)) {
    m <- t(m)
    rename <- function(x) {
      names(x) <- c("row_start", "row_end", "col_start", "col_end", "value")
      rownames(x) <- NULL
      x
    }
  } else {
    rename <- function(x) {
      rownames(x) <- NULL
      x
    }
  }

  dim <- dim(m)
  levels <- unique(as.vector(m))

  # Simplified case when there is just a single level
  if ((ignore.na && sum(!is.na(levels)) == 1) || length(levels) == 1L) {
    ans <- data.frame(
      col_start = 1L,
      col_end   = dim[2],
      row_start = 1L,
      row_end   = dim[1],
      value     = sort(levels, na.last = TRUE)[1]
    )
    return(rename(ans))
  }

  # Simplified case when all levels are different
  if (length(levels) == n) {
    col <- as.vector(col(m))
    row <- as.vector(row(m))
    ans <- data.frame(
      col_start = col,
      col_end   = col,
      row_start = row,
      row_end   = row,
      value     = as.vector(m)
    )
    return(rename(ans))
  }

  # Treat matrix content as levels, so we can deal with NAs
  m <- matrix(match(m, levels), nrow(m), ncol(m))

  # Simplified case when m has only a single row
  if (dim[1] == 1L) {
    rle  <- rle(as.vector(m))
    ends <- cumsum(rle$lengths)
    ans <- data.frame(
      col_start = ends - rle$lengths + 1,
      col_end   = ends,
      row_start = 1L,
      row_end   = 1L,
      value     = levels[rle$values]
    )
  }

  # Run length encoding by column
  #                classic RLE            column-wise RLE
  #            |------------------|   |----------------------|
  ends    <- c(which(m[-1] != m[-n] | (row(m) == nrow(m))[-n]), n)
  lengths <- diff(c(0L, ends))
  values  <- m[ends]
  starts  <- ends - lengths + 1L

  # Simplified case when m has only a single column
  if (dim[2] == 1L) {
    ans <- data.frame(
      col_start = 1L,
      col_end   = 1L,
      row_start = starts,
      row_end   = ends,
      value     = levels[values]
    )
    return(rename(ans))
  }

  # Translate to indices
  # `col_end` is initialised as `col_start` but will be updated throughout
  # the coming for-loop
  row_start <- arrayInd(starts, dim)[, 1]
  row_end   <- row_start + lengths - 1L
  col_start <- col_end <- arrayInd(ends, dim)[, 2]

  # Initialise hash table no longer than number of runs
  # Inspiration for using hash tables for this problem taken from TimTaylor:
  # https://fosstodon.org/@_TimTaylor/111266682218212785
  htab <- hashtab("identical", size = length(values))

  for (i in seq_along(values)) {

    # Lookup if there has been a similar column
    key <- c(row_start[i], row_end[i], values[i])
    hsh <- gethash(htab, key)


    if (!is.null(hsh) && col_start[i] == col_end[hsh] + 1L) {
      # Matches run in previous column, merge by updating column end
      # and deleting current run (NA value will be filtered out later)
      col_end[hsh] <- col_start[i]
      values[i] <- NA_integer_
    } else {
      # Add run-index to the table
      sethash(htab, key, i)
    }
  }

  ans <- data.frame(
    col_start = col_start,
    col_end   = col_end,
    row_start = row_start,
    row_end   = row_end,
    value     = levels[values]
  )[!is.na(values), , drop = FALSE]

  rename(ans)
}
