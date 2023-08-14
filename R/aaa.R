# Standard gtable layout
TABLE_ROWS <- 18
TABLE_COLS <- 15
PANEL_ROW <- 10
PANEL_COL <- 8
PLOT_TOP <- 7
PLOT_BOTTOM <- 13
PLOT_LEFT <- 5
PLOT_RIGHT <- 11
TITLE_ROW <- 3
SUBTITLE_ROW <- 4
CAPTION_ROW <- 16

GUIDE_RIGHT <- 13
GUIDE_LEFT <- 3
GUIDE_TOP <- 5
GUIDE_BOTTOM <- 15

patchwork_namespace_link <- function() NULL

check_object <- function(x,
                         check_fun,
                         what,
                         ...,
                         allow_null = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {

  if (!missing(x)) {
    if (check_fun(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    what,
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
