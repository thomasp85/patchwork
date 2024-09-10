.onLoad <- function(...) {
  run_on_load()
}

print_plot.patchwork <- function(p, title = '') {
  if (is.null(p$patches$annotation$title)) {
    p <- p + plot_annotation(title = title)
  }
  print(p)
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  check_string(pkg)
  check_string(generic)
  check_string(class)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    check_function(fun)
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

unitType <- function(x) {
  unit <- attr(x, "unit")
  if (!is.null(unit)) {
    return(unit)
  }
  if (is.list(x) && is.unit(x[[1]])) {
    unit <- vapply(x, unitType, character(1))
    return(unit)
  } else if ("fname" %in% names(x)) {
    return(x$fname)
  }
  rep("", length(x)) # we're only interested in simple units for now
}

is_abs_unit <- function(x) {
  unitType(x) %in% c("cm", "inches", "mm", "points", "picas", "bigpts", "dida", "cicero", "scaledpts")
}

on_load({
  register_s3_method("vdiffr", "print_plot", "patchwork")
  if ("unitType" %in% getNamespaceExports("grid")) {
    unitType <- grid::unitType
  }
})
