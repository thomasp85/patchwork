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
on_load(
  register_s3_method("vdiffr", "print_plot", "patchwork")
)
