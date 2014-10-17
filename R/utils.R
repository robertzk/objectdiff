`%||%` <- function(x, y) if (is.null(x)) y else x

bare <- function(x) {
  attributes(x) <- NULL
  unclass(unname(x))
}

benchmarks <- function(path, filter = '') {
  find_benchmarks <- function(path) {
    files <- list.files(pattern = '^benchmark', path, full.names = TRUE)
    files[grepl(filter, files)]
  }

  benchmarks <- find_benchmarks(
    if (missing(path)) system.file(package = 'objectdiff', 'benchmarks')
    else file.path(path, 'inst', 'benchmarks')
  )

  invisible(lapply(benchmarks, source))
}

