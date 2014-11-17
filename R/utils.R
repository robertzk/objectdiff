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

# A test helper for comparing patched to actual.
expect_diff <- function(x, y, small) {
  testthat::expect_identical((patch <- objectdiff(x, y))(x), y)
  
  if (!missing(small)) {
    if (isTRUE(small)) small <- 1000

    if (identical(small, FALSE)) {
      testthat::expect_more_than(object.size(environment(patch)), 10000)
    } else {
      stopifnot(is.numeric(small))
      testthat::expect_less_than(object.size(environment(patch)), small)
    }
  }
}


