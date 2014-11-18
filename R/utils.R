`%||%` <- function(x, y) if (is.null(x)) y else x

bare <- function(x) {
  attributes(x) <- NULL
  unclass(unname(x))
}

clear_environment <- function(env) {
  rm(ls(env, all = TRUE), envir = env)
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

# An S3 class that implements a stack data structure.
make_stack <- function() {
  elements <- list()
  structure(class = 'stack', list(
    clear      = function()  { elements <<- list() },
    empty      = function()  { length(elements) == 0 },
    push       = function(x) { elements[[length(elements) + 1]] <<- x },
    peek       = function(n = 1)  {
      if (isTRUE(n)) return(elements)
      els <- seq(length(elements), length(elements) - n + 1)
      if (length(els) == 1) elements[[els]]
      else elements[els]
    },
    pop        = function()  {
      if (length(elements) == 0) stop("objectdiff:::stack is empty")
      tmp <- tail(elements, 1)[[1]]
      elements[[length(elements)]] <<- NULL
      tmp
    },
    pop_all    = function()  { tmp <- elements; elements <<- list(); tmp }
  ))
}                                                                      

