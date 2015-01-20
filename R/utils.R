`%||%` <- function(x, y) if (is.null(x)) y else x

bare <- function(x) {
  attributes(x) <- NULL
  unclass(unname(x))
}

clear_environment <- function(env) {
  rm(list = ls(env, all = TRUE), envir = env)
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
# This is not a proper stack, but supports the ability to provide a pointer
# to the current "head".
make_stack <- function() {
  elements <- list()
  head <- 0
  structure(class = 'stack', list(
    clear      = function()  { elements <<- list() },
    empty      = function()  { length(elements) == 0 },
    push       = function(x) { elements[[head <<- head + 1]] <<- x },
    peek       = function(n = 1)  {
      if (isTRUE(n)) return(elements)
      els <- seq(length(elements), length(elements) - n + 1)
      if (length(els) == 1) elements[[els]]
      else elements[els]
    },
    peek_all   = function() { elements },
    count      = function() { length(elements) },
    pop        = function()  {
      if (head == 0) stop("objectdiff:::stack is empty")
      tmp <- elements[[head]]
      head <<- head - 1
      elements[[head + 1]] <<- NULL
      tmp
    },
    pop_all    = function()  { tmp <- elements; elements <<- list(); head <<- 0; tmp },
    head       = function() { head },
    set_head   = function(new_head) { head <<- new_head }
  ))
}                                                                      

#' Copy one environment into another recursively.
#' 
#' @name copy_env
#' @param to environment. The new environment.
#' @param from environment. The old environment.
copy_env <- function(to, from) {
  stopifnot(is.environment(to) && is.environment(from))
  rm(list = ls(to, all.names = TRUE), envir = to)
  for (name in ls(from, all.names = TRUE)) {
    if (is.environment(from[[name]])) {
      # Copy a sub-environment in full.
      assign(name, new.env(parent = parent.env(from[[name]])), envir = to) 
      copy_env(to[[name]], from[[name]])
    } else assign(name, from[[name]], envir = to)
  }
}

# Compose multiple functions into one.
# @examples
# fn1 <- function(x) x + 1
# fn2 <- function(x) x ^ 2
# compose(fn1, fn2)(1) # will be (1+1)^2 = 4
compose <- function(...) {
  funs <- list(...)
  function(z) { Reduce(function(y, w) w(y), funs, z) }
}

