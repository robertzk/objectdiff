`%||%` <- function(x, y) if (is.null(x)) y else x

identity_patch <- function(object) {
  patch <- function(...) ..1
  environment(patch) <- emptyenv()
  patch
}
trivial_patch <- function(object) as.patch(function(...) object)

as.patch <- function(x) {
  stopifnot(is.function(x))
  class(x) <- c('patch', class(x))
  x
}

bare <- function(x) {
  attributes(x) <- NULL
  unclass(unname(x))
}

benchmarks <- function(path) {
  if (missing(path)) {
    # TODO: (RK) Run all benchmarks in installed package.
  } else {
    # TODO: (RK) Run all benchmarks in dev mode.
  }
}
