`%||%` <- function(x, y) if (is.null(x)) y else x

trivial_patch <- function(object) as.patch(function(...) object)

as.patch <- function(x) {
  stopifnot(is.function(x))
  class(x) <- c('patch', class(x))
  x
}
