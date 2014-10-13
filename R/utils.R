`%||%` <- function(x, y) if (is.null(x)) y else x

identity_patch <- function(object) as.patch(function(...) ..1)
trivial_patch <- function(object) as.patch(function(...) object)

as.patch <- function(x) {
  stopifnot(is.function(x))
  class(x) <- c('patch', class(x))
  x
}
