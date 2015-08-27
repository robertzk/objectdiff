#' Transform an atomic vector into an atomic vector.
#' 
#' @inheritParams objectdiff
#' @include objectdiff.R
setMethod("objectdiff", signature = c("numeric", "numeric"),
  definition = atomic_diff <- function(old_object, new_object) {
    # TODO: (RK) Improve these heuristics.
    if (identical(old_object, new_object)) {
      identity_patch()
    } else if (length(old_object) != length(new_object) ||
             length(new_object) < 200) {
      trivial_patch(new_object)
    } else {
      atomic_differences_patch(old_object, new_object)
    }
  })

atomic_types <- c("character", "logical", "raw", "complex")
for (type in atomic_types) {
  setMethod("objectdiff", signature = rep(type, 2), definition = atomic_diff)
}

