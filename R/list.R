#' Transform a list into another list.
#'
#' @inheritParams objectdiff
#' @include objectdiff.R
setMethod('objectdiff', signature = c('list', 'list'),
  definition = atomic_diff <- function(old_object, new_object) {
    if (identical(old_object, new_object)) identity_patch()
    else if (length(old_object) != length(new_object) ||
             object.size(new_object) < 5000) trivial_patch(new_object)
    else atomic_differences_patch(old_object, new_object)
  })
