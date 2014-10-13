#' Transform a numeric into a numeric.
#' 
#' @include objectdiff.R
setMethod('objectdiff', signature = c('numeric', 'numeric'),
  definition = function(old_object, new_object) {
    if (length(old_object) != length(new_object)) trivial_patch(new_object)
  })
