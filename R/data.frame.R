#' Record what changes have been made to a tracked environment.
#'
#' @inheritParams objectdiff
#' @include objectdiff.R
setMethod('objectdiff', signature = c('data.frame', 'data.frame'),
  definition = function(old_object, new_object) {
    if (NROW(old_object) != NROW(new_object)) { 
    }
    getMethod("objectdiff", c("list", "list"))(old_object, new_object)
  })

