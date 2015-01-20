#' Record what changes have been made to a tracked environment.
#'
#' @inheritParams objectdiff
#' @include objectdiff.R tracked_environment.R
setMethod('objectdiff', signature = c('tracked_environment', 'tracked_environment'),
  definition = function(old_object, new_object) {
    if (!identical(old_object, new_object)) {
      stop("tracked_environments can only be diffed against themselves")
    }

    diff(old_object, new_object)
  })

 

