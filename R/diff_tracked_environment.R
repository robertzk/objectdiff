#' Record what changes have been made to the latest tracked environment.
#'
#' @inheritParams objectdiff
#' @include objectdiff.R tracked_environment.R
setMethod('objectdiff', signature = c('tracked_environment', 'tracked_environment'),
  definition = function(old_object, new_object) {
    if (!identical(old_object, new_object))
      stop("tracked_environments can only be diffed against themselves")

    setdiff(new_object%$%universe, ls(new_object, all = TRUE))

    
  })

 

