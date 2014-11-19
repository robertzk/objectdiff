#' Transform an environment into another environment.
#'
#' @inheritParams objectdiff
#' @include objectdiff.R
setMethod('objectdiff', signature = c('environment', 'environment'),
  definition = function(old_object, new_object) {
    if (identical(old_object, new_object)) return(identity_patch())

    # TODO: (RK) Finish this.
    trivial_patch(new_object)
  })
 
