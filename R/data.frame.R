#' Record what changes have been made to a tracked environment.
#'
#' @inheritParams objectdiff
#' @include objectdiff.R
setMethod('objectdiff', signature = c('data.frame', 'data.frame'),
  definition = function(old_object, new_object) {
    if (NROW(old_object) < NROW(new_object)) { 
      trivial_patch(new_object)
    } else if (NROW(old_object) > NROW(new_object)) {
      # TODO: (RK) Use C++ for this, since it is probably slow. 
      old_rownames <- row.names(old_object)
      new_rownames <- row.names(new_object)
      if (length(setdiff(new_rownames, old_rownames)) == 0) {
        # The new row names are a subset of the old row names.

        # If columns *were* modified, just do a full trivial patch or the results
        # will certainly be wrong.
        if (length(old_object) != length(new_object)) {
          trivial_patch(new_object)
        } else {
          # To save on time, we will assume the rows themselves were
          # not modified, or a full check could take a long time.
          warning("Row dropping detected. Diff may not be accurate.", call. = FALSE)

          trivial_patch(new_object)
        }
      } else {
        trivial_patch(new_object)
      }
    } else {
      getMethod("objectdiff", c("list", "list"))(old_object, new_object)
    }
  })

