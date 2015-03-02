#' Record what changes have been made to a tracked environment.
#'
#' @inheritParams objectdiff
#' @include objectdiff.R diff.R diff_list.R
setMethod('objectdiff', signature = c('data.frame', 'data.frame'),
  definition = function(old_object, new_object) {
    # TODO: (RK) Don't break on 0-column data.frames
    old_nrow <- length(old_object[[1]])
    # Note that NROW gives the wrong answer if row.names are artificially changed
    new_nrow <- length(new_object[[1]])

    if (old_nrow < new_nrow) { 
      trivial_patch(new_object)
    } else if (old_nrow > new_nrow) {
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

          # TODO: (RK) This will fail on row duplication.
          subset <- match(row.names(new_object), row.names(old_object))
          patch_template(list(subset = subset), { object[subset, ] })
        }
      } else {
        trivial_patch(new_object)
      }
    } else {
      getMethod("objectdiff", c("list", "list"))(old_object, new_object)
    }
  })

#' @export
deletions.data.frame     <- deletions.list
#' @export
additions.data.frame     <- additions.list
#' @export
modifications.data.frame <- modifications.list

