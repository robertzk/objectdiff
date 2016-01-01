#' Compute a patch that takes a diff of a recursive object (list or tracked_environment).
#'
#' @param old_object list or tracked_environment.
#' @param new_object list or tracked_environment.
diff <- function(old_object, new_object) {
  patches <- list(deletions = deletions, modifications = modifications,
                    additions = additions)
  patches <- structure(names = names(patches),
    lapply(patches, do.call, list(old_object, new_object)))
  as.patch(do.call(compose, Filter(Negate(is.identity_patch), patches)))
}

#' Compute a patch of deletions on a recursive object.
#' 
#' @inheritParams objectdiff
#' @export
deletions <- function(old_object, new_object) {
  UseMethod("deletions")
}

#' Compute a patch of modifications on a recursive object.
#' 
#' @inheritParams objectdiff
#' @export
modifications <- function(old_object, new_object) {
  UseMethod("modifications")
}

#' Compute a patch of additions on a recursive object.
#' 
#' @inheritParams objectdiff
#' @export
additions <- function(old_object, new_object) {
  UseMethod("additions")
}

