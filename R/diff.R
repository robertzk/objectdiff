#' Compute a patch that takes a diff of a recursive object (list or tracked_environment).
#'
#' @param old_object list or tracked_environment.
#' @param new_object list or tracked_environment.
diff <- function(old_object, new_object) {
  list(deletions = deletions, modifications = modifications, additions = additions) %>%
    invoke(old_object, new_object) %>%
    Filter(f = Negate(is.identity_patch)) %>%
    map_call(compose)
}

#' Compute a patch of deletions on a recursive object.
#' 
#' @inheritParams objectdiff
deletions <- function(old_object, new_object) {
  UseMethod("deletions")
}

deletions.tracked_environment <- function(old_object, new_object) {
  deletions <- setdiff(new_object%$%universe, ls(new_object, all = TRUE))

  if (length(deletions) == 0) {
    return(identity_patch())
  } else {
    patch_template(list(deletions = deletions), {
      rm(list = deletions, envir = object)
    })
  }
}

#' Compute a patch of modifications on a recursive object.
#' 
#' @inheritParams objectdiff
modifications <- function(old_object, new_object) {
  UseMethod("modifications")
}

modifications.tracked_environment <- function(old_object, new_object) {
  num_changed <- length(ls(new_object%$%ghost, all = TRUE))

  if (num_changed == 0) { identity_patch() } 
  else {
    ghost <- new_object%$%ghost
    deletions <- setdiff(new_object%$%universe, ls(new_object, all = TRUE))
    additions <- setdiff(ls(new_object, all = TRUE), new_object%$%universe)
    changed_objects <- setdiff(ls(ghost, all = TRUE), c(deletions, additions))
    change_patches <- setNames(nm = changed_objects,
      lapply(changed_objects, function(obj) {
        objectdiff(ghost[[obj]], new_object[[obj]])
      }))

    if (length(changed_objects > 0)) {
      patch_template(list(change_patches = change_patches), {
        for (patch in names(change_patches)) {
          object[[patch]] <- change_patches[[patch]](object[[patch]])
        }
      })
    } else { identity_patch() } 
  }
}


#' Compute a patch of additions on a recursive object.
#' 
#' @inheritParams objectdiff
additions <- function(old_object, new_object) {
  UseMethod("additions")
}

additions.tracked_environment <- function(old_object, new_object) {
  num_changed <- length(ls(new_object%$%ghost, all = TRUE))

  if (num_changed == 0) { identity_patch() } 
  else {
    additions <- setdiff(ls(new_object, all = TRUE), new_object%$%universe)
    patch_template(list(new_objects = setNames(nm = additions, 
      lapply(additions, function(name) new_object[[name]]))), {
      for (obj in names(new_objects)) {
        object[[obj]] <- new_objects[[obj]]
      }
    })
  }
}

