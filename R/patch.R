#' Patching related methods.
#' 
#' @param x function. Apply \code{patch} class.
#'
#' @name patch
#' @title patch
#' @rdname patch
as.patch <- function(x) {
  stopifnot(is.function(x))
  class(x) <- c('patch', class(x))
  x
}

#' Check if an R object is a patch.
#'
#' @param x ANY. Some R object.
#' @export
is.patch <- function(x) { inherits(x, 'patch') }

#' Check if an R object is a trivial patch.
#'
#' A trivial patch stores a fully copy of the diffed object when no heuristics
#' were found for determining object differences. In other words, it is the
#' worst scenario and should be avoided.
#'
#' @param fn function. Any function.
#' @return TRUE or FALSE according as the function is or is not a trivial patch.
is.trivial_patch <- function(fn) {
  is.patch(fn) && inherits(fn, 'trivial')
}

#' @rdname patch
identity_patch <- function() {
  patch <- function(...) ..1
  environment(patch) <- emptyenv()
  patch <- as.patch(patch)
  class(patch) <- c('identity', class(patch))
  patch
}

#' Check if an R object is an identity patch.
#'
#' The identity patch just returns the object that was passed in.
#'
#' @param fn function. Any function.
#' @return TRUE or FALSE according as the function is or is not an identity patch.
is.identity_patch <- function(fn) {
  is.patch(fn) && inherits(fn, 'identity')
}


#' @param object ANY. An R object that will be returned by the
#'    function created from \code{trivial_patch}. This is equivalent to
#'    "create a function that does nothing except return this object".
#' @rdname patch
trivial_patch <- function(object) {
  patch <- as.patch(function(...) object)
  class(patch) <- c('trivial', class(patch))
  patch
  # TODO: (RK) Use copy_env for environments on trivial_patch
}

#' Create a patch from environment injected objects and body.
#' @param provides list. Objects to inject into the 
#'   patch's environment.
#' @return A bodiless patch with parent base environment.
#' @examples
#' p <- objectdiff:::patch_template(list(a = 1), { a + object })
#' # function(object) { a + object } 
#' # with environment containing a = 1
#' stopifnot(p(1) == 2)
patch_template <- function(provides, body) {
  patch <- function(object) { }
  body(patch) <- substitute(body)
  if (length(provides) == 0) {
    environment(patch) <- new.env(parent = baseenv())
  } else {
    environment(patch) <- list2env(provides, parent = baseenv())
  }
  as.patch(patch)
}

#' Re-order names according to the names of object.
#'
#' @param object ANY. Any named R object (e.g., a named list).
#' @note This only works on uniquely named objects.
reorder_names_patch <- function(object) {
  patch_template(list(names = names(object)), { object[names] })
}

#' Generate a patch for two atomic objects that are close in values.
#'
#' @rdname patch
#' @param old_object atomic. 
#' @param new_object atomic. 
#' @param transition logical. Whether or not to use a transition depending
#'   on how many element do not match. Namely, if over 50% do not match in
#'   from a random sample of 100 elements (so most of \code{new_object} is
#'   probably different than \code{old_object}) then replace it completely
#'   with a trivial patch; otherwise, perform a more subtle calculation
#'   using \code{base::!=} and stores only exactly which elements changed.
#' @examples
#' x <- 1:10; y <- x; y[1] <- 5
#' patch <- objectdiff:::atomic_differences_patch(x, y) 
#' stopifnot(identical(y, patch(x)))
atomic_differences_patch <- function(old_object, new_object, transition = TRUE) {
  # Our first strategy is to sample 100 values and compare them.
  # If they match, the objects are "probably" the same.
  if (!isTRUE(transition))
    differences_patch(new_object, old_object, old_object != new_object)
  else {
    tested_indices <- sample(seq_len(length(new_object)), 100, replace = TRUE)

    use_trivial <- FALSE
    if (!identical(old_object[tested_indices], new_object[tested_indices])) {
      differences <- old_object[tested_indices] != new_object[tested_indices]
      # If most values are different, just patch with the new object.
      if (mean(differences) > 0.5) use_trivial <- TRUE
    }

    if (use_trivial) trivial_patch(new_object)
    else { # objects differ by a non-100% amount. Patch the differences.
      # TODO: (RK) Can we make this faster with C++? Need to be careful about 
      # attributes and class.
      differences_patch(old_object, new_object, which(old_object != new_object))
    }
  }
}

#' Generate a patch given a recording of differences between two objects.
#'
#' This patch will use another patch to record changes to attributes
#' (if any). Otherwise, given indices of changed object, it will 
#' generate a patch over those indices.
#' 
#' @rdname patch
#' @inheritParams atomic_differences_patch
#' @param differences logical or integer. The differences in first and second object.
#'   These should be calculated externally because a different approach
#'   could be used for different objects (e.g., lists versus atomic;
#'   in the former we would need \code{base::identical} on each element,
#'   whereas in the latter we could use \code{base::`!=`}).
differences_patch <- function(old_object, new_object, differences) {
  if ((is.logical(differences) && sum(differences) == 0) ||
      length(differences) == 0) {
    # Patch only attributes / class.
    attributes_patch(old_object, new_object)
  } else {
    if (is.logical(differences)) {
      # Takes up much less space for sparse differences.
      differences <- which(differences)
    }

    patch <- new('function')
    formals(patch) <- alist(object = )
    body(patch) <- quote({ object[differences] <- new_values })
    environment(patch) <- new.env(parent = baseenv())
    environment(patch)$new_values <- new_object[differences]
    environment(patch)$differences <- differences

    # If attributes do not match, patch the attributes as well.
    if (!identical(attributes(new_object), attributes(old_object))) {
      body(patch)[[3]] <- quote(patch_attributes(object))
      environment(patch)$patch_attributes <-
        attributes_patch(old_object, new_object)
    } else body(patch)[[3]] <- quote(object)

    as.patch(patch)
  }
}

#' Assume two objects are identical and only patch their attributes.
#'
#' @inheritParams atomic_differences_patch
#' @rdname patch
attributes_patch <- function(old_object, new_object) {
  attributes_patch <- trivial_patch(attributes(new_object))
  patch <- function(object) {
    attributes(object) <- attributes_patch()
    object
  }
  environment(patch) <-
    list2env(list(attributes_patch = attributes_patch), parent = baseenv())
  return(as.patch(patch))

  # TODO: (RK) Fix attributes patching. It depends on correct list diffing.
  # TODO: (RK) Fix row names patching
  ignored_attributes <- "row.names"

  patch <- function(object) {
    good <- !is.element(names(attributes(object)), ignored_attributes)
    attributes(object) <-
      c(patch_attributes(attributes(object)),
        attributes(object)[intersect(names(attributes(object)), ignored_attributes)])
    object
  }
  environment(patch) <- new.env(parent = baseenv())
  environment(patch)$ignored_attributes <- ignored_attributes

  old_good <- !is.element(names(attributes(old_object)), ignored_attributes)
  new_good <- !is.element(names(attributes(new_object)), ignored_attributes)
  environment(patch)$patch_attributes <-
    objectdiff(attributes(old_object)[old_good],
               attributes(new_object)[new_good])

  as.patch(patch)
}



