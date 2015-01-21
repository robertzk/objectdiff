#' Transform a list into another list.
#'
#' @inheritParams objectdiff
#' @include objectdiff.R
setMethod('objectdiff', signature = c('list', 'list'),
  definition = function(old_object, new_object) {
    if (identical(old_object, new_object)) { identity_patch() }
    else if (length(old_object) != length(new_object) ||
        !identical(names(old_object), names(new_object))) {
      heterogeneous_list_patch(old_object, new_object)
    } else {
      homogeneous_list_patch(old_object, new_object)
    }
  })

#' Compute a patch between two lists of different length or different names.
#' 
#' @inheritParams objectdiff
#' @return a \code{\link{patch}} translating the \code{old_object} to
#'    the \code{new_object}.
heterogeneous_list_patch <- function(old_object, new_object) {
  if ((length(old_object) != 0 && is.null(names(old_object))) ||
      (length(new_object) != 0 && is.null(names(new_object))) ||
      anyDuplicated(names(old_object)) > 0 || anyDuplicated(names(new_object)) > 0) {
    trivial_patch(new_object)
  } else {
    as.patch(compose(diff(old_object, new_object), reorder_names_patch(new_object)))
  }
}

#' Compute a patch between two same-name same-length lists.
#' 
#' @note
#' Taking the diff of two lists requires good performance on the following
#' two competing scenarios:
#'   1. Long lists with small elements
#'   2. Short lists with large elements
#' We will call the former long lists and the latter wide lists.
#' 
#' In practice, we will not have to deal with incredibly nested lists,
#' and R places stack overflow limits on these in any case. We make use of
#' this fact by performing a stochastic estimation to determine whether the
#' list is short or wide.
#'
#' In particular, we sample up to 5% of the positions in the list and recursively
#' determine their size with the sample sampling strategy using
#' utils::object.size.
#' @inheritParams objectdiff
#' @return a \code{\link{patch}} translating the \code{old_object} to
#'    the \code{new_object}.
homogeneous_list_patch <- function(old_object, new_object) {
  approximate_size <- estimate_size(old_object)
  wide <- approximate_size / length(old_object) > 100

  # If the new list is relatively small (under 1000 bytes), just replace it
  # directly
  if (approximate_size < 1000) return(trivial_patch(new_object))

  # Determine how many list elements differ.
  # TODO: (RK) Figure out if there is a faster way (C++?).
  # This takes 50ms on a 20k element list!
  differ <- !mapply(identical, old_object, new_object) # Note they are the same length.

  if (mean(differ) > 0.5 && !wide) {
    # If most differ in long list, just replace outright.
    trivial_patch(new_object)
  } else {
    # For wide lists, it may always be beneficial to use a differences patch.
    differences_patch(old_object, new_object, differ)
  }
}

#' Estimate the size of a list stochastically.
#'
#' We assume each element in the list is approximately the same size,
#' sample some small percentage, and multiply by that amount.
#'
#' @param list The list object whose size we are estimating.
#' @param sampling_percentage numeric. Default is \code{0.05}.
estimate_size <- function(list, sampling_percentage = 0.05) {
  if (!is.list(list)) { object.size(list) }
  else if ((len <- length(list)) <= 10) { object.size(list) }
  else {
    chunk <- max(10, ceiling(len * sampling_percentage))
    sum(vapply(list[sample.int(len, size = chunk, replace = TRUE)],
               estimate_size, double(1))) * len / chunk
  }
}

