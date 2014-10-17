#' Transform a list into another list.
#'
#' @inheritParams objectdiff
#' @include objectdiff.R
setMethod('objectdiff', signature = c('list', 'list'),
  definition = function(old_object, new_object) {
    # Taking the diff of two lists requires good performance on the following
    # two competing scenarios:
    #   1. Long lists with small elements
    #   2. Short lists with large elements
    # We will call the former long lists and the latter wide lists.
    # 
    # In practice, we will not have to deal with incredibly nested lists,
    # and R places stack overflow limits on these in any case. We make use of
    # this fact by performing a stochastic estimation to determine whether the
    # list is short or wide.
    #
    # In particular, we sample up to positions in the list and recursively
    # determine their size with the sample sampling strategy using
    # utils::object.size.
    approximate_size <- estimate_size(old_object)

    trivial_patch(new_object)
  })

#' Estimate the size of a list stochastically.
#'
#' We assume each element in the list is approximately the same size,
#' sample some small percentage, and multiply by that amount.
#'
#' @param list list
#' @param sampling_percentage numeric. Default is \code{0.05}.
estimate_size <- function(list, sampling_percentage = 0.05) {
  if (!is.list(list)) object.size(list)
  else if ((len <- length(list)) <= 10) object.size(list)
  else {
    chunk <- 10 # max(10, ceiling(len * sampling_percentage))
    sum(vapply(list[sample.int(len, size = chunk, replace = TRUE)],
               estimate_size, double(1))) * len / chunk
  }
}

