#' A patching function to transform one R object into another.
#' 
#' The base class for all results of the \code{objectdiff} function.
#' For example, \code{objectdiff(data1, data2)} will return a function that
#' converts \code{data1} into \code{data2}, and that function will be of class
#' \code{patch}.
#' 
#' @docType class
#' @name patch-class
#' @family patching functions
#' @export
setClass('patch', contains = 'function')

#' Generate patch to turn one R object into another.
#'
#' \code{objectdiff} is the central method used to generate "patches",
#' closures that record a minimal amount of information to convert one
#' object to another. 
#'
#' @param old_object ANY. The "before" object.
#' @param new_object ANY. The "new" object. These are usually a data.frame or
#'   an environment.
#' @return a function that can transform \code{old_object} into \code{new_object},
#'   but tries to keep memory foot print minimal. For example, if both are two
#'   data.frame's of 100,000s of rows and 1,000s of columns, but the only change
#'   made was the dropping of one column, the patching function will record
#'   only that information and not a full copy of both datasets.
#' @import methods
#' @export
#' @examples
#' \dontrun { # TODO: (RK) Run these when package is complete.
#' iris2 <- iris[-1]
#' stopifnot(identical(objectdiff(iris, iris2)(iris), iris))
#'
#' beaver <- beaver1
#' patches <- list()
#' for (i in seq_len(10)) {
#'   old_beaver <- beaver
#'   beaver[seq(step*10, 9 + step*10), 1] <- step
#'   patches <- c(patches, objectdiff(old_beaver, beaver))
#' }
#' stopifnot(identical(
#'  beaver, Reduce(function(data, patch) patch(data), patches, beaver1)))
#' # The patches record the history of how we got from beaver1 to beaver
#' # We could go back to any previous step by applying only some of the
#' # patches.
#' }
setGeneric("objectdiff",
  def = function(old_object, new_object, ...) standardGeneric("objectdiff"),
  valueClass = "patch")

setMethod('objectdiff', signature = c('ANY', 'ANY'),
  definition = function(old_object, new_object) {
    trivial_patch(new_object)
  })


