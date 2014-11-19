#' Squish a sequence of patches off a base object into one.
#'
#' @param base ANY. R object to base the squishing off.
#' @param patches list. A list of patches (functions derived from
#'   \code{\link{objectdiff}}).
#' @return a single \code{patch}.
#' @seealso \code{\link{objectdiff}}
#' @export
#' @examples
#' iris2 <- iris; iris2[1,1] <- 1
#' patch1 <- objectdiff(iris, iris2)
#' iris3 <- iris2; iris3[1,1] <- 2
#' patch2 <- objectdiff(iris2, iris3)
#' patch3 <- squish_patches(iris, list(patch1, patch2))
#' stopifnot(identical(iris3, patch3(iris)))
squish_patches <- function(base, patches) {
  stopifnot(is.list(patches) && all(vapply(patches, is.patch, logical(1))))

  objectdiff(base, Reduce(function(clay, mold) mold(clay), patches, base))
}

