#' An environment object that tracks changes made to its constituents.
#' 
#' A typical R environment is like a mathematical set. It is a bag of 
#' stuff and not much else. A tracked environment is like such an environment
#' that can also replay "changes" to its history.
#'
#' To create a tracked environment from any environment \code{e}, simply write
#' \code{tracked_environment(e)}.
#' 
#' @param env environment. When converted to a \code{tracked_environment},
#'   all changes will be remembered whenever a "commit" is registered
#'   on the environment. Commits can be named and labeled.
#' @examples
#' \dontrun{
#'   e <- tracked_environment()
#'   e$x <- 1
#'   commit(e) <- 'First message'
#'   e$x <- 2
#'   commit(e) <- 'Second message'
#'   stopifnot(identical(e$x, 2))
#'   rollback(e) <- 1
#'   stopifnot(identical(e$x, 1)) # The changes have been rolled back one step.
#' }
tracked_environment <- function(env) {
  force(env)
  stopifnot(is.environment(env))
  if (is.tracked_environment(env))
    stop("Recursion! Can't track an already-tracked environment.")

  structure(class = 'tracked_environment', list2env(parent = emptyenv(),
    list(env = env, staged = make_stack(), commits = make_stack())
  ))
}

ls <- function(...) UseMethod('ls')
ls.tracked_environment <- function(x, ...) base::ls(x$env, ...)
ls.environment <- function(...) base::ls(...)

as.environment <- function(...) UseMethod('as.environment')
as.environment.tracked_environment <- function(env) { env$env }
as.environment.character <- function(...) base::as.environment(...)

environment <- function(...) UseMethod('environment')
environment.function <- function(...) base::environment(...)
environment.tracked_environment <- as.environment.tracked_environment 
is.tracked_environment <- function(x) { is(x, 'tracked_environment') }

`commit<-` <- function(...) UseMethod('commit<-')
`rollback<-` <- function(...) UseMethod('rollback<-')

#' Commit a change to a tracked environment.
#'
#' Committing a change is equivalent to storing a patch object (see
#' \code{\link{objectdiff}}). For the moment, any comment will produce
#' a single patch by "squishing" any changes made to the environment.
#' If you are iterating over an environment object many many times,
#' you should probably batch changes if it lives in a tracked environment.
#' Otherwise, for example, making 1,000,000 small modifications to
#' a data.frame before commiting could cause a long time to recompute
#' the "final patch".
#'
#' @seealso \code{\link{objectdiff}})
#' @param env tracked_environment.
#' @param value character. Commit message. May be \code{NULL}.
`commit<-.tracked_environment` <- function(env, value) {
  #env$staged$pop_all()
  #squish_patches(
}

`%$%` <- function(tracked_env, name) {
  base::get(deparse(substitute(name)), envir = tracked_env, inherits = FALSE)
}

`$.tracked_environment` <- function(env, ...) {
  base::get(..., envir = z%$%env)
}

`$<-.tracked_environment` <- function(env, name, value) {
  tmp <- class(env)
  assign_call <- quote(`$<-`(env%$%env, name, value))
  assign_call[[3]] <- as.name(substitute(name))
  eval(assign_call)
  env
}

`[[<-.tracked_environment` <- function(env, name, value) {
  tmp <- class(env)
  `[[<-`(env%$%env, name, value)
  env
}

assign <- function(x, value, envir, ...) {
  if (!missing(envir)) {
    if (is.tracked_environment(envir)) {
      (envir%$%env)[[x]] <- value
    } else base::assign(x, value, envir, ...)
  } else base::assign(x, value, ...)
}

