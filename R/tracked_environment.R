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
#'   on the environment. Commits can be named and labeled. The default is
#'   \code{new.env(parent = emptyenv())} (a new environment with no parent).
#' @rdname tracked_environment
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
tracked_environment <- function(env = new.env(parent = emptyenv())) {
  force(env)
  stopifnot(is.environment(env))
  if (is.tracked_environment(env))
    stop("Recursion! Can't track an already-tracked environment.")

  structure(class = 'tracked_environment', list2env(parent = emptyenv(),
    list(env = env,
         ghost = new.env(parent = emptyenv()),
         universe = ls(env, all = TRUE),
         commits = make_stack())
  ))
}
setClass('tracked_environment')

ls <- function(...) UseMethod('ls')
ls.tracked_environment <- function(x, ...) base::ls(x%$%env, ...)
ls.environment <- function(...) base::ls(...)

rm <- function(...) UseMethod('rm')
rm.tracked_environment <- function(x, ...) base::rm(x%$%env, ...)
rm.environment <- function(...) base::rm(...)

as.environment <- function(...) UseMethod('as.environment')
as.environment.tracked_environment <- function(env) { env%$%env }
as.environment.character <- as.environment.list <-
 function(...) base::as.environment(...)

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
#' @export
`commit<-.tracked_environment` <- function(env, value) {
  # TODO: (RK) Do something with the commit message..?
  out <- env%$%commits$push(squish_patches(env%$%ghost, env$staged$pop_all()))
  env%$%universe <- character(0)
  clear_environment(env%$%ghost)
  out
}

#' Roll back commits to an earlier version of the tracked environment.
#'
#' @param env tracked_environment.
#' @param value integer. Number of commits to roll back.
#' @export
`rollback<-.tracked_environment` <- function(env, value) {
  .NotYetImplemented()
}

#' @param name character. When using the \code{\%$\%} infix operator,
#'    access a meta-datum from the \code{tracked_environment} (for example,
#'    "env", "staged", or "commits").
#' @note
#' A tracked_environment is itself an environment that contains
#' \itemize{
#'   \item{\code{env}. }{The environment that is getting tracked.}
#'   \item{\code{staged}. }{A list of staged changes (\code{patch} objects, that is,
#'     functions that record what has changed in an atomic modification
#'     operation on the \code{env}.}
#'   \item{\code{commits}. }{A list of commits (a curated list of \code{patch}es
#'     that represent the history of the \code{tracked_environment}}.
#' }
#' 
#' From within the objectdiff package, it is possible to access these
#' explicitly using the \code{\%$\%} operator, for example, 
#' \code{some_tracked_env\%$\%commits}.
#'
#' @export
#' @rdname tracked_environment
`%$%` <- function(env, name) {
  base::get(deparse(substitute(name)), envir = env, inherits = FALSE)
}

`%$%<-` <- function(env, name, value) {
  stopifnot(is.tracked_environment(env))

  base::assign(deparse(substitute(name)), value, envir = env, inherits = FALSE)
  env
}

`$.tracked_environment` <- function(env, ...) {
  base::get(..., envir = env%$%env)
}

`$<-.tracked_environment` <- function(env, name, value) {
  assign_call <- quote(`[[<-`(env, name, value))
  assign_call[[3]] <- substitute(name)
  eval(assign_call)
  env
}

`[[<-.tracked_environment` <- function(env, name, value) {
  # Record the before-value in the ghost environment.
  # TODO: (RK) What about environments...? Those won't work correctly.
  e <- env%$%env; g <- env%$%ghost
  if (!exists(name, envir = g, inherits = FALSE))
    g[[name]] <-
      if (exists(name, envir = e, inherits = FALSE)) e[[name]]
      else NULL

  if (!exists(name, envir = e, inherits = FALSE))
    env%$%universe <- c(env%$%universe, name)

  `[[<-`(e, name, value)
  env
}

assign <- function(x, value, envir, ...) {
  if (!missing(envir)) {
    if (is.tracked_environment(envir)) {
      envir[[x]] <- value
    } else base::assign(x, value, envir, ...)
  } else base::assign(x, value, ...)
}

