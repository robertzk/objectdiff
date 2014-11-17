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
#'   commit(e)
#'   e$x <- 2
#'   commit(e)
#'   stopifnot(identical(e$x, 2))
#'   e$rollback(1)
#'   stopifnot(identical(e$x, 1)) # The changes have been rolled back one step.
#' }
tracked_environment <- function(env) {
  structure(list(env = env), class = 'tracked_environment')
}

as.environment.tracked_environment <- function(env) { env$env }
environment <- function(...) UseMethod('environment')
`environment<-` <- function(...) UseMethod('environment<-')
environment.tracked_environment <- as.environment.tracked_environment 
`environment<-.tracked_environment` <- function(tracked_env, value) {
  stop("Cannot replace tracked_environments. Use ",
       "tracked_environment$env <- ", deparse(substitute(value)))
}
is.tracked_environment <- function(x) { is(x, 'tracked_environment') }

`$<-.tracked_environment` <- function(env, name, value) {
  tmp <- class(env)
  assign_call <- quote(`$<-`(env$env, name, value))
  assign_call[[3]] <- as.name(substitute(name))
  eval(assign_call)
  env
}

`[[<-.tracked_environment` <- function(env, name, value) {
  tmp <- class(env)
  `[[<-`(env$env, name, value)
  env
}

assign <- function(x, value, envir, ...) {
  if (!missing(envir)) {
    if (is.tracked_environment(envir)) base::assign(x, value, envir$env, ...)
    else base::assign(x, value, envir, ...)
  } else base::assign(x, value, ...)
}


