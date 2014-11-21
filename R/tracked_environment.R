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
#' @param snapshot integer. The interval at which to snapshot the environment.
#'   For example, if 100 commits have been made, and you would like to go
#'   back to commit 95, it would be very time-consuming to apply all
#'   the commits starting from the beginning. Instead a full copy
#'   of the environment will be made every \code{snapshot} commits.
#' @rdname tracked_environment
#' @export
#' @examples
#' e <- tracked_environment()
#' e$x <- 1
#' commit(e) <- 'First message'
#' e$x <- 2
#' commit(e) <- 'Second message'
#' stopifnot(identical(e$x, 2))
#' rollback(e) <- 1
#' stopifnot(identical(e$x, 1)) # The changes have been rolled back one step.
#'
#' classical_env <- list2env(list(x = 1, y = 2))
#' e <- tracked_environment(classical_env, snapshot = 5)
#' # Any changes to e will record full snapshots of the environment
#' # every 5 commits. This way, when the environment is rolled back to an
#' # earlier commit, it will not have to apply patches starting from the
#' # very beginning.
tracked_environment <- function(env = new.env(parent = emptyenv()), snapshot = 10) {
  force(env)
  stopifnot(is.environment(env))
  if (is.tracked_environment(env))
    stop("Recursion! Can't track an already-tracked environment.")

  initial <- new.env(parent = emptyenv())
  copy_env(initial, env)

  structure(class = 'tracked_environment', list2env(parent = emptyenv(),
    list(reference = list(initial),
         env = env,
         ghost = new.env(parent = emptyenv()),
         universe = ls(env, all = TRUE),
         commits = make_stack(),
         snapshot = snapshot)
  ))
}
#' @export
setClass('tracked_environment')

#' @export
ls <- function(name, ...) UseMethod('ls')
#' @export
ls.tracked_environment <- function(name, ...) base::ls(environment(name), ...)
#' @export
ls.environment <- function(name, ...) base::ls(name, ...)

#' @export
rm <- function(..., envir) {
  base::rm(..., envir =
    if (is.tracked_environment(envir)) environment(envir) else envir)
}

#' @export
as.environment <- function(...) UseMethod('as.environment')
#' @export
as.environment.tracked_environment <- function(env) { env%$%env }
#' @export
as.environment.character <- as.environment.list <-
 function(...) base::as.environment(...)

#' @export
environment <- function(...) UseMethod('environment')
#' @export
environment.function <- function(...) base::environment(...)
#' @export
environment.tracked_environment <- as.environment.tracked_environment 
#' @export
is.tracked_environment <- function(x) { is(x, 'tracked_environment') }

#' @export
`environment<-` <- function(env, value) UseMethod('environment<-')
#' @export
`environment<-.function` <- function(env, value) base::`environment<-`(env, value)
#' @export
`environment<-.tracked_environment` <- function(env, value) {
  stop("Cannot assign environment of a tracked_environment directly.")
}

#' @export
`commit<-` <- function(...) UseMethod('commit<-')
#' @export
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
#' @rdname commit
#' @examples
#' x <- tracked_environment()
#' x$foo <- 1
#' commit(x) <- 'First message'
#' x$foo <- 2
#' @export
`commit<-.tracked_environment` <- function(env, value) {
  # TODO: (RK) Do something with the commit message..?
  (env%$%commits)$push(objectdiff(env, env))
  if ((env%$%commits)$count() > length(env%$%reference) * (env%$%snapshot)) {
    snapshot(env)
  }

  env%$%universe <- ls(env%$%env, all = TRUE)
  clear_environment(env%$%ghost)
  env
}

#' Roll back commits to an earlier version of the tracked environment.
#'
#' @param env tracked_environment.
#' @param value integer. Number of commits to roll back.
#' @export
`rollback<-.tracked_environment` <- function(env, value) {
  stopifnot(is.numeric(value))
  num_commits <- (env%$%commits)$count()

  replay_count <- num_commits - value
  if (replay_count < 0) stop("Cannot rollback ", value, " commits ",
    "because only ", num_commits, " commits have been made.")

  replay(env, replay_count)
}

#' @param name character. When using the \code{\%$\%} infix operator,
#'    access a meta-datum from the \code{tracked_environment} (for example,
#'    "env", "ghost", "universe", or "commits").
#' @note
#' A tracked_environment is itself an environment that contains
#' \itemize{
#'   \item{\code{env}. }{The environment that is getting tracked.}
#'   \item{\code{reference}. }{When the first commit is published, a full
#'     copy of the original environment gets saved so that it can be
#'     replayed during rollbacks. Any additional snapshots (i.e., full
#'     copies of the environment) will be appended to this list at the
#'     \code{snapshot} interval.}
#'   \item{\code{ghost}. }{An environment that holds the "before" version
#'     of objects prior to committing a change. When a
#'     \code{tracked_environment} receives a commit, it will clear
#'     its ghost environment.}
#'   \item{\code{universe}. }{Essentially just running \code{base::ls} (i.e.,
#'     fetching the names of all objects in) the \code{env} before any
#'     changes occur. This is re-computed after a commit.}
#'   \item{\code{commits}. }{A list of commits (a curated list of \code{patch}es
#'     that represent the history of the \code{tracked_environment}}.
#'   \item{\code{snapshot}. }{The integer number of commits to wait before
#'     recording a full copy of the environment for rollbacks and for
#'     peeking back to past commits.}
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

#' @export
`%$%<-` <- function(env, name, value) {
  stopifnot(is.tracked_environment(env))

  base::assign(deparse(substitute(name)), value, envir = env, inherits = FALSE)
  env
}

#' @export
`[[.tracked_environment` <- function(env, name) {
  if (exists(name, envir = environment(env), inherits = FALSE))
    base::get(name, envir = environment(env))
  else NULL
}

#' @export
`$.tracked_environment` <- `[[.tracked_environment`
# TODO: (RK) Overwrite base::get

#' @export
`$<-.tracked_environment` <- function(env, name, value) {
  assign_call <- quote(`[[<-`(env, name, value))
  assign_call[[3]] <- substitute(name)
  eval(assign_call)
  env
}

#' @export
`[[<-.tracked_environment` <- function(env, name, value) {
  # Record the before-value in the ghost environment.
  # TODO: (RK) What about environments...? Those won't work correctly.
  e <- env%$%env; g <- env%$%ghost
  if (!exists(name, envir = g, inherits = FALSE))
    g[[name]] <-
      if (exists(name, envir = e, inherits = FALSE)) e[[name]]
      else NULL

  `[[<-`(e, name, value)
  env
}

#' @export
assign <- function(x, value, envir, ...) {
  if (!missing(envir)) {
    if (is.tracked_environment(envir)) {
      envir[[x]] <- value
    } else base::assign(x, value, envir, ...)
  } else base::assign(x, value, ...)
}

replay <- function(env, count) {
  stopifnot(is.tracked_environment(env))

  snapshot <- env%$%snapshot
  reference_index <-
    if (snapshot > count) 1
    else 1 + floor(count / ((length(env%$%reference) - 1) * snapshot))

  rm(list = ls(env, all = TRUE), envir = env)
  copy_env(env%$%env, (env%$%reference)[[reference_index]])

  seq2    <- function(x, y) { if (y >= x) seq(x, y) else integer(0) }
  commits <- (env%$%commits)$peek_all()[
    seq2(1 + (reference_index - 1) * snapshot, count)]

  for (commit in commits) { commit(env) }
  for (i in seq_len((env%$%commits)$count() - count)) {
    (env%$%commits)$pop() # Remove these commits from history
  }

  env%$%reference <- (env%$%reference)[seq_len(reference_index)]
  env
}

snapshot <- function(env) {
  stopifnot(is.tracked_environment(env))
  reference <- new.env(parent = emptyenv())
  copy_env(reference, env%$%env)

  env%$%reference <- c(env%$%reference, reference)
}

