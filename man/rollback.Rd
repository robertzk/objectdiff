% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tracked_environment.R
\name{rollback<-.tracked_environment}
\alias{rollback}
\alias{rollback<-.tracked_environment}
\title{Roll back commits to an earlier version of the tracked environment.}
\usage{
\method{rollback}{tracked_environment}(env, silent = FALSE) <- value

rollback(env, value = 1, silent = FALSE)
}
\arguments{
\item{env}{tracked_environment.}

\item{silent}{logical. Whether or not to commit a silent rollback.
If \code{TRUE}, the current chain of commits will not be pruned,
so it will be possible to use \code{rollback} with a negative
number to go back to a future commit. It is the user's responsibility
to ensure that the commit stack does not become corrupt.}

\item{value}{integer. Number of commits to roll back.}
}
\description{
Roll back commits to an earlier version of the tracked environment.
}
\note{
Rolling back 0 commits clears the current staged changes.
}

