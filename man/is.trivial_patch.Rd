% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patch.R
\name{is.trivial_patch}
\alias{is.trivial_patch}
\title{Check if an R object is a trivial patch.}
\usage{
is.trivial_patch(fn)
}
\arguments{
\item{fn}{function. Any function.}
}
\value{
TRUE or FALSE according as the function is or is not a trivial patch.
}
\description{
A trivial patch stores a fully copy of the diffed object when no heuristics
were found for determining object differences. In other words, it is the
worst scenario and should be avoided.
}

