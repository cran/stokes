\name{symbolic}
\alias{symbolic}
\alias{as.symbolic}
\title{Symbolic form}
\description{\loadmathjax


stokes version 1.0-9 is a reduced functionality placeholder pending
upload of a newer spray package to CRAN which uses disordR discipline.



  Prints \eqn{k}-tensor and \eqn{k}-form objects in symbolic form
}
\usage{
as.symbolic(M,symbols=letters,d="")
}
\arguments{
  \item{M}{Object of class \code{kform} or \code{ktensor}; a map from
    \mjeqn{V^k}{V^k} to \eqn{R}, where \mjeqn{V=R^n}{V=R^n}}
  \item{symbols}{A character vector giving the names of the symbols}
  \item{d}{String specifying the appearance of the differential operator}
}
\value{Returns a noquote character string.}
\author{Robin K. S. Hankin}
