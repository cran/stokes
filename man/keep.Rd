\name{keep}
\alias{keep}
\alias{discard}
\alias{retain}
\alias{drop.free}
\title{Keep or drop variables}
\description{

stokes version 1.0-9 is a reduced functionality placeholder pending
upload of a newer spray package to CRAN which uses disordR discipline.


  Keep or drop variables}
\usage{
keep(K, yes)
discard(K, no)
}
\arguments{
  \item{K}{Object of class \code{kform}}
  \item{yes,no}{Specification of dimensions to either keep (yes) or discard 
    (no), coerced to a free object}
}
\details{
  Function \code{keep(omega,yes)} keeps the terms specified and
  \code{discard(omega,no)} discards the terms specified.  It is not clear
  to me what these functions mean from a mathematical perspective.
}
\value{The functions documented here all return a \code{kform} object.}
\author{Robin K. S. Hankin}
\seealso{\code{\link{lose}}}
