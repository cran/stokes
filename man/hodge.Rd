\name{hodge}
\alias{hodge}
\alias{Hodge}
\alias{star}
\title{Hodge star operator}
\description{

stokes version 1.0-9 is a reduced functionality placeholder pending
upload of a newer spray package to CRAN which uses disordR discipline.



  Given a \eqn{k}-form, return its Hodge dual}
\usage{
hodge(K, n=max(index(K)), g=rep(1,n), lose=TRUE)
}
\arguments{
  \item{K}{Object of class \code{kform}}
  \item{n}{Dimensionality of space, defaulting the the largest element
    of the index}
  \item{g}{Diagonal of the metric tensor, defaulting to the standard
    metric}
  \item{lose}{Boolean, with default \code{TRUE} meaning to coerce to a
    scalar if appropriate}
}
\value{Given a \eqn{k}-form, in an \eqn{n}-dimensional space, returns a
  \eqn{(n-k)}-form.}
\author{Robin K. S. Hankin}
\seealso{\code{\link{wedge}}}
