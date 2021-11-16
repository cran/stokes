\name{zero}
\alias{zeroform}
\alias{zerotensor}
\title{Zero tensors and zero forms}
\description{



stokes version 1.0-9 is a reduced functionality placeholder package
pending upload of a newer spray package to CRAN which uses disordR
discipline.

  Correct idiom for generating zero \eqn{k}-tensors and \eqn{k}-forms
}
\usage{
zeroform(n)
zerotensor(n)
}
\arguments{
  \item{n}{Arity of the \eqn{k}-form or \eqn{k}-tensor}
}
\author{Robin K. S. Hankin}
\note{
  Idiom such as \code{as.ktensor(rep(1,n),0)} and
  \code{as.kform(rep(1,5),0)} and indeed \code{as.kform(1:5,0)} is
  incorrect as the arity of the tensor is lost.

  A \eqn{0}-form is not the same thing as a zero tensor.  A \eqn{0}-form
  maps \eqn{V^0} to the reals; a scalar.  A zero tensor maps \eqn{V^k} to
  zero.
}
\value{Returns an object of class \code{kform} or \code{ktensor}.}
\seealso{\code{\link{scalar}}}
