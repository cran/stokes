\name{zap}
\alias{zap}
\alias{zap}
\alias{zapsmall}
\alias{zaptiny}
\alias{zap.spray}
\alias{zap.kform}
\alias{zap.ktensor}
\title{Zap small values in \eqn{k}-forms and \eqn{k}-tensors}
\description{

stokes Version 1.0-9 is a reduced functionality placeholder pending
upload of a newer spray package to CRAN which uses disordR discipline.


Equivalent to \code{zapsmall()}}
\usage{
zap(X)
\method{zap}{kform}(X)
\method{zap}{ktensor}(X)
}
\arguments{
  \item{X}{Tensor or \eqn{k}-form to be zapped}
}
\details{

  Given an object of class \code{ktensor} or \code{kform}, coefficients
  close to zero are \sQuote{zapped}, i.e., replaced by \sQuote{0}, using
  \code{base::zapsmall()}.

  Note, \code{zap()} actually changes the numeric value, it is not just
  a print method.

}
\value{Returns an object of the same class}
\author{Robin K. S. Hankin}
