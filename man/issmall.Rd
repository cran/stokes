\name{issmall}
\alias{issmall}
\title{Is a form zero to within numerical precision?}
\description{

stokes Version 1.0-9 is a reduced functionality placeholder pending
upload of a newer spray package to CRAN which uses disordR discipline.

  Given a \eqn{k}-form, return \code{TRUE} if it is \dQuote{small}}
\usage{
issmall(M, tol=1e-8)
}
\arguments{
  \item{M}{Object of class \code{kform} or \code{ktensor}}
  \item{tol}{Small tolerance, defaulting to \code{1e-8}}
}
\value{Returns a logical}
\author{Robin K. S. Hankin}
