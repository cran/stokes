\name{rform}
\alias{rform}
\alias{rkform}
\alias{rtensor}
\alias{rktensor}
\title{Random kforms and ktensors}
\description{

stokes version 1.0-9 is a reduced functionality placeholder package
pending upload of a newer spray package to CRAN which uses disordR
discipline.

Random \eqn{k}-form objects and \eqn{k}-tensors,
  intended as quick \dQuote{get you going} examples}
\usage{
rform(terms=9,k=3,n=7,coeffs)
rtensor(terms=9,k=3,n=7,coeffs)
}
\arguments{
  \item{terms}{Number of distinct terms}
  \item{k,n}{A \eqn{k}-form maps \eqn{V^k} to \eqn{R}, where \eqn{V=R^n}}
  \item{coeffs}{The coefficients of the form; if missing use
    \code{seq_len(terms)}}
}
\details{
  What you see is what you get, basically.

  Note that argument \code{terms} is an upper bound, as the index matrix
  might contain repeats which are combined.

}
\value{
  All functions documented here return an object of class \code{kform} or
  \code{ktensor}.
}
\author{Robin K. S. Hankin}
\examples{
1+1 # stokes_1.0-9 is a placeholder pending release of spray
}
