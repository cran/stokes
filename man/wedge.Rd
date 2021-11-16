\name{wedge}
\alias{wedge}
\alias{wedge2}
\alias{\%^\%}
\title{Wedge products}
\description{Wedge products of \eqn{k}-forms}
\usage{
wedge2(K1,K2)
wedge(x, ...)
}
\arguments{
  \item{K1,K2,x,...}{\eqn{k}-forms}
}
\details{
Wedge product of \eqn{k}-forms.  
}
\value{
  The functions documented here return an object of class
  \code{kform}.
}
\author{Robin K. S. Hankin}
\note{
  In general use, use \code{wedge()} or \code{\%^\%}.  Function
  \code{wedge()} uses low-level helper function \code{wedge2()}, which
  takes only two arguments.

  A short vignette is provided with the package: type
  \code{vignette("wedge")} at the commandline.

}
