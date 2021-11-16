\name{inner}
\alias{inner}
\alias{inner_product}
\alias{inner.product}
\title{Inner product operator}
\description{\loadmathjax
  The inner product
}
\usage{inner(M)}
\arguments{
  \item{M}{square matrix}
}
\value{Returns a \eqn{k}-tensor, an inner product}
\details{

The inner product of two vectors \mjeqn{\mathbf{x}}{x} and
\mjeqn{\mathbf{y}}{y} is usually written
\mjeqn{\left\langle\mathbf{x},\mathbf{y}\right\rangle}{<x,y>} or
\mjeqn{\mathbf{x}\cdot\mathbf{y}}{x.y}, but the most general form would
be \mjeqn{\mathbf{x}^TM\mathbf{y}}{x'My} where \mjeqn{M}{M} is a matrix.
Noting that inner products are multilinear, that is
\mjeqn{\left\langle\mathbf{x},a\mathbf{y}+b\mathbf{z}\right\rangle=a\left\langle\mathbf{x},\mathbf{y}\right\rangle+b\left\langle\mathbf{x},\mathbf{z}\right\rangle}{<x,ay+bz>=a<x,y>+b<y,z>}
and \mjeqn{\left\langle
a\mathbf{x}+b\mathbf{y},\mathbf{z}\right\rangle=a\left\langle\mathbf{x},\mathbf{z}\right\rangle+b\left\langle\mathbf{y},\mathbf{z}\right\rangle}{<x,ay+bz>=a<x,y>+b<y,z>},
we see that the inner product is indeed a multilinear map, that is, a
tensor.

Given a square matrix \mjseqn{M}, function \code{inner(M)} returns the
\eqn{2}-form that maps \mjeqn{\mathbf{x},\mathbf{y}}{x,y} to
\mjeqn{\mathbf{x}^TM\mathbf{y}}{x'My}.

A short vignette is provided with the package: type
\code{vignette("inner")} at the commandline.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{kform}}}
