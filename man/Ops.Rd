\name{Ops.kform}
\alias{Ops.kform}
\alias{Ops.ktensor}
\alias{Ops}
\title{Arithmetic Ops Group Methods for \code{kform} and \code{ktensor}
  objects}
\description{
  Allows arithmetic operators to be used for \eqn{k}-forms and
  \eqn{k}-tensors such as addition, multiplication, etc, where defined.
}
\usage{
\method{Ops}{kform}(e1, e2 = NULL)
\method{Ops}{ktensor}(e1, e2 = NULL)
}
\arguments{
  \item{e1,e2}{Objects of class \code{kform} or \code{ktensor}}
}
\details{
  The functions \code{Ops.kform()} and \code{Ops.ktensor()} pass unary
  and binary arithmetic operators (\dQuote{\code{+}}, \dQuote{\code{-}},
  \dQuote{\code{*}}, and \dQuote{\code{/}}) to the appropriate
  specialist function by coercing to \code{spray} objects.

  For wedge products of \eqn{k}-forms, use \code{wedge()} or
  \code{\%^\%}; and for cross products of \eqn{k}-tensors, use
  \code{cross()} or \code{\%X\%}.
}

\value{All functions documented here return an object of class \code{kform} or
  \code{ktensor}.
}
\author{Robin K. S. Hankin}
\note{
  A plain asterisk, \dQuote{\code{*}}, given two kforms or two ktensors,
  will return the wedge product or the cross product respectively, on
  the grounds that the idiom has only one natural interpretation.  But
  its use is discouraged.
}
\keyword{symbolmath}
