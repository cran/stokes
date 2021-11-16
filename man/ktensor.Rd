\name{ktensor}
\alias{ktensor}
\alias{as.ktensor}
\alias{is.ktensor}
\alias{is.tensor}
\alias{as.function.ktensor}
\title{k-tensors}
\description{\loadmathjax
Functionality for \eqn{k}-tensors
}
\usage{
ktensor(S)
as.ktensor(M,coeffs)
is.ktensor(x)
\method{as.function}{ktensor}(x,...)
}
\arguments{
  \item{M,coeffs}{Matrix of indices and coefficients, as in
    \code{spray(M,coeffs)}}
  \item{S}{Object of class \code{spray}}
  \item{x}{Object of class \code{ktensor}}
  \item{...}{Further arguments, currently ignored}
}
\details{

A \dfn{\eqn{k}-tensor} object \eqn{S} is a map from \eqn{V^k} to the
reals \eqn{R}, where \eqn{V} is a vector space (here \eqn{R^n}) that
satisfies multilinearity:

\mjdeqn{S\left(v_1,\ldots,av_i,\ldots,v_k\right)=a\cdot
  S\left(v_1,\ldots,v_i,\ldots,v_k\right)}{omitted; see latex}

and

\mjdeqn{S\left(v_1,\ldots,v_i+{v_i}',\ldots,v_k\right)=S\left(v_1,\ldots,v_i,\ldots,x_v\right)+
  S\left(v_1,\ldots,{v_i}',\ldots,v_k\right).}{omitted; see latex}

Note that this is \emph{not} equivalent to linearity over
\mjeqn{V^{nk}}{V^nk} (see examples).

In the \pkg{stokes} package, \eqn{k}-tensors are represented as sparse
arrays (\code{spray} objects), but with a class of \code{c("ktensor",
"spray")}.  This is a natural and efficient representation for tensors
that takes advantage of sparsity using \pkg{spray} package features.

Function \code{as.ktensor()} will coerce a \eqn{k}-form to a
\eqn{k}-tensor via \code{kform_to_ktensor()}.

}
\value{
  All functions documented here return a \code{ktensor} object
  except \code{as.function.ktensor()}, which returns a function.
}
\references{Spivak 1961}
\author{Robin K. S. Hankin}
\seealso{\code{\link{cross}},\code{\link{kform}},\code{\link{wedge}}}
