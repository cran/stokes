\name{Alt}
\alias{Alt}
\title{Alternating multilinear forms}
\description{
  Converts a \eqn{k}-tensor to alternating form
}
\usage{Alt(S,give_kform)}
\arguments{
  \item{S}{A multilinear form, an object of class \code{ktensor}}
  \item{give_kform}{Boolean, with default \code{FALSE} meaning to return
    an alternating \eqn{k}-tensor [that is, an object of class
    \code{ktensor} that happens to be alternating] and \code{TRUE}
    meaning to return a \eqn{k}-form [that is, an object of class
    \code{kform}]} 
}
\details{
  Given a \eqn{k}-tensor \eqn{T}, we have

  \deqn{\mathrm{Alt}(T)\left(v_1,\ldots,v_k\right)=
    \frac{1}{k!}\sum_{\sigma\in S_k}\mathrm{sgn}(\sigma)\cdot
    T\left(v_{\sigma(1)},\ldots,v_{\sigma(k)}\right)
  }{omitted; see latex}

  Thus for example if \eqn{k=3}:

    \deqn{\mathrm{Alt}(T)\left(v_1,v_2,v_3\right)=
      \frac{1}{6}\left(\begin{array}{c}
      +T\left(v_1,v_2,v_3\right)\quad
      -T\left(v_1,v_3,v_2\right)\cr
      -T\left(v_2,v_1,v_3\right)\quad
      +T\left(v_2,v_3,v_1\right)\cr
      +T\left(v_3,v_1,v_2\right)\quad
      -T\left(v_3,v_2,v_1\right)
      \end{array}
      \right)
    }{omitted; see latex}

    and it is reasonably easy to see that \eqn{\mathrm{Alt}(T)}{Alt(T)}
    is alternating, in the sense that

  \deqn{\mathrm{Alt}(T)\left(v_1,\ldots,v_i,\ldots,v_j,\ldots,v_k\right)=
    -\mathrm{Alt}(T)\left(v_1,\ldots,v_j,\ldots,v_i,\ldots,v_k\right)
  }{omitted; see latex}

  Function \code{Alt()} is intended to take and return an object of
  class \code{ktensor}; but if given a \code{kform} object, it just
  returns its argument unchanged.

  A short vignette is provided with the package: type
  \code{vignette("Alt")} at the commandline.

} 
\value{
  Returns an alternating \eqn{k}-tensor.  To work with \eqn{k}-forms,
  which are a much more efficient representation of alternating tensors,
  use \code{as.kform()}.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{kform}}}
\examples{


(X <- ktensor(spray(rbind(1:3),6)))
Alt(X)
Alt(X,give_kform=TRUE)

S <- as.ktensor(expand.grid(1:3,1:3),rnorm(9))
S
Alt(S)

issmall(Alt(S) - Alt(Alt(S)))  # should be TRUE; Alt() is idempotent

a <- rtensor()
V <- matrix(rnorm(21),ncol=3)
LHS <- as.function(Alt(a))(V)
RHS <- as.function(Alt(a,give_kform=TRUE))(V)
c(LHS=LHS,RHS=RHS,diff=LHS-RHS)

}
