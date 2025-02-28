## ----setup, include=FALSE-----------------------------------------------------
set.seed(0)
library("stokes")
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(echo = TRUE)
knit_print.function <- function(x, ...){dput(x)}
registerS3method(
  "knit_print", "function", knit_print.function,
  envir = asNamespace("knitr")
)

## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/stokes.png", package = "stokes"))

## ----label=showAlt,comment=""-------------------------------------------------
phi

## -----------------------------------------------------------------------------
phi(4)

## -----------------------------------------------------------------------------
f <- as.function(phi(3))
c(f(as.matrix(e(2,5))), f(as.matrix(e(3,5))), f(as.matrix(e(4,5))))

## -----------------------------------------------------------------------------
aa <- function(n){
   outer(seq_len(n), seq_len(n),
   Vectorize(function(i, j){as.function(phi(i))(as.matrix(e(j, n)))}))
}

aa(5)

## -----------------------------------------------------------------------------
all(aa(9) == diag(9))

## -----------------------------------------------------------------------------
phi(4) %X% phi(3) %X% phi(5)

## -----------------------------------------------------------------------------
Reduce(`%X%`,sapply(4:8,phi))

## -----------------------------------------------------------------------------
phi(c(2,5,1))

## -----------------------------------------------------------------------------
(v <- sample(9))
phi(v) == Reduce(`%X%`,sapply(v,phi))

## -----------------------------------------------------------------------------
(X <- ktensor(spray(matrix(c(1,2,3,2,1,1),3,2),1:3)))

## -----------------------------------------------------------------------------
1*phi(c(1,2)) + 2*phi(c(2,1)) + 3*phi(c(3,1))

## -----------------------------------------------------------------------------
apply(expand.grid(rep(list(seq_len(3)),2)),1,phi)

## -----------------------------------------------------------------------------
s <- function(...){phi(unlist(list(...)))}
s(3,4,6)

## -----------------------------------------------------------------------------
1*s(1,2) + 2*s(2,1) + 3*s(3,1)
1*s(1,2) + 2*s(2,1) + 3*s(3,1) == X

## -----------------------------------------------------------------------------
(2*phi(1) + 3*phi(2)) %X% (5*phi(3) + 7*phi(4) )

## -----------------------------------------------------------------------------
(b <- ktensor(spray(matrix(c(3,4,7,5,4,3),3,2),7:9)))

## -----------------------------------------------------------------------------
7*phi(c(3,5)) + 8*phi(c(4,4)) + 9*phi(c(7,3))

## -----------------------------------------------------------------------------
b == 7*phi(c(3,5)) + 8*phi(c(4,4)) + 9*phi(c(7,3))

## -----------------------------------------------------------------------------
phi(1:3)
Alt(6*phi(1:3))

