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

## ----label=showvol,comment=""-------------------------------------------------
volume

## -----------------------------------------------------------------------------
(V <- volume(7))

## -----------------------------------------------------------------------------
f <- as.function(V)
f(diag(7))

## -----------------------------------------------------------------------------
A <- matrix(rnorm(49),7,7)
LHS <- f(A)
RHS <- det(A)
c(LHS=LHS,RHS=RHS,diff=LHS-RHS)

## -----------------------------------------------------------------------------
M1 <- qr.Q(qr(matrix(rnorm(49),7,7)))  # M1: a random orthogonal matrix
M2 <- M1[c(2,1,3,4,5,6,7),]            # M2: (odd) permutation of rows of M1
c(f(M1),f(M2))

