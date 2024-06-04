## ----setup, include=FALSE-----------------------------------------------------
set.seed(0)
library("stokes")
library("quadform")    # needed for quad3.form()
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)
knit_print.function <- function(x, ...){dput(x)}
registerS3method(
  "knit_print", "function", knit_print.function,
  envir = asNamespace("knitr")
)

## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/stokes.png", package = "stokes"))

## ----label=showAlt,comment=""-------------------------------------------------
inner

## -----------------------------------------------------------------------------
inner(diag(7))

## -----------------------------------------------------------------------------
x <- rnorm(7)
y <- rnorm(7)
V <- cbind(x,y)
LHS <- sum(x*y)
RHS <- as.function(inner(diag(7)))(V)
c(LHS=LHS,RHS=RHS,diff=LHS-RHS)

## -----------------------------------------------------------------------------
M <- matrix(rnorm(49),7,7)
f <- as.function(inner(M))
LHS <- quad3.form(M,x,y)
RHS <- f(V)
c(LHS=LHS,RHS=RHS,diff=LHS-RHS)

## -----------------------------------------------------------------------------
M1 <- matrix(rnorm(49),7,7)
M2 <- matrix(rnorm(49),7,7)
g <- as.function(inner(M1+M2))
LHS <- quad3.form(M1+M2,x,y)
RHS <- g(V)
c(LHS=LHS,RHS=RHS,diff=LHS-RHS)

## -----------------------------------------------------------------------------
h <- as.function(inner(M1 + t(M1))) # send inner() a symmetric matrix
LHS <- h(V)
RHS <- h(V[,2:1])
c(LHS=LHS,RHS=RHS,diff=LHS-RHS)

## -----------------------------------------------------------------------------
M3 <- crossprod(matrix(rnorm(56),8,7))  # 7x7 pos-def matrix
as.function(inner(M3))(kronecker(rnorm(7),t(c(1,1))))>0  # should be TRUE

## -----------------------------------------------------------------------------
jj <- matrix(rpois(49,lambda=3.2),7,7)
M <- jj-t(jj) # M is antisymmetric
f <- as.function(inner(M))
LHS <- f(V)
RHS <- -f(V[,2:1])   # NB negative as we are checking for an alternating form
c(LHS=LHS,RHS=RHS,diff=LHS-RHS) 

