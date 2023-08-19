## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)
library("stokes")
library("emulator")    # needed for quad.3form()
set.seed(0)

## -----------------------------------------------------------------------------
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
LHS <- quad.3form(M,x,y)
RHS <- f(V)
c(LHS=LHS,RHS=RHS,diff=LHS-RHS)

## -----------------------------------------------------------------------------
M1 <- matrix(rnorm(49),7,7)
M2 <- matrix(rnorm(49),7,7)
g <- as.function(inner(M1+M2))
LHS <- quad.3form(M1+M2,x,y)
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

