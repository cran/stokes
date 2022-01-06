## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library("stokes")
library("emulator")
set.seed(0)

## -----------------------------------------------------------------------------
inner

## -----------------------------------------------------------------------------
inner(diag(7))

## -----------------------------------------------------------------------------
x <- rnorm(7)
y <- rnorm(7)
V <- cbind(x,y)
c(as.function(inner(diag(7)))(V),sum(x*y))  # should match

## -----------------------------------------------------------------------------
M <- matrix(rnorm(49),7,7)
f <- as.function(inner(M))
c(f(V),quad.3form(M,x,y)) # should match

## -----------------------------------------------------------------------------
M1 <- matrix(rnorm(49),7,7)
M2 <- matrix(rnorm(49),7,7)
g <- as.function(inner(M1+M2))
c(g(V),quad.3form(M1+M2,x,y)) # should match

## -----------------------------------------------------------------------------
h <- as.function(inner(M1 + t(M1)))
c(h(V), h(V[,2:1]))  # should match

## -----------------------------------------------------------------------------
M3 <- crossprod(matrix(rnorm(56),8,7))  # 7x7 pos-def matrix
as.function(inner(M3))(kronecker(rnorm(7),t(c(1,1))))>0  # should be TRUE

## -----------------------------------------------------------------------------
jj <- matrix(rpois(49,lambda=3.2),7,7)
M <- jj-t(jj) # M is antisymmetric
f <- as.function(inner(M))
c(f(V),f(V[,2:1])) # differ in sign

