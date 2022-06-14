## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)
library("stokes")
library("emulator")
set.seed(1)

## -----------------------------------------------------------------------------
vector_cross_product

## -----------------------------------------------------------------------------
(M <- cbind(c(5,-2,1),c(1,2,0)))
vector_cross_product(M)

## -----------------------------------------------------------------------------
vector_cross_product(matrix(rnorm(30),6,5))

## -----------------------------------------------------------------------------
det(cbind(M,vector_cross_product(M)))>0

## -----------------------------------------------------------------------------
f <- function(n){
  M <- matrix(rnorm(n^2+n),n+1,n)
  det(cbind(M,vector_cross_product(M)))>0
}

all(sapply(sample(3:10,100,replace=TRUE),f))

## -----------------------------------------------------------------------------
M <- cbind(c(0,5,-2,1),c(0,1,1,0),c(2,2,2,3))
vector_cross_product(M)
hodge(as.1form(M[,1]) ^ as.1form(M[,2])^ as.1form(M[,3]))

## -----------------------------------------------------------------------------
set.seed(2)
M <- matrix(rnorm(30),6,5)
vector_cross_product(M)
H <- hodge(as.1form(M[,1]) ^ as.1form(M[,2]) ^ as.1form(M[,3]) ^ as.1form(M[,4]) ^ as.1form(M[,5]))
H - as.1form(vector_cross_product(M))  # zero to numerical precision.

