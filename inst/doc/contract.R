## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)
library("stokes")
set.seed(0)

## -----------------------------------------------------------------------------
contract
contract_elementary

## ----label=simpleexample------------------------------------------------------
(phi <- as.kform(1:5))

## ----label=contract10000------------------------------------------------------
v <- c(1,0,0,0,0)
contract(phi,v)

## ----label=contract01000------------------------------------------------------
w <- c(0,1,0,0,0)
contract(phi,w)

## ----complicatedvectors-------------------------------------------------------
contract(phi,c(1,3,0,0,0))
contract(phi,1:5)

## ----label=verifylinearityinv-------------------------------------------------
a <- 1.23
b <- -0.435
v <- 1:5
w <- c(-3, 2.2, 1.1, 2.1, 1.8)

contract(phi,a*v + b*w) == a*contract(phi,v) + b*contract(phi,w)

## ----label=verifylinearityinphi-----------------------------------------------
(phi <- rform(2,5))
(psi <- rform(2,5))
a <- 7
b <- 13
v <- 1:7
contract(a*phi + b*psi,v) == a*contract(phi,v) + b*contract(psi,v)

## ----label=straight-----------------------------------------------------------
(phi <- rform(2,5))
u <- c(1,3,2,4,5,4,6)
v <- c(8,6,5,3,4,3,2)
contract(contract(phi,u),v)

## ----bothatonce---------------------------------------------------------------
M <- cbind(u,v)
contract(contract(phi,u),v) == contract(phi,M)

## ----verifydirect-------------------------------------------------------------
(o <- kform(spray(t(replicate(2, sample(9,4))), runif(2))))
V <- matrix(rnorm(36),ncol=4)
jj <- c(
   as.function(o)(V),
   as.function(contract(o,V[,1,drop=TRUE]))(V[,-1]), # scalar
   as.function(contract(o,V[,1:2]))(V[,-(1:2),drop=FALSE]),
   as.function(contract(o,V[,1:3]))(V[,-(1:3),drop=FALSE]),
   as.function(contract(o,V[,1:4],lose=FALSE))(V[,-(1:4),drop=FALSE])
)
print(jj)
max(jj) - min(jj) # zero to numerical precision

## ----label=getazeroform-------------------------------------------------------
contract(o,V)

## ----doanothercontractnolose--------------------------------------------------
contract(o,V,lose=FALSE)

## -----------------------------------------------------------------------------
o <- as.kform(1:5)
V <- matrix(rnorm(25),5,5)
LHS <- det(V)
RHS <- contract(o,V)
c(LHS=LHS,RHS=RHS,diff=LHS-RHS)

