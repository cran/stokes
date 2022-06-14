## ----setup, include=FALSE-----------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(echo = TRUE)
library("stokes")
set.seed(0)

## -----------------------------------------------------------------------------
Alt

## -----------------------------------------------------------------------------
S <- as.ktensor(rbind(c(1,7,8)))*6  # the "6" is to stop rounding error
S
Alt(S)

## -----------------------------------------------------------------------------
V <- matrix(rnorm(24),ncol=3)
c(as.function(Alt(S))(V),as.function(Alt(S))(V[,c(2,1,3)]))

## -----------------------------------------------------------------------------
S <- as.ktensor(rbind(c(1,2,4),c(2,2,3)),c(12,1000))
S
Alt(S)

## -----------------------------------------------------------------------------
S <- as.ktensor(matrix(c(
3,2,1,1,
1,4,1,4,
1,1,2,3,
7,7,4,7,
1,2,3,3
),ncol=4,byrow=TRUE),1:5)
S
Alt(S)

## -----------------------------------------------------------------------------
S <- rtensor(k=5,n=9)
S
AS <- Alt(S)
V <- matrix(rnorm(45),ncol=5) # element of (R^9)^5

## -----------------------------------------------------------------------------
V_even <- V[,c(1,2,5,3,4)]  # an even permutation
V_odd  <- V[,c(2,1,5,3,4)]  # an odd permutation
V_rep  <- V[,c(2,1,5,2,4)]  # not a permutation
c(as.function(AS)(V),as.function(AS)(V_even))   # should be identical (even permutation)
c(as.function(AS)(V),as.function(AS)(V_odd))    # should differ in sign only (odd permutation)
as.function(AS)(V_rep)                          # should be zero

## -----------------------------------------------------------------------------
P <- as.ktensor(1+diag(2),c(-7,7))
P
P == Alt(P)

## -----------------------------------------------------------------------------
P <- rtensor()*6 # the "6" avoids numerical round-off issues
Alt(Alt(P))==Alt(P)   # should be TRUE

## -----------------------------------------------------------------------------
omega <- as.ktensor(2+diag(2),c(-7,7))
eta   <- Alt(rtensor(2))*30
omega
eta
f <- as.function(Alt(omega %X% eta))

## -----------------------------------------------------------------------------
V <-  matrix(rnorm(35),ncol=5)
c(f(V),f(V[,c(2:1,3:5)]))

## -----------------------------------------------------------------------------
(S <- as.ktensor(rbind(c(1,2,3,3),c(1,1,2,3)),1000:1001)) 
Alt(S)  # each row of S includes repeats
T <- rtensor()
c(is.zero(Alt(S %X% T)), is.zero(Alt(T %X% S)))

## ----probablytakesalongtime,cache=TRUE----------------------------------------
omega <- Alt(as.ktensor(rbind(1:3),6))
eta <- Alt(as.ktensor(rbind(4:5),60))
theta <- Alt(as.ktensor(rbind(6:7),14))

omega
eta
theta

f1 <- as.function(Alt(Alt(omega %X% eta) %X% theta))
f2 <- as.function(Alt(omega %X% eta %X% theta))
f3 <- as.function(Alt(omega %X% Alt(eta %X% theta)))
V <- matrix(rnorm(9*14),ncol=9)
c(f1(V),f2(V),f3(V))

## ----asktensor,cache=TRUE-----------------------------------------------------
omega <- rform(2,2,19)
eta <- rform(3,2,19)
theta <- rform(2,2,19)

a1 <- as.ktensor(omega ^ (eta ^ theta))
a2 <- as.ktensor((omega ^ eta) ^ theta)
a3 <- Alt(as.ktensor(omega) %X% as.ktensor(eta) %X% as.ktensor(theta))*90

c(is.zero(a1-a2),is.zero(a1-a3),is.zero(a2-a3))

## -----------------------------------------------------------------------------
(rand_tensor <- rtensor(k=5,n=9)*120)
S1 <- Alt(rand_tensor)  # 720 terms, too long to print
(SA1 <- Alt(rand_tensor,give_kform=TRUE))

## -----------------------------------------------------------------------------
V <- matrix(rnorm(45),ncol=5)
c(as.function(S1)(V),as.function(SA1)(V)) # should match

