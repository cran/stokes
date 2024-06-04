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
Alt

## ----label=defineS------------------------------------------------------------
S <- as.ktensor(rbind(c(1,7,8)))*6  # the "6" is to stop rounding error
S

## ----label=showaltS-----------------------------------------------------------
Alt(S)

## ----label=verifyv3-----------------------------------------------------------
V <- matrix(rnorm(24),ncol=3)
c(as.function(Alt(S))(V),as.function(Alt(S))(V[,c(2,1,3)]))

## ----label=defineSwithtwoterms------------------------------------------------
(S <- as.ktensor(rbind(c(1,2,4),c(2,2,3)),c(12,1000)))

## ----label=usealtwithrepeats--------------------------------------------------
S
Alt(S)

## ----altonerepeat-------------------------------------------------------------
S <- as.ktensor(matrix(c(
3,2,1,1,
1,4,1,4,
1,1,2,3,
7,7,4,7,
1,2,3,3
),ncol=4,byrow=TRUE),1:5)
S
Alt(S)

## ----altcheckcomplicatedcase--------------------------------------------------
S <- rtensor(k=5,n=9)
S
AS <- Alt(S)
V <- matrix(rnorm(45),ncol=5) # element of (R^9)^5

## ----verifyevenandodd---------------------------------------------------------
V_even <- V[,c(1,2,5,3,4)]  # an even permutation
V_odd  <- V[,c(2,1,5,3,4)]  # an odd permutation
V_rep  <- V[,c(2,1,5,2,4)]  # not a permutation
c(as.function(AS)(V),as.function(AS)(V_even))   # should be identical (even permutation)
c(as.function(AS)(V),as.function(AS)(V_odd))    # should differ in sign only (odd permutation)
as.function(AS)(V_rep)                          # should be zero

## ----verifyaltofalternating---------------------------------------------------
P <- as.ktensor(1+diag(2),c(-7,7))
P
P == Alt(P)

## ----verifyidempotence--------------------------------------------------------
P <- rtensor()*6 # the "6" avoids numerical round-off issues
Alt(Alt(P))==Alt(P)   # should be TRUE

## ----label = omegatensoreta---------------------------------------------------
omega <- as.ktensor(2+diag(2),c(-7,7))
eta <- Alt(ktensor(6*spray(matrix(c(1,2,3,1,4,7,4,5,6),3,3,byrow=TRUE),1:3)))
omega
eta

## ----label=omegawedgeetadirect------------------------------------------------
Alt(omega %X% eta,give_kform = TRUE)
f <- as.function(Alt(omega %X% eta))

## ----label=defineV------------------------------------------------------------
V <-  matrix(rnorm(35),ncol=5)
c(f(V),f(V[,c(2:1,3:5)]))

## ----label=firstpoint---------------------------------------------------------
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

## ----altrand------------------------------------------------------------------
(rand_tensor <- rtensor(k=5,n=9)*120)
S1 <- Alt(rand_tensor)  # 120 terms, too long to print all of it
summary(S1)

## ----altrandgiveTRUE----------------------------------------------------------
(SA1 <- Alt(rand_tensor,give_kform=TRUE))

## ----verifyS1SA1--------------------------------------------------------------
V <- matrix(rnorm(45),ncol=5)
LHS <- as.function(S1)(V)
RHS <- as.function(SA1)(V)
c(LHS=LHS,RHS=RHS,diff=LHS-RHS)

