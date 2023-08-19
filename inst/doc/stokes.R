## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)
library("stokes")
set.seed(1)

## ----label=loadstokeslibrary,message=FALSE------------------------------------
library("stokes")

## ----label=straightforwardidiom-----------------------------------------------
k <- 4
n <- 5
M <- matrix(c(5,1,1,1, 1,1,2,3, 1,3,4,2),3,4,byrow=TRUE)
M
S <- as.ktensor(M,coeffs= 0.5 + 1:3)
S

## ----setupsetseed-------------------------------------------------------------
set.seed(0)
(E <- matrix(rnorm(n*k),n,k))   # A random point in V^k

## ----showfandfE---------------------------------------------------------------
f <- as.function(S)
f(E)

## ----simpletensorarith--------------------------------------------------------
S1 <- as.ktensor(1+diag(4),1:4)
2*S-3*S1

## ----verifylinearity----------------------------------------------------------
LHS <- as.function(2*S-3*S1)(E)
RHS <- 2*as.function(S)(E) -3*as.function(S1)(E)
c(lhs=LHS,rhs=RHS,diff=LHS-RHS)

## ----E1E2E3columns------------------------------------------------------------

E1 <- E
E2 <- E
E3 <- E

x1 <- rnorm(n)
x2 <- rnorm(n)
r1 <- rnorm(1)
r2 <- rnorm(1)

E1[,2] <- x1
E2[,2] <- x2
E3[,2] <- r1*x1 + r2*x2

## ----asfuncS------------------------------------------------------------------
f <- as.function(S)
LHS <- r1*f(E1) + r2*f(E2)
RHS <- f(E3)
c(lhs=LHS,rhs=RHS,diff=LHS-RHS)

## ----E1E2matrix---------------------------------------------------------------
E1 <- matrix(rnorm(n*k),n,k)
E2 <- matrix(rnorm(n*k),n,k)
LHS <- f(r1*E1+r2*E2)
RHS <- r1*f(E1)+r2*f(E2)
c(lhs=LHS,rhs=RHS,diff=LHS-RHS)

## ----S1S2tensors--------------------------------------------------------------
(S1 <- ktensor(spray(cbind(1:3,2:4),1:3)))
(S2 <- as.ktensor(matrix(1:6,2,3)))

## ----S1S2tensorprod-----------------------------------------------------------
tensorprod(S1,S2)

## ----tensorS1S2verify---------------------------------------------------------
E <- matrix(rnorm(30),6,5)
LHS <- as.function(tensorprod(S1,S2))(E)
RHS <- as.function(S1)(E[,1:2]) * as.function(S2)(E[,3:5])
c(lhs=LHS,rhs=RHS,diff=LHS-RHS)

## ----showAltS1----------------------------------------------------------------
S1
Alt(S1)

## ----verifyAltisAlt-----------------------------------------------------------
E <- matrix(rnorm(8),4,2)
Erev <- E[,2:1]
as.function(Alt(S1))(E) + as.function(Alt(S1))(Erev)  # should be zero

## ----usedifferentials---------------------------------------------------------
M <- matrix(c(4,2,3,1,4,2),2,3,byrow=TRUE)
M
K <- as.kform(M,c(1,5))
K

## ----dx3inmatrixform----------------------------------------------------------
dx3 <- as.kform(matrix(3,1,1),1)
options(kform_symbolic_print = NULL) # revert to default print method
dx3

## ----showdx3beingused---------------------------------------------------------
as.function(dx3)(c(14,15,16))
as.function(dx3)(c(14,15,16,17,18))  # idiom can deal with arbitrary vectors

## ----dx5askform---------------------------------------------------------------
dx5 <- as.kform(matrix(5,1,1),1)
as.function(dx3 + 2*dx5)(1:10)  # picks out element 3 + 2*element 5

## ----M1K1M2K2-----------------------------------------------------------------
M1 <- matrix(c(3,5,4, 4,6,1),2,3,byrow=TRUE)
K1 <- as.kform(M1,c(2,7))
K1
M2 <- cbind(1:5,3:7)
K2 <- as.kform(M2,1:5)
K2

## ----K1wedgeK2----------------------------------------------------------------
K1 ^ K2

## ----F1F2F3kforms-------------------------------------------------------------
F1 <- as.kform(matrix(c(3,4,5, 4,6,1,3,2,1),3,3,byrow=TRUE))
F2 <- as.kform(cbind(1:6,3:8),1:6)
F3 <- kform_general(1:8,2)
(F1 ^ F2) ^ F3
F1 ^ (F2 ^ F3)

## ----wedgearithmeticF1F2F3----------------------------------------------------
(F1 ^ F2) ^ F3 - F1 ^ (F2 ^ F3)

## ----kformgeneralandrel-------------------------------------------------------
Krel <- kform_general(4,2,1:6)
Krel

## ----addkformsnontrivial------------------------------------------------------
K1 <- as.kform(matrix(1:4,2,2),c(1,109))
K2 <- as.kform(matrix(c(1,3,7,8,2,4),ncol=2,byrow=TRUE),c(-1,5,4))
K1
K2
K1+K2

## ----definetensorU------------------------------------------------------------
U <- ktensor(spray(cbind(1:4,2:5),1:4))
U

## ----coerceUsymbolic----------------------------------------------------------
as.symbolic(U)

## ----showassymbolicK----------------------------------------------------------
K <- kform_general(3,2,1:3)
K
as.symbolic(K,d="d",symbols=letters[23:26])

## ----useprintmethodsymbolic---------------------------------------------------
options(kform_symbolic_print = "d")
K

## ----printshowofelementaryforms-----------------------------------------------
(d(1) + d(5)) ^ (d(3)-5*d(2)) ^ d(7)
options(kform_symbolic_print = NULL) # restore default

## ----lookatfunc---------------------------------------------------------------
(o <- rform())  # a random 3-form
V <- matrix(runif(21),ncol=3)
LHS <- as.function(o)(V)
RHS <- as.function(contract(o,V[,1]))(V[,-1])
c(LHS=LHS,RHS=RHS,diff=LHS-RHS)

## ----coerceotoafunction-------------------------------------------------------
as.function(contract(o,V[,1:2]))(V[,-(1:2),drop=FALSE])

## ----contractovlosetrue-------------------------------------------------------
contract(o,V)

## ----contractovlosefalse------------------------------------------------------
contract(o,V,lose=FALSE)

## ----usetransform-------------------------------------------------------------
options(kform_symbolic_print = "dx")   # uses dx etc in print method
pullback(dx^dy+5*dx^dz, matrix(1:9,3,3))
options(kform_symbolic_print = NULL) # revert to default

## ----defineoandM--------------------------------------------------------------
(o <- 2 * as.kform(2) ^ as.kform(4) ^ as.kform(5))
M <- matrix(rnorm(25),5,5)

## ----transformbackandforward--------------------------------------------------
o |> pullback(M) |> pullback(solve(M))

## ----zapsmallroundofferrors---------------------------------------------------
o |> pullback(M) |> pullback(solve(M)) |> zap()

## ----showgradinuse------------------------------------------------------------
grad(c(0.4,0.1,-3.2,1.5))

## ----definefofakform----------------------------------------------------------
f <- function(x){
    n <- length(x)
    as.kform(t(apply(diag(n)<1,2,which)))
}

## ----exampleuseoff------------------------------------------------------------
f(1:5)

## ----definedfanduseit---------------------------------------------------------
df  <- function(x){
    n <- length(x)
    S <- sum(x^2)
    grad(rep(c(1,-1),length=n)*(S^(n/2) - n*x^2*S^(n/2-1))/S^n
    )
}

## ----dfevaluatedatonetofive---------------------------------------------------
df(1:5)

## ----dfevaluatedatrandompoint-------------------------------------------------
x <- rnorm(9)
print(df(x) ^ f(x))  # should be zero

## ----definef1f2f3usingarith---------------------------------------------------
f1 <- function(w,x,y,z){x + y^3 + x*y*w*z}
f2 <- function(w,x,y,z){w^2*x*y*z + sin(w) + w+z}
f3 <- function(w,x,y,z){w*x*y*z + sin(x) + cos(w)}

## ----definedwdxdydzofkforms---------------------------------------------------
dw <- as.kform(1)
dx <- as.kform(2)
dy <- as.kform(3)
dz <- as.kform(4)

## ----definephi----------------------------------------------------------------
phi <-
  (
    +f1(1,2,3,4) ^ dw ^ dx
    +f2(1,2,3,4) ^ dw ^ dy
    +f3(1,2,3,4) ^ dy ^ dz
  )

## ----e1e2e3usingwedge---------------------------------------------------------
e1 <- dw ^ dx
e2 <- dw ^ dy
e3 <- dy ^ dz

phi <-
  (
    +f1(1,2,3,4) ^ e1
    +f2(1,2,3,4) ^ e2
    +f3(1,2,3,4) ^ e3
  )
phi

## ----demonstrateDerivpackage--------------------------------------------------
library("Deriv")
Df1 <- Deriv(f1)(1,2,3,4)
Df2 <- Deriv(f2)(1,2,3,4)
Df3 <- Deriv(f3)(1,2,3,4)

## ----showDf1fromderiv---------------------------------------------------------
Df1

## ----calculatedphi------------------------------------------------------------
dphi <-
  (
    +grad(Df1) ^ e1
    +grad(Df2) ^ e2
    +grad(Df3) ^ e3
  )
dphi

## ----diffofdiff---------------------------------------------------------------
Hf1 <- matrix(Deriv(f1,nderiv=2)(1,2,3,4),4,4)
Hf2 <- matrix(Deriv(f2,nderiv=2)(1,2,3,4),4,4)
Hf3 <- matrix(Deriv(f3,nderiv=2)(1,2,3,4),4,4)

## ----setrownames, echo=FALSE--------------------------------------------------
rownames(Hf1) <- c("w","x","y","z")
colnames(Hf1) <- c("w","x","y","z")

## ----Hf1showexample-----------------------------------------------------------
Hf1

## ----ddphishouldbezero--------------------------------------------------------
ij <- expand.grid(seq_len(nrow(Hf1)),seq_len(ncol(Hf1)))

ddphi <- # should be zero
  (
    +as.kform(ij,c(Hf1))
    +as.kform(ij,c(Hf2))
    +as.kform(ij,c(Hf3))
  )

ddphi

## ----slickcreationofphi-------------------------------------------------------
phi <- function(x){
    n <- length(x)
    sum(x^seq_len(n)*rep_len(c(1,-1),n)) * as.kform(t(apply(diag(n)<1,2,which)))
}
phi(1:9)

## ----useslickdefforf----------------------------------------------------------
f <- as.function(phi(1:9))
E <- matrix(runif(72),9,8)   # (R^9)^8
f(E)

## ----dphiusingpowerdef--------------------------------------------------------
dphi <- function(x){
    nn <- seq_along(x)
    sum(nn*x^(nn-1)) * as.kform(seq_along(x))
}
dphi(1:9)

## ----verifyfEbothways---------------------------------------------------------
f <- as.function(dphi(1:9))
E <- matrix(runif(81),9,9)
f(E)
det(E)*f(diag(9))  # should match f(E) by Spivak's 4.6

## ----tidyup,include=FALSE-----------------------------------------------------
i <- 0
j <- 0
k <- 0
rm(i)
rm(j)
rm(k)

