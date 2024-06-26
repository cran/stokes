## ----setup, include=FALSE-----------------------------------------------------
set.seed(0)
library("stokes")
library("spray")  # needed for spraycross()
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(echo = TRUE)
knit_print.function <- function(x, ...){dput(x)}
registerS3method(
  "knit_print", "function", knit_print.function,
  envir = asNamespace("knitr")
)

## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/stokes.png", package = "stokes"))

## ----label=showwedge,comment=""-----------------------------------------------
wedge
wedge2

## -----------------------------------------------------------------------------
(a <- spray(matrix(1:4,2,2),c(2,5)))
(b <- spray(matrix(c(10,11,12,13),2,2),c(7,11)))
spraycross(a,b)
spraycross(b,a)

## -----------------------------------------------------------------------------
(x <- as.kform(cbind(1,2),5))
(y <- as.kform(cbind(3,4,7),7))
wedge2(x,y)

## -----------------------------------------------------------------------------
 tx <- as.ktensor(x)    # "tx" = tensor 'x'
(ty <- as.ktensor(y))   # "ty" = tensor 'y'

## -----------------------------------------------------------------------------
M <- matrix(round(rnorm(21),2),7,3) # member of (R^7)^3
c(as.function(y)(M),as.function(ty)(M))

## -----------------------------------------------------------------------------
7*(
 +M[3,1]*M[4,2]*M[7,3] 
 -M[3,1]*M[4,3]*M[7,2] 
 -M[3,2]*M[4,1]*M[7,3] 
 +M[3,2]*M[4,3]*M[7,1] 
 +M[3,3]*M[4,1]*M[7,2]
 -M[3,3]*M[4,2]*M[7,1]
 )

## -----------------------------------------------------------------------------
(z <- tensorprod(as.ktensor(x),as.ktensor(y)))

## -----------------------------------------------------------------------------
wedge(x,y)

## -----------------------------------------------------------------------------
(omega <- as.kform(rbind(c(1,2,8),c(1,3,7)),5:6))

## -----------------------------------------------------------------------------
eta <- as.kform(rbind(c(2,3,5),c(3,5,6)),2:3)
wedge(omega,eta)

