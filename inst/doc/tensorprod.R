## ----setup, include=FALSE-----------------------------------------------------
set.seed(0)
library("spray")
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
tensorprod
tensorprod2

## -----------------------------------------------------------------------------
(a <- ktensor(spray(matrix(c(1,1,2,1),2,2),3:4)))
(b <- ktensor(spray(matrix(c(3,4,7,5,4,3),3,2),7:9)))

## -----------------------------------------------------------------------------
tensorprod(a,b)

## -----------------------------------------------------------------------------
S <- rtensor()
T <- rtensor()
U <- rtensor()
c( left_distributive = S %X% (T+U) == S*T + S*U,
  right_distributive = (S+T) %X% U == S %X% U + T %X% U,
  associative        = S %X% (T %X% U) == (S %X% T) %X% U
  )

## -----------------------------------------------------------------------------
x <- ktensor(spray(matrix(c(1,1,2,1),2,2),1:2))
y <- ktensor(spray(matrix(c(3,4,7,5,4,3),3,2),1:3))
z <- ktensor(spray(matrix(c(1,1,2,1),2,2),1:2))
tensorprod(x, tensorprod(y, z))
tensorprod(tensorprod(x, y), z)

## ----echo=FALSE---------------------------------------------------------------
rm(T)  # tidyup

