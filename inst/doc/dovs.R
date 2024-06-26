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
dovs

## ----showrform----------------------------------------------------------------
set.seed(0)
a <- rform(n=4,k=2)
a

## ----coerceatofunction--------------------------------------------------------
f <- as.function(a)
(M <- matrix(1:8,4,2))
f(M)

## ----evaluatef----------------------------------------------------------------
f <- as.function(a)
(M <- matrix(c(1,2,3,4,1454,5,6,7,8,-9564),ncol=2))  # row 5 large numbers
f(M)

## ----fonM---------------------------------------------------------------------
(M <- cbind(c(0,0,0,0,1),runif(5)))
f(M)

## ----dxalone------------------------------------------------------------------
dx

## ----dovsdx-------------------------------------------------------------------
dovs(dx)

## ----hodgedx------------------------------------------------------------------
hodge(dx)

## ----hodgedx3-----------------------------------------------------------------
hodge(dx,3)

## ----hodgeoptions-------------------------------------------------------------
options(kform_symbolic_print="dx")
hodge(dx,3)

## ----reset_default_print_method, include=FALSE--------------------------------
options(kform_symbolic_print = NULL)

