## ----setup, include=FALSE-----------------------------------------------------
set.seed(0)
library("stokes")
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/stokes.png", package = "stokes"))

## ----label=definedxdydz-------------------------------------------------------
dx <- d(1)
dy <- d(2)
dz <- d(3)

## -----------------------------------------------------------------------------
v <- c(2,3,7)
c(as.function(dx)(v),as.function(dx+dy)(v),as.function(dx+100*dz)(v))

## -----------------------------------------------------------------------------
e(1,3)
e(2,3)
e(3,3)

## -----------------------------------------------------------------------------
u <- e(1,3)
v <- e(2,3)
w <- e(3,3)
matrix(c(
    as.function(dx)(u), as.function(dx)(v), as.function(dx)(w),
    as.function(dy)(u), as.function(dy)(v), as.function(dy)(w),
    as.function(dz)(u), as.function(dz)(v), as.function(dz)(w)
),3,3)

## -----------------------------------------------------------------------------
as.function(dx ^ dy)(cbind(c(2,3,5),c(4,1,2)))

## ----label=showdx-------------------------------------------------------------
dx

## ----label=morecomplicatedcombination-----------------------------------------
(X <- dx^dy -7*dx^dz + 3*dy^dz)

## ----dxdyequalsminusdydx------------------------------------------------------
dx ^ dy == -dy ^ dx

## ----setusedx-----------------------------------------------------------------
options(kform_symbolic_print = 'dx')

## ----showdxwithusedx----------------------------------------------------------
dx
dx^dy + 56*dy^dz

## ----runsoutofalphabet--------------------------------------------------------
rform()

## ----hodgedxdydz--------------------------------------------------------------
hodge(dx^dy + 13*dy^dz)

## ----hodgedx------------------------------------------------------------------
hodge(dx)

## ----hodgedx3-----------------------------------------------------------------
hodge(dx,3)

## -----------------------------------------------------------------------------
options(kform_symbolic_print = NULL)
d(8)

## ----label=savedxdydz---------------------------------------------------------
save(dx, dy, dz, file="dx.rda")

