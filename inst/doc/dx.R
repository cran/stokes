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

## ----label=showdx-------------------------------------------------------------
dx

## ----label=morecomplicatedcombination-----------------------------------------
dx^dy -7*dx^dz + 3*dy^dz

## ----coercedxtoafunction------------------------------------------------------
as.function(dx)(c(113,3,6))

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
d(1) == dx

## ----label=savedxdydz---------------------------------------------------------
save(dx,dy,dz,file="dx.rda")

