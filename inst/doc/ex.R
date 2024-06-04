## ----setup, include=FALSE-----------------------------------------------------
set.seed(0)
library("stokes")
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/stokes.png", package = "stokes"))

## ----label=defineexeyez-------------------------------------------------------
ex <- e(1,3)
ey <- e(2,3)
ez <- e(3,3)

## -----------------------------------------------------------------------------
fdx <- as.function(dx)
fdy <- as.function(dy)
fdz <- as.function(dz)
matrix(c(
      fdx(ex),fdx(ey),fdx(ez),
      fdy(ex),fdy(ey),fdy(ez),
      fdz(ex),fdz(ey),fdz(ez)
      ),3,3)

## ----label=savedxdydz---------------------------------------------------------
save(ex,ey,ez,file="exeyez.rda")

