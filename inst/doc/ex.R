## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)
library("stokes")
set.seed(0)

## ----defineexeyez-------------------------------------------------------------
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

