## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)
library("stokes")
set.seed(1)

## -----------------------------------------------------------------------------
kinner

## -----------------------------------------------------------------------------
dt <- d(1)
dx <- d(2)
dy <- d(3)
dz <- d(4)
p <- c("dt^dx","dt^dy","dt^dz","dx^dy","dx^dz","dy^dz")

mink <- diag(c(1,-1,-1,-1)) # Minkowski metric

M <- matrix(NA,6,6)
rownames(M) <- p
colnames(M) <- p

do <- function(x){eval(parse(text=x))}
for(i in seq_len(6)){
  for(j in seq_len(6)){
    M[i,j] <- kinner(do(p[i]),do(p[j]),M=mink)
  }
}	
M

## -----------------------------------------------------------------------------
outer(p,p,Vectorize(function(i,j){kinner(do(i),do(j),M=mink)}))

## ----removedtdxdydz-----------------------------------------------------------
rm(dt,dx,dy,dz)

## ----reset_default_print_method, include=FALSE--------------------------------
options(kform_symbolic_print = NULL)

