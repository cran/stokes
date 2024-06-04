## ----setup, include=FALSE-----------------------------------------------------
set.seed(0)
library("permutations")
library("stokes")
options(rmarkdown.html_vignette.check_title = FALSE)
options(polyform = FALSE)
knitr::opts_chunk$set(echo = TRUE)
knit_print.function <- function(x, ...){dput(x)}
registerS3method(
  "knit_print", "function", knit_print.function,
  envir = asNamespace("knitr")
)

## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/stokes.png", package = "stokes"))

## ----label=showhodge,comment=""-----------------------------------------------
hodge

## ----label=setsymbprint,include = FALSE---------------------------------------
options(kform_symbolic_print = NULL)

## ----label=simpexamp----------------------------------------------------------
(a <- d(2) ^ d(6) ^ d(7))
hodge(a,9)

## ----label=useperm------------------------------------------------------------
p <- c(2,6,7,  1,3,4,5,8,9)
(pw <- as.word(p))
print_word(pw)
sgn(pw)

## ----label=shorthodge---------------------------------------------------------
hodge(d(c(2,6,7)),9)

## ----label=defaulthodge-------------------------------------------------------
hodge(d(c(2,6,7)))

## ----defineo------------------------------------------------------------------
(o <- rform())
hodge(o)

## ----verifyhodgeo-------------------------------------------------------------
o ^ hodge(o)
kinner(o,o)*volume(dovs(o))

## ----defdif-------------------------------------------------------------------
diff <- function(a,b){a^hodge(b) ==  kinner(a,b)*volume(dovs(a))}

## ----calldefdif---------------------------------------------------------------
diff(rform(),rform())

## ----alldefdif----------------------------------------------------------------
all(replicate(10,diff(rform(),rform())))

## -----------------------------------------------------------------------------
options(kform_symbolic_print = "dx")
hodge(dx,3)

## ----showvcp------------------------------------------------------------------
vcp3

## -----------------------------------------------------------------------------
u <- c(1,4,2)
v <- c(2,1,5)
w <- c(1,-3,2)
x <- c(-6,5,7)
c(
  hodge(as.1form(u) ^ vcp3(v,w))        == as.1form(v*sum(w*u) - w*sum(u*v)),
  hodge(vcp3(u,v) ^ as.1form(w))        == as.1form(v*sum(w*u) - u*sum(v*w)),
  as.1form(as.function(vcp3(v,w))(u)*u) == hodge(vcp3(u,v) ^ vcp3(u,w))     ,
  hodge(hodge(vcp3(u,v)) ^ vcp3(w,x))   == sum(u*w)*sum(v*x) - sum(u*x)*sum(v*w)
)		  

## -----------------------------------------------------------------------------
options(kform_symbolic_print = NULL)  # default print method
(o <- kform_general(4,2,1:6))

## -----------------------------------------------------------------------------
options(kform_symbolic_print = "txyz")
o

## -----------------------------------------------------------------------------
hodge(o)
hodge(o,g=c(-1,1,1,1))
hodge(o)-hodge(o,g=c(-1,1,1,1))

## ----reset_default_print_method, include=FALSE--------------------------------
options(kform_symbolic_print = NULL)

