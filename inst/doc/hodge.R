## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)
library("stokes")
library("emulator")
options(polyform = FALSE)
set.seed(1)

## -----------------------------------------------------------------------------
hodge

## ---- include = FALSE---------------------------------------------------------
options(kform_symbolic_print = NULL)

## -----------------------------------------------------------------------------
(o <- rform())
hodge(o)

## -----------------------------------------------------------------------------
o ^ hodge(o)
kinner(o,o)*volume(dovs(o))

## -----------------------------------------------------------------------------
diff <- function(a,b){a^hodge(b) ==  kinner(a,b)*volume(dovs(a))}

## -----------------------------------------------------------------------------
diff(rform(),rform())

## -----------------------------------------------------------------------------
all(replicate(10,diff(rform(),rform())))

## -----------------------------------------------------------------------------
options(kform_symbolic_print = "dx")
hodge(dx,3)

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

