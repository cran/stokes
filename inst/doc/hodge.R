## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)
library("stokes")
library("emulator")
library("permutations")
options(polyform = FALSE)
set.seed(1)

## -----------------------------------------------------------------------------
hodge

## ---- include = FALSE---------------------------------------------------------
options(kform_symbolic_print = NULL)

## -----------------------------------------------------------------------------
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

