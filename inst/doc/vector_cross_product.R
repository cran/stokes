## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/stokes.png", package = "stokes"))

## ----setup, include=FALSE-----------------------------------------------------
set.seed(0)
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)
library("stokes")
library("permutations")
knit_print.function <- function(x, ...){dput(x)}
registerS3method(
  "knit_print", "function", knit_print.function,
  envir = asNamespace("knitr")
)

## ----label=showfunc1,comment=""-----------------------------------------------
vector_cross_product

## ----setup2, include=FALSE----------------------------------------------------
knit_print.function <- function(x, ...){
a <- capture.output(print(x))
paste(gsub(" " ,"",a[seq(from=1,to=length(a)-2)]),collapse="")
}
registerS3method(
  "knit_print", "function", knit_print.function,
  envir = asNamespace("knitr")
)

## ----label=showfunc2,comment=""-----------------------------------------------
vcp3

## ----resetdefaults,include=FALSE----------------------------------------------


## ----label=521120-------------------------------------------------------------
(M <- cbind(c(5,-2,1),c(1,2,0)))
vector_cross_product(M)

## ----label=rnorm30------------------------------------------------------------
vector_cross_product(matrix(rnorm(30),6,5))

## ----label=checkrighthand-----------------------------------------------------
det(cbind(M,vector_cross_product(M)))>0

## ----label=severecheck--------------------------------------------------------
f <- function(n){
  M <- matrix(rnorm(n^2+n),n+1,n)
  det(cbind(M,vector_cross_product(M)))>0
}

all(sapply(sample(3:10,100,replace=TRUE),f))

## ----verifyalternatingproperty------------------------------------------------
M <- matrix(rnorm(42),7,6)
crossprod(M,vector_cross_product(M))

## ----verifyspivakfirst--------------------------------------------------------
M <- matrix(rnorm(30),6,5)
sigma <- as.cycle("(12)(345)")
sgn(sigma)
Mdash <- M[,as.function(sigma)(seq_len(5))]
vector_cross_product(M) + vector_cross_product(Mdash)

## ----verifyspivaksecond-------------------------------------------------------
Mdash <- M
Mdash[,3] <- pi*Mdash[,3]
vector_cross_product(Mdash) - vector_cross_product(M) * pi

## ----verifyspivakthird--------------------------------------------------------
M1 <- M
M2 <- M
Msum <- M
v1 <- runif(6)
v2 <- runif(6)
M1[,3] <- v1
M2[,3] <- v2
Msum[,3] <- v1+v2
vector_cross_product(M1) + vector_cross_product(M2) - vector_cross_product(Msum)

## ----label=sixdimcheck--------------------------------------------------------
set.seed(2)
M <- matrix(rnorm(30),6,5)
(ans1 <- vector_cross_product(M))

## ----label=wedgeprodcheck-----------------------------------------------------
(jj <- as.1form(M[,1]) ^ as.1form(M[,2]) ^ as.1form(M[,3]) ^ as.1form(M[,4]) ^ as.1form(M[,5]))
(ans2 <- hodge(jj))

## ----label=slickcheck---------------------------------------------------------
(ans3 <- hodge(Reduce(`^`,lapply(1:5,function(i){as.1form(M[,i])}))))

## ----label=subtract1form------------------------------------------------------
(diff <- as.1form(ans1) - ans3)
coeffs(diff)

## ----showalternativevcp,eval=FALSE--------------------------------------------
# function(u,v){contract(volume(3),cbind(u,v))}

## ----showvcp3-----------------------------------------------------------------
vcp3

## ----label=definevcp----------------------------------------------------------
u <- c(1,4,2)
v <- c(2,1,5)
(p <- vcp3(u,v))  # 'p' for (cross) product

## ----label=ucvijk-------------------------------------------------------------
ucv  <- as.function(p)
c(i=ucv(ex), j=ucv(ey), k=ucv(ez))

## ----label=ucvw---------------------------------------------------------------
w <- c(1,-3,2)
ucv(w)

## ----label=verifyallfour------------------------------------------------------
x <- c(-6,5,7)  # u,v,w as before
c(
  hodge(as.1form(u) ^ vcp3(v,w))        == as.1form(v*sum(w*u) - w*sum(u*v)),
  hodge(vcp3(u,v) ^ as.1form(w))        == as.1form(v*sum(w*u) - u*sum(v*w)),
  as.1form(as.function(vcp3(v,w))(u)*u) == hodge(vcp3(u,v) ^ vcp3(u,w))     ,
  hodge(hodge(vcp3(u,v)) ^ vcp3(w,x))   == sum(u*w)*sum(v*x) - sum(u*x)*sum(v*w)
)		  

## ----showunaryvectorcrossproduct----------------------------------------------
vector_cross_product(rbind(4,7))

## ----createzerobyonematrix----------------------------------------------------
M <- matrix(data=NA,nrow=1,ncol=0)
M
dput(M)

## ----try vectorcrossproductzerobyone------------------------------------------
vector_cross_product(M)

## ----showzerobyzeromatrix-----------------------------------------------------
M[-1,,drop=FALSE]

## ----calculatedetermiantofzerobyzeromatrix------------------------------------
det(matrix(NA,0,0))

