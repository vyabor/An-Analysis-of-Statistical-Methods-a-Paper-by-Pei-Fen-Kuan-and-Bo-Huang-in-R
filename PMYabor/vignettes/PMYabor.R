## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PMYabor)

## ----eval=FALSE---------------------------------------------------------------
#  liptak.z(x,y,alternative.pool='greater',alternative.t='two.sided')

## -----------------------------------------------------------------------------
x <- c(120,212,NA,300,323,NA,NA,96,178,164)
y <- c(NA,153,304,177,NA,90,89,200,110,40)
liptak.z(x,y,alternative.pool = "greater")
liptak.z(x,y,alternative.pool = "less")
liptak.z(x,y,alternative.pool = "two.sided")

## ----eval=FALSE---------------------------------------------------------------
#  kim.t(x,y,alternative = 'two.sided')

## -----------------------------------------------------------------------------
x <- c(120,212,NA,300,323,NA,NA,96,178,164)
y <- c(NA,153,304,177,NA,90,89,200,110,40)
kim.t(x,y,alternative = "two.sided")
kim.t(x,y,alternative = "greater")
kim.t(x,y,alternative = "less")

## ----eval=FALSE---------------------------------------------------------------
#  z.corrected(x,y,alternative = 'two.sided')

## -----------------------------------------------------------------------------
x <- c(120,212,NA,300,323,NA,NA,96,178,164)
y <- c(NA,153,304,177,NA,90,89,200,110,40)
z.corrected(x,y,alternative = "two.sided")
z.corrected(x,y,alternative = "greater")
z.corrected(x,y,alternative = "less")

## ----eval=FALSE---------------------------------------------------------------
#  MLE.het(x,y,alternative = 'two.sided')

## -----------------------------------------------------------------------------
# Generating heteroscedastic data:
set.seed(123)
n <- 15
x1 <- 1:n
sd1 <- runif(n,0,5)
het1 <- rnorm(n,0,sd1*x1)
y1 <- x1+het1
y1[c(1,2,3,6)] <- NA

set.seed(321)
x2 <- 1:n
sd2 <- runif(n,0,5)
het2 <- rnorm(n,0,sd2*x2)
y2 <- x2+het2
y2[c(10,13,14,15)] <- NA

MLE.het(y1,y2,alternative = "two.sided")
MLE.het(y1,y2,alternative = "greater")
MLE.het(y1,y2,alternative = "less")

## ----eval=FALSE---------------------------------------------------------------
#  MLE.hom(x,y,alternative = 'two.sided')

## -----------------------------------------------------------------------------
# Generating homoscedastic data:
set.seed(123)
n <- 15
x1 <- 1:n
het1 <- rnorm(n,0,5)
y1 <- x1+het1
y1[c(1,2,3,6)] <- NA

set.seed(321)
x2 <- 1:n
het2 <- rnorm(n,0,5)
y2 <- x2+het2
y2[c(10,13,14,15)] <- NA

MLE.hom(y1,y2,alternative = "two.sided")
MLE.hom(y1,y2,alternative = "greater")
MLE.hom(y1,y2,alternative = "less")

