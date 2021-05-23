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
x <- c(120,212,NA,300,323,NA,NA,96,178,164)
y <- c(NA,153,304,177,NA,90,89,200,110,40)
MLE.het(x,y,alternative = "two.sided")
MLE.het(x,y,alternative = "greater")
MLE.het(x,y,alternative = "less")

## ----eval=FALSE---------------------------------------------------------------
#  MLE.hom(x,y,alternative = 'two.sided')

## -----------------------------------------------------------------------------
x <- c(120,212,NA,300,323,NA,NA,96,178,164)
y <- c(NA,153,304,177,NA,90,89,200,110,40)
MLE.hom(x,y,alternative = "two.sided")
MLE.hom(x,y,alternative = "greater")
MLE.hom(x,y,alternative = "less")

