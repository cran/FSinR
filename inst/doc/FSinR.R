## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE, 'Install package'-------------------------------------
#  install.packages("FSinR")

## ----message=FALSE, 'Load libraries'-------------------------------------
library(caret)
library(FSinR)

data(iris)

## ---- 'Generate wrapper'-------------------------------------------------
resamplingParams <- list(method = "cv", number = 10)
fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy", tuneGrid = expand.grid(k = c(1:20)))

wrapper <- wrapperGenerator("knn", resamplingParams, fittingParams)

## ---- 'Use wrapper with SFS'---------------------------------------------
result.search.fs <- sfs(iris, "Species", wrapper)

## ----eval=FALSE, 'Use wrapper with TS'-----------------------------------
#  result.search.fs <- ts(iris, 'Species', wrapper)

## ----eval=FALSE, 'Use wrapper with customized TS'------------------------
#  result.search.fs <- ts(iris, 'Species', wrapper, tamTabuList = 4, iter = 5, intensification=2, iterIntensification=5, diversification=1, iterDiversification=5, verbose=FALSE)

## ---- 'Check results'----------------------------------------------------
result.search.fs$bestFeatures
result.search.fs$bestFitness

## ----eval=FALSE, 'Use wrapper as measure'--------------------------------
#  wrapperMeasure <- wrapper(iris,"Species",c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
#  wrapperMeasure

## ----message=FALSE, eval=FALSE, 'Load data for filtering'----------------
#  library(FSinR)
#  
#  data(iris)

## ---- 'Use filter method'------------------------------------------------
result.search.fs <- sfs(iris, "Species", giniIndex)

## ---- 'Check filter results'---------------------------------------------
result.search.fs

## ---- 'Use filter measure'-----------------------------------------------
filterMeasure <- giniIndex(iris, "Species", c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))

## ---- eval=FALSE, 'Apply filter to regression'---------------------------
#  result.search.fs <- sfs(mtcars, "mpg", giniIndex)
#  result.search.fs

