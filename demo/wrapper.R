source("R/exhaustiveSearch.R")
source("R/sequentialSelection.R")
source("R/metaheuristic.R")
source("R/probabilistic.R")
source("R/localSearch.R")
source("R/wrapperGenerator.R")


## Example 1 (iris classification with knn)

#resamplingParams <- list(method = "repeatedcv", repeats = 3) # Values for trainControl function
resamplingParams <- list(method = "cv", number = 10) # Values for trainControl function
#fittingParams <- list(preProc = c("center", "scale"), metric="Kappa", tuneGrid = expand.grid(k = seq(1,30,by=2))) # Values for train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy", tuneGrid = expand.grid(k = c(1:20))) # Values for train function (x, y, method and trainControl not neccesary)

wrapper <- wrapperGenerator("knn", resamplingParams, fittingParams) # wrapper method

sfs(iris, 'Species', wrapper)
sffs(iris, 'Species', wrapper)
sbs(iris, 'Species', wrapper)
sfbs(iris, 'Species', wrapper)
breadthFirstSearch(iris, 'Species', wrapper)
deepFirstSearch(iris, 'Species', wrapper)
#ga(iris, 'Species', wrapper)
ga(iris, 'Species', wrapper, popSize = 10, pcrossover = 0.8, pmutation = 0.1, maxiter=5, verbose=TRUE)
#sa(iris, 'Species', wrapper)
sa(iris, 'Species', wrapper, temperature = 5, temperature_min=0.01, reduction=0.6, innerIter=1, verbose=TRUE)
woa(iris, 'Species', wrapper, population = 10, iter = 5, verbose = TRUE)
aco(iris, 'Species', wrapper, population =10, iter = 5, q = 5, verbose = TRUE)
#ts(iris, 'Species', wrapper)
#ts(iris, 'Species', wrapper, numNeigh = 4, tamTabuList = 4, iter = 5, verbose=TRUE)
#ts(iris, 'Species', wrapper, numNeigh = 4, tamTabuList = 4, iter = 5, intensification=2, iterIntensification=5, verbose=TRUE)
ts(iris, 'Species', wrapper, numNeigh = 4, tamTabuList = 4, iter = 5, intensification=2, iterIntensification=5, diversification=1, iterDiversification=5, verbose=TRUE)
hc(iris, 'Species', wrapper)
#lvw(iris, 'Species', wrapper)
lvw(iris, 'Species', wrapper, K=15, verbose=TRUE)
selectKBest(iris, 'Species', wrapper, 2)
selectPercentile(iris, 'Species', wrapper, 80)
selectThreshold(iris, 'Species', wrapper, 0.70) 
selectThresholdRange(iris, 'Species', wrapper, 0.50)
selectDifference(iris, 'Species', wrapper, 0.25)
selectSlope(iris, 'Species', wrapper, 0.5)
  
  
## Example 2 (mtcars regression with nnet)

#resamplingParams <- list(method = "repeatedcv", repeats = 3) # Values for trainControl function
resamplingParams <- list(method = "cv", number = 10) # Values for trainControl function
fittingParams <- list(preProc = c("center", "scale"), metric="RMSE", tuneGrid = expand.grid(size = seq(1,12,by=2), decay=0), trace=FALSE) # Values for train function (x, y, method and trainControl not neccesary)

wrapper <- wrapperGenerator("nnet",resamplingParams, fittingParams) # wrapper method

sfs(mtcars, 'mpg', wrapper)
sffs(mtcars, 'mpg', wrapper)
sbs(mtcars, 'mpg', wrapper)
sfbs(mtcars, 'mpg', wrapper)
breadthFirstSearch(mtcars, 'mpg', wrapper)
deepFirstSearch(mtcars, 'mpg', wrapper)
ga(mtcars, 'mpg', wrapper, popSize = 10, pcrossover = 0.8, pmutation = 0.1, maxiter=5, verbose=TRUE)
sa(mtcars, 'mpg', wrapper, temperature = 5, temperature_min=0.01, reduction=0.6, innerIter=1, verbose=TRUE)
#ts(mtcars, 'mpg', wrapper)
#ts(mtcars, 'mpg', wrapper, numNeigh = 10, tamTabuList = 6, iter = 15, verbose=TRUE)
#ts(mtcars, 'mpg', wrapper, numNeigh = 4, tamTabuList = 4, iter = 5, intersification=2, iterIntersification=5, verbose=TRUE)
ts(iris, 'Species', wrapper, numNeigh = 4, tamTabuList = 4, iter = 5, intersification=2, iterIntersification=5, diversification=1, iterDiversification=5, verbose=TRUE)

hc(mtcars, 'mpg', wrapper)
lvw(mtcars, 'mpg', wrapper, K=15, verbose=TRUE)
