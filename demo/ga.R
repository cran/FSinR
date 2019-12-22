## Genetic algorithm for iris dataset (filter method)
ga(iris, 'Species', roughsetConsistency, popSize = 10, maxiter=5, verbose=TRUE)

## Genetic algorithm for mtcars dataset (filter method)
ga(mtcars, 'mpg', roughsetConsistency, popSize = 10, maxiter=5, verbose=TRUE)

## Genetic algorithm for iris dataset (wrapper classification)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
# Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy",
                      tuneGrid = expand.grid(k = c(1:20)))
wrapper <- wrapperGenerator("knn", resamplingParams, fittingParams) # wrapper method
ga(iris, 'Species', wrapper, popSize = 10, pcrossover = 0.8,
   pmutation = 0.1, maxiter=5, verbose=TRUE)

## Genetic algorithm for mtcars dataset (wrapper regression)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
# Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="RMSE",
                      tuneGrid = expand.grid(size = seq(1,12,by=2), decay=0), trace=FALSE)
wrapper <- wrapperGenerator("nnet",resamplingParams, fittingParams) # wrapper method
ga(mtcars, 'mpg', wrapper, popSize = 10, pcrossover = 0.8, pmutation = 0.1, maxiter=5)