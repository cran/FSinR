## Ant Colony Optimization for iris dataset (filter method)
aco(iris, 'Species', roughsetConsistency, population = 10, iter = 5, verbose = TRUE)

## Ant Colony Optimization for mtcars dataset (filter method)
aco(mtcars, 'mpg', roughsetConsistency, population = 10, iter = 5, verbose = TRUE)

## Ant Colony Optimization for iris dataset (wrapper classification)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
# Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy",
                      tuneGrid = expand.grid(k = c(1:20)))
wrapper <- wrapperGenerator("knn", resamplingParams, fittingParams) # wrapper method
aco(iris, 'Species', wrapper, population = 10, iter = 5, verbose = TRUE)

## Ant Colony Optimization for mtcars dataset (wrapper regression)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
 # Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="RMSE",
                      tuneGrid = expand.grid(size = seq(1,12,by=2), decay=0), trace=FALSE)
wrapper <- wrapperGenerator("nnet",resamplingParams, fittingParams) # wrapper method
aco(mtcars, 'mpg', wrapper, population = 10, iter = 5, verbose = TRUE)