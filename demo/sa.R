## Simulated Annealing for iris dataset (filter method)
sa(iris, 'Species', roughsetConsistency, temperature = 5, temperature_min=0.01,
   reduction=0.6, verbose=TRUE)

## Simulated Annealing for mtcars dataset (filter method)
sa(mtcars, 'mpg', roughsetConsistency, temperature = 5, temperature_min=0.01,
   reduction=0.6, verbose=TRUE)

## Simulated Annealing for iris dataset (wrapper classification)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
# Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy",
                      tuneGrid = expand.grid(k = c(1:20)))
wrapper <- wrapperGenerator("knn", resamplingParams, fittingParams) # wrapper method
sa(iris, 'Species', wrapper, temperature = 5, temperature_min=0.01, reduction=0.6,
   innerIter=1, verbose=TRUE)
sa(iris, 'Species', wrapper, temperature = 5, temperature_min=0.01, reduction=0.6,
   innerIter=5, verbose=TRUE) # Simulated Annealing with 5 inner iterations

## Simulated Annealing for mtcars dataset (wrapper regression)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
# Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="RMSE",
                      tuneGrid = expand.grid(size = seq(1,12,by=2), decay=0), trace=FALSE)
wrapper <- wrapperGenerator("nnet",resamplingParams, fittingParams) # wrapper method
sa(mtcars, 'mpg', wrapper, temperature = 5, temperature_min=0.01, reduction=0.6,
   innerIter=1, verbose=TRUE)
sa(mtcars, 'mpg', wrapper, temperature = 5, temperature_min=0.01, reduction=0.6,
   innerIter=5, verbose=TRUE) # Simulated Annealing with 5 inner iterations