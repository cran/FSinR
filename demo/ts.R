## Taboo-Search algorithm for iris dataset (filter method)
ts(iris, 'Species', roughsetConsistency, iter = 5)

## Taboo-Search algorithm for mtcars dataset (filter method)
ts(mtcars, 'mpg', roughsetConsistency, iter = 5)

## Taboo-Search algorithm for iris dataset (wrapper classification)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
# Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy",
                      tuneGrid = expand.grid(k = c(1:20)))
wrapper <- wrapperGenerator("knn", resamplingParams, fittingParams) # wrapper method
ts(iris, 'Species', wrapper, numNeigh = 4, tamTabuList = 4, iter = 5, intensification=2,
   iterIntensification=5, diversification=1, iterDiversification=5, verbose=TRUE)

## Taboo-Search algorithm for mtcars dataset (wrapper regression)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
# Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="RMSE",
                      tuneGrid = expand.grid(size = seq(1,12,by=2), decay=0), trace=FALSE)
wrapper <- wrapperGenerator("nnet",resamplingParams, fittingParams) # wrapper method
ts(mtcars, 'mpg', wrapper, numNeigh = 4, tamTabuList = 4, iter = 5, intensification=2,
   iterIntensification=5, verbose=TRUE)