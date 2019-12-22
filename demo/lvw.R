## lvw method for iris dataset (filter method)
lvw(iris, 'Species', roughsetConsistency, K=15, verbose=TRUE)

## lvw method for mtcars dataset (filter method)
lvw(mtcars, 'mpg', roughsetConsistency, K=15, verbose=TRUE)

## lvw method for iris dataset (wrapper classification)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
# Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy",
                      tuneGrid = expand.grid(k = c(1:20)))
wrapper <- wrapperGenerator("knn", resamplingParams, fittingParams) # wrapper method
lvw(iris, 'Species', wrapper, K=15, verbose=TRUE)

## sfs method for mtcars dataset (wrapper regression)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
# Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="RMSE",
                      tuneGrid = expand.grid(size = seq(1,12,by=2), decay=0), trace=FALSE)
wrapper <- wrapperGenerator("nnet",resamplingParams, fittingParams) # wrapper method
lvw(mtcars, 'mpg', wrapper, K=15, verbose=TRUE)