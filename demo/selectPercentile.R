## Select Percentile for iris dataset (filter method)
selectPercentile(iris, 'Species', giniIndex, 80) # 80% best features

## Select Percentile for mtcars dataset (filter method)
selectPercentile(mtcars, 'mpg', giniIndex, 70) # 70% best features

## Select Percentile for iris dataset (wrapper classification)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
# Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy",
                      tuneGrid = expand.grid(k = c(1:20)))
wrapper <- wrapperGenerator("knn", resamplingParams, fittingParams) # wrapper method
selectPercentile(iris, 'Species', wrapper, 80) # 80% best features

## Select Percentile for mtcars dataset (wrapper regression)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
# Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="RMSE",
tuneGrid = expand.grid(size = seq(1,12,by=2), decay=0), trace=FALSE)
wrapper <- wrapperGenerator("nnet",resamplingParams, fittingParams) # wrapper method
selectPercentile(mtcars, 'mpg', wrapper, 70) # 70% best features