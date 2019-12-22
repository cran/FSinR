## Select Threshold for iris dataset (filter method)
# Features with a evaluation measure higher than 0.7
selectThreshold(iris, 'Species', mutualInformation, 0.7)

## Select Threshold for mtcars dataset (filter method)
# Features with a evaluation measure fewer than 20
selectThreshold(mtcars, 'mpg', mutualInformation, 20.0)

## Select Threshold for iris dataset (wrapper classification)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
# Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy",
                      tuneGrid = expand.grid(k = c(1:20)))
wrapper <- wrapperGenerator("knn", resamplingParams, fittingParams) # wrapper method
selectThreshold(iris, 'Species', wrapper, 0.7) # Features with a evaluation measure higher than 0.7

## Select Threshold for mtcars dataset (wrapper regression)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
# Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="RMSE",
                      tuneGrid = expand.grid(size = seq(1,12,by=2), decay=0), trace=FALSE)
wrapper <- wrapperGenerator("nnet",resamplingParams, fittingParams) # wrapper method
selectThreshold(mtcars, 'mpg', wrapper, 20.0) # Features with a evaluation measure fewer than 20