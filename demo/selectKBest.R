## Select K best for iris dataset (filter method)
selectKBest(iris, 'Species', roughsetConsistency, 2) # 2 best features

## Select K best for mtcars dataset (filter method)
selectKBest(mtcars, 'mpg', roughsetConsistency, 5) # 5 best features

## Select K best for iris dataset (wrapper classification)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
# Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy",
                      tuneGrid = expand.grid(k = c(1:20)))
wrapper <- wrapperGenerator("knn", resamplingParams, fittingParams) # wrapper method
selectKBest(iris, 'Species', wrapper, 2) # 2 best features

## Select K best for mtcars dataset (wrapper regression)
resamplingParams <- list(method = "cv", number = 10) # Values for the caret trainControl function
# Values for the caret train function (x, y, method and trainControl not neccesary)
fittingParams <- list(preProc = c("center", "scale"), metric="RMSE",
                      tuneGrid = expand.grid(size = seq(1,12,by=2), decay=0), trace=FALSE)
wrapper <- wrapperGenerator("nnet",resamplingParams, fittingParams) # wrapper method
selectKBest(mtcars, 'mpg', wrapper, 5) # 5 best features