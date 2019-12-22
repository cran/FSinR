# Values for the caret trainControl function
resamplingParams <- list(method = "repeatedcv", repeats = 3)
# Values for the caret train function
fittingParams <- list(preProc = c("center", "scale"), metric = "Accuracy", tuneGrid = expand.grid(k = c(1:20)))
# Generation of the wrapper function
wrapper <- wrapperGenerator("knn", resamplingParams, fittingParams)

# Example of using the wrapper function in a search method
sfs(iris, 'Species', wrapper)

# Example of how the wrapper function is called internally in the search methods.
# The function call generates the evaluation measure
wrapper(iris, 'Species', c('Sepal.Length', 'Sepal.Width', 'Petal.Length'))
