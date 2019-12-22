library(caret)

#' generaWrapper
#'
#' @author Alfonso Jiménez-Vílchez
#' @author Francisco Aragón Royón
#' @title Wrapper measure generator
#' @description Generates a wrapper function to be used as an evaluator \insertCite{kohavi1997}{FSinR}, given a learner algorithm and related customizable parameters \insertCite{caret}{FSinR}. More specifically, the result of calling this function is another function that is used as an evaluator in the search methods, although you can also call it up to generate an evaluation measure individually.
#' @param learner - Learner to be used. The models available are the models available in caret: http://topepo.github.io/caret/available-models.html
#' @param resamplingParams - Control parameters for evaluating the impact of model tuning parameters. The arguments are the same as those of the caret trainControl function
#' @param fittingParams - Control parameters for choose the best model across the parameters. The arguments are the same as those of the caret train function (minus the parameters: x, y, form, data, method and trainControl).
#'
#' @return Returns a wrapper function that is used to generate an evaluation measure
#' @references
#'    \insertAllCited{}
#' @export
#' @import caret
#' @import e1071
#' @importFrom stats as.formula
#'
#' @examples
#' # Values for the caret trainControl function
#' resamplingParams <- list(method = "repeatedcv", repeats = 3)
#' # Values for the caret train function
#' fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy", 
#'                       tuneGrid = expand.grid(k = c(1:20)))
#' # Generation of the wrapper function
#' wrapper <- wrapperGenerator("knn", resamplingParams, fittingParams)
#' # The function call generates the evaluation measure
#' wrapper(iris, 'Species', c('Sepal.Length', 'Sepal.Width', 'Petal.Length'))
wrapperGenerator <- function(learner, resamplingParams, fittingParams) {
  
  wrapper <- function(original_data, class, features) {
    
    # Check for empty set
    if( length(features) == 0 || features[1] == ''){
      return(0);
    }
    # Check for missing data
    if( any( apply(original_data, 2, function(x) { any(is.na(x)) } ) ) ){
      stop('Feature selection cannot be performed with missing values. Try to impute them previously with the preProcces function of the caret package')
    }
    # Obtain only the desired columns 
    train_data <- subset(original_data, select = c(features,class))
    
    # trainControl
    ctrl <- do.call(caret::trainControl,resamplingParams)
    
    # train
    modelFit <- do.call(caret::train, append(list(
                                        form = as.formula(paste( class, ".", sep = "~")),
                                        data = train_data,
                                        method = learner,
                                        trControl = ctrl),
                                    fittingParams))
    
    # best result
    max(modelFit$results[,modelFit$metric])
  }
  
  # Determine according to the metric whether maximization or minimization is to be done
  if (fittingParams$metric %in% c("Accuracy","Kappa","ROC","Sens","Spec","Rsquared","F","Precision","Recall")) {
    max <- TRUE
  } else if (fittingParams$metric %in% c("RMSE","MAE","logLoss")) {
    max <- FALSE
  }else{
    stop("Metric not supported")
  }
  
  # Added as an attribute
  attr(wrapper,'maximize') <- max
  attr(wrapper,'name') <- paste(learner, " Wrapper")
  attr(wrapper,'kind') <- "Set measure"
  
  return( wrapper )
}
