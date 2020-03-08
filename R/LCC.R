#' @importFrom purrr map_dbl
orderFeatures <- function(data, class, features, measure) {
  values <- features %>% map_dbl(function(x) measure(data, class ,x))
  result <- data.frame(feature = features, value = values)
  max <- attr(measure,'maximize')
  if (max) {
    return(as.vector(result[order(-result$value),c(1)]))
  } else {
    return(as.vector(result[order(result$value),c(1)]))
  }
}

#' @author Alfonso Jiménez-Vílchez
#' @title Linear Consistency-Constrained algorithm
#' @description Linear Consistency-Constrained algorithm described in \insertCite{ShinXu2009}{FSinR}.
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param featureSetEval - The measure for evaluate feature sets
#' @param featureEval - The measure for evaluate individual features
#' @param threshold - Threshold
#'
#' @return A list is returned containing:
#' \describe{
#'   \item{bestFeatures}{A vector with all features. Selected features are marked with 1, unselected features are marked with 0}
#'   \item{bestFitness}{Evaluation measure obtained with the feature selection}
#' }
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#' ## sfbs method for iris dataset (filter method)
#' LCC(iris, 'Species', IEConsistency)
LCC <- function(data, class, featureSetEval, featureEval=symmetricalUncertain, threshold = 0.9) {
  if (attr(featureSetEval, 'kind') == "Individual measure") {
    stop('Only feature set measures can be used');
  }
  column.names <- names(data) 
  class.position <- which(column.names == class) 
  features <- column.names[-class.position] 
  feat.sub <- as.vector(features)
  
  best.value <- featureSetEval(data,class, features)
  max <- attr(featureSetEval,'maximize')
  
  if ((max && best.value >= threshold) || (!max && best.value <= threshold)) {
    feat.sub <- orderFeatures(data, class, feat.sub, featureEval)
    for (i in rev(seq(along = feat.sub))) {
      feat <- feat.sub[[i]]
      feat.prueba <- feat.sub
      feat.prueba <- feat.prueba[feat.prueba != feat]
      value <- featureSetEval(data, class, feat.prueba)
      
      # Find the feature that removing it, we can get a better evaluation
      if ((max && value >= threshold) || (!max && value <= threshold)) {
          best.value <- value
          feat.sub <- feat.prueba
        }
      }
    }
  res <- list(NULL)
  best.set.aux <- matrix(rep(0,(ncol(data)-1)), ncol=(ncol(data)-1), byrow=FALSE, dimnames=list(c(),column.names[-class.position]))
  best.set.aux[which(column.names[-class.position]%in%feat.sub)] <- 1
  res[[1]] <- best.set.aux
  res[[2]] <- best.value
  names(res) <- c("bestFeatures","bestFitness") 
  
  res
}
attr(LCC,'shortName') <- "LCC"
attr(LCC,'name') <- "Linear Consistency-Constrained"
