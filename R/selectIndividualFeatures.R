# Cutting criteria
#' @author Adan M. Rodriguez
#' @author Francisco Aragón Royón
#' @title Select K best
#' @description Takes the 'k' features with the greatest evaluations
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param featureEval - The measure used to evaluate features
#' @param k - Number (positive integer) of returned features
#'
#' @return A list is returned containing:
#' \describe{
#'   \item{bestFeatures}{A vector with all features. Selected features are marked with 1, unselected features are marked with 0}
#'   \item{featuresSelected}{The names of the k returned features sorted according to the result of the evaluation measure}
#'   \item{valuePerFeature}{The evaluation measures of the k returned features}
#' }
#' @export
#'
#' @examples
#' ## Select K best for iris dataset (filter method)
#' selectKBest(iris, 'Species', roughsetConsistency, 2) # 2 best features
selectKBest <- function(data, class, featureEval, k=1) {
  # Take only the features of the data set
  column.names <- names(data)
  class.position <- which(column.names == class) 
  features <- data[-class.position] 

  # Check if the k value is valid
  if (k <= 0) {
    stop("k should be > 0")
  } else if (k > ncol(features)) {
    stop("k cannot be greater than the number of features")
  }
  
  # Check for maximization-minimization
  max <- attr(featureEval,'maximize')
  
  values <- NULL
  names <- NULL

  # Evaluate each feature separately
  for (i in colnames(features)) {
    names[length(names) + 1] <- i
    value <- featureEval(data, class, i)
    values[length(values) + 1] <- value
  }

  # Create a data.frame with the values
  feats <- data.frame(values)
  rownames(feats) <- names
  
  # Check for maximization-minimization
  if(max){ # Classification -> maximize
    # Sort the features
    sorted.features <- rownames(feats)[order(feats$values, decreasing = TRUE)] 
    sorted.values <- feats[order(feats$values, decreasing = TRUE),1]
  }else{ # Regression -> minimize
    # Sort the features
    sorted.features <- rownames(feats)[order(feats$values)]
    sorted.values <- feats[order(feats$values),1]
  }
  
  # List with results
  res <- list(NULL)
  res[[1]] <- matrix(rep(0,ncol(features)), nrow=1, byrow=FALSE, dimnames=list(c(),colnames(features)))
  res[[1]][match(sorted.features[1:k],colnames(features))] <- 1
  res[[2]] <- sorted.features[1:k] # Select features according to the k highest/lowest scores
  res[[3]] <- sorted.values[1:k] # Select features according to the k highest/lowest scores
  names(res) <- c("bestFeatures","featuresSelected","valuePerFeature") 
  
  return(res)
}
attr(selectKBest,'shortName') <- "selectKBest"
attr(selectKBest,'name') <- "Select K Best"

#TODO: Reference
#' @author Adan M. Rodriguez
#' @author Francisco Aragón Royón
#' @title Select Percentile
#' @description Selects a fraction, given as a percentage, of the total number of available features
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param featureEval - The measure used to evaluate features
#' @param percentile - Number (positive integer) between 0 and 100
#'
#' \describe{
#'   \item{bestFeatures}{A vector with all features. Selected features are marked with 1, unselected features are marked with 0}
#'   \item{featuresSelected}{The names of the returned features sorted according to the result of the evaluation measure}
#'   \item{valuePerFeature}{The evaluation measures of the returned features}
#' }
#' @export
#'
#' @examples
#' ## Select Percentile for iris dataset (filter method)
#' selectPercentile(iris, 'Species', giniIndex, 80) # 80% best features
selectPercentile <- function(data, class, featureEval, percentile=10) {
  # Take only the features of the data set
  column.names <- names(data) 
  class.position <- which(column.names == class) 
  features <- data[-class.position] 

  # Check if the percentile value is valid
  if (percentile < 0 || percentile > 100) {
    stop ("Percentile should be >=0 and <= 100")
  }
  
  # Check for maximization-minimization
  max <- attr(featureEval,'maximize')
  
  values <- NULL
  names <- NULL
  
  # Evaluate each feature separately
  for (i in colnames(features)) {
    names[length(names) + 1] <- i
    value <- featureEval(data, class, i)
    values[length(values) + 1] <- value
  }
  
  # Create a data.frame with the values
  feats <- data.frame(values)
  rownames(feats) <- names
  
  # Check for maximization-minimization
  if(max){ # Classification -> maximize
    # Sort the features
    sorted.features <- rownames(feats)[order(feats$values, decreasing = TRUE)] 
    sorted.values <- feats[order(feats$values, decreasing = TRUE),1]
  }else{ # Regression -> minimize
    # Sort the features
    sorted.features <- rownames(feats)[order(feats$values)]
    sorted.values <- feats[order(feats$values),1]
  }
  
  percentile <- percentile / 100
  max.features <- (length(sorted.features) * percentile)
  
  # List with results
  res <- list(NULL)
  res[[1]] <- matrix(rep(0,ncol(features)), nrow=1, byrow=FALSE, dimnames=list(c(),colnames(features)))
  res[[1]][match(sorted.features[1:round(max.features)],colnames(features))] <- 1
  res[[2]] <- sorted.features[1:round(max.features)] # Select features according to a percentile of the highest/lowest scores
  res[[3]] <- sorted.values[1:round(max.features)] # Select features according to a percentile of the highest/lowest scores
  names(res) <- c("bestFeatures","featuresSelected","valuePerFeature") 
  
  return(res)
}
attr(selectPercentile,'shortName') <- "selectPercentile"
attr(selectPercentile,'name') <- "Select Percentile"

#TODO: Reference
#' @author Adan M. Rodriguez
#' @author Francisco Aragón Royón
#' @title Select threshold
#' @description Selects the features whose evaluation is over/under a user given threshold (It depends on the method that generates the evaluation measure. For example: under for regression methods, over for classification methods, etc.). Features that do not satisfy the threshold, will be removed
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param featureEval - The measure used to evaluate features
#' @param threshold - Number between 0 and 1
#'
#' @return A list is returned containing:
#' \describe{
#'   \item{bestFeatures}{A vector with all features. Selected features are marked with 1, unselected features are marked with 0}
#'   \item{featuresSelected}{The names of the returned features sorted according to the result of the evaluation measure}
#'   \item{valuePerFeature}{The evaluation measures of the returned features}
#' }
#' @export
#'
#' @examples
#' ## Select Threshold for iris dataset (filter method)
#' # Features with a evaluation measure higher than 0.7
#' selectThreshold(iris, 'Species', mutualInformation, 0.7) 
selectThreshold <- function(data, class, featureEval, threshold=0.5) {
  # Take only the features of the data set
  column.names <- names(data) 
  class.position <- which(column.names == class)
  features <- data[-class.position] 
  
  # Check for maximization-minimization
  max <- attr(featureEval,'maximize')
  
  values <- NULL
  names <- NULL
  
  # Evaluate each feature separately
  for (i in colnames(features)) {
    names[length(names) + 1] <- i
    value <- featureEval(data, class, i)
    values[length(values) + 1] <- value
  }
  
  # Create a data.frame with the values
  feats <- data.frame(values)
  rownames(feats) <- names
  
  # Check for maximization-minimization
  if(max){ # Classification -> maximize
    # Select the features that exceed the threshold
    over.threshold <- NULL
    over.threshold.value <- NULL
    for (i in 1:nrow(feats)) {
      if (feats$values[i] > threshold) {
        row <- rownames(feats)[i]
        over.threshold <- c(over.threshold, row)
        over.threshold.value <- c(over.threshold.value, feats$values[i])
      }
    }    
    # Sort the features
    if(!is.null(over.threshold)){
      sorted.features <- over.threshold[order(over.threshold.value, decreasing = TRUE)]
      sorted.values <- over.threshold.value[order(over.threshold.value, decreasing = TRUE)]      
    }else{
      sorted.features <- NA
      sorted.values <- NA
    }

  }else{ # Regression -> minimize
    # Select the features that not exceed the threshold
    under.threshold <- NULL
    under.threshold.value <- NULL
    for (i in 1:nrow(feats)) {
      if (feats$values[i] < threshold) {
        row <- rownames(feats)[i]
        under.threshold <- c(under.threshold, row)
        under.threshold.value <- c(under.threshold.value, feats$values[i])
      }
    }    
    # Sort the features
    if(!is.null(under.threshold)){
      sorted.features <- under.threshold[order(under.threshold.value)]
      sorted.values <- under.threshold.value[order(under.threshold.value)]
    }else{
      sorted.features <- NA
      sorted.values <- NA
    }
  }

  
  # List with results
  res <- list(NULL)
  res[[1]] <- matrix(rep(0,ncol(features)), nrow = 1, byrow = FALSE, dimnames = list(c(),colnames(features)))
  res[[1]][match(sorted.features,colnames(features))] <- 1
  res[[2]] <- sorted.features
  res[[3]] <- sorted.values
  names(res) <- c("bestFeatures","featuresSelected","valuePerFeature") 
  
  return(res)
}
attr(selectThreshold,'shortName') <- "selectThreshold"
attr(selectThreshold,'name') <- "Select Threshold"

#TODO: Reference
# Evaluation over a threshold given as a fraction of the range of evaluation
#' @author Adan M. Rodriguez
#' @author Francisco Aragón Royón
#' @title Select threshold range
#' @description Selects the features whose evaluation is over a threshold, where this threshold is given as: (((min - max) * p.threshold) + max)
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param featureEval - The measure used to evaluate features
#' @param p.threshold - Number between 0 and 1
#'
#' @return A list is returned containing:
#' \describe{
#'   \item{bestFeatures}{A vector with all features. Selected features are marked with 1, unselected features are marked with 0}
#'   \item{featuresSelected}{The names of the returned features sorted according to the result of the evaluation measure}
#'   \item{valuePerFeature}{The evaluation measures of the returned features}
#' }
#' @export
#'
#' @examples
#' ## Select Threshold range for iris dataset (filter method)
#' selectThresholdRange(iris, 'Species', determinationCoefficient, 0.3)
selectThresholdRange <- function(data, class, featureEval, p.threshold=0.3) {
  # Take only the features of the data set
  column.names <- names(data) 
  class.position <- which(column.names == class) 
  features <- data[-class.position] 

  # Check for maximization-minimization
  max <- attr(featureEval,'maximize')
  
  values <- NULL
  names <- NULL
  
  # Evaluate each feature separately
  for (i in colnames(features)) {
    names[length(names) + 1] <- i
    value <- featureEval(data, class, i)
    values[length(values) + 1] <- value
  }
  
  # Create a data.frame with the values
  feats <- data.frame(values)
  rownames(feats) <- names

  # Take the max and min value to evaluate the threshold
  max <- max(feats)
  min <- min(feats)
  threshold <- ((max - min) * p.threshold) + min

  # Check for maximization-minimization
  if(max){ # Classification -> maximize
    # Select the features that exceed the threshold
    over.threshold <- NULL
    over.threshold.value <- NULL
    for (i in 1:nrow(feats)) {
      if (feats$values[i] > threshold) {
        row <- rownames(feats)[i]
        over.threshold <- c(over.threshold, row)
        over.threshold.value <- c(over.threshold.value, feats$values[i])
      }
    }    
    # Sort the features
    if(!is.null(over.threshold)){
      sorted.features <- over.threshold[order(over.threshold.value, decreasing = TRUE)]
      sorted.values <- over.threshold.value[order(over.threshold.value, decreasing = TRUE)]      
    }else{
      sorted.features <- NA
      sorted.values <- NA
    }
    
  }else{ # Regression -> minimize
    # Select the features that not exceed the threshold
    under.threshold <- NULL
    under.threshold.value <- NULL
    for (i in 1:nrow(feats)) {
      if (feats$values[i] < threshold) {
        row <- rownames(feats)[i]
        under.threshold <- c(under.threshold, row)
        under.threshold.value <- c(under.threshold.value, feats$values[i])
      }
    }    
    # Sort the features
    if(!is.null(under.threshold)){
      sorted.features <- under.threshold[order(under.threshold.value)]
      sorted.values <- under.threshold.value[order(under.threshold.value)]
    }else{
      sorted.features <- NA
      sorted.values <- NA
    }
  }
  
  
  # List with results
  res <- list(NULL)
  res[[1]] <- matrix(rep(0,ncol(features)), nrow=1, byrow=FALSE, dimnames=list(c(),colnames(features)))
  res[[1]][match(sorted.features,colnames(features))] <- 1
  res[[2]] <- sorted.features
  res[[3]] <- sorted.values
  names(res) <- c("bestFeatures","featuresSelected","valuePerFeature") 
  
  return(res)
}
attr(selectThresholdRange,'shortName') <- "selectThresholdRange"
attr(selectThresholdRange,'name') <- "Select Threshold Range"

#TODO: Reference
#' @author Adan M. Rodriguez
#' @author Francisco Aragón Royón
#' @title Select difference
#' @description Selects features (in descending order from the best evaluation measure to the lowest) until evaluation difference is over a threshold.
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param featureEval - The measure used to evaluate features
#' @param d.threshold - Number between 0 and 1, to calculate the slope
#'
#' @return A list is returned containing:
#' \describe{
#'   \item{bestFeatures}{A vector with all features. Selected features are marked with 1, unselected features are marked with 0}
#'   \item{featuresSelected}{The names of the returned features sorted according to the result of the evaluation measure}
#'   \item{valuePerFeature}{The evaluation measures of the returned features}
#' }
#' @export
#'
#' @examples
#' ## Select Difference for iris dataset (filter method)
#' # Selects features in descending order as long as the difference between them is less than 0.1
#' selectDifference(iris, 'Species', chiSquared, 0.1) 
selectDifference <- function(data, class, featureEval, d.threshold=0.1) {
  # Take only the features of the data set
  column.names <- names(data) 
  class.position <- which(column.names == class) 
  features <- data[-class.position] 
  
  # Check for maximization-minimization
  max <- attr(featureEval,'maximize')
  
  values <- NULL
  names <- NULL
  
  # Evaluate each feature separately
  for (i in colnames(features)) {
    names[length(names) + 1] <- i
    value <- featureEval(data, class, i)
    values[length(values) + 1] <- value
  }
  
  # Create a data.frame with the values
  feats <- data.frame(values)
  rownames(feats) <- names
  
  # Check for maximization-minimization
  if(max){ # Classification -> maximize
    # Sort the features in descending order
    sorted.features <- rownames(feats)[order(-feats$values)] 
    sorted.values <- feats$values[order(-feats$values)]
    sorted.frame <- data.frame(row.names = sorted.features, values = sorted.values)
    difference <- 0
    num.features <- nrow(sorted.frame)
    under.threshold <- NULL
    under.value <- NULL
    
    # Always select the first feature
    row <- (rownames(sorted.frame)[1])
    under.threshold <- c(under.threshold, row)
    under.value <- c(under.value, sorted.frame[row,1])
    
    # Select the features whose difference does not exceed the threshold
    for (i in 1:num.features) {
      if (i == num.features)
        break
      else {
        # Calculate the difference between 2 features
        first <- sorted.frame$values[i]
        second <- sorted.frame$values[i+1]
        difference <- abs(first - second)
        
        # Select features that meet the criteria. Stop the algorithm when criterion is not met
        if (difference < d.threshold) {
          row <- rownames(sorted.frame)[i+1]
          under.threshold <- c(under.threshold, row) 
          under.value <- c(under.value, sorted.frame[row,1])
        } else { 
          break
        }
      }
    }
    
  }else{ # Regression -> minimize
    # Sort the features in ascending order
    sorted.features <- rownames(feats)[order(feats$values)] 
    sorted.values <- feats$values[order(feats$values)]
    sorted.frame <- data.frame(row.names = sorted.features, values = sorted.values)
    difference <- 0
    num.features <- nrow(sorted.frame)
    under.threshold <- NULL
    under.value <- NULL
    
    # Always select the first feature
    row <- (rownames(sorted.frame)[1])
    under.threshold <- c(under.threshold, row)
    under.value <- c(under.value, sorted.frame[row,1])
    
    # Select the features whose difference does not exceed the threshold
    for (i in 1:num.features) {
      if (i == num.features)
        break
      else {
        # Calculate the difference between 2 features
        first <- sorted.frame$values[i]
        second <- sorted.frame$values[i+1]
        difference <- abs(first - second)
        
        # Select features that meet the criteria. Stop the algorithm when criterion is not met
        if (difference < d.threshold) {
          row <- rownames(sorted.frame)[i+1]
          under.threshold <- c(under.threshold, row) 
          under.value <- c(under.value, sorted.frame[row,1])
        } else { 
          break
        }
      }
    }
    
  }
  

  # List with results
  res <- list(NULL)
  res[[1]] <- matrix(rep(0,ncol(features)), nrow=1, byrow=FALSE, dimnames=list(c(),colnames(features)))
  res[[1]][match(under.threshold,colnames(features))] <- 1
  res[[2]] <- under.threshold
  res[[3]] <- under.value
  names(res) <- c("bestFeatures","featuresSelected","valuePerFeature") 
  
  return(res)
}
attr(selectDifference,'shortName') <- "selectDifference"
attr(selectDifference,'name') <- "Select Difference"

#TODO: Reference
#' @author Adan M. Rodriguez
#' @title Select slope
#' @description Selects features (in descending order from the best evaluation measure to the lowest) until the slope to the next feature is over a threshold. The slope is calculated as: (s.threshold) / (number of features)
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param featureEval - The measure used to evaluate features
#' @param s.threshold - Number between 0 and 1
#'
#' @return A list is returned containing:
#' \describe{
#'   \item{bestFeatures}{A vector with all features. Selected features are marked with 1, unselected features are marked with 0}
#'   \item{featuresSelected}{The names of the returned features sorted according to the result of the evaluation measure}
#'   \item{valuePerFeature}{The evaluation measures of the returned features}
#' }
#' @export
#'
#' @examples
#' ## Select Slope for iris dataset (filter method)
#' selectSlope(iris, 'Species', IEPConsistency, 0.8)
selectSlope <- function(data, class, featureEval, s.threshold=0.8) {
  column.names <- names(data) 
  class.position <- which(column.names == class) 
  features <- data[-class.position] 
  n <- ncol(features)
  
  # Calculate the slope
  s.threshold <- s.threshold / n

  # Select features until the slope to the next feature is over a threshold
  selectDifference(data, class, featureEval, s.threshold)
}
attr(selectSlope,'shortName') <- "selectSlope"
attr(selectSlope,'name') <- "Select Slope"