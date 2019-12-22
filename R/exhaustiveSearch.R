# Exhaustive searchs

#' @author Adan M. Rodriguez
#' @author Francisco Aragón Royón
#' @title Exhaustive Search. Breadth First Search
#' @description The method searches the whole features subset in breadth first order \insertCite{Kozen1992}{FSinR}
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param featureSetEval - The measure for evaluate features
#'
#' @return A list is returned containing:
#' \describe{
#'   \item{bestFeatures}{A vector with all features. Selected features are marked with 1, unselected features are marked with 0}
#'   \item{bestFitness}{Evaluation measure obtained with the feature selection}
#' }
#' @references
#'    \insertAllCited{}
#' @export
#'
#' @examples
#' ## Breadth First Search for iris dataset (filter method)
#' breadthFirstSearch(iris, 'Species', binaryConsistency)
breadthFirstSearch <- function(data, class, featureSetEval) {
  if (attr(featureSetEval, 'kind') == "Individual measure") {
    stop('Only feature set measures can be used');
  }
  # Extract and eliminate the class to have only the features in the variable 'features'
  column.names <- names(data)
  class.position <- which(column.names == class)
  features <- column.names[-class.position]
  
  # Check for maximization-minimization
  max <- attr(featureSetEval,'maximize')
  
  # In best.set, we store the features that are part of the solution
  best.set <- NULL
  best.value <- NULL
  
  # Queue the root of the tree
  queue <- NULL
  queue[[length(queue)+1]] <- list(list(), as.list(features))

  # Visite each feature
  while (length(queue) > 0) {
    # Pop (introduce) an unvisited node 
    node <- queue[[1]] 
    trunk <- node[[1]] 
    branches <- node[[2]]
    queue[[1]] <- NULL
    
    # visit each branch of the current node
    for (i in seq(along=branches)) {
      set <- c(trunk, branches[[i]])
     
      # Evaluate and check if better
      value <- featureSetEval(data, class, unlist(set))

      # Store the new feature if is better
      if(max){ # Classification -> Maximize
        if (is.null(best.value) || value > best.value) {
          best.value <- value
          best.set <- set
        }
      }else{ # Regression -> Minimize
        if (is.null(best.value) || value < best.value) {
          best.value <- value
          best.set <- set
        }
      }
      
      # Generate branch nodes if there are remaining features to combine. In breadth
      n <- length(branches)
      if (i < n) {
        queue[[length(queue)+1]] <- list(set, branches[(i+1):n])
      }
    }
    
  }

  # List with results
  res <- list(NULL)
  best.set.aux <- matrix(rep(0,length(features)), ncol=length(features), byrow=FALSE, dimnames=list(c(),features))
  pos <- match(unlist(best.set),features)
  best.set.aux[pos] <- 1
  
  res[[1]] <- best.set.aux
  res[[2]] <- best.value
  names(res) <- c("bestFeatures","bestFitness") 
  
  res
}
attr(breadthFirstSearch,'name') <- "Breadth First Search"

#' @author Francisco Aragón Royón
#' @title Exhaustive Search. Deep First Search
#' @description The method searches the whole features subset in deep first order \insertCite{Kozen1992}{FSinR}
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param featureSetEval - The measure for evaluate features
#'
#' @return A list is returned containing:
#' \describe{
#'   \item{bestFeatures}{A vector with all features. Selected features are marked with 1, unselected features are marked with 0}
#'   \item{bestFitness}{Evaluation measure obtained with the feature selection}
#' }
#' @references
#'    \insertAllCited{}
#' @export
#'
#' @examples
#' ## Deep First Search for iris dataset (filter method)
#' deepFirstSearch(iris, 'Species', binaryConsistency)
deepFirstSearch <- function(data, class, featureSetEval) {
  if (attr(featureSetEval, 'kind') == "Individual measure") {
    stop('Only feature set measures can be used');
  }
  # Extract and eliminate the class to have only the features in the variable 'features'
  column.names <- names(data)
  class.position <- which(column.names == class)
  features <- column.names[-class.position]
  
  # Check for maximization-minimization
  max <- attr(featureSetEval,'maximize')
  
  # In best.set, we store the features that are part of the solution
  best.set <- NULL
  best.value <- NULL
  
  # Queue the root of the tree
  queue <- NULL
  queue[[length(queue)+1]] <- list(list(), as.list(features))
  
  # Visite each feature
  while (length(queue) > 0) { 
    # Pop (introduce) an unvisited node 
    node <- queue[[1]] 
    trunk <- node[[1]] 
    branches <- node[[2]]
    queue[[1]] <- NULL

    # visit the first branch of the current node
    set <- c(trunk, branches[[1]])
    # Evaluate and check if better
    value <- featureSetEval(data, class, unlist(set))
    
    # Store the new feature if is better
    if(max){ # Classification -> Maximize
      if (is.null(best.value) || value > best.value) {
        best.value <- value
        best.set <- set
      }
    }else{ # Regression -> Minimize
      if (is.null(best.value) || value < best.value) {
        best.value <- value
        best.set <- set
      }
    }

    
    # Generate branch nodes if there are remaining features to combine. In deep
    n <- length(branches)
    if(n>1){
      queue <- append(queue, list(list(trunk, branches[2:n])), 0)
      queue <- append(queue, list(list(set, branches[2:n])), 0)      
    }
  }
  
  # List with results
  res <- list(NULL)
  best.set.aux <- matrix(rep(0,length(features)), ncol=length(features), byrow=FALSE, dimnames=list(c(),features))
  pos <- match(unlist(best.set),features)
  best.set.aux[pos] <- 1
  
  res[[1]] <- best.set.aux
  res[[2]] <- best.value
  names(res) <- c("bestFeatures","bestFitness") 
  
  res
}
attr(deepFirstSearch,'name') <- "Deep First Search"