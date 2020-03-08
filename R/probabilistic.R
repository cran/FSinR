# Probabilistic Searchs
#' @author Francisco Aragón Royón
#' @title Las Vegas Wrapper
#' @description The lvw method  \insertCite{LiuSetiono1996}{FSinR} starts with a certain set of features and in each step a new set is randomly generated, if the new set is better it is saved as the best solution. The algorithm ends when there are no improvements in a certain number of iterations.
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param featureSetEval - The measure for evaluate features
#' @param start - Binary vector with the set of initial features (1: selected and 0: unselected) for the algorithm
#' @param K - The maximum number of iterations without improvement to finalize the algorithm
#' @param verbose - Print the partial results in each iteration
#'
#' @return A list is returned containing:
#' \describe{
#'   \item{bestFeatures}{A vector with all features. Selected features are marked with 1, unselected features are marked with 0}
#'   \item{bestFitness}{Evaluation measure obtained with the feature selection}
#'   \item{initialVector}{The vector with which the algorithm started}
#'   \item{initialFitness}{The evaluation measure of the initial vector}
#'   \item{trace}{Matrix with the results of each iteration. It contains the number of the iteration, the value of k, the best set of features selected by the algorithm up to that iteration (1: selected, 0: not selected) and the value of the evaluation measure obtained from that best set of features}
#' }
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#' ## lvw method for iris dataset (filter method)
#' lvw(iris, 'Species', roughsetConsistency, K=15, verbose=TRUE)
lvw <- function(data, class, featureSetEval, start=sample(0:1,ncol(data)-1,replace=TRUE), K=50, verbose=FALSE) {
  if (attr(featureSetEval, 'kind') == "Individual measure") {
    stop('Only feature set measures can be used');
  }
  
  # Check parameters
  if(K < 1) stop('The minimum number of executions must be at least 1')
  
  # Function to generate new subset
  generateRandomSubset <- function(){
    repeat{
      subset <- sample(0:1,ncol(data)-1,replace=TRUE)
      if(!all(subset==0)){
        break
      }
    }
    
    subset
  }
  
  # Extract and eliminate the class to have only the features in the variable 'features' 
  column.names <- names(data) 
  class.position <- which(column.names == class) 
  features <- column.names[-class.position]  
  
  # Check for maximization-minimization
  max <- attr(featureSetEval,'maximize')
  
  # Initialize params
  k <- 0 # Number of iterations without improvement
  iter <- 0 # Number of total iterations
  if(all(start==0)){
    repeat{
      start <- sample(0:1,ncol(data)-1,replace=TRUE)
      if(!all(start==0)){
        break
      }
    }
  }
  
  # List with results
  res <- list(NULL)
  res[[1]] <- NA
  res[[2]] <- NA
  res[[3]] <- start 
  res[[4]] <- NA
  ncol <- 3 + length(start)
  res[[5]] <- matrix(rep(NA,ncol), ncol=ncol, dimnames = list(c(),c(c("iter", "k"), paste0("x", 1:(length(start))), c("fitness"))))
  names(res) <- c("bestFeatures","bestFitness","initialVector","initialFitness","trace") 
  
  best.features <- start
  best.value <- featureSetEval(data, class, features[which(start==1)])
  res[[4]] <- best.value
  C <- length(which(start==1)) # Number of features
  
  if(verbose){
    cat(paste("LVW | InitialVector = ",paste(best.features,collapse="")," | InitialFitness = ",round(best.value,7),"\n"))
  }
  
  while(k<K){
    # generate new subset
    new_subset <- generateRandomSubset() 
    C1 <- length(which(new_subset==1))

    # Obtain the new fitness
    new.value <- featureSetEval(data, class, features[which(new_subset==1)])
    
    if(max){ # Classification -> maximize
      better <- new.value > best.value     
    }else{ # Regression -> minimize
      better <- new.value < best.value  
    }

    if( (better) || ((new.value == best.value) && (C1<C)) ){
      k <- 0
      best.value <- new.value
      best.features <- new_subset
      C <- C1
    }else{
      k <- k + 1       
    }
    
    iter <- iter + 1
    # Print the partial results
    if(verbose){
      cat(paste("LVW | iter = ",iter," | k = ",k," | Vector = ",paste(new_subset,collapse="")," | Fitness = ",round(new.value,7), " | Best_Vector = ",paste(best.features,collapse="")," | Best_Fitness = ",round(best.value,7),"\n") )
    }
    # Save the iteration results
    if(iter==1){
      res$trace[iter,] <- c(iter, k, best.features, best.value)
    }else{
      res$trace <- rbind(res$trace, c(iter, k, best.features, best.value))    
    }
    
  }
  
  # List with results
  best.set.aux <- matrix(best.features, ncol=(ncol(data)-1), byrow=FALSE, dimnames=list(c(),features))
  res[[1]] <- best.set.aux
  res[[2]] <- best.value
  
  res
}

attr(lvw,'shortName') <- "lvw"
attr(lvw,'name') <- "Las Vegas Wrapper"