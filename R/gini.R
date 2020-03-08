#' @author Adan M. Rodriguez
#' @title Gini index measure
#' @description This measure calculates the gini index \insertCite{Ceriani2012}{FSinR} of discrete features
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param features - The names of the selected feature
#' @return - The Gini index value for the selected features
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#' giniIndex(iris,'Species',c('Sepal.Width', 'Sepal.Length'))
giniIndex <- function(data, class, features) {
  # Get values of the features
  features <- unlist(features) 
  feature.data <- data[, features, drop=FALSE]

  # We will store the values of each feature in a hash table
  hash.vector <- as.factor(apply(feature.data, 1, digest)) 

  # Calculate Gini of each value of the feature
  gini <- function(vector) {
    gini.result <- 0
    for (i in vector)
      gini.result <- gini.result + (i / sum(vector)) ^ 2
    
    return(gini.result)
  }
  
  # Store the gini index of each feature value
  result <- aggregate(data[[class]], list(hash = hash.vector), function(classes) {
    return(gini(as.vector(table(classes)))) 
  }) 
  
  # Store the number of times each feature value appears
  ni <- aggregate(data[[class]], list(hash = hash.vector), function(classes) {
    return (sum(table(classes)))  
  })
  div <- (ni[[dim(ni)[2]]] / dim(feature.data)[1])
 
  # Final gini index of the feature
  final.gini <- (sum(div * result[[dim(result)[2]]]))
  
  return(final.gini)
}
attr(giniIndex,'shortName') <- "giniIndex"
attr(giniIndex,'name') <- "Gini Index"
attr(giniIndex,'maximize') <- TRUE
attr(giniIndex,'kind') <- "Set measure"
