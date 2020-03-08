# Measures based on information theory
#' @author Adan M. Rodriguez
#' @author Alfonso Jiménez-Vílchez
#' @title Entropy
#' @description Calculates the entropy value, using the information theory.
#' @param x - Collection of values
#'
#' @return - Entropy value
#' @export
#'
#' @examples
#' entropy(iris$Sepal.Length)
entropy <- function(x) {
  # Calculate the frequency of each value of the feature
  freqs <- table(x) / length(x) 
  freqs <- as.vector(freqs)   
  
  # Calculate the entropy
  result <- -sum(freqs * log2(freqs))   
  
  return(result) 
}

#' @author Adan M. Rodriguez
#' @author Alfonso Jiménez-Vílchez
#' @title EntropyJ
#' @description Calculates the entropyJ value, using the information theory.
#' @param x - Collection of values
#'
#' @return - EntropyJ value
#' @export
#'
#' @examples
#' entropyJ(iris$Sepal.Length)
entropyJ <- function(x) {
  # Calculate the frequency of each value of the feature in the selected class
  freqs <- x / sum(x) 
  freqs <- as.vector(freqs)    
  logs <- freqs * log2(freqs)
  logs <- replace(logs, is.na(logs), 0)
  
  # Calculate the joint entropy
  summatory <- -sum(logs)
  
  return(summatory)
}

#' @author Adan M. Rodriguez
#' @title The mutual information measure
#' @description This measure calculates the mutual information value, using the information theory \insertCite{QianShu2015}{FSinR}.
#' @param data - A data frame with the features and the class of the examples.  Feature columns are expected to be factors, as all features should be discrete.
#' @param class - The name of the dependent variable
#' @param features - The names of the selected features
#'
#' @return - The mutual information value for the selected features
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#' mutualInformation(iris,'Species',c('Sepal.Width', 'Sepal.Length'))
mutualInformation <- function(data, class, features) {
  # Get values of the features
  features <- unlist(features) 
  feature.data <- data[, features, drop = FALSE] 
  
  # Calculate entropy of the class
  classes <- c(data[[class]]) # Obtenemos valores de la clase en numeros
  class.entropy <- entropy(classes)

  # To calculate the measure of only one feature...
  if (ncol(feature.data) == 1) {
    # Calculate the entropy of the feature
    features.entropies <- sapply(feature.data, entropy) 
    vector <- unlist(feature.data) 
    
    # Calculate joint entropy between the feature and the class
    joint.entropy <- entropyJ(table(vector, classes))
    
    # To calculate the measure of only 2 or more features...
  } else { 
      # Calculate joint entropy between the features
      features.entropies <- entropyJ(table(feature.data))
      tabla <- data.frame(feature.data, classes)
      
      # Calculate joint entropy between the features and the class
      joint.entropy <- entropyJ(table(tabla))
  }
  
  # Calculate the mutual information
  result <- class.entropy + unname(features.entropies) - joint.entropy
  
  return(result)
}
attr(mutualInformation,'shortName') <- "mutualInformation"
attr(mutualInformation,'name') <- "Mutual Information"
attr(mutualInformation,'maximize') <- TRUE
attr(mutualInformation,'kind') <- "Set measure"

#' @author Adan M. Rodriguez
#' @title The gain ratio measure
#' @description This measure calculates the gain ratio value \insertCite{Quinlan1986}{FSinR}, using the information theory.
#' @param data - A data frame with the features and the class of the examples.  Feature columns are expected to be factors, as all features should be discrete.
#' @param class - The name of the dependent variable
#' @param features - The names of the selected features
#'
#' @return - The gain ratio value for the selected features.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#' gainRatio(iris,'Species',c('Sepal.Width', 'Sepal.Length'))
gainRatio <- function(data, class, features) {
  # Get values of the features
  features <- unlist(features) 
  feature.data <- data[, features, drop = FALSE] 

  # Calculate the entropy of the features
  if (ncol(feature.data) == 1) {
    features.entropies <- sapply(feature.data, entropy) 
  } else {
      features.entropies <- entropyJ(table(feature.data))
  }

  # Invoke the mutual information function
  mutual.information <- mutualInformation(data, class, features)
  
  # Calculate the Gain Ratio
  gain <- mutual.information / unname(features.entropies)
  
  if (is.nan(gain))
    gain <- 0

  return(gain)
}
attr(gainRatio,'shortName') <- "gainRatio"
attr(gainRatio,'name') <- "Gain Ratio"
attr(gainRatio,'maximize') <- TRUE
attr(gainRatio,'kind') <- "Set measure"

#' @author Adan M. Rodriguez
#' @title Symmetrical uncertain measure
#' @description This measure calculates the symmetrical uncertain value \insertCite{WittenFrank2005}{FSinR}, using the information theory.
#' @param data - A data frame with the features and the class of the examples.  Feature columns are expected to be factors, as all features should be discrete.
#' @param class - The name of the dependent variable
#' @param features - The names of the selected features
#'
#' @return - The symmetrical uncertain value for the selected features
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#' symmetricalUncertain(iris,'Species',c('Sepal.Width', 'Sepal.Length'))
symmetricalUncertain <- function(data, class, features) {
  # Get values of the features
  features <- unlist(features) 
  feature.data <- data[, features, drop = FALSE] 

  # Calculate entropy of the class
  classes <- c(data[[class]]) # Obtenemos valores de la clase en numeros
  class.entropy <- entropy(classes)

  # Calculate the entropy of the features
  if (ncol(feature.data) == 1) {
    features.entropies <- sapply(feature.data, entropy) 
  } else {
      features.entropies <- entropyJ(table(feature.data))
  }
  
  # Invoke the mutual information function
  mutual.information <- mutualInformation(data, class, features)
  
  # calculate the symmetrical uncertain
  symm.uncertain <- (2 * mutual.information) / (class.entropy + unname(features.entropies))

  return(symm.uncertain)
}
attr(symmetricalUncertain,'shortName') <- "symmetricalUncertain"
attr(symmetricalUncertain,'name') <- "Symmetrical Uncertain"
attr(symmetricalUncertain,'maximize') <- TRUE
attr(symmetricalUncertain,'kind') <- "Set measure"
