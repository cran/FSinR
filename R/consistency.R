# Measures based on consistency

library(digest)

#' @author Adan M. Rodriguez
#' @title Rough Set consistency measure
#' @description Calculates the rough sets consistency value \insertCite{Pawlak1982}{FSinR} \insertCite{Pawlak1991}{FSinR}, using hash tables
#' @param data - A data frame with the features and the class of the examples. Feature columns are expected to be factors, as all features should be discrete.
#' @param class - The name of the dependent variable
#' @param features - The names of the selected features
#'
#' @return - The consistency value for the selected features
#' @references
#'    \insertAllCited{}
#' @import digest
#' @importFrom stats aggregate
#' @export
#'
#' @examples
#' roughsetConsistency(iris,'Species',c('Sepal.Width', 'Sepal.Length'))
roughsetConsistency <- function(data, class, features) {
  # Get values of the features
  features <- unlist(features)
  feature.data <- data[, features, drop = FALSE]

  # Store the values of each feature in a hash table
  hash.vector <- as.factor(apply(feature.data, 1, digest))

  # If for one value of a feature, we have more than 1 different class, this examples will be inconsistent
  rough.set <- function(vector) {
    if ((length(which(vector != 0))) > 1) {
      return(0)
    } else {
        return(max(vector))
    }
  }

  # Calculate consistent examples
  result <- aggregate(data[[class]], list(hash = hash.vector), function(classes) {
    return(rough.set(as.vector(table(classes))))
  })

  # Calculate rough sets consistecy
  result <- sum(result[[dim(result)[2]]]) / dim(feature.data)[1]

  return(result)
}
attr(roughsetConsistency,'name') <- "Rough Set Consistency"
attr(roughsetConsistency,'maximize') <- TRUE
attr(roughsetConsistency,'kind') <- "Set measure"

#' @author Adan M. Rodriguez
#' @title Binary consistency measure
#' @description Calculates the binary consistency, also known as "Sufficiency test" from FOCUS \insertCite{AlmuallimDietterich1991}{FSinR}
#' @param data - A data frame with the features and the class of the examples. Feature columns are expected to be factors, as all features should be discrete.
#' @param class - The name of the dependent variable
#' @param features - The names of the selected features
#'
#' @return - The consistency value for the selected features
#' @references
#'    \insertAllCited{}
#' @export
#'
#' @examples
#' binaryConsistency(iris,'Species',c('Sepal.Width', 'Sepal.Length'))
binaryConsistency <- function(data, class, features) {
  # Invoke the IEConsistency function
  binary <- IEConsistency(data, class, features)

  # The feature will be consistent 100% or inconsistent 0%
  if (binary == 1) {
    result <- 1
  } else {
      result <- 0
  }

  return(result)
}
attr(binaryConsistency,'name') <- "Binary Consistency"
attr(binaryConsistency,'maximize') <- TRUE
attr(binaryConsistency,'kind') <- "Set measure"

#' @author Adan M. Rodriguez
#' @title Inconsistent Examples consistency measure
#' @description Calculates the inconsistent examples consistency value \insertCite{DashLiu2003}{FSinR}, using hash tables
#' @param data - A data frame with the features and the class of the examples. Feature columns are expected to be factors, as all features should be discrete.
#' @param class - The name of the dependent variable
#' @param features - The names of the selected features
#'
#' @return - The consistency value for the selected features
#' @references
#'    \insertAllCited{}
#' @import digest
#' @importFrom stats aggregate
#' @export
#'
#' @examples
#' IEConsistency(iris,'Species',c('Sepal.Width', 'Sepal.Length'))
IEConsistency <- function(data, class, features) {
  # Get values of the features
  features <- unlist(features)
  feature.data <- data[, features, drop = FALSE]

  # Store the values (column 1) of each feature in a hash table
  hash.vector <- as.factor(apply(feature.data, 1, digest))

  # The examples of the majoritarian class, will be the consistent examples
  result <- aggregate(data[[class]], list(hash = hash.vector), function(classes) {
    return(max(as.vector(table(classes))))
  })

  # Calculate the IEP consistency
  result <- sum(result[[dim(result)[2]]]) / dim(feature.data)[1]

  return(result)
}
attr(IEConsistency,'name') <- "Inconsistent Examples Consistency"
attr(IEConsistency,'maximize') <- TRUE
attr(IEConsistency,'kind') <- "Set measure"


#' @author Adan M. Rodriguez
#' @title Inconsistent Examples Pairs consistency measure
#' @description Calculates the inconsistent examples pairs consistency value, using hash tables \insertCite{Arauzo2007}{FSinR}

#' @param data - A data frame with the features and the class of the examples.  Feature columns are expected to be factors, as all features should be discrete.
#' @param class - The name of the dependent variable
#' @param features - The names of the selected features
#'
#' @return - The consistency value for the selected features
#' @references
#'    \insertAllCited{}
#' @import digest
#' @importFrom stats aggregate
#' @export
#'
#' @examples
#' IEPConsistency(iris,'Species',c('Sepal.Width', 'Sepal.Length'))
IEPConsistency <- function(data, class, features) {
  # Get values of the features
  features <- unlist(features)
  feature.data = data[, features, drop=FALSE]

  # Store the values (column 1) of each feature in a hash table
  hash.vector = as.factor(apply(feature.data, 1, digest))

  # Count the number of inconsistent pairs
  example.pairs <- function(vector) {
    num.class <- sum(vector)

    # If the number of zeros equals to number of classes - 1... return 0
    if ((length(which(vector == 0))) == (length(vector) - 1)) {
      return(0)
    } else {
        different <- 0
        left <- num.class

        for (i in vector) {
          ne <- i
          left <- left - ne
          different <- different + (ne * left)
      }
      return(different)
    }
  }

  # For each value of the feature...
  inconsistent.pairs <- aggregate(data[[class]], list(hash = hash.vector), function(classes) {
    return(example.pairs(as.vector(table(classes))))
  })

  # Number of pairs = (number of elements * (number of elements - 1)) / 2
  num.pairs <- (dim(feature.data)[1] * (dim(feature.data)[1] - 1)) / 2

  # Calculate IEP consistency
  result <- 1 - (sum(inconsistent.pairs[[dim(inconsistent.pairs)[2]]]) / num.pairs)

  return(result)
}
attr(IEPConsistency,'name') <- "Inconsistent Examples Pairs Consistency"
attr(IEPConsistency,'maximize') <- TRUE
attr(IEPConsistency,'kind') <- "Set measure"
