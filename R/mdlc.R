#' @author Alfonso Jiménez-Vílchez
#' @title MDLC evaluation measure
#' @description Applies the Minimum-Description_Length-Criterion (MDLC) \insertCite{Sheinvald1990}{FSinR} to evaluate a set of features.
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param features - The names of the selected features
#'
#' @return - MDLC value for the selected features
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @import digest
#' @importFrom stats cov
#' @export
#'
#' @examples
#' MDLC(iris,'Species',c('Sepal.Width', 'Sepal.Length'))
MDLC <- function(data, class, features) {
  # table = featureSelection.continuize(table)
  # #table = featureSelection.removeExamplesWithNulls(table)
  # table = featureSelection.replaceContinuousNullsByMean(table)
  if (!length(features)) {
    return(0);
  }

  # Group all proyected examples in lists by their class
  # Get values of the features
  features <- unlist(features)
  feature.data <- data[, features, drop = FALSE]
  feature.classes <- as.data.frame(data[,class,drop = FALSE])
  feature.list <- unique(feature.classes)

  # Store the values (column 1) of each feature in a hash table
  hash.vector = as.factor(apply(feature.data, 1, digest))

  hash.table = list()
  for (i in 1:nrow(feature.list)) {
    hash.table[[i]] <- feature.data[0,]
  }
  for (i in 1:nrow(feature.data)) {
    classIndex <- which(feature.list == as.character(feature.classes[i,]))
    hash.table[[classIndex]] <- rbind(hash.table[[classIndex]], feature.data[i,])
  }

  covMatrix = cov(feature.data)
  det = determinant(covMatrix, logarithm = FALSE)

  m = ncol(data) - 1

  j = length(features)
  n = nrow(feature.data)
  hk = 0.5 * (m - j) * (m + j + 3) * log2(n)

  s <- 0

  for (examples in hash.table) {
    ni <- nrow(examples)
    covMatrix <- cov(examples)
    deti <- determinant(covMatrix, logarithm = FALSE)
    if ((det$modulus > 0 || det$modulus < 0) && (deti$modulus / det$modulus > 0)) {
      s <- s + ni / 2.0 * log2(deti$modulus / det$modulus)
      hk <- hk + j * (j + 3) * log2(ni)
    }
  }
  return(as.numeric(s + hk))
}
attr(MDLC,'shortName') <- "MDLC"
attr(MDLC,'name') <- "MDLC"
attr(MDLC,'maximize') <- TRUE
attr(MDLC,'kind') <- "Set measure"
