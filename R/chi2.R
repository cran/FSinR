# Measures based on the chi squared test. Individual Measures

#' Cramer V measure
#'
#' Calculates Cramer's V value \insertCite{Cramer1946}{FSinR}, evaluating features individually
#'
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param features - The feature or features to evalute individually
#'
#' @return - Cramer's V value for each selected feature
#' @references
#'    \insertAllCited{}
#' @export
#'
#' @examples
#' cramer(iris,'Species','Sepal.Length')
cramer <- function(data, class, features) {
    measures <- NULL

    # Get values of the features and the class
    features <- unlist(features)
    feature.data <- data[, features, drop = FALSE]
    classes <- c(data[[class]])

    # For each individual feature...
    for (i in feature.data) {
        # Count the number of times that each class is repeated for an attribute value
        joint <- table(classes, i)

        # Count the number of times the class 'j' appears
        row.sums <- rowSums(joint)

        # Count the number of times the value 'i' appears in the feature
        feature.count <- colSums(joint)

        # Calculate frequency of each class
        num.elements <- length(i)
        class.prob <- as.matrix(row.sums) / num.elements

        # Transpose matrix with t() function, and calculate matrix multiplication
        expected <- t(as.matrix(feature.count) %*% t(as.matrix(class.prob)))
        # (observed - expected)² / expected
        chi.squared <- sum(((joint - expected) ^ 2) / expected)

        # Calculate Cramer V measure: sqrt( (X²/n) / (min(k-1, r-1))
        if (length(row.sums) < 2 || length(feature.count) < 2 || chi.squared == 0) {
          result <- 0
        } else {
          k <- ncol(joint)
          r <- nrow(joint)
          cramer <- sqrt( (chi.squared / num.elements) / min(k - 1, r - 1))
          result <- cramer
        }

        # Store a value for each feature
        measures[[length(measures) + 1]] <- result
    }
    
    if (length(features) == 1) {
        return(measures[[1]])
    }
    names(measures) <- features
    return(measures)
}
attr(cramer,'name') <- "Cramer"
attr(cramer,'maximize') <- TRUE
attr(cramer,'kind') <- "Individual measure"

#' Chi squared measure
#'
#' Calculates the Chi squared value \insertCite{Pearson1900}{FSinR}, evaluating the selected features individually
#'
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param features - The feature or features to evalute individually
#'
#' @return - The chi squared value for each selected feature
#' @references
#'    \insertAllCited{}
#' @export
#'
#' @examples
#' chiSquared(iris,'Species','Sepal.Length')
chiSquared <- function(data, class, features) {
    measures <- NULL

    # Get values of the features and the class
    features <- unlist(features)
    feature.data <- data[, features, drop = FALSE]
    classes <- c(data[[class]])

    # For each individual feature...
    for (i in feature.data) {
        # Count the number of times that each class is repeated for an attribute value
        joint <- table(classes, i)

        # Count the number of times the class 'j' appears
        row.sums <- rowSums(joint)

        # Count the number of times the value 'i' appears in the feature
        feature.count <- colSums(joint)

        # Calculate frequency of each class
        num.elements <- length(i)
        class.prob <- as.matrix(row.sums) / num.elements

        # Transpose matrix with t() function, and calculate matrix multiplication
        expected <- t(as.matrix(feature.count) %*% t(as.matrix(class.prob)))
        chi.squared <- sum(((joint - expected) ^ 2) / expected)

        # Store a value for each feature
        measures[[length(measures) + 1]] <- chi.squared
    }
    if (length(features) == 1) {
        return(measures[[1]])
    }
    names(measures) <- features
    return(measures)
}
attr(chiSquared,'name') <- "Chi Squared"
attr(chiSquared,'maximize') <- TRUE
attr(chiSquared,'kind') <- "Individual measure"
