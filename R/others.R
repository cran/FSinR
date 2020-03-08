#' Normalize the data (without the class)

#' @author Adan M. Rodriguez
#' @title Normalize a data frame
#' @description Takes in any data frame and normalize the data of their features
#' @param data - A data frame with the features and the class of the examples
#' @param class - The dependent variable
#'
#' @return - The dataframe with the independent variables or features normalized
#' @export
#'
#' @examples
#' normalization(iris,'Species')
normalization <- function(data, class) {
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  class.name <- colnames(data[class]) 
  data <- data[ ,!colnames(data) == class.name]

  # Normalize each feature
  normalized.data <- as.data.frame(lapply(data, normalize))

  return(normalized.data)
}
