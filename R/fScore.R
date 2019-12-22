#' F-score measure
#'
#' Evaluates a feature using the F-score approach defined in \insertCite{Wang2018}{FSinR}.
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param features - The name of the selected feature (only 1 feature)
#'
#' @return - The value of the function for the selected feature
#' @references
#'    \insertAllCited{}
#' @import dplyr
#' @export
#'
#' @examples
#' fscore(ToothGrowth, 'supp', c('len'))
fscore <- function(data, class, features) {

  if (!length(features)) {
    return(0);
  }

  feature.classes <- unique(as.data.frame(data[,class,drop = FALSE]))
  if (nrow(feature.classes) != 2) {
    stop('Data set is required to have only 2 classes');
  }

  
  measures <- NULL  
  for (feature in features) {
    x.mean = mean(data[,feature])
  
    x_plus  <- data %>%
      filter(UQ(as.name(class)) == feature.classes[1,1]) %>%
      select(feature) %>%
      as.matrix()
    x_plus.mean = mean(x_plus)
    x_plus.n = nrow(x_plus)
  
    x_minus  <- data %>%
      filter(UQ(as.name(class)) == feature.classes[2,1]) %>%
      select(feature) %>%
      as.matrix()
    x_minus.mean = mean(x_minus)
    x_minus.n = nrow(x_minus)
  
    x_plus.sum = 0
    for (x in x_plus) {
      x_plus.sum = x_plus.sum + (x - x_plus.mean)^2
    }
    x_plus.sum = x_plus.sum / (x_plus.n - 1)
  
    x_minus.sum = 0
    for (x in x_minus) {
      x_minus.sum = x_minus.sum + (x - x_minus.mean)^2
    }
    x_minus.sum = x_minus.sum / (x_minus.n - 1)
    measures[[length(measures) + 1]] <- ((x_plus.mean - x.mean)^2 + (x_minus.mean - x.mean)^2) / (x_plus.sum + x_minus.sum)
  }
  if (length(features) == 1) {
    return(measures[[1]])
  }
  names(measures) <- features
  return(measures)
}
attr(fscore,'name') <- "F-score"
attr(fscore,'maximize') <- TRUE
attr(fscore,'kind') <- "Individual measure"
