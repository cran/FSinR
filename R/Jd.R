#' @author Alfonso Jiménez-Vílchez
#' @title Jd evaluation measure
#' @description Applies the discriminant function designed by Narendra and Fukunaga \insertCite{Narendra1977}{FSinR} to evaluate a set of features.
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param features - The names of the selected features
#'
#' @return - The value of the function for the selected features
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @import dplyr
#' @importFrom stats cov
#' @import rlang
#' @importFrom rlang UQ
#' @export
#'
#' @examples
#' Jd(ToothGrowth,'supp',c('len','dose'))
Jd <- function(data, class, features) {

  if (!length(features)) {
    return(0);
  }

  feature.classes <- unique(as.data.frame(data[,class,drop = FALSE]))
  if (nrow(feature.classes) != 2) {
    stop('Data set is required to have only 2 classes');
  }

  vectors <- data %>%
    select(features, class) %>%
    group_by_at(class) %>%
    summarise_at(features,list(mean)) %>%
    select(features)
  vector <- unlist(vectors[1,] - vectors[2,])

  matrixA  <- data %>%
    filter(UQ(as.name(class)) == feature.classes[1,1]) %>%
    select(features) %>%
    as.matrix() %>%
    cov()

  matrixB  <- data %>%
    filter(UQ(as.name(class)) == feature.classes[2,1]) %>%
    select(features) %>%
    as.matrix() %>%
    cov()

  return (as.numeric(t(vector) %*% solve((matrixA + matrixB)/2) %*% vector))
}
attr(Jd,'shortName') <- "Jd"
attr(Jd,'name') <- "Jd"
attr(Jd,'maximize') <- TRUE
attr(Jd,'kind') <- "Set measure"
