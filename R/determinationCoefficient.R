# Measures for regression problems, where the class is continous

#' @author Adan M. Rodriguez
#' @title R Squared, to continous features
#' @description This measure calculates the determinantion coefficient \insertCite{rsquared}{FSinR} of continuous features
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param features - The names of the selected features
#'
#' @return - The R squared value for the selected features
#' @references
#'    \insertAllCited{}
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @export
#'
#' @examples
#' determinationCoefficient(iris,'Species',c('Sepal.Width', 'Sepal.Length'))
determinationCoefficient <- function(data, class, features) {
  # Create the formula formed with the features and the class
  #data2$class <- as.numeric(as.character(data2$class))
  data2 = data
  data2[,class] <- as.numeric(data2[,class]) # fix to transform factor to numeric
  formula <- as.formula(paste(class, "~", paste(features, collapse="+")))

  # Fit the linear model
  model <- lm(formula = formula, data = data2)

  # Take the measure
  value <- summary(model)$r.squared

  return(value)
}
suppressWarnings(warning("In summary.lm(model) : essentially perfect fit: summary may be unreliable"))
attr(determinationCoefficient,'name') <- "Determination Coefficient"
attr(determinationCoefficient,'maximize') <- TRUE
attr(determinationCoefficient,'kind') <- "Set measure"
