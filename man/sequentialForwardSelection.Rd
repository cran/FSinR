% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sequentialSelection.R
\name{sequentialForwardSelection}
\alias{sequentialForwardSelection}
\title{Sequential Forward Selection}
\usage{
sequentialForwardSelection(stopCriterion = -1, stop = FALSE)
}
\arguments{
\item{stopCriterion}{Define a maximum number of iterations. Disabled if the value is -1 (default: -1 )}

\item{stop}{If true, the function stops if next iteration does not improve current results (default: FALSE)}
}
\value{
Returns a search function that is used to guide the feature selection process.
}
\description{
Generates a search function based on sequential forward selection. This function is called internally within the \code{\link{searchAlgorithm}} function. The SFS method \insertCite{Whitney1971}{FSinR} starts with an empty set of features and add a single feature at each step with a view to improving the evaluation of the set.
}
\examples{
\dontrun{ 

## The direct application of this function is an advanced use that consists of using this 
# function directly and performing a search process in a feature space
## Classification problem

# Generates the filter evaluation function with sfs
filter_evaluator <- filterEvaluator('determinationCoefficient')

# Generates the search function
sfs_search <- sequentialForwardSelection()
# Performs the search process directly (parameters: dataset, target variable and evaluator)
sfs_search(iris, 'Species', filter_evaluator)
}
}
\references{
\insertAllCited{}
}
\author{
Adan M. Rodriguez

Alfonso Jiménez-Vílchez

Francisco Aragón Royón
}
