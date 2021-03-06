% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LCC.R
\name{LCC}
\alias{LCC}
\title{Linear Consistency-Constrained algorithm}
\usage{
LCC(threshold = 0.9)
}
\arguments{
\item{threshold}{Threshold}
}
\value{
Returns a hybrid search function that is used to guide the feature selection process.
}
\description{
Generates a hybrid search function based on Linear Consistency-Constrained algorithm described in \insertCite{ShinXu2009}{FSinR}. The algorithm combines two evaluation measures, the first evaluates each feature individually, and the second measure evaluate feature sets.
}
\examples{
\dontrun{ 

## The direct application of this function is an advanced use that consists of using this 
# function directly and performing a hybrid search process in a feature space
## Classification problem

# Generates the first filter evaluation function (individual or set measure)
filter_evaluator_1 <- filterEvaluator('determinationCoefficient')
# Generates the second filter evaluation function (mandatory set measure)
filter_evaluator_2 <- filterEvaluator('ReliefFeatureSetMeasure')

  
# Generates the hybrid search function with LCC
LCC_hybrid_search <- LCC()
# Run the search process directly (params: dataset, target variable, evaluator1 & evaluator2)
LCC_hybrid_search(iris, 'Species', filter_evaluator_1, filter_evaluator_2)
}
}
\references{
\insertAllCited{}
}
\author{
Alfonso Jiménez-Vílchez
}
