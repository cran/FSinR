% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/consistency.R
\name{IEConsistency}
\alias{IEConsistency}
\title{Inconsistent Examples consistency measure}
\usage{
IEConsistency()
}
\value{
Returns a function that is used to generate an evaluation set measure using the inconsistent examples consistency value for the selected features.
}
\description{
Generates an evaluation function that calculates the inconsistent examples consistency value \insertCite{DashLiu2003}{FSinR}, using hash tables (set measure). This function is called internally within the \code{\link{filterEvaluator}} function.
}
\examples{
\dontrun{ 

## The direct application of this function is an advanced use that consists of using this 
# function directly to evaluate a set of features
## Classification problem

# A discrete dataset is used (in this case we use only several discrete columns)
adult <- adult[,c(4,9,10,15)]

# Generate the evaluation function with IE Consistency
IEC_evaluator <- IEConsistency()
# Evaluate the features (parameters: dataset, target variable and features)
IEC_evaluator(adult,'income',c('race','sex','education'))
}
}
\references{
\insertAllCited{}
}
\author{
Adan M. Rodriguez
}
