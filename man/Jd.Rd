% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Jd.R
\name{Jd}
\alias{Jd}
\title{Jd evaluation measure}
\usage{
Jd()
}
\value{
Returns a function that is used to generate an evaluation set measure using the Jd.
}
\description{
Generates an evaluation function that applies the discriminant function designed by Narendra and Fukunaga \insertCite{Narendra1977}{FSinR} to generate an evaluation measure for a set of features (set measure). This function is called internally within the \code{\link{filterEvaluator}} function.
}
\examples{
\dontrun{ 

## The direct application of this function is an advanced use that consists of using this 
# function directly to evaluate a set of features
## Classification problem

# Generate the evaluation function with JD
Jd_evaluator <- Jd()
# Evaluate the features (parametes: dataset, target variable and features)
Jd_evaluator(ToothGrowth,'supp',c('len','dose'))
}
}
\references{
\insertAllCited{}
}
\author{
Alfonso Jiménez-Vílchez
}
