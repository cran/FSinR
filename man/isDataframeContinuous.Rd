% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discrete.R
\name{isDataframeContinuous}
\alias{isDataframeContinuous}
\title{isDataframeContinuous(dataframe)}
\usage{
isDataframeContinuous(dataframe)
}
\arguments{
\item{dataframe}{\itemize{
\item A data frame
}}
}
\value{
\itemize{
\item True if all variables are continuous, False otherwise
}
}
\description{
Estimate if all variables in a data frame are continuous
}
\examples{
isDataframeContinuous(mtcars)
isDataframeContinuous(iris)
}
\author{
Alfonso Jiménez Vílchez
}
