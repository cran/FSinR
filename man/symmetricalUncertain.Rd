% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entropy.R
\name{symmetricalUncertain}
\alias{symmetricalUncertain}
\title{Symmetrical uncertain measure}
\usage{
symmetricalUncertain(data, class, features)
}
\arguments{
\item{data}{\itemize{
\item A data frame with the features and the class of the examples.  Feature columns are expected to be factors, as all features should be discrete.
}}

\item{class}{\itemize{
\item The name of the dependent variable
}}

\item{features}{\itemize{
\item The names of the selected features
}}
}
\value{
\itemize{
\item The symmetrical uncertain value for the selected features
}
}
\description{
This measure calculates the symmetrical uncertain value \insertCite{WittenFrank2005}{FSinR}, using the information theory.
}
\examples{
symmetricalUncertain(iris,'Species',c('Sepal.Width', 'Sepal.Length'))
}
\references{
\insertAllCited{}
}
\author{
Adan M. Rodriguez
}