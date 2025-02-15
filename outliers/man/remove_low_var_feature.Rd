% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{remove_low_var_feature}
\alias{remove_low_var_feature}
\title{A function filtering out samples having a variance smaller than min_var}
\usage{
remove_low_var_feature(data, min_var = 0)
}
\arguments{
\item{data}{the original dataframe}

\item{min_var}{the minimal accepted variance. default to 0 as constant features does not impact outliers.}
}
\description{
A function filtering out samples having a variance smaller than min_var
}
\keyword{filter}
\keyword{variance,}
