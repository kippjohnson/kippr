% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/head5.R
\name{head5}
\alias{head5}
\title{Print first 5 rows and first 5 columns}
\usage{
head5(df, nc = 5, nr = 5)
}
\arguments{
\item{df}{A data frame or data table}

\item{nc}{Number of columns to print, default 5}

\item{nr}{Number of rows to print, default 5}
}
\description{
A slightly modified head function for large datasets with many columns.
Works with dataframes and data tables
}
\examples{
abc <- data.table(matrix(1:2000, 20, 100))
head5(abc)
}
