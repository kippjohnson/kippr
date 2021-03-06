% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/goodnames.R
\name{goodnames}
\alias{goodnames}
\title{Make good names}
\usage{
goodnames(string)
}
\arguments{
\item{string}{The string(s) to make into good names}
}
\description{
Generates good names: lower-cased and appropriate for R
}
\examples{
bad.names <- c("ABC","A bad name", "A + B")
goodnames(bad.names)
}
