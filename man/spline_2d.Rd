% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{spline_2d}
\alias{spline_2d}
\title{A function that takes a vector and compatable matrix and
returns a 2 dimensional restricted cubic spline}
\usage{
spline_2d(vector, matrix)
}
\arguments{
\item{vector}{A vector}

\item{matrix}{The M matrix to calculate the restricted cubic spline;
number of columns should be the same as the length of the vector}
}
\value{
Vector with the sline values; length is the same as the number
of rows in matrix
}
\description{
A function that takes a vector and compatable matrix and
returns a 2 dimensional restricted cubic spline
}
\examples{
v <- runif(25)
M <- make_matrix(1000, 5, 2)
spline_2d(v, M)
}
