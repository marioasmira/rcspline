#include <Rcpp.h>
using namespace Rcpp;

#include <algorithm>

//' A function that takes a vector and compatable matrix and
//' returns a restricted cubic spline
//'
//' @param vector A vector
//' @param matrix The M matrix to calculate the restricted cubic spline;
//' number of columns should be the same as the length of the vector
//' @return Vector with the spline values; length is the same as the number
//' of rows in matrix
//' @examples
//' v <- runif(5)
//' M <- make_matrix(1000, 5)
//' spline_1d(v, M)
//' @export
//[[Rcpp::export]]
NumericVector spline_1d(
    const NumericVector &vector, const NumericMatrix &matrix)
{
  NumericVector y(matrix.nrow());
  for (int i = 0; i < matrix.nrow(); i++)
  {
    NumericVector v = matrix(i, _);
    y[i] = std::inner_product(v.begin(), v.end(), vector.begin(), 0.0);
  }
  return y;
}