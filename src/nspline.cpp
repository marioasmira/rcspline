// Copyright 2026 'rcspline' authors. See repository licence in LICENSE.md.
#include <Rcpp.h>
// Can't use 'using-declarations' because the Rcpp '_' symbol is used
using namespace Rcpp;

#include <algorithm>

//' A function that takes a vector and compatible matrix and
//' returns an `n` dimensional restricted cubic spline
//'
//' @param vector A vector
//' @param matrix The M matrix to calculate the restricted cubic spline;
//' number of columns should be the same as the length of the vector
//' @param dimensions The number of dimensions in the system
//' @param precision The precision of the system
//' @return Vector/matrix with the spline values; the length of the return
//' object is the same as the precision used to created the matrix; the
//' dimensions of the object will be the same as the input
//' @examples
//' precision <- 100
//' k <- 5
//' dimensions <- 2
//' v <- rnorm(k^2)
//' M <- make_matrix(precision, k, dimensions)
//' nspline(v, M, dimensions, precision)
//' @export
// [[Rcpp::export]]
NumericVector nspline(const NumericVector &vector,
                        const NumericMatrix &matrix,
                        const int &dimensions,
                        const int & precision) {
  // make sure dimensions are positive
  if (dimensions < 1 || precision < 0) {
    stop(
      "Dimensions and precision should be greater than zero.");
  }
  // make vector to calculated output dimensions, size d and elements p
  IntegerVector dims(dimensions, precision);
  // set up output matrix (total) size and dimensions
  NumericVector output(matrix.nrow(), 0.0);
  output.attr("dim") = dims;

  // all members of the output can be accessed linearly
  // should match the same order as `make_matrix()`
  for (int i = 0; i < output.length(); ++i) {
    output[i] = std::inner_product(
      matrix(i, _).begin(), matrix(i, _).end(), vector.begin(), 0.0);
  }

  return output;
}
