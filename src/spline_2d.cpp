#include <Rcpp.h>
using namespace Rcpp;

#include <algorithm>


//' A function that takes a vector and compatable matrix and
//' returns a 2 dimensional restricted cubic spline
//'
//' @param vector A vector
//' @param matrix The M matrix to calculate the restricted cubic spline;
//' number of columns should be the same as the length of the vector
//' @return Vector with the sline values; length is the same as the number
//' of rows in matrix
//' @examples
//' v <- runif(25)
//' M <- make_matrix(1000, 5, 2)
//' spline_2d(v, M)
//' @export
//[[Rcpp::export]]
NumericMatrix spline_2d(
    const NumericVector &vector, const NumericMatrix &matrix
    ) {
    // get the precision from the dimensions of the provided matrix
    const int precision = static_cast<int>(sqrt(matrix.nrow()));
    NumericMatrix output(precision, precision);
    for (int i = 0; i < precision; ++i){
        for (int j = 0; j < precision; ++j){
            const int row = i + precision * j;
            output(i, j) = std::inner_product(matrix(row, _ ).begin(), matrix(row, _ ).end(), vector.begin(), 0.0);
        }
    }
    return output;
}

