#include <Rcpp.h>
using namespace Rcpp;

// to make a vector of values depending on the approximation needed
NumericVector linear_spaced_array(double a, double b, size_t k){
    double h = (b - a) / (k-1);
    NumericVector xs(k);
    double val = a;
    for (size_t i=0; i < k; ++i, val += h) {
         xs[i] = val;
    }
    return xs;
}
// cube expression
double cubed(const double &exp){
    return exp * exp * exp;
}
// calculate (x)+
// return x if positive and 0 if negative
template <typename T>
T min0(const T &u){
    if(u > 0.0) return u;
    else return 0.0;
}
//' A function to create an M matrix to help with
//' restricted cubic spline calculations
//'
//' @param precision how precise the calculation should be;
//' it will the be the number of rows in the output
//' @param k the number of knots used
//' @param dimensions the number of dimentions to create
//' (this grows the size VERY fast)
//' @param max the maximum value of the x values; default = 1
//' @param first_knot position of the first knot; default = 0.05 * max
//' @param last_knot position of the last knot; default = 0.95 * max
//' @return a matrix of size precision by k
//' @examples
//' make_matrix(1000, 5)
//' @export
// [[Rcpp::export]]
NumericMatrix make_matrix(
    const int &precision,
    const int &k,
    const int &dimensions = 1,
    const double &max = 1,
    const double &first_knot = 0.05,
    const double &last_knot = 0.95
    ) {
  // set the knots
  NumericVector t = linear_spaced_array(first_knot * max, last_knot * max, k);
  // set precision intervals
  NumericVector R = linear_spaced_array(0.0 * max, 1.0 * max, precision);

  // initialize basis matrix M with the correct size and filled with zeros
  NumericMatrix M(precision, k);

  // populate basis matrix
  for(size_t i=0; i < precision; ++i){
      M(i, 0) = 1.0;
      M(i, 1) = R[i];
      for (size_t j = 2; j < k; ++j){
          M(i, j) = cubed(min0(R[i] - t[j-2]));
          M(i, j) -= cubed(min0(R[i] - t[k - 2])) * (t[k - 1] - t[j-2]) / (t[k - 1] - t[k - 2]);
          M(i, j) += cubed(min0(R[i] - t[k - 1])) * (t[k - 2] - t[j-2]) / (t[k - 1] - t[k - 2]);
          M(i, j) /= (t[k - 1] - t[0]) * (t[k - 1] - t[0]);
      }
  }

  const size_t tensor_p = pow(precision, dimensions);
  const size_t tensor_k = pow(k, dimensions);
	NumericMatrix T(tensor_p, tensor_k);

  // build the T matrix by multiplication
  for(size_t i = 0; i < tensor_p; ++i){
      for(size_t j = 0; j < tensor_k; ++j){
          for(size_t var = 0; var < dimensions; ++var){
              if(var == 0){
                  T(i, j) = M(i % precision, j % k);
              }
              else{
                  const size_t scale_i = precision * pow(2, (var - 1));
                  const size_t scale_j = k * pow(2, (var - 1));
                  T(i, j) *= M(i / scale_i, j / scale_j);
              }
          }
      }
  } 
  return T;
}

