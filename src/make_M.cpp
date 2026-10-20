#include <Rcpp.h>
using namespace Rcpp;

//' Function to make a vector of values with specific starting and ends points,
//' and length. Behaviour in R should be similar to seq(). Used only in C++ code
//' and package tests.
//'
//' @param a Lowest value
//' @param b Highest value
//' @param k Length of the vector desired
//' @return A vector where the first and last members correspond to a and b,
//' respectively, with other members evenly spaced between them and a length of
//' k
//' @examples
//' linear_spaced_array(0.05, 0.95, 5)
// [[Rcpp::export]]
NumericVector linear_spaced_array(double a, double b, size_t k)
{
  double h = (b - a) / (k - 1);
  NumericVector xs(k);
  double val = a;
  for (size_t i = 0; i < k; ++i, val += h)
  {
    xs[i] = val;
  }
  return xs;
}
// cube expression
double cubed(const double &exp)
{
  return exp * exp * exp;
}
// calculate (x)+
// return x if positive and 0 if negative
template <typename T>
T min0(const T &u)
{
  if (u > 0.0)
    return u;
  else
    return 0.0;
}

//' Basis function implemented from Harrell, F. (2001). Regression Modeling
//' Strategies: With Applications to Linear Models, Logistic Regression, and
//' Survival Analysis. Springer-Verlag.
//' https://www.springer.com/gp/book/9781441929181
//' Used internally and for tests.
//'
//' @param R_i ith member of the precision vector (row)
//' @param t Knots vector
//' @param j Position to calculate the basis starting at zero (column);
//' different from the source implementation since here it varies between 2 and
//' k, instead of between 1 and k-1
//' @return The calculated basis function for the provided member of the
//' precision vector (R_i) and the specified column (j)
// [[Rcpp::export]]
double basis(
  const double &R_i,
  const NumericVector &t,
  const size_t &j
)
{
  const size_t k = t.length();
  double calc = 0.0;
  calc = cubed(min0(R_i - t[j - 2]));
  calc -= cubed(min0(R_i - t[k - 2])) * (t[k - 1] - t[j - 2]) / (t[k - 1] - t[k - 2]);
  calc += cubed(min0(R_i - t[k - 1])) * (t[k - 2] - t[j - 2]) / (t[k - 1] - t[k - 2]);
  calc /= (t[k - 1] - t[0]) * (t[k - 1] - t[0]);
  return calc;
}


//' A function to create an M matrix to help with
//' restricted cubic spline calculations
//'
//' @param precision how precise the calculation should be;
//' it will the be the number of rows in the output
//' @param k the number of knots used
//' @param dimensions the number of dimentions to create
//' (this grows the size VERY fast)
//' @param min the minimum value of the x values; default = 0
//' @param max the maximum value of the x values; default = 1
//' @param first_knot position of the first knot as a fraction of max; default = 0.05
//' @param last_knot position of the last knot as a fraction of max; default = 0.95
//' @return a matrix of size precision by k
//' @examples
//' make_matrix(1000, 5)
//' @export
// [[Rcpp::export]]
NumericMatrix make_matrix(
    const int &precision,
    const int &k,
    const int &dimensions = 1,
    const double &min = 0,
    const double &max = 1,
    const double &first_knot = 0.05,
    const double &last_knot = 0.95)
{
  // throw error if parameters are outside of scope
  if ((precision < 1) || (k < 1) || (dimensions < 1))
  {
    stop("Integer parameters (precision, k, and dimensions) should be greater than zero.");
  }
  // explicit cast into size_t if greater than zero
  const size_t s_precision = static_cast<size_t>(precision);
  const size_t s_k = static_cast<size_t>(k);
  const size_t s_dimensions = static_cast<size_t>(dimensions);

  // throw error if negative ranges for vectors
  if ((min > max) || (first_knot > last_knot))
  {
    stop("Function not defined for negative ranges (min > max or first_knot > last_knot).");
  }

  // throw error if negative values for continuous values
  if ((min < 0) || (max < 0) || (first_knot < 0) || (last_knot < 0))
  {
    stop("Function not defined for negative values.");
  }
  // set the knots
  NumericVector t = linear_spaced_array(min + first_knot * (max - min), min + last_knot * (max - min), s_k);
  // set precision intervals
  NumericVector R = linear_spaced_array(min, max, s_precision);

  // initialize basis matrix M with the correct size and filled with zeros
  NumericMatrix M(s_precision, s_k);

  // populate basis matrix
  for (size_t i = 0; i < s_precision; ++i)
  {
    M(i, 0) = 1.0;
    M(i, 1) = R[i];
    for (size_t j = 2; j < s_k; ++j)
    {
      M(i, j) = basis(R[i], t, j);
    }
  }

  // return basis matrix if only using one dimension
  if (s_dimensions == 1)
  {
    return M;
  }
  // otherwise continue to make the tensor product
  const size_t tensor_p = pow(s_precision, s_dimensions);
  const size_t tensor_k = pow(s_k, s_dimensions);
  NumericMatrix T(tensor_p, tensor_k);

  // build the T matrix by multiplication
  for (size_t i = 0; i < tensor_p; ++i)
  {
    for (size_t j = 0; j < tensor_k; ++j)
    {
      for (size_t var = 0; var < s_dimensions; ++var)
      {
        if (var == 0)
        {
          T(i, j) = M(i % s_precision, j % s_k);
        }
        else
        {
          const size_t scale_i = pow(s_precision, var);
          const size_t scale_j = pow(s_k, var);
          T(i, j) *= M((i / scale_i) % s_precision, (j / scale_j) % s_k);
        }
      }
    }
  }
  return T;
}
