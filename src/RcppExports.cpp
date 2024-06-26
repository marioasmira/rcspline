// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// make_matrix
NumericMatrix make_matrix(const int& precision, const int& k, const int& dimensions, const double& min, const double& max, const double& first_knot, const double& last_knot);
RcppExport SEXP _rcspline_make_matrix(SEXP precisionSEXP, SEXP kSEXP, SEXP dimensionsSEXP, SEXP minSEXP, SEXP maxSEXP, SEXP first_knotSEXP, SEXP last_knotSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int& >::type precision(precisionSEXP);
    Rcpp::traits::input_parameter< const int& >::type k(kSEXP);
    Rcpp::traits::input_parameter< const int& >::type dimensions(dimensionsSEXP);
    Rcpp::traits::input_parameter< const double& >::type min(minSEXP);
    Rcpp::traits::input_parameter< const double& >::type max(maxSEXP);
    Rcpp::traits::input_parameter< const double& >::type first_knot(first_knotSEXP);
    Rcpp::traits::input_parameter< const double& >::type last_knot(last_knotSEXP);
    rcpp_result_gen = Rcpp::wrap(make_matrix(precision, k, dimensions, min, max, first_knot, last_knot));
    return rcpp_result_gen;
END_RCPP
}
// spline_1d
NumericVector spline_1d(const NumericVector& vector, const NumericMatrix& matrix);
RcppExport SEXP _rcspline_spline_1d(SEXP vectorSEXP, SEXP matrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type vector(vectorSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type matrix(matrixSEXP);
    rcpp_result_gen = Rcpp::wrap(spline_1d(vector, matrix));
    return rcpp_result_gen;
END_RCPP
}
// spline_2d
NumericMatrix spline_2d(const NumericVector& vector, const NumericMatrix& matrix);
RcppExport SEXP _rcspline_spline_2d(SEXP vectorSEXP, SEXP matrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type vector(vectorSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type matrix(matrixSEXP);
    rcpp_result_gen = Rcpp::wrap(spline_2d(vector, matrix));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rcspline_make_matrix", (DL_FUNC) &_rcspline_make_matrix, 7},
    {"_rcspline_spline_1d", (DL_FUNC) &_rcspline_spline_1d, 2},
    {"_rcspline_spline_2d", (DL_FUNC) &_rcspline_spline_2d, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_rcspline(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
