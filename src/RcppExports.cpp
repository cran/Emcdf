// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// getCoreNum
unsigned int getCoreNum();
RcppExport SEXP Emcdf_getCoreNum() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getCoreNum());
    return rcpp_result_gen;
END_RCPP
}
// build
RcppExport SEXP build(SEXP x_in, int num);
RcppExport SEXP Emcdf_build(SEXP x_inSEXP, SEXP numSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x_in(x_inSEXP);
    Rcpp::traits::input_parameter< int >::type num(numSEXP);
    rcpp_result_gen = Rcpp::wrap(build(x_in, num));
    return rcpp_result_gen;
END_RCPP
}
// compute
SEXP compute(SEXP& p_in, NumericVector& a);
RcppExport SEXP Emcdf_compute(SEXP p_inSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP& >::type p_in(p_inSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(compute(p_in, a));
    return rcpp_result_gen;
END_RCPP
}
// compute_m
SEXP compute_m(SEXP& p_in, NumericMatrix& a);
RcppExport SEXP Emcdf_compute_m(SEXP p_inSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP& >::type p_in(p_inSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_m(p_in, a));
    return rcpp_result_gen;
END_RCPP
}
// biemcdf_output
NumericMatrix biemcdf_output(NumericVector& x, NumericVector& y, bool is_tie);
RcppExport SEXP Emcdf_biemcdf_output(SEXP xSEXP, SEXP ySEXP, SEXP is_tieSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type y(ySEXP);
    Rcpp::traits::input_parameter< bool >::type is_tie(is_tieSEXP);
    rcpp_result_gen = Rcpp::wrap(biemcdf_output(x, y, is_tie));
    return rcpp_result_gen;
END_RCPP
}
// single_m
NumericVector single_m(NumericMatrix& data, const NumericMatrix& a);
RcppExport SEXP Emcdf_single_m(SEXP dataSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(single_m(data, a));
    return rcpp_result_gen;
END_RCPP
}
// single
int single(NumericMatrix& data, const NumericVector& a);
RcppExport SEXP Emcdf_single(SEXP dataSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(single(data, a));
    return rcpp_result_gen;
END_RCPP
}
// single1_m
NumericVector single1_m(const NumericMatrix& data, const NumericMatrix& a);
RcppExport SEXP Emcdf_single1_m(SEXP dataSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(single1_m(data, a));
    return rcpp_result_gen;
END_RCPP
}
// single1
int single1(const NumericMatrix& data, const NumericVector& a);
RcppExport SEXP Emcdf_single1(SEXP dataSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(single1(data, a));
    return rcpp_result_gen;
END_RCPP
}
// single2_m
NumericVector single2_m(const NumericMatrix& data, const NumericMatrix& a);
RcppExport SEXP Emcdf_single2_m(SEXP dataSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(single2_m(data, a));
    return rcpp_result_gen;
END_RCPP
}
// single2
int single2(const NumericMatrix& data, const NumericVector& a);
RcppExport SEXP Emcdf_single2(SEXP dataSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(single2(data, a));
    return rcpp_result_gen;
END_RCPP
}
// single3_m
NumericVector single3_m(const NumericMatrix& data, const NumericMatrix& a);
RcppExport SEXP Emcdf_single3_m(SEXP dataSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(single3_m(data, a));
    return rcpp_result_gen;
END_RCPP
}
// single3
int single3(const NumericMatrix& data, const NumericVector& a);
RcppExport SEXP Emcdf_single3(SEXP dataSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(single3(data, a));
    return rcpp_result_gen;
END_RCPP
}
// single4_m
NumericVector single4_m(const NumericMatrix& data, const NumericMatrix& a);
RcppExport SEXP Emcdf_single4_m(SEXP dataSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(single4_m(data, a));
    return rcpp_result_gen;
END_RCPP
}
// single4
int single4(const NumericMatrix& data, const NumericVector& a);
RcppExport SEXP Emcdf_single4(SEXP dataSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(single4(data, a));
    return rcpp_result_gen;
END_RCPP
}
// single5_m
NumericVector single5_m(const NumericMatrix& data, const NumericMatrix& a);
RcppExport SEXP Emcdf_single5_m(SEXP dataSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(single5_m(data, a));
    return rcpp_result_gen;
END_RCPP
}
// single5
int single5(const NumericMatrix& data, const NumericVector& a);
RcppExport SEXP Emcdf_single5(SEXP dataSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(single5(data, a));
    return rcpp_result_gen;
END_RCPP
}
// SortByX
NumericMatrix SortByX(NumericVector& x, NumericVector& y);
RcppExport SEXP Emcdf_SortByX(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(SortByX(x, y));
    return rcpp_result_gen;
END_RCPP
}
