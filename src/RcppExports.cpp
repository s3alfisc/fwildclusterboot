// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// eigenMatMult
SEXP eigenMatMult(Eigen::MatrixXd A, Eigen::MatrixXd B, int nthreads);
RcppExport SEXP _fwildclusterboot_eigenMatMult(SEXP ASEXP, SEXP BSEXP, SEXP nthreadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type A(ASEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type B(BSEXP);
    Rcpp::traits::input_parameter< int >::type nthreads(nthreadsSEXP);
    rcpp_result_gen = Rcpp::wrap(eigenMatMult(A, B, nthreads));
    return rcpp_result_gen;
END_RCPP
}
// eigenMapMatMult
SEXP eigenMapMatMult(const Eigen::Map<Eigen::MatrixXd> A, Eigen::Map<Eigen::MatrixXd> B, int nthreads);
RcppExport SEXP _fwildclusterboot_eigenMapMatMult(SEXP ASEXP, SEXP BSEXP, SEXP nthreadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd> >::type A(ASEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::MatrixXd> >::type B(BSEXP);
    Rcpp::traits::input_parameter< int >::type nthreads(nthreadsSEXP);
    rcpp_result_gen = Rcpp::wrap(eigenMapMatMult(A, B, nthreads));
    return rcpp_result_gen;
END_RCPP
}
// cpp_get_nb_threads
int cpp_get_nb_threads();
RcppExport SEXP _fwildclusterboot_cpp_get_nb_threads() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(cpp_get_nb_threads());
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fwildclusterboot_eigenMatMult", (DL_FUNC) &_fwildclusterboot_eigenMatMult, 3},
    {"_fwildclusterboot_eigenMapMatMult", (DL_FUNC) &_fwildclusterboot_eigenMapMatMult, 3},
    {"_fwildclusterboot_cpp_get_nb_threads", (DL_FUNC) &_fwildclusterboot_cpp_get_nb_threads, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_fwildclusterboot(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
