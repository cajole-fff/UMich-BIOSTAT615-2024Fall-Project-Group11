// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// metric_adjusted_rand_index_cpp
double metric_adjusted_rand_index_cpp(Rcpp::IntegerVector true_labels, Rcpp::IntegerVector pred_labels);
RcppExport SEXP _DBSCAN615_metric_adjusted_rand_index_cpp(SEXP true_labelsSEXP, SEXP pred_labelsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type true_labels(true_labelsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type pred_labels(pred_labelsSEXP);
    rcpp_result_gen = Rcpp::wrap(metric_adjusted_rand_index_cpp(true_labels, pred_labels));
    return rcpp_result_gen;
END_RCPP
}
// metric_silhouette_score_cpp
double metric_silhouette_score_cpp(Rcpp::RObject X, Rcpp::IntegerVector labels);
RcppExport SEXP _DBSCAN615_metric_silhouette_score_cpp(SEXP XSEXP, SEXP labelsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type X(XSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type labels(labelsSEXP);
    rcpp_result_gen = Rcpp::wrap(metric_silhouette_score_cpp(X, labels));
    return rcpp_result_gen;
END_RCPP
}
// util_dbscan_fit_cpp
List util_dbscan_fit_cpp(SEXP X, double eps, int min_samples, std::string metric, List metric_params, std::string algorithm, int leaf_size, double p, int n_jobs);
RcppExport SEXP _DBSCAN615_util_dbscan_fit_cpp(SEXP XSEXP, SEXP epsSEXP, SEXP min_samplesSEXP, SEXP metricSEXP, SEXP metric_paramsSEXP, SEXP algorithmSEXP, SEXP leaf_sizeSEXP, SEXP pSEXP, SEXP n_jobsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< int >::type min_samples(min_samplesSEXP);
    Rcpp::traits::input_parameter< std::string >::type metric(metricSEXP);
    Rcpp::traits::input_parameter< List >::type metric_params(metric_paramsSEXP);
    Rcpp::traits::input_parameter< std::string >::type algorithm(algorithmSEXP);
    Rcpp::traits::input_parameter< int >::type leaf_size(leaf_sizeSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type n_jobs(n_jobsSEXP);
    rcpp_result_gen = Rcpp::wrap(util_dbscan_fit_cpp(X, eps, min_samples, metric, metric_params, algorithm, leaf_size, p, n_jobs));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_DBSCAN615_metric_adjusted_rand_index_cpp", (DL_FUNC) &_DBSCAN615_metric_adjusted_rand_index_cpp, 2},
    {"_DBSCAN615_metric_silhouette_score_cpp", (DL_FUNC) &_DBSCAN615_metric_silhouette_score_cpp, 2},
    {"_DBSCAN615_util_dbscan_fit_cpp", (DL_FUNC) &_DBSCAN615_util_dbscan_fit_cpp, 9},
    {NULL, NULL, 0}
};

RcppExport void R_init_DBSCAN615(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
